;;; etaf-telega-backend.el --- Telegram backends for etaf-telega -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; A backend reports connection state, chats, one chat's history and sends
;; text.  It owns no buffers and never reads the model; the application passes
;; a CALLBACKS plist and projects the answers:
;;
;;   :status   (string)                 connection state for the header
;;   :me       (plist)                  own account (:id :name)
;;   :chats    (list of chat plists)    (:id :title :preview :unread)
;;   :messages (list of message plists) (:id :outgoing :sender :time :text)
;;             newest first, exactly like TDLib history; the application
;;             reverses them into reading order for display
;;   :sent     (message plist)          accepted outgoing message
;;   :error    (string)                 readable failure
;;
;; `etaf-telega-backend-telega' starts and observes Telega's own server and
;; TDLib event stream.  Telega keeps the account, database and session; this
;; backend only projects what the interface needs.  `etaf-telega-backend-fake'
;; replays fixed data for tests and never touches the network.

;;; Code:

(require 'cl-lib)
(require 'etaf-telega-model)

(cl-defgeneric etaf-telega-backend-start (backend callbacks)
  "Start BACKEND and report through CALLBACKS.  Return BACKEND.")

(cl-defgeneric etaf-telega-backend-select (backend chat-id)
  "Make CHAT-ID current in BACKEND and load its recent history.")

(cl-defgeneric etaf-telega-backend-send (backend text)
  "Send TEXT through BACKEND.")

(cl-defgeneric etaf-telega-backend-stop (backend)
  "Release BACKEND's connection without touching Telega's account data.")

;;; Fake backend

(cl-defstruct etaf-telega-backend-fake
  chats histories sent responder callbacks)

(cl-defun etaf-telega-backend-fake-create (&key chats histories responder)
  "Create a fake backend seeded with CHATS and HISTORIES.
HISTORIES is a hash table from chat id to a list of message plists.  When
RESPONDER is non-nil it is called with (BACKEND TEXT) after each send, so a
test can acknowledge the message through `etaf-telega-backend-fake-complete'."
  (make-etaf-telega-backend-fake
   :chats chats :histories (or histories (make-hash-table :test #'equal))
   :responder responder :sent nil))

(cl-defmethod etaf-telega-backend-start ((backend etaf-telega-backend-fake)
                                         callbacks)
  "Seed CALLBACKS from BACKEND's fixed data."
  (setf (etaf-telega-backend-fake-callbacks backend) callbacks)
  (when-let* ((report (plist-get callbacks :status)))
    (funcall report "就绪"))
  (when-let* ((report (plist-get callbacks :me)))
    (funcall report (list :id 1 :name "Fixture")))
  (when-let* ((report (plist-get callbacks :chats)))
    (funcall report (copy-tree (etaf-telega-backend-fake-chats backend))))
  backend)

(cl-defmethod etaf-telega-backend-select ((backend etaf-telega-backend-fake)
                                          chat-id)
  "Report BACKEND's fixed history for CHAT-ID."
  (when-let* ((report (plist-get (etaf-telega-backend-fake-callbacks backend)
                                 :messages)))
    (funcall report
             (copy-tree (gethash chat-id
                                 (etaf-telega-backend-fake-histories backend)))))
  chat-id)

(cl-defmethod etaf-telega-backend-send ((backend etaf-telega-backend-fake) text)
  "Record TEXT and invoke BACKEND's responder."
  (push text (etaf-telega-backend-fake-sent backend))
  (when-let* ((responder (etaf-telega-backend-fake-responder backend)))
    (funcall responder backend text))
  text)

(cl-defmethod etaf-telega-backend-stop ((_backend etaf-telega-backend-fake))
  "Fake backends own no connection.")

(defun etaf-telega-backend-fake-complete (backend text)
  "Report TEXT as accepted by BACKEND, exactly like a server acknowledgement."
  (when-let* ((report (plist-get (etaf-telega-backend-fake-callbacks backend)
                                 :sent)))
    (funcall report (list :id (1+ (length (etaf-telega-backend-fake-sent backend)))
                          :outgoing t :sender "我"
                          :time (format-time-string "%H:%M")
                          :text text))))

;;; Telega backend

(declare-function telega "telega" (&optional arg))
(declare-function telega-server-live-p "telega-server" ())
(declare-function telega-chat-get "telega-chat" (chat-id &optional offline-p))
(declare-function telega-chat-title "telega-chat" (chat &optional fmt-type no-badges))
(declare-function telega--getChatHistory "telega-tdlib"
                  (chat from-msg-id offset &optional limit only-local callback))
(declare-function telega--sendMessage "telega-tdlib" (chat imc &rest args))
(declare-function telega-msg-content-text "telega-msg"
                  (msg &optional with-speech-recognition-p))
(declare-function telega-msg-sender "telega-msg" (tl-obj))
(declare-function telega-msg-sender-title "telega-msg" (msg-sender &rest args))
(defvar telega--chats)
(defvar telega-ready-hook)
(defvar telega-chat-update-hook)

(cl-defstruct (etaf-telega-backend-telega
               (:constructor etaf-telega-backend-telega-create))
  callbacks chat-id)

(defvar etaf-telega-backend-telega-instances nil
  "Live telega backends observing the shared Telega event stream.")

(defun etaf-telega-backend-telega--project-chat (chat)
  "Return CHAT's projection plist."
  (list :id (plist-get chat :id)
        :title (or (ignore-errors (telega-chat-title chat)) "?")
        :preview ""
        :unread (or (plist-get chat :unread_count) 0)
        :order (or (plist-get chat :order) 0)))

(defun etaf-telega-backend-telega--project-message (message)
  "Return MESSAGE's projection plist."
  (list :id (plist-get message :id)
        :outgoing (eq t (plist-get message :is_outgoing))
        :sender (or (ignore-errors (telega-msg-sender-title
                                    (telega-msg-sender message)))
                    "")
        :time (format-time-string "%H:%M"
                                  (seconds-to-time (or (plist-get message :date) 0)))
        :text (or (ignore-errors (telega-msg-content-text message)) "")))

(defun etaf-telega-backend-telega--chats ()
  "Return all known chats as projections, newest first."
  (let (result)
    (maphash (lambda (_id chat)
               (push (etaf-telega-backend-telega--project-chat chat) result))
             telega--chats)
    (sort result (lambda (a b)
                   (> (or (plist-get a :order) 0)
                      (or (plist-get b :order) 0))))))

(cl-defun etaf-telega-backend-telega--report (backend &key status chats me sent)
  "Report BACKEND's current facts through its callbacks.
Only the supplied keys are reported; STATUS and CHATS are recomputed when
requested so every live backend sees the shared Telega state."
  (let ((callbacks (etaf-telega-backend-telega-callbacks backend)))
    (when (and status (plist-get callbacks :status))
      (funcall (plist-get callbacks :status) status))
    (when (and me (plist-get callbacks :me))
      (funcall (plist-get callbacks :me) me))
    (when (and chats (plist-get callbacks :chats))
      (funcall (plist-get callbacks :chats)
               (etaf-telega-backend-telega--chats)))
    (when (and sent (plist-get callbacks :sent))
      (funcall (plist-get callbacks :sent) sent))))

(defun etaf-telega-backend-telega--ready ()
  "Report readiness to every live telega backend."
  (dolist (backend etaf-telega-backend-telega-instances)
    (etaf-telega-backend-telega--report backend :status "就绪" :chats t)))

(defun etaf-telega-backend-telega--chat-update (chat)
  "Report the updated CHAT list to every live telega backend."
  (dolist (backend etaf-telega-backend-telega-instances)
    (when-let* ((callbacks (etaf-telega-backend-telega-callbacks backend))
                (report (plist-get callbacks :chats)))
      (funcall report (etaf-telega-backend-telega--chats)))
    (when (equal (etaf-telega-backend-telega-chat-id backend)
                 (plist-get chat :id))
      (etaf-telega-backend-select backend (plist-get chat :id)))))

(cl-defmethod etaf-telega-backend-start ((backend etaf-telega-backend-telega)
                                         callbacks)
  "Start Telega's server if needed and observe CALLBACKS.
The root buffer is created without being displayed; Telega keeps owning the
account and its database."
  (setf (etaf-telega-backend-telega-callbacks backend) callbacks)
  (add-to-list 'etaf-telega-backend-telega-instances backend)
  (condition-case err
      (progn
        (require 'telega)
        (add-hook 'telega-ready-hook #'etaf-telega-backend-telega--ready)
        (add-hook 'telega-chat-update-hook #'etaf-telega-backend-telega--chat-update)
        (if (telega-server-live-p)
            (etaf-telega-backend-telega--ready)
          (telega '(4))))
    (error (when-let* ((report (plist-get callbacks :error)))
             (funcall report (error-message-string err)))))
  backend)

(cl-defmethod etaf-telega-backend-select ((backend etaf-telega-backend-telega)
                                          chat-id)
  "Load CHAT-ID's recent history through Telega."
  (setf (etaf-telega-backend-telega-chat-id backend) chat-id)
  (let ((callbacks (etaf-telega-backend-telega-callbacks backend)))
    (condition-case err
        (when-let* ((chat (telega-chat-get chat-id)))
          (telega--getChatHistory
           chat 0 0 50 nil
           (lambda (event)
             (when-let* ((report (plist-get callbacks :messages)))
               (funcall report
                        (mapcar #'etaf-telega-backend-telega--project-message
                                (append (plist-get event :messages) nil)))))))
      (error (when-let* ((report (plist-get callbacks :error)))
               (funcall report (error-message-string err)))))))

(cl-defmethod etaf-telega-backend-send ((backend etaf-telega-backend-telega)
                                        text)
  "Send TEXT to the selected Telega chat."
  (let* ((chat-id (etaf-telega-backend-telega-chat-id backend))
         (chat (and chat-id (telega-chat-get chat-id))))
    (unless chat
      (error "没有选中的对话"))
    (telega--sendMessage
     chat (list :@type "inputMessageText"
                :text (list :@type "formattedText" :text text)))
    (etaf-telega-backend-telega--report
     backend :sent (list :id nil :outgoing t :sender "我"
                         :time (format-time-string "%H:%M") :text text))
    text))

(cl-defmethod etaf-telega-backend-stop ((backend etaf-telega-backend-telega))
  "Stop observing Telega without closing its server or account."
  (setq etaf-telega-backend-telega-instances
        (delq backend etaf-telega-backend-telega-instances))
  (unless etaf-telega-backend-telega-instances
    (remove-hook 'telega-ready-hook #'etaf-telega-backend-telega--ready)
    (remove-hook 'telega-chat-update-hook
                 #'etaf-telega-backend-telega--chat-update)))

(provide 'etaf-telega-backend)
;;; etaf-telega-backend.el ends here
