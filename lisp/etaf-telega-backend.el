;;; etaf-telega-backend.el --- Telegram backends for etaf-telega -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The capability contract
;;
;; A backend owns one account's connection, data and actions.  It owns no
;; buffers and never reads the model; the application passes a CALLBACKS plist
;; and projects every answer:
;;
;;   :status   (string)                 connection state for the header
;;   :me       (plist)                  own account (:id :name)
;;   :chats    (list of chat plists)    (:id :title :preview :unread :pinned
;;                                      :muted :time :order)
;;   :messages (list of message plists) the chat's newest page, newest first
;;   :older    (list of message plists) an older page, newest first
;;   :message  (message plist)          one later message for the selected
;;             chat; the application appends it in reading order
;;   :sent     (message plist)          accepted outgoing message
;;   :edited   (message plist)          message whose text was replaced
;;   :deleted  (id)                     id of a removed message
;;   :read     (list of ids)            messages accepted as read
;;   :error    (string)                 readable failure
;;
;; Message plists carry (:id :outgoing :sender :time :date :edited :text) in
;; reading order; the application reverses pages into reading order for
;; display.  Every live backend implements the generics below; the Telega
;; adapter reuses Telega's server, TDLib calls and object cache, and the fake
;; backend replays fixed data for tests and offline use.
;;
;;   (start BACKEND CALLBACKS)            connect and report status, me, chats
;;   (stop BACKEND)                       stop observing; keep the account
;;   (select BACKEND CHAT-ID)             open a chat and load its newest page
;;   (history BACKEND &key before limit)  load messages older than BEFORE
;;   (send BACKEND TEXT &optional REPLY-TO)  send, optionally replying
;;   (edit BACKEND MESSAGE TEXT)          replace one message's text
;;   (delete BACKEND MESSAGE)             remove one message
;;   (read BACKEND MESSAGE-IDS)           accept messages as read
;;   (chat-action BACKEND CHAT-ID ACTION &optional VALUE)
;;                                        ACTION is `read', `unread', `pin',
;;                                        `mute' or `archive'; pin/mute/archive
;;                                        toggle, read/unread set the state
;;
;; Media and file capabilities join this contract when their views land.
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
  "Make CHAT-ID current in BACKEND and load its newest history page.")

(cl-defgeneric etaf-telega-backend-history (backend &key before limit)
  "Load a page older than BEFORE in BACKEND's selected chat.
BEFORE is the oldest loaded message id, or nil for the newest page.")

(cl-defgeneric etaf-telega-backend-send (backend text &optional reply-to)
  "Send TEXT through BACKEND, replying to the message REPLY-TO when non-nil.")

(cl-defgeneric etaf-telega-backend-edit (backend message text)
  "Replace MESSAGE's text with TEXT through BACKEND.")

(cl-defgeneric etaf-telega-backend-delete (backend message)
  "Delete MESSAGE through BACKEND.")

(cl-defgeneric etaf-telega-backend-read (backend message-ids)
  "Accept MESSAGE-IDS in BACKEND's selected chat as read.")

(cl-defgeneric etaf-telega-backend-chat-action (backend chat-id action &optional value)
  "Apply ACTION to CHAT-ID through BACKEND.
`read' and `unread' set the state; `pin', `mute' and `archive' toggle it.")

(cl-defgeneric etaf-telega-backend-stop (backend)
  "Release BACKEND's connection without touching Telega's account data.")

;;; Fake backend

(cl-defstruct etaf-telega-backend-fake
  chats histories sent responder callbacks chat-id replies)

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

(defconst etaf-telega-backend-fake-page-size 2
  "Messages per reported page, so tests can exercise pagination.")

(defun etaf-telega-backend-fake--page (backend chat-id start)
  "Return BACKEND's stored CHAT-ID messages from START, one page, newest first."
  (let* ((all (gethash chat-id (etaf-telega-backend-fake-histories backend)))
         (from (min start (length all)))
         (to (min (length all) (+ from etaf-telega-backend-fake-page-size))))
    (cl-subseq all from to)))

(defun etaf-telega-backend-fake--set-unread (backend chat-id unread)
  "Set CHAT-ID's stored unread count in BACKEND's chats."
  (setf (etaf-telega-backend-fake-chats backend)
        (cl-mapcar (lambda (chat)
                     (if (equal (plist-get chat :id) chat-id)
                         (plist-put (copy-tree chat) :unread unread)
                       chat))
                   (etaf-telega-backend-fake-chats backend))))

(cl-defmethod etaf-telega-backend-select ((backend etaf-telega-backend-fake)
                                          chat-id)
  "Open CHAT-ID, report its newest page and accept it as read."
  (setf (etaf-telega-backend-fake-chat-id backend) chat-id)
  (when-let* ((report (plist-get (etaf-telega-backend-fake-callbacks backend)
                                 :messages)))
    (funcall report (copy-tree (etaf-telega-backend-fake--page backend chat-id 0))))
  ;; Opening a conversation reads its newest page, like Telega's own chatbuf.
  (etaf-telega-backend-fake--set-unread backend chat-id 0)
  (when-let* ((report (plist-get (etaf-telega-backend-fake-callbacks backend)
                                 :chats)))
    (funcall report (copy-tree (etaf-telega-backend-fake-chats backend))))
  chat-id)

(cl-defmethod etaf-telega-backend-history ((backend etaf-telega-backend-fake)
                                           &key before limit)
  "Report a page older than BEFORE in BACKEND's selected chat."
  (ignore limit)
  (let* ((chat-id (etaf-telega-backend-fake-chat-id backend))
         (all (gethash chat-id (etaf-telega-backend-fake-histories backend)))
         (index (if before
                    (1+ (or (cl-position before all
                                         :key (lambda (message) (plist-get message :id))
                                         :test #'equal)
                            -1))
                  0)))
    (when-let* ((report (plist-get (etaf-telega-backend-fake-callbacks backend)
                                   :older)))
      (funcall report (copy-tree (etaf-telega-backend-fake--page backend chat-id index))))))

(cl-defmethod etaf-telega-backend-send ((backend etaf-telega-backend-fake)
                                        text &optional reply-to)
  "Record TEXT and invoke BACKEND's responder."
  (push text (etaf-telega-backend-fake-sent backend))
  (when reply-to
    (push (cons text (plist-get reply-to :id))
          (etaf-telega-backend-fake-replies backend)))
  (when-let* ((responder (etaf-telega-backend-fake-responder backend)))
    (funcall responder backend text))
  text)

(cl-defmethod etaf-telega-backend-edit ((backend etaf-telega-backend-fake)
                                        message text)
  "Replace MESSAGE's stored text with TEXT and report the result."
  (let* ((chat-id (etaf-telega-backend-fake-chat-id backend))
         (updated (copy-tree message)))
    (setf (plist-get updated :text) text
          (plist-get updated :edited) t)
    (puthash chat-id
             (cl-mapcar (lambda (item)
                          (if (equal (plist-get item :id) (plist-get message :id))
                              updated item))
                        (gethash chat-id (etaf-telega-backend-fake-histories backend)))
             (etaf-telega-backend-fake-histories backend))
    (when-let* ((report (plist-get (etaf-telega-backend-fake-callbacks backend)
                                   :edited)))
      (funcall report (copy-tree updated)))
    text))

(cl-defmethod etaf-telega-backend-delete ((backend etaf-telega-backend-fake)
                                          message)
  "Remove MESSAGE from BACKEND's stored history and report its id."
  (let ((chat-id (etaf-telega-backend-fake-chat-id backend)))
    (puthash chat-id
             (cl-remove (plist-get message :id)
                        (gethash chat-id (etaf-telega-backend-fake-histories backend))
                        :key (lambda (item) (plist-get item :id)) :test #'equal)
             (etaf-telega-backend-fake-histories backend))
    (when-let* ((report (plist-get (etaf-telega-backend-fake-callbacks backend)
                                   :deleted)))
      (funcall report (plist-get message :id)))
    (plist-get message :id)))

(cl-defmethod etaf-telega-backend-stop ((_backend etaf-telega-backend-fake))
  "Fake backends own no connection.")

(cl-defmethod etaf-telega-backend-read ((backend etaf-telega-backend-fake)
                                        message-ids)
  "Accept MESSAGE-IDS as read in BACKEND's selected chat."
  (let ((chat-id (etaf-telega-backend-fake-chat-id backend)))
    (etaf-telega-backend-fake--set-unread backend chat-id 0)
    (when-let* ((report (plist-get (etaf-telega-backend-fake-callbacks backend)
                                   :read)))
      (funcall report (copy-sequence message-ids)))
    (when-let* ((report (plist-get (etaf-telega-backend-fake-callbacks backend)
                                   :chats)))
      (funcall report (copy-tree (etaf-telega-backend-fake-chats backend))))
    message-ids))

(cl-defmethod etaf-telega-backend-chat-action ((backend etaf-telega-backend-fake)
                                               chat-id action &optional value)
  "Apply ACTION to CHAT-ID in BACKEND's stored chats."
  (setf (etaf-telega-backend-fake-chats backend)
        (cl-mapcar (lambda (chat)
                     (if (equal (plist-get chat :id) chat-id)
                         (let ((updated (copy-tree chat)))
                           (pcase action
                             ('read (plist-put updated :unread 0))
                             ('unread (plist-put updated :unread (or (plist-get updated :unread) 1)))
                             ('pin (plist-put updated :pinned (not (plist-get updated :pinned))))
                             ('mute (plist-put updated :muted (or value (not (plist-get updated :muted)))))
                             ('archive (plist-put updated :archived (not (plist-get updated :archived))))
                             (_ (error "Unknown chat action: %s" action)))
                           updated)
                       chat))
                   (etaf-telega-backend-fake-chats backend)))
  (when-let* ((report (plist-get (etaf-telega-backend-fake-callbacks backend)
                                 :chats)))
    (funcall report (copy-tree (etaf-telega-backend-fake-chats backend))))
  action)

(defun etaf-telega-backend-fake-message (backend id)
  "Return BACKEND's stored message with ID, or nil."
  (cl-find id (gethash (etaf-telega-backend-fake-chat-id backend)
                       (etaf-telega-backend-fake-histories backend))
           :key (lambda (item) (plist-get item :id)) :test #'equal))

(defun etaf-telega-backend-fake-deliver (backend message)
  "Report MESSAGE as arriving in BACKEND's selected chat."
  (when-let* ((report (plist-get (etaf-telega-backend-fake-callbacks backend)
                                 :message)))
    (funcall report (copy-tree message))))

(defun etaf-telega-backend-fake-complete (backend text)
  "Report TEXT as accepted by BACKEND, exactly like a server acknowledgement."
  (let* ((reply-id (cdr (assoc text (etaf-telega-backend-fake-replies backend))))
         (reply (and reply-id (etaf-telega-backend-fake-message backend reply-id))))
    (when-let* ((report (plist-get (etaf-telega-backend-fake-callbacks backend)
                                   :sent)))
      (funcall report
               (append (list :id (1+ (length (etaf-telega-backend-fake-sent backend)))
                             :outgoing t :sender "我"
                             :time (format-time-string "%H:%M")
                             :text text)
                       (when reply (list :reply reply)))))))

;;; Telega backend

(declare-function telega "telega" (&optional arg))
(declare-function telega-server-live-p "telega-server" ())
(declare-function telega-chat-get "telega-chat" (chat-id &optional offline-p))
(declare-function telega-chat-title "telega-chat" (chat &optional fmt-type no-badges))
(declare-function telega-chat-match-p "telega-chat" (chat chat-temex &optional no-error))
(declare-function telega-chat-muted-p "telega-chat" (chat))
(declare-function telega-chat-toggle-read "telega-chat" (chat))
(declare-function telega-chat-toggle-pin "telega-chat" (chat))
(declare-function telega-chat-toggle-muted "telega-chat" (chat &optional muted-for))
(declare-function telega-chat-toggle-archive "telega-chat" (chat))
(declare-function telega--viewMessages "telega-tdlib" (chat messages &rest args))
(declare-function telega--getChatHistory "telega-tdlib"
                  (chat from-msg-id offset &optional limit only-local callback))
(declare-function telega--sendMessage "telega-tdlib" (chat imc &rest args))
(declare-function telega--editMessageText "telega-tdlib" (msg imc &rest args))
(declare-function telega--deleteMessages "telega-tdlib" (messages &optional revoke))
(declare-function telega--on-updateMessageSendFailed "telega-tdlib-events" (event))
(declare-function telega-msg-content-text "telega-msg"
                  (msg &optional with-speech-recognition-p))
(declare-function telega-msg-sender "telega-msg" (tl-obj))
(declare-function telega-msg-sender-title "telega-msg" (msg-sender &rest args))
(defvar telega--chats)
(defvar telega-ready-hook)
(defvar telega-chat-update-hook)
(defvar telega-chat-post-message-hook)

(cl-defstruct (etaf-telega-backend-telega
               (:constructor etaf-telega-backend-telega-create))
  callbacks chat-id pending)

(defvar etaf-telega-backend-telega-instances nil
  "Live telega backends observing the shared Telega event stream.")

(defun etaf-telega-backend-telega--project-chat (chat)
  "Return CHAT's projection plist."
  (let ((last (plist-get chat :last_message)))
    (list :id (plist-get chat :id)
          :title (or (ignore-errors (telega-chat-title chat)) "?")
          :preview (or (ignore-errors (telega-msg-content-text last)) "")
          :unread (or (plist-get chat :unread_count) 0)
          :pinned (eq t (plist-get chat :is_pinned))
          :muted (and (ignore-errors (telega-chat-muted-p chat)) t)
          :kind (cond ((ignore-errors (telega-chat-match-p chat '(type channel)))
                       'channel)
                      ((ignore-errors (telega-chat-match-p chat '(type private)))
                       'private)
                      (t 'group))
          :archived (and (ignore-errors (telega-chat-match-p chat 'archive)) t)
          :time (if last
                    (format-time-string
                     "%H:%M" (seconds-to-time (or (plist-get last :date) 0)))
                  "")
          :order (or (plist-get chat :order) 0))))

(defun etaf-telega-backend-telega--project-message (message)
  "Return MESSAGE's projection plist."
  (list :id (plist-get message :id)
        :outgoing (eq t (plist-get message :is_outgoing))
        :sender (or (ignore-errors (telega-msg-sender-title
                                    (telega-msg-sender message)))
                    "")
        :time (format-time-string "%H:%M"
                                  (seconds-to-time (or (plist-get message :date) 0)))
        :date (format-time-string "%Y-%m-%d"
                                  (seconds-to-time (or (plist-get message :date) 0)))
        :edited (eq t (plist-get message :edit_date))
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

(cl-defun etaf-telega-backend-telega--report (backend &key status chats me error)
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
    (when (and error (plist-get callbacks :error))
      (funcall (plist-get callbacks :error) error))))

(defun etaf-telega-backend-telega--ready ()
  "Report readiness to every live telega backend."
  (when etaf-telega-backend-telega--chats-timer
    (cancel-timer etaf-telega-backend-telega--chats-timer)
    (setq etaf-telega-backend-telega--chats-timer nil))
  (dolist (backend etaf-telega-backend-telega-instances)
    (etaf-telega-backend-telega--report backend :status "就绪" :chats t)))

(defvar etaf-telega-backend-telega--chats-timer nil
  "Pending chat-list refresh; a TDLib sync storm becomes one report.")

(defun etaf-telega-backend-telega--flush-chats ()
  "Report the current chat list to every live backend."
  (setq etaf-telega-backend-telega--chats-timer nil)
  (dolist (backend etaf-telega-backend-telega-instances)
    (when-let* ((callbacks (etaf-telega-backend-telega-callbacks backend))
                (report (plist-get callbacks :chats)))
      (funcall report (etaf-telega-backend-telega--chats)))))

(defun etaf-telega-backend-telega--schedule-chats ()
  "Coalesce chat updates into one refreshed list per window."
  (unless etaf-telega-backend-telega--chats-timer
    (setq etaf-telega-backend-telega--chats-timer
          (run-at-time 0.15 nil #'etaf-telega-backend-telega--flush-chats))))

(defun etaf-telega-backend-telega--chat-update (_chat)
  "Schedule a chat-list refresh for every live telega backend.
The message list of an open chat is maintained by
`etaf-telega-backend-telega--message' instead of reloading its history."
  (etaf-telega-backend-telega--schedule-chats))

(defun etaf-telega-backend-telega--message (message)
  "Report a later MESSAGE to every backend viewing its chat.
An outgoing message that matches a pending send confirms it through `:sent';
every other message reaches the application through `:message'."
  (let ((chat-id (plist-get message :chat_id)))
    (dolist (backend etaf-telega-backend-telega-instances)
      (when (equal (etaf-telega-backend-telega-chat-id backend) chat-id)
        (let* ((callbacks (etaf-telega-backend-telega-callbacks backend))
               (projected (etaf-telega-backend-telega--project-message message))
               (pending (etaf-telega-backend-telega-pending backend))
               (text (plist-get projected :text))
               (record (cl-find text pending
                                :key (lambda (item) (plist-get item :text))
                                :test #'equal))
               (ack (and (plist-get projected :outgoing) record)))
          (if ack
              (progn
                (setf (etaf-telega-backend-telega-pending backend)
                      (delq record pending))
                (when-let* ((report (plist-get callbacks :sent)))
                  (funcall report
                           (append projected
                                   (when (plist-get record :reply)
                                     (list :reply (plist-get record :reply)))))))
            (when-let* ((report (plist-get callbacks :message)))
              (funcall report projected))))))))

(defun etaf-telega-backend-telega--send-failed (event)
  "Report a failed message send to every backend viewing its chat.
The text stays pending, so a later retry that succeeds still confirms the
draft normally; the failure only reaches the status line."
  (let* ((message (plist-get event :message))
         (chat-id (plist-get message :chat_id))
         (detail (or (plist-get (plist-get event :error) :message) "未知原因")))
    (dolist (backend etaf-telega-backend-telega-instances)
      (when (equal (etaf-telega-backend-telega-chat-id backend) chat-id)
        (etaf-telega-backend-telega--report
         backend :error (format "发送失败：%s" detail))))))

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
        (add-hook 'telega-chat-post-message-hook #'etaf-telega-backend-telega--message)
        (advice-add 'telega--on-updateMessageSendFailed :after
                    #'etaf-telega-backend-telega--send-failed)
        (if (telega-server-live-p)
            (etaf-telega-backend-telega--ready)
          (telega '(4))))
    (error (when-let* ((report (plist-get callbacks :error)))
             (funcall report (error-message-string err)))))
  backend)

(cl-defmethod etaf-telega-backend-select ((backend etaf-telega-backend-telega)
                                          chat-id)
  "Load CHAT-ID's newest history page through Telega and read it."
  (setf (etaf-telega-backend-telega-chat-id backend) chat-id)
  (let ((callbacks (etaf-telega-backend-telega-callbacks backend)))
    (condition-case err
        (when-let* ((chat (telega-chat-get chat-id)))
          (telega--getChatHistory
           chat 0 0 50 nil
           (lambda (event)
             (let ((messages (append (plist-get event :messages) nil)))
               (when-let* ((report (plist-get callbacks :messages)))
                 (funcall report
                          (mapcar #'etaf-telega-backend-telega--project-message
                                  messages)))
               ;; Opening a conversation reads its loaded page, exactly like
               ;; Telega's own chat buffer.
               (when messages
                 (ignore-errors
                   (telega--viewMessages
                    chat messages :source '(:@type "messageSourceChatHistory"))))))))
      (error (when-let* ((report (plist-get callbacks :error)))
               (funcall report (error-message-string err)))))))

(cl-defmethod etaf-telega-backend-history ((backend etaf-telega-backend-telega)
                                           &key before limit)
  "Load a page older than BEFORE in the selected Telega chat."
  (let* ((chat-id (etaf-telega-backend-telega-chat-id backend))
         (callbacks (etaf-telega-backend-telega-callbacks backend))
         (chat (and chat-id (telega-chat-get chat-id))))
    (condition-case err
        (when chat
          (telega--getChatHistory
           chat (or before 0) 0 (or limit 50) nil
           (lambda (event)
             (when-let* ((report (plist-get callbacks :older)))
               (funcall report
                        (mapcar #'etaf-telega-backend-telega--project-message
                                (append (plist-get event :messages) nil)))))))
      (error (etaf-telega-backend-telega--report
              backend :error (error-message-string err))))))

(cl-defmethod etaf-telega-backend-read ((backend etaf-telega-backend-telega)
                                        message-ids)
  "Accept MESSAGE-IDS as read through Telega."
  (let* ((chat-id (etaf-telega-backend-telega-chat-id backend))
         (callbacks (etaf-telega-backend-telega-callbacks backend))
         (chat (and chat-id (telega-chat-get chat-id))))
    (when (and chat message-ids)
      (condition-case err
          (progn
            (telega--viewMessages
             chat (mapcar (lambda (id) (list :chat_id chat-id :id id)) message-ids)
             :source '(:@type "messageSourceChatHistory"))
            (when-let* ((report (plist-get callbacks :read)))
              (funcall report (copy-sequence message-ids))))
        (error (etaf-telega-backend-telega--report
                backend :error (error-message-string err)))))))

(cl-defmethod etaf-telega-backend-chat-action ((backend etaf-telega-backend-telega)
                                               chat-id action &optional value)
  "Apply ACTION to CHAT-ID through Telega's own chat commands."
  (condition-case err
      (let ((chat (telega-chat-get chat-id)))
        (unless chat (error "没有这个对话：%s" chat-id))
        (pcase action
          ('read (when (> (or (plist-get chat :unread_count) 0) 0)
                   (telega-chat-toggle-read chat)))
          ('unread (when (= (or (plist-get chat :unread_count) 0) 0)
                     (telega-chat-toggle-read chat)))
          ('pin (telega-chat-toggle-pin chat))
          ('mute (telega-chat-toggle-muted chat value))
          ('archive (telega-chat-toggle-archive chat))
          (_ (error "不支持的会话动作：%s" action)))
        action)
    (error (etaf-telega-backend-telega--report
            backend :error (error-message-string err))
           nil)))

(cl-defmethod etaf-telega-backend-send ((backend etaf-telega-backend-telega)
                                        text &optional reply-to)
  "Send TEXT to the selected Telega chat, replying to REPLY-TO.
Telega confirms the message asynchronously through its message hook, so the
draft stays until `etaf-telega-backend-telega--message' accepts it; immediate
failures are reported without keeping a half-send."
  (let* ((chat-id (etaf-telega-backend-telega-chat-id backend))
         (chat (and chat-id (telega-chat-get chat-id))))
    (unless chat
      (error "没有选中的对话"))
    (push (list :text text :reply reply-to)
          (etaf-telega-backend-telega-pending backend))
    (condition-case err
        (telega--sendMessage
         chat (list :@type "inputMessageText"
                    :text (list :@type "formattedText" :text text))
         (when reply-to
           (list :@type "inputMessageReplyToMessage"
                 :message_id (plist-get reply-to :id) :quote nil)))
      (error
       (setf (etaf-telega-backend-telega-pending backend)
             (cl-remove text (etaf-telega-backend-telega-pending backend)
                        :key (lambda (item) (plist-get item :text)) :test #'equal))
       (etaf-telega-backend-telega--report
        backend :error (error-message-string err))))
    text))

(defun etaf-telega-backend-telega--error-p (result)
  "Return non-nil when RESULT is a TDLib error object."
  (equal (plist-get result :@type) "error"))

(cl-defmethod etaf-telega-backend-edit ((backend etaf-telega-backend-telega)
                                        message text)
  "Replace MESSAGE's text with TEXT through Telega."
  (let* ((chat-id (etaf-telega-backend-telega-chat-id backend))
         (callbacks (etaf-telega-backend-telega-callbacks backend))
         (target (list :chat_id chat-id :id (plist-get message :id))))
    (condition-case err
        (telega--editMessageText
         target (list :@type "inputMessageText"
                      :text (list :@type "formattedText" :text text))
         :callback
         (lambda (result)
           (cond
            ((etaf-telega-backend-telega--error-p result)
             (etaf-telega-backend-telega--report
              backend :error (format "编辑失败：%s" (plist-get result :message))))
            (t (when-let* ((report (plist-get callbacks :edited)))
                 (funcall report
                          (etaf-telega-backend-telega--project-message result)))))))
      (error (etaf-telega-backend-telega--report
              backend :error (error-message-string err))))
    text))

(cl-defmethod etaf-telega-backend-delete ((backend etaf-telega-backend-telega)
                                          message)
  "Delete MESSAGE from the selected Telega chat."
  (let* ((chat-id (etaf-telega-backend-telega-chat-id backend))
         (callbacks (etaf-telega-backend-telega-callbacks backend))
         (target (list :chat_id chat-id :id (plist-get message :id))))
    (condition-case err
        (progn
          (telega--deleteMessages (list target))
          (when-let* ((report (plist-get callbacks :deleted)))
            (funcall report (plist-get message :id))))
      (error (etaf-telega-backend-telega--report
              backend :error (error-message-string err))))
    (plist-get message :id)))

(cl-defmethod etaf-telega-backend-stop ((backend etaf-telega-backend-telega))
  "Stop observing Telega without closing its server or account."
  (setq etaf-telega-backend-telega-instances
        (delq backend etaf-telega-backend-telega-instances))
  (unless etaf-telega-backend-telega-instances
    (when etaf-telega-backend-telega--chats-timer
      (cancel-timer etaf-telega-backend-telega--chats-timer)
      (setq etaf-telega-backend-telega--chats-timer nil))
    (remove-hook 'telega-ready-hook #'etaf-telega-backend-telega--ready)
    (remove-hook 'telega-chat-update-hook
                 #'etaf-telega-backend-telega--chat-update)
    (remove-hook 'telega-chat-post-message-hook
                 #'etaf-telega-backend-telega--message)
    (advice-remove 'telega--on-updateMessageSendFailed
                   #'etaf-telega-backend-telega--send-failed)))

(provide 'etaf-telega-backend)
;;; etaf-telega-backend.el ends here
