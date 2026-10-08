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
;;   :chats    (list of chat plists)    (:id :title :preview :author :unread
;;                                      :pinned :muted :kind :archived :mention
;;                                      :online :username :time :order
;;                                      :photo :members), main list then
;;                                      archive, each in the server's order
;;   :messages (list of message plists) the chat's newest page, newest first
;;   :older    (list of message plists) an older page, newest first
;;   :message  (message plist)          one later message for the selected
;;             chat; the application appends it in reading order
;;   :sent     (message plist)          accepted outgoing message
;;   :edited   (message plist)          message whose text this client replaced
;;   :updated  (message plist)          message changed elsewhere (content or
;;                                      edit), replacing the copy with its id
;;   :deleted  (id)                     id of a removed message
;;   :read     (list of ids)            messages accepted as read
;;   :error    (string)                 readable failure
;;
;; Message plists carry (:id :outgoing :sender :time :date :edited :kind
;; :file :link :media :avatar :reply :reactions :duration :views :text);
;; :views is a channel post's view count or nil, :reply is
;; (:sender :kind :text) of the quoted message, :reactions a list of
;; (:key :label :count :chosen) in server order and :duration the length in
;; seconds of video, animation, voice and audio.  A chat's :photo, a message's sender :avatar and its
;; :media :src are image sources:
;; a local file path, an Emacs image descriptor (an in-memory preview), or
;; nil.  :media is (:src :width :height) in source pixels for photos, video
;; and animation thumbnails and stickers.  A backend fetches missing files
;; itself and reports the chat list or an `:updated' message once they are
;; local, so sources only ever improve.
;; Every live backend implements the generics below; the Telega adapter reuses
;; Telega's server, TDLib calls and object cache, and the fake backend replays
;; fixed data for tests and offline use.
;;
;;   (start BACKEND CALLBACKS)            connect and report status, me, chats
;;   (stop BACKEND)                       stop observing; keep the account
;;   (select BACKEND CHAT-ID)             open a chat and load its newest page
;;   (history BACKEND &key before limit)  load messages older than BEFORE
;;   (send BACKEND TEXT &optional REPLY-TO)  send, optionally replying
;;   (edit BACKEND MESSAGE TEXT)          replace one message's text
;;   (read BACKEND MESSAGE-IDS)           accept messages as read
;;   (react BACKEND MESSAGE-ID KEY)       toggle reaction KEY (an emoji string)
;;                                        on a message; the change arrives
;;                                        as an `:updated' message
;;   (media BACKEND MESSAGE-ID CALLBACK)  fetch a message's full media file;
;;                                        CALLBACK receives (PROGRESS SOURCE)
;;                                        with PROGRESS from 0.0 to 1.0 and a
;;                                        non-nil SOURCE once it is local
;;   (native BACKEND COMMAND CHAT-ID &optional MESSAGE-ID)
;;                                        run the backend's own interactive
;;                                        COMMAND on that chat or message
;;
;; `native' is how the client reuses Telega's own commands (transient menus,
;; reactions, forwarding, chat operations) instead of reimplementing them.

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

(cl-defgeneric etaf-telega-backend-read (backend message-ids)
  "Accept MESSAGE-IDS in BACKEND's selected chat as read.")

(cl-defgeneric etaf-telega-backend-react (backend message-id key)
  "Toggle reaction KEY on MESSAGE-ID in BACKEND's selected chat.")

(cl-defgeneric etaf-telega-backend-media (backend message-id callback)
  "Fetch MESSAGE-ID's full media in BACKEND's selected chat.
CALLBACK receives (PROGRESS SOURCE); SOURCE is non-nil once local.")

(cl-defgeneric etaf-telega-backend-native (backend command chat-id &optional message-id)
  "Run BACKEND's interactive COMMAND on CHAT-ID, or on its MESSAGE-ID.
The command sees that chat or message as the one at point.")

(cl-defgeneric etaf-telega-backend-stop (backend)
  "Release BACKEND's connection without touching Telega's account data.")

;;; Fake backend

(cl-defstruct etaf-telega-backend-fake
  chats histories sent responder callbacks chat-id replies next-sent-id natives)

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

(cl-defmethod etaf-telega-backend-react ((backend etaf-telega-backend-fake)
                                         message-id key)
  "Toggle KEY on MESSAGE-ID's stored reactions and report the message."
  (when-let* ((message (etaf-telega-backend-fake-message backend message-id)))
    (let* ((reactions (plist-get message :reactions))
           (old (cl-find key reactions :key (lambda (item) (plist-get item :key))
                         :test #'equal))
           (chosen (not (plist-get old :chosen)))
           (count (+ (or (plist-get old :count) 0) (if chosen 1 -1)))
           (item (list :key key :label (or (plist-get old :label) key)
                       :count count :chosen chosen))
           (updated (copy-tree message)))
      (setf (plist-get updated :reactions)
            (cond ((<= count 0) (remq old reactions))
                  (old (cl-substitute item old reactions))
                  (t (append reactions (list item)))))
      (etaf-telega-backend-fake-update backend updated)))
  key)

(cl-defmethod etaf-telega-backend-media ((backend etaf-telega-backend-fake)
                                         message-id callback)
  "Report MESSAGE-ID's stored full media, or its preview, as local."
  (let* ((message (etaf-telega-backend-fake-message backend message-id))
         (media (plist-get message :media)))
    (funcall callback 1.0 (or (plist-get media :full) (plist-get media :src)))))

(cl-defmethod etaf-telega-backend-native ((backend etaf-telega-backend-fake)
                                          command chat-id &optional message-id)
  "Record COMMAND on CHAT-ID and MESSAGE-ID instead of running it."
  (push (list command chat-id message-id)
        (etaf-telega-backend-fake-natives backend))
  command)

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

(defun etaf-telega-backend-fake-update (backend message)
  "Store MESSAGE in BACKEND's selected chat and report it as changed."
  (let ((chat-id (etaf-telega-backend-fake-chat-id backend)))
    (puthash chat-id
             (cl-mapcar (lambda (item)
                          (if (equal (plist-get item :id) (plist-get message :id))
                              message item))
                        (gethash chat-id (etaf-telega-backend-fake-histories backend)))
             (etaf-telega-backend-fake-histories backend))
    (when-let* ((report (plist-get (etaf-telega-backend-fake-callbacks backend)
                                   :updated)))
      (funcall report (copy-tree message)))))

(defun etaf-telega-backend-fake-remove (backend id)
  "Remove message ID from BACKEND's selected chat and report the deletion."
  (let ((chat-id (etaf-telega-backend-fake-chat-id backend)))
    (puthash chat-id
             (cl-remove id (gethash chat-id (etaf-telega-backend-fake-histories backend))
                        :key (lambda (item) (plist-get item :id)) :test #'equal)
             (etaf-telega-backend-fake-histories backend))
    (when-let* ((report (plist-get (etaf-telega-backend-fake-callbacks backend)
                                   :deleted)))
      (funcall report id))))

(defun etaf-telega-backend-fake-complete (backend text)
  "Report TEXT as accepted by BACKEND, exactly like a server acknowledgement.
The accepted message gets the next id after the stored history, so it sorts
like a real sent message instead of colliding with existing ids."
  (let* ((reply-id (cdr (assoc text (etaf-telega-backend-fake-replies backend))))
         (reply (and reply-id (etaf-telega-backend-fake-message backend reply-id)))
         (history (gethash (etaf-telega-backend-fake-chat-id backend)
                           (etaf-telega-backend-fake-histories backend)))
         (base (max (or (etaf-telega-backend-fake-next-sent-id backend) 0)
                   (or (cl-loop for message in history
                                maximize (or (plist-get message :id) 0))
                       0)))
         (id (1+ base)))
    (setf (etaf-telega-backend-fake-next-sent-id backend) id)
    (when-let* ((report (plist-get (etaf-telega-backend-fake-callbacks backend)
                                   :sent)))
      (funcall report
               (append (list :id id
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
(declare-function telega-chat--info "telega-chat" (chat &optional locally-p))
(declare-function telega-chat--info-locally "telega-chat" (chat))
(declare-function telega-msg-match-p "telega-msg" (msg msg-temex &optional no-error))
(declare-function telega-tl-str "telega-core" (obj &optional prop no-properties))
(declare-function telega-user-online-p "telega-user" (user))
(declare-function telega-chat-muted-p "telega-chat" (chat))
(declare-function telega--viewMessages "telega-tdlib" (chat messages &rest args))
(declare-function telega--getChatHistory "telega-tdlib"
                  (chat from-msg-id offset &optional limit only-local callback))
(declare-function telega--sendMessage "telega-tdlib" (chat imc &rest args))
(declare-function telega--editMessageText "telega-tdlib" (msg imc &rest args))
(declare-function telega--on-updateMessageSendFailed "telega-tdlib-events" (event))
(declare-function telega--getRepliedMessage "telega-tdlib" (msg &optional callback))
(declare-function telega-msg-content-text "telega-msg"
                  (msg &optional with-speech-recognition-p))
(declare-function telega-msg-sender "telega-msg" (tl-obj))
(declare-function telega-msg-sender-title "telega-msg" (msg-sender &rest args))
(declare-function telega-file--renew "telega-media" (place prop))
(declare-function telega-file--download "telega-media" (file &rest args))
(declare-function telega-photo--best "telega-media" (photo &optional limits))
(declare-function telega-file--downloaded-p "telega-core" (file))
(declare-function telega-file--can-download-p "telega-core" (file))
(declare-function telega-file--downloading-progress "telega-core" (file))
(declare-function telega--addMessageReaction "telega-tdlib"
                  (msg tl-reaction-type &optional big-p update-recent-reactions-p))
(declare-function telega--removeMessageReaction "telega-tdlib"
                  (msg tl-reaction-type &optional callback))
(defvar telega--chats)
(defvar telega--cached-messages)
(defvar telega-ready-hook)
(defvar telega-chat-update-hook)
(defvar telega-chat-post-message-hook)

(cl-defstruct (etaf-telega-backend-telega
               (:constructor etaf-telega-backend-telega-create))
  callbacks chat-id pending
  (raws (make-hash-table :test #'equal)))

(defvar etaf-telega-backend-telega-instances nil
  "Live telega backends observing the shared Telega event stream.")

(defun etaf-telega-backend-telega--time-label (seconds)
  "Return SECONDS as a chat list stamp: time today, weekday this week, date."
  (let ((time (seconds-to-time (or seconds 0))))
    (cond ((null seconds) "")
          ((equal (format-time-string "%F" time) (format-time-string "%F"))
           (format-time-string "%H:%M" time))
          ((< (float-time (time-subtract nil time)) (* 6 24 3600))
           (format-time-string "%a" time))
          (t (format-time-string "%m-%d" time)))))

(defun etaf-telega-backend-telega--kind (chat)
  "Return CHAT's interface kind: `channel', `private' or `group'."
  (cond ((ignore-errors (telega-chat-match-p chat '(type channel))) 'channel)
        ((ignore-errors (telega-chat-match-p chat '(type private))) 'private)
        (t 'group)))

(defun etaf-telega-backend-telega--sender (message)
  "Return MESSAGE's sender title, or the empty string."
  (or (ignore-errors (telega-msg-sender-title (telega-msg-sender message))) ""))

(defvar etaf-telega-backend-telega--downloads (make-hash-table :test #'eql)
  "Ids of Telega files whose download this client has requested.")

(defun etaf-telega-backend-telega--minithumb (minithumbnail)
  "Return MINITHUMBNAIL as an in-memory JPEG image descriptor, or nil."
  (when-let* ((data (plist-get minithumbnail :data))
              ((stringp data))
              (bytes (ignore-errors (base64-decode-string data))))
    (list 'image :type 'jpeg :data bytes)))

(defun etaf-telega-backend-telega--file-source (place prop &optional on-ready)
  "Return the local path of the Telega file at PLACE's PROP, or nil.
A missing file starts downloading once; ON-READY runs when it is local."
  (when-let* ((file (and place (ignore-errors (telega-file--renew place prop)))))
    (if (telega-file--downloaded-p file)
        (plist-get (plist-get file :local) :path)
      (let ((id (plist-get file :id)))
        (when (and id on-ready
                   (not (gethash id etaf-telega-backend-telega--downloads))
                   (telega-file--can-download-p file))
          (puthash id t etaf-telega-backend-telega--downloads)
          (telega-file--download
           file :priority 16
           :update-callback
           (lambda (updated)
             (when (telega-file--downloaded-p updated)
               (funcall on-ready))))))
      nil)))

(defun etaf-telega-backend-telega--position (chat)
  "Return CHAT's main or archive chat list position, or nil."
  (let (main archive)
    (seq-doseq (position (plist-get chat :positions))
      (pcase (plist-get (plist-get position :list) :@type)
        ("chatListMain" (setq main position))
        ("chatListArchive" (setq archive position))))
    (let ((position (or main archive)))
      (and position (not (equal (plist-get position :order) "0"))
           position))))

(defun etaf-telega-backend-telega--project-chat (chat &optional position)
  "Return CHAT's projection plist at chat list POSITION."
  (let* ((last (plist-get chat :last_message))
         (position (or position (etaf-telega-backend-telega--position chat)))
         (kind (etaf-telega-backend-telega--kind chat))
         (text (or (and last (ignore-errors (telega-msg-content-text last))) "")))
    (list :id (plist-get chat :id)
          :title (or (ignore-errors (telega-chat-title chat)) "?")
          :preview text
          :author (and last (eq kind 'group)
                       (if (eq t (plist-get last :is_outgoing))
                           "我"
                         (etaf-telega-backend-telega--sender last)))
          :unread (or (plist-get chat :unread_count) 0)
          :pinned (eq t (plist-get position :is_pinned))
          :muted (and (ignore-errors (telega-chat-muted-p chat)) t)
          :kind kind
          :archived (equal (plist-get (plist-get position :list) :@type)
                           "chatListArchive")
          :mention (or (plist-get chat :unread_mention_count) 0)
          :online (and (ignore-errors
                        (telega-user-online-p (telega-chat--info-locally chat)))
                       t)
          :username (or (ignore-errors
                         (telega-tl-str (telega-chat--info-locally chat) :username))
                        "")
          :members (and (memq kind '(group channel))
                        (let ((count (ignore-errors
                                       (plist-get (telega-chat--info-locally chat)
                                                  :member_count))))
                          (and (natnump count) (> count 0) count)))
          :time (if last
                    (etaf-telega-backend-telega--time-label (plist-get last :date))
                  "")
          :order (string-to-number (or (plist-get position :order) "0"))
          :photo (etaf-telega-backend-telega--file-source
                  (plist-get chat :photo) :small
                  #'etaf-telega-backend-telega--schedule-chats))))

(defun etaf-telega-backend-telega--raw (backend chat-id id)
  "Return the raw TDLib message ID of CHAT-ID known to BACKEND or Telega."
  (let ((key (cons chat-id id)))
    (or (and (hash-table-p telega--cached-messages)
             (gethash key telega--cached-messages))
        (gethash key (etaf-telega-backend-telega-raws backend)))))

(defvar etaf-telega-backend-telega--quote-requests (make-hash-table :test #'equal)
  "Messages whose quoted message was requested from TDLib, by (CHAT . ID).")

(defun etaf-telega-backend-telega--quoted (backend message)
  "Return the raw message MESSAGE replies to, or nil while unknown.
An unloaded quote is requested once; MESSAGE is reported again when it
arrives, so its reply preview appears without a placeholder."
  (let* ((reply-to (plist-get message :reply_to))
         (reply-id (plist-get reply-to :message_id))
         (chat-id (plist-get message :chat_id))
         (quote-chat (let ((id (plist-get reply-to :chat_id)))
                       (if (and (numberp id) (/= id 0)) id chat-id))))
    (when (and reply-id (/= reply-id 0))
      (or (etaf-telega-backend-telega--raw backend quote-chat reply-id)
          (let ((key (cons chat-id (plist-get message :id))))
            (unless (gethash key etaf-telega-backend-telega--quote-requests)
              (puthash key t etaf-telega-backend-telega--quote-requests)
              (ignore-errors
                (telega--getRepliedMessage message
                  (lambda (quoted)
                    (when (equal (plist-get quoted :@type) "message")
                      (puthash (cons (plist-get quoted :chat_id)
                                     (plist-get quoted :id))
                               quoted (etaf-telega-backend-telega-raws backend))
                      (etaf-telega-backend-telega--media-ready
                       backend chat-id (cdr key)))))))
            nil)))))

(defun etaf-telega-backend-telega--project-message (backend message)
  "Return MESSAGE's projection plist and remember MESSAGE in BACKEND.
The raw message stays available to Telega's own commands run through
`etaf-telega-backend-native'."
  (let* ((chat-id (plist-get message :chat_id))
         (date (seconds-to-time (or (plist-get message :date) 0)))
         (edit-date (plist-get message :edit_date))
         (quoted (etaf-telega-backend-telega--quoted backend message)))
    (puthash (cons chat-id (plist-get message :id)) message
             (etaf-telega-backend-telega-raws backend))
    (list :id (plist-get message :id)
          :outgoing (eq t (plist-get message :is_outgoing))
          :sender (etaf-telega-backend-telega--sender message)
          :time (format-time-string "%H:%M" date)
          :date (format-time-string "%Y-%m-%d" date)
          :edited (and (numberp edit-date) (> edit-date 0))
          :kind (etaf-telega-backend-telega--message-kind message)
          :file (etaf-telega-backend-telega--message-file message)
          :link (etaf-telega-backend-telega--message-link message)
          :media (etaf-telega-backend-telega--message-media backend message)
          :avatar (etaf-telega-backend-telega--sender-photo backend message)
          :reactions (etaf-telega-backend-telega--reactions message)
          :duration (etaf-telega-backend-telega--duration message)
          :views (let ((count (ignore-errors
                                (plist-get (plist-get message :interaction_info)
                                           :view_count))))
                   (and (numberp count) (> count 0) count))
          :reply (and quoted
                      (list :sender (etaf-telega-backend-telega--sender quoted)
                            :kind (etaf-telega-backend-telega--message-kind quoted)
                            :text (or (ignore-errors
                                        (telega-msg-content-text quoted))
                                      "")))
          :text (or (ignore-errors (telega-msg-content-text message)) ""))))

(defun etaf-telega-backend-telega--reaction-key (type)
  "Return the interface key of TDLib reaction TYPE, or nil."
  (pcase (plist-get type :@type)
    ("reactionTypeEmoji" (telega-tl-str type :emoji 'no-properties))
    ("reactionTypeCustomEmoji" (cons 'custom (plist-get type :custom_emoji_id)))
    ("reactionTypePaid" 'paid)))

(defun etaf-telega-backend-telega--reactions (message)
  "Return MESSAGE's reactions as (:key :label :count :chosen) plists."
  (let (result)
    (seq-doseq (reaction (ignore-errors
                           (plist-get (plist-get (plist-get message :interaction_info)
                                                 :reactions)
                                      :reactions)))
      (when-let* ((key (etaf-telega-backend-telega--reaction-key
                        (plist-get reaction :type))))
        (push (list :key key
                    :label (cond ((stringp key) key) ((eq key 'paid) "⭐") (t "✦"))
                    :count (or (plist-get reaction :total_count) 0)
                    :chosen (eq t (plist-get reaction :is_chosen)))
              result)))
    (nreverse result)))

(defun etaf-telega-backend-telega--duration (message)
  "Return the playing time in seconds of MESSAGE's media, or nil."
  (let ((content (plist-get message :content)))
    (ignore-errors
      (pcase (plist-get content :@type)
        ("messageVideo" (plist-get (plist-get content :video) :duration))
        ("messageAnimation" (plist-get (plist-get content :animation) :duration))
        ("messageVoiceNote" (plist-get (plist-get content :voice_note) :duration))
        ("messageAudio" (plist-get (plist-get content :audio) :duration))))))

(defun etaf-telega-backend-telega--full-file (message)
  "Return (PLACE . PROP) of MESSAGE's full-size media file, or nil."
  (let ((content (plist-get message :content)))
    (pcase (plist-get content :@type)
      ("messagePhoto"
       (let (best)
         (seq-doseq (size (plist-get (plist-get content :photo) :sizes))
           (when (or (null best)
                     (> (* (or (plist-get size :width) 0) (or (plist-get size :height) 0))
                        (* (or (plist-get best :width) 0) (or (plist-get best :height) 0))))
             (setq best size)))
         (and best (cons best :photo))))
      ("messageVideo" (cons (plist-get content :video) :video))
      ("messageAnimation" (cons (plist-get content :animation) :animation))
      ("messageSticker" (cons (plist-get content :sticker) :sticker))
      ("messageDocument" (cons (plist-get content :document) :document))
      ("messageVoiceNote" (cons (plist-get content :voice_note) :voice))
      ("messageAudio" (cons (plist-get content :audio) :audio)))))

(defun etaf-telega-backend-telega--message-kind (message)
  "Return MESSAGE's content kind for the interface."
  (cond ((ignore-errors (telega-msg-match-p message '(type Photo))) 'photo)
        ((ignore-errors (telega-msg-match-p message '(type Video))) 'video)
        ((ignore-errors (telega-msg-match-p message '(type Animation))) 'animation)
        ((ignore-errors (telega-msg-match-p message '(type VoiceNote))) 'voice)
        ((ignore-errors (telega-msg-match-p message '(type Audio))) 'audio)
        ((ignore-errors (telega-msg-match-p message '(type Sticker))) 'sticker)
        ((ignore-errors (telega-msg-match-p message '(type Document))) 'file)
        (t 'text)))

(defun etaf-telega-backend-telega--media-ready (backend chat-id id)
  "Report message ID of CHAT-ID again to BACKEND once its media is local."
  (when (and (memq backend etaf-telega-backend-telega-instances)
             (equal (etaf-telega-backend-telega-chat-id backend) chat-id))
    (etaf-telega-backend-telega--guard
     backend "加载媒体"
     (lambda ()
       (when-let* ((raw (etaf-telega-backend-telega--raw backend chat-id id))
                   (report (plist-get (etaf-telega-backend-telega-callbacks backend)
                                      :updated)))
         (funcall report (etaf-telega-backend-telega--project-message
                          backend raw)))))))

(defun etaf-telega-backend-telega--senders-ready (backend chat-id sender)
  "Report BACKEND's loaded CHAT-ID messages from SENDER again."
  (when (and (memq backend etaf-telega-backend-telega-instances)
             (equal (etaf-telega-backend-telega-chat-id backend) chat-id))
    (let (ids)
      (maphash (lambda (key raw)
                 (when (and (equal (car key) chat-id)
                            (equal (plist-get raw :sender_id) sender))
                   (push (cdr key) ids)))
               (etaf-telega-backend-telega-raws backend))
      (dolist (id ids)
        (etaf-telega-backend-telega--media-ready backend chat-id id)))))

(defun etaf-telega-backend-telega--sender-photo (backend message)
  "Return the image source of MESSAGE's sender photo, or nil."
  (let* ((sender-id (plist-get message :sender_id))
         (chat-id (plist-get message :chat_id))
         (sender (ignore-errors (telega-msg-sender message)))
         (photo (or (plist-get sender :profile_photo) (plist-get sender :photo))))
    (and photo
         (etaf-telega-backend-telega--file-source
          photo :small
          (lambda ()
            (etaf-telega-backend-telega--senders-ready backend chat-id sender-id))))))

(defun etaf-telega-backend-telega--thumbnail (owner ready)
  "Return (SRC WIDTH HEIGHT) of OWNER's displayable thumbnail.
READY runs once a missing thumbnail file becomes local."
  (let* ((thumb (plist-get owner :thumbnail))
         (format (plist-get (plist-get thumb :format) :@type))
         (src (and (member format '("thumbnailFormatJpeg" "thumbnailFormatPng"
                                    "thumbnailFormatWebp" "thumbnailFormatGif"))
                   (etaf-telega-backend-telega--file-source thumb :file ready))))
    (list (or src (etaf-telega-backend-telega--minithumb
                   (plist-get owner :minithumbnail)))
          (or (plist-get owner :width) (plist-get thumb :width))
          (or (plist-get owner :height) (plist-get thumb :height)))))

(defun etaf-telega-backend-telega--message-media (backend message)
  "Return MESSAGE's visual media (:src :width :height), or nil."
  (let* ((content (plist-get message :content))
         (chat-id (plist-get message :chat_id))
         (id (plist-get message :id))
         (ready (lambda ()
                  (etaf-telega-backend-telega--media-ready backend chat-id id)))
         (found
          (ignore-errors
            (pcase (plist-get content :@type)
              ("messagePhoto"
               (let* ((photo (plist-get content :photo))
                      (size (telega-photo--best photo)))
                 (list (or (etaf-telega-backend-telega--file-source size :photo ready)
                           (etaf-telega-backend-telega--minithumb
                            (plist-get photo :minithumbnail)))
                       (plist-get size :width) (plist-get size :height))))
              ("messageVideo"
               (etaf-telega-backend-telega--thumbnail (plist-get content :video) ready))
              ("messageAnimation"
               (etaf-telega-backend-telega--thumbnail
                (plist-get content :animation) ready))
              ("messageSticker"
               (let ((sticker (plist-get content :sticker)))
                 (if (and (equal (plist-get (plist-get sticker :format) :@type)
                                 "stickerFormatWebp")
                          (image-type-available-p 'webp))
                     (list (etaf-telega-backend-telega--file-source
                            sticker :sticker ready)
                           (plist-get sticker :width) (plist-get sticker :height))
                   (etaf-telega-backend-telega--thumbnail sticker ready))))))))
    (when (and found (natnump (nth 1 found)) (natnump (nth 2 found))
               (> (nth 1 found) 0) (> (nth 2 found) 0))
      (list :src (nth 0 found) :width (nth 1 found) :height (nth 2 found)))))

(defun etaf-telega-backend-telega--message-file (message)
  "Return MESSAGE's document (:name :size) when it carries one."
  (let ((document (ignore-errors (plist-get (plist-get message :content) :document))))
    (when document
      (list :name (or (plist-get document :file_name) "")
            :size (or (plist-get document :file_size) 0)))))

(defun etaf-telega-backend-telega--message-link (message)
  "Return MESSAGE's web page (:site :title) when it carries one."
  (let ((page (ignore-errors (plist-get (plist-get message :content) :web_page))))
    (when page
      (list :site (or (plist-get page :site_name) "")
            :title (or (plist-get page :title) "")
            :description (or (plist-get page :description) "")))))

(defun etaf-telega-backend-telega--chats ()
  "Return the main list then the archive as projections, in server order."
  (let (result)
    (maphash (lambda (_id chat)
               (when-let* ((position (etaf-telega-backend-telega--position chat)))
                 (push (etaf-telega-backend-telega--project-chat chat position)
                       result)))
             telega--chats)
    (sort result (lambda (a b)
                   (if (eq (plist-get a :archived) (plist-get b :archived))
                       (> (plist-get a :order) (plist-get b :order))
                     (plist-get b :archived))))))

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

(defun etaf-telega-backend-telega--guard (backend label function)
  "Call FUNCTION for BACKEND, reporting failures as readable status text.
These entry points run inside Telega's command dispatch and TDLib callbacks;
an escaping error makes Telega print the whole failing command, including
message bodies, to the echo area and can hang Emacs on a large payload.  The
failure reaches the application as a clipped status line instead."
  (condition-case err
      (funcall function)
    ((error quit)
     (let ((text (error-message-string err)))
       (etaf-telega-backend-telega--report
        backend :error
        (format "%s：%s" label
                (if (> (length text) 120)
                    (concat (substring text 0 120) "…")
                  text)))))))

(defun etaf-telega-backend-telega--ready ()
  "Report readiness to every live telega backend."
  (when etaf-telega-backend-telega--chats-timer
    (cancel-timer etaf-telega-backend-telega--chats-timer)
    (setq etaf-telega-backend-telega--chats-timer nil))
  (dolist (backend etaf-telega-backend-telega-instances)
    (etaf-telega-backend-telega--guard
     backend "连接就绪"
     (lambda () (etaf-telega-backend-telega--report backend :status "就绪" :chats t)))))

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
        (etaf-telega-backend-telega--guard
         backend "接收消息"
         (lambda ()
           (let* ((callbacks (etaf-telega-backend-telega-callbacks backend))
                  (projected (etaf-telega-backend-telega--project-message
                              backend message))
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
                 (funcall report projected))))))))))

(defun etaf-telega-backend-telega--send-failed (event)
  "Report a failed message send to every backend viewing its chat.
The text stays pending, so a later retry that succeeds still confirms the
draft normally; the failure only reaches the status line."
  (let* ((message (plist-get event :message))
         (chat-id (plist-get message :chat_id))
         (detail (or (plist-get (plist-get event :error) :message) "未知原因")))
    (dolist (backend etaf-telega-backend-telega-instances)
      (when (equal (etaf-telega-backend-telega-chat-id backend) chat-id)
        (etaf-telega-backend-telega--guard
         backend "发送失败"
         (lambda ()
           (etaf-telega-backend-telega--report
            backend :error (format "发送失败：%s" detail))))))))

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
        (dolist (advice etaf-telega-backend-telega--advices)
          (advice-add (nth 0 advice) (nth 1 advice) (nth 2 advice)))
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
  (clrhash (etaf-telega-backend-telega-raws backend))
  (let ((callbacks (etaf-telega-backend-telega-callbacks backend)))
    (condition-case err
        (when-let* ((chat (telega-chat-get chat-id)))
          (telega--getChatHistory
           chat 0 0 50 nil
           (lambda (event)
             (etaf-telega-backend-telega--guard
              backend "加载会话历史"
              (lambda ()
                (let ((messages (append (plist-get event :messages) nil)))
                  (when-let* ((report (plist-get callbacks :messages)))
                    (funcall report
                             (mapcar (lambda (message)
                                       (etaf-telega-backend-telega--project-message
                                        backend message))
                                     messages)))
                  ;; Opening a conversation reads its loaded page, exactly like
                  ;; Telega's own chat buffer.
                  (when messages
                    (ignore-errors
                      (telega--viewMessages
                       chat messages :source '(:@type "messageSourceChatHistory"))))))))))
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
             (etaf-telega-backend-telega--guard
              backend "加载更早历史"
              (lambda ()
                (when-let* ((report (plist-get callbacks :older)))
                  (funcall report
                           (mapcar (lambda (message)
                                     (etaf-telega-backend-telega--project-message
                                      backend message))
                                   (append (plist-get event :messages) nil)))))))))
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
           (etaf-telega-backend-telega--guard
            backend "编辑消息"
            (lambda ()
              (cond
               ((etaf-telega-backend-telega--error-p result)
                (etaf-telega-backend-telega--report
                 backend :error
                 (format "编辑失败：%s" (plist-get result :message))))
               (t (when-let* ((report (plist-get callbacks :edited)))
                    (funcall report
                             (etaf-telega-backend-telega--project-message
                              backend result)))))))))
      (error (etaf-telega-backend-telega--report
              backend :error (error-message-string err))))
    text))

(cl-defmethod etaf-telega-backend-react ((backend etaf-telega-backend-telega)
                                         message-id key)
  "Toggle reaction KEY on MESSAGE-ID through Telega.
TDLib answers with an interaction update, reported as `:updated'."
  (let* ((chat-id (etaf-telega-backend-telega-chat-id backend))
         (raw (etaf-telega-backend-telega--raw backend chat-id message-id))
         (type (cond ((stringp key) (list :@type "reactionTypeEmoji" :emoji key))
                     ((eq (car-safe key) 'custom)
                      (list :@type "reactionTypeCustomEmoji"
                            :custom_emoji_id (cdr key)))))
         (chosen (cl-find key (etaf-telega-backend-telega--reactions raw)
                          :key (lambda (item) (plist-get item :key)) :test #'equal)))
    (etaf-telega-backend-telega--guard
     backend "表情回应"
     (lambda ()
       (unless (and raw type)
         (error "无法回应这条消息"))
       (if (plist-get chosen :chosen)
           (telega--removeMessageReaction raw type)
         (telega--addMessageReaction raw type))))
    key))

(cl-defmethod etaf-telega-backend-media ((backend etaf-telega-backend-telega)
                                         message-id callback)
  "Download MESSAGE-ID's full media through Telega, reporting progress."
  (let* ((chat-id (etaf-telega-backend-telega-chat-id backend))
         (raw (etaf-telega-backend-telega--raw backend chat-id message-id))
         (where (and raw (etaf-telega-backend-telega--full-file raw)))
         (file (and where (car where)
                    (ignore-errors (telega-file--renew (car where) (cdr where)))))
         (report (lambda (file)
                   (etaf-telega-backend-telega--guard
                    backend "加载媒体"
                    (lambda ()
                      (if (telega-file--downloaded-p file)
                          (funcall callback 1.0
                                   (plist-get (plist-get file :local) :path))
                        (funcall callback
                                 (telega-file--downloading-progress file) nil)))))))
    (cond ((null file)
           (etaf-telega-backend-telega--report backend :error "这条消息没有媒体文件"))
          ((telega-file--downloaded-p file) (funcall report file))
          (t (funcall report file)
             (telega-file--download file :priority 32 :update-callback report)))))

(defvar-local etaf-telega-backend-telega--target nil
  "The (CHAT . MESSAGE) a native command run from this buffer operates on.
It stays after the command returns so transient suffixes invoked later from
the same buffer still find their target.")

(defun etaf-telega-backend-telega--chat-at (&rest _)
  "Return the current buffer's native command chat, if any."
  (car-safe etaf-telega-backend-telega--target))

(defun etaf-telega-backend-telega--msg-at (&rest _)
  "Return the current buffer's native command message, if any."
  (cdr-safe etaf-telega-backend-telega--target))

(cl-defmethod etaf-telega-backend-native ((backend etaf-telega-backend-telega)
                                          command chat-id &optional message-id)
  "Run Telega's interactive COMMAND with CHAT-ID or MESSAGE-ID at point.
Telega's commands resolve their target through `telega-chat-at' and
`telega-msg-at'; this buffer answers both with the requested objects."
  (let* ((chat (or (ignore-errors (telega-chat-get chat-id 'offline))
                   (user-error "没有这个对话：%s" chat-id)))
         (message (and message-id
                       (or (etaf-telega-backend-telega--raw backend chat-id message-id)
                           (user-error "这条消息尚未载入")))))
    (setq etaf-telega-backend-telega--target (cons chat message))
    (call-interactively command)))

(defun etaf-telega-backend-telega--message-changed (event)
  "Report the message changed by update EVENT to backends viewing its chat."
  (let ((chat-id (plist-get event :chat_id))
        (id (plist-get event :message_id)))
    (dolist (backend etaf-telega-backend-telega-instances)
      (when (equal (etaf-telega-backend-telega-chat-id backend) chat-id)
        (etaf-telega-backend-telega--guard
         backend "更新消息"
         (lambda ()
           (when-let* ((raw (etaf-telega-backend-telega--raw backend chat-id id))
                       (report (plist-get (etaf-telega-backend-telega-callbacks backend)
                                          :updated)))
             ;; Telega patches only messages it displays or caches; keep the
             ;; client's own copy current too.
             (pcase (plist-get event :@type)
               ("updateMessageContent"
                (plist-put raw :content (plist-get event :new_content)))
               ("updateMessageEdited"
                (plist-put raw :edit_date (plist-get event :edit_date)))
               ("updateMessageInteractionInfo"
                (plist-put raw :interaction_info
                           (plist-get event :interaction_info))))
             (funcall report (etaf-telega-backend-telega--project-message
                              backend raw)))))))))

(defun etaf-telega-backend-telega--messages-deleted (event)
  "Report the permanently deleted messages of EVENT to viewing backends."
  (let ((chat-id (plist-get event :chat_id)))
    (when (eq t (plist-get event :is_permanent))
      (dolist (backend etaf-telega-backend-telega-instances)
        (when (equal (etaf-telega-backend-telega-chat-id backend) chat-id)
          (etaf-telega-backend-telega--guard
           backend "删除消息"
           (lambda ()
             (when-let* ((report (plist-get (etaf-telega-backend-telega-callbacks
                                             backend)
                                            :deleted)))
               (seq-doseq (id (plist-get event :message_ids))
                 (remhash (cons chat-id id)
                          (etaf-telega-backend-telega-raws backend))
                 (funcall report id))))))))))

(defconst etaf-telega-backend-telega--advices
  '((telega--on-updateMessageSendFailed :after
                                        etaf-telega-backend-telega--send-failed)
    (telega--on-updateMessageContent :after
                                     etaf-telega-backend-telega--message-changed)
    (telega--on-updateMessageEdited :after
                                    etaf-telega-backend-telega--message-changed)
    (telega--on-updateMessageInteractionInfo
     :after etaf-telega-backend-telega--message-changed)
    (telega--on-updateDeleteMessages :after
                                     etaf-telega-backend-telega--messages-deleted)
    (telega-chat-at :before-until etaf-telega-backend-telega--chat-at)
    (telega-msg-at :before-until etaf-telega-backend-telega--msg-at))
  "Advices the Telega backend installs while any instance is live.")

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
    (dolist (advice etaf-telega-backend-telega--advices)
      (advice-remove (nth 0 advice) (nth 2 advice)))))

(provide 'etaf-telega-backend)
;;; etaf-telega-backend.el ends here
