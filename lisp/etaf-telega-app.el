;;; etaf-telega-app.el --- Application shell of etaf-telega -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The client is one buffer that follows Telega's own workflow: a conversation
;; list screen and a chat screen showing one conversation's history above its
;; composer.  The buffer mounts one View over the model, so a backend report
;; updates whichever rows it touches and nothing else.
;;
;; Point is the selection, as in Telega: moving point onto a row selects its
;; conversation or message, and every message or chat command acts on the row
;; under point.  Commands Telega already implements (transient menus,
;; reactions, forwarding, chat operations) run natively through
;; `etaf-telega-backend-native' with the row's chat or message as their target.

;;; Code:

(require 'cl-lib)
(require 'etaf)
(require 'etaf-ui)
(require 'etaf-telega-model)
(require 'etaf-telega-backend)
(require 'etaf-telega-views)

(declare-function telega-msg-at "telega-msg" (&optional pos))
(declare-function telega-msg-open-content "telega-msg" (msg &optional clicked-p))
(declare-function telega-chat-create "telega-chat" (chat-type))
(declare-function telega-chat-join-by-link "telega-chat" (link))

(defcustom etaf-telega-buffer-name "*ETAF Telegram*"
  "Buffer name of the client.
Do not start it with \"*Telegram\": `telega-buffer-p' claims that prefix and
Telega's root-buffer cleanup kills every matching buffer."
  :type 'string
  :group 'etaf-telega)

(defvar etaf-telega-backend-create-function
  #'etaf-telega-backend-telega-create
  "Return a fresh backend for `etaf-telega-open'.
Tests bind this to a fake backend factory so no network is touched.")

(defvar etaf-telega--model nil
  "Model shared by the conversation list and the chat buffer.")

(defvar etaf-telega--backend nil
  "Backend feeding `etaf-telega--model'.")

(defvar etaf-telega--buffer nil
  "The mounted client buffer.")

(defvar etaf-telega--history-end nil
  "Non-nil when the open chat has no older history.")

(defvar etaf-telega--older-pending nil
  "Non-nil while an older history page is in flight.")

(defvar etaf-telega--history-retries 0
  "Empty answers accepted so far for the open chat's history.")

(defvar etaf-telega--jump-pending nil
  "Non-nil until the opened chat's first page moves point to the composer.")

(defvar etaf-telega--jump-target nil
  "Message id a jump is loading around, or nil.")

(defvar etaf-telega--input-history (make-hash-table :test #'equal)
  "Sent texts per conversation id, newest first, for M-p and M-n.")

(defvar etaf-telega--input-index nil
  "Position while browsing the open conversation's input history.")

(defvar etaf-telega--input-stash nil
  "Composer text kept while browsing the input history.")

(defconst etaf-telega--input-history-limit 50
  "Sent texts remembered per conversation for M-p and M-n.")

(defvar etaf-telega--upload-ref nil
  "Unused placeholder removed; uploads live on the model.")

(defconst etaf-telega--history-retry-limit 2
  "Empty history answers retried before the history counts as complete.
TDLib answers its first requests for a chat from the local database and may
return nothing until the server synchronized it.")

(defconst etaf-telega--history-retry-delay 0.8
  "Seconds before an empty history answer is asked again.")

(defvar-local etaf-telega--root-active nil
  "Non-nil while the client buffer shows the conversation list.")

(defvar-local etaf-telega--chat-active nil
  "Non-nil while the client buffer shows a chat.")

;;;; Helpers

(defun etaf-telega--model ()
  "Return the live model or signal a user error."
  (or etaf-telega--model (user-error "Telegram 客户端没有打开")))

(defun etaf-telega--runtime (buffer)
  "Return BUFFER's mounted Runtime, or nil."
  (and (buffer-live-p buffer) (etaf-runtime-for-buffer buffer)))

(defun etaf-telega--window (buffer)
  "Return a window showing BUFFER, if any."
  (and (buffer-live-p buffer) (get-buffer-window buffer t)))

(defconst etaf-telega--status-limit 160
  "Longest status or error text kept in the header.
Backend or renderer failures can carry huge payloads (a Telega command with
message bodies); the header keeps only a readable prefix.")

(defun etaf-telega--clip-text (text)
  "Return TEXT shortened to one readable status line."
  (let ((text (or text "")))
    (if (<= (length text) etaf-telega--status-limit)
        text
      (concat (substring text 0 etaf-telega--status-limit) "…"))))

(defun etaf-telega--report-callback-failure (error)
  "Report ERROR from a backend callback without letting it escape.
These callbacks run inside Telega's command dispatch; an escaping failure
makes Telega print the entire failing command, including message bodies, to
the echo area and can hang Emacs.  The status line carries a clipped reason."
  (condition-case nil
      (etaf-telega-model-set-status
       etaf-telega--model
       (format "回调错误：%s" (etaf-telega--clip-text (error-message-string error))))
    (error nil)))

(defun etaf-telega--row-at (kind &optional buffer)
  "Return the id of the KIND row under point in BUFFER, or nil."
  (let ((buffer (or buffer (current-buffer))))
    (when-let* ((runtime (etaf-telega--runtime buffer)))
      (with-current-buffer buffer
        (etaf-telega--ref-id
         kind (etaf-host-ref-at runtime (point)
                                (lambda (ref) (etaf-telega--ref-id kind ref))))))))

(defun etaf-telega--goto-ref (buffer ref)
  "Scroll REF into view and move point, and BUFFER's window point, to it."
  (when-let* ((runtime (etaf-telega--runtime buffer)))
    (ignore-errors (etaf-scroll-into-view runtime ref))
    (when-let* ((position (etaf-host-ref-position runtime ref)))
      (with-current-buffer buffer (goto-char position))
      (when-let* ((window (etaf-telega--window buffer)))
        (set-window-point window position))
      position)))

(defun etaf-telega--history-scroll (function &rest arguments)
  "Apply FUNCTION to the history Host and ARGUMENTS when the chat is shown."
  (when-let* ((runtime (etaf-telega--runtime etaf-telega--buffer))
              ((and etaf-telega--model
                    (eq (etaf-telega-model-screen etaf-telega--model) 'chat))))
    (ignore-errors (apply function runtime 'telega-history arguments))))

(defun etaf-telega--show-newest ()
  "Scroll the history to its newest message and keep following it."
  (etaf-telega--history-scroll #'etaf-scroll-to 'end))

;;;; Older history

(defun etaf-telega-load-more ()
  "Load one page of history older than the oldest loaded message."
  (interactive)
  (let* ((model (etaf-telega--model))
         (oldest (car (etaf-telega-model-messages model))))
    (when (and oldest (etaf-telega-model-chat model)
               (not etaf-telega--older-pending)
               (not etaf-telega--history-end))
      (setq etaf-telega--older-pending t)
      (etaf-telega-backend-history etaf-telega--backend
                                   :before (plist-get oldest :id)))))

(defun etaf-telega--fill-older ()
  "Load older history while the history's top line is shown."
  (when (and etaf-telega--model (not etaf-telega--jump-pending)
             (eql 0 (etaf-telega--history-scroll #'etaf-scroll-offset)))
    (with-current-buffer etaf-telega--buffer
      (etaf-telega-load-more))))

(defun etaf-telega--schedule-fill ()
  "Check, once the current update is painted, whether older history should load."
  (etaf-next-tick #'etaf-telega--fill-older))

(defun etaf-telega--history-scrolled (offset)
  "Load older history when the reader scrolls the history to OFFSET 0."
  (when (zerop offset)
    (etaf-telega--schedule-fill)))

(defun etaf-telega--retry (function &rest arguments)
  "Call FUNCTION with ARGUMENTS after the history retry delay.
The call is dropped when the client closed or the chat changed meanwhile."
  (let ((model etaf-telega--model)
        (chat (and etaf-telega--model (etaf-telega-model-chat etaf-telega--model))))
    (run-at-time etaf-telega--history-retry-delay nil
                 (lambda ()
                   (when (and model (eq model etaf-telega--model)
                              (equal (plist-get chat :id)
                                     (plist-get (etaf-telega-model-chat model) :id)))
                     (apply function arguments))))))

;;;; Backend callbacks

(defun etaf-telega--on-messages (messages)
  "Show MESSAGES, newest first, as the open chat's newest page."
  (let ((model etaf-telega--model))
    (etaf-telega-model-set-messages model (reverse messages))
    (cond
     ((and (null messages)
           (< etaf-telega--history-retries etaf-telega--history-retry-limit))
      (cl-incf etaf-telega--history-retries)
      (etaf-telega--retry #'etaf-telega-backend-select etaf-telega--backend
                          (plist-get (etaf-telega-model-chat model) :id)))
     (t
      (setq etaf-telega--history-end (null messages))
      (when etaf-telega--jump-pending
        (setq etaf-telega--jump-pending nil)
        (etaf-telega--show-newest)
        (etaf-telega--goto-ref etaf-telega--buffer 'telega-composer))
      (etaf-telega--schedule-fill)))))

(defun etaf-telega--keeping-view (function)
  "Call FUNCTION, which adds rows above the history, keeping the reader's row.
When the history was scrolled to its top, the previously oldest message
stays on the first line afterwards, like a chat app growing upwards; a
history following its newest message keeps following it."
  (let* ((oldest (car (etaf-telega-model-messages etaf-telega--model)))
         (anchor (and oldest (etaf-telega--ref 'message (plist-get oldest :id))))
         (runtime (etaf-telega--runtime etaf-telega--buffer))
         (top (eql 0 (etaf-telega--history-scroll #'etaf-scroll-offset))))
    (prog1 (funcall function)
      (when (and anchor top runtime)
        (etaf-next-tick
         (lambda ()
           (ignore-errors (etaf-scroll-into-view runtime anchor 'start))))))))

(defun etaf-telega--on-older (messages)
  "Prepend the older MESSAGES page, newest first."
  (setq etaf-telega--older-pending nil)
  (cond
   (messages
    (setq etaf-telega--history-retries 0)
    (etaf-telega--keeping-view
     (lambda ()
       (etaf-telega-model-prepend-messages etaf-telega--model (reverse messages))))
    (etaf-telega--schedule-fill))
   ((< etaf-telega--history-retries etaf-telega--history-retry-limit)
    (cl-incf etaf-telega--history-retries)
    (etaf-telega--retry #'etaf-telega-load-more))
   (t (setq etaf-telega--history-end t))))

(defun etaf-telega--on-jump (messages)
  "Merge the MESSAGES page loaded around a requested message and select it.
`etaf-telega--jump-target' names the message to select; the value t selects
the newest message of the reported page, which is what a date jump wants."
  (let* ((model etaf-telega--model)
         (target etaf-telega--jump-target)
         (id (if (eq target t)
                 (plist-get (car messages) :id)
               target)))
    (when messages
      (etaf-telega-model-prepend-messages model (reverse messages)))
    (when (or target messages)
      (setq etaf-telega--jump-target nil)
      (if (and id (etaf-telega-model-message-by-id model id))
          (progn
            (etaf-telega-model-set-selected model id)
            (etaf-telega--goto-ref etaf-telega--buffer
                                   (etaf-telega--ref 'message id)))
        (etaf-telega-model-set-status model "没有找到这条消息")))))

(defun etaf-telega--on-updated (message)
  "Replace MESSAGE's copy and refresh a viewer still showing its preview."
  (let ((model etaf-telega--model))
    (etaf-telega-model-replace-message model message)
    (let ((viewer (etaf-telega-model-viewer model)))
      (when (and viewer (equal (plist-get viewer :id) (plist-get message :id))
                 (not (plist-get viewer :full)))
        (etaf-telega--show-media message)))))

(defun etaf-telega--callbacks (model)
  "Return backend callbacks that update MODEL while it is the live model."
  (cl-labels ((report (function &rest arguments)
                (lambda (value)
                  (when (eq model etaf-telega--model)
                    (condition-case error
                        (apply function (append arguments (list value)))
                      ((error quit)
                       (etaf-telega--report-callback-failure error))))))
              (finish-compose (message)
                (when (equal (etaf-telega-model-draft model)
                             (plist-get message :text))
                  (etaf-telega-model-set-draft model ""))
                (etaf-telega--save-draft
                 (plist-get (etaf-telega-model-chat model) :id) "")
                (when (etaf-telega-model-upload model)
                  (etaf-telega-model-set-upload model nil)
                  (etaf-telega-model-set-status model "就绪"))
                (etaf-telega-model-set-compose model nil)))
    (list :status (report (lambda (status)
                            (etaf-telega-model-set-status
                             model (etaf-telega--clip-text status))))
          :me (report #'etaf-telega-model-set-me model)
          :chats (report #'etaf-telega-model-set-chats model)
          :messages (report #'etaf-telega--on-messages)
          :older (report #'etaf-telega--on-older)
          :sent (report (lambda (message)
                          (etaf-telega--remember-input
                           (plist-get (etaf-telega-model-chat model) :id)
                           (plist-get message :text))
                          (etaf-batch
                            (finish-compose message)
                            (etaf-telega-model-append-message model message))
                          (etaf-telega--show-newest)))
          :edited (report (lambda (message)
                            (etaf-batch
                              (finish-compose message)
                              (etaf-telega-model-replace-message model message))))
          :updated (report #'etaf-telega--on-updated)
          :jump (report #'etaf-telega--on-jump)
          :upload (report (lambda (event)
                            (etaf-telega--on-upload model event)))
          :deleted (report #'etaf-telega-model-remove-message model)
          :message (report #'etaf-telega-model-append-message model)
          :error (report (lambda (text)
                           (setq etaf-telega--older-pending nil)
                           (etaf-telega-model-set-status
                            model (format "错误：%s" (etaf-telega--clip-text text))))))))

;;;; Point selection

(defun etaf-telega--focus-clicked-pane ()
  "Give keys to the pane a mouse click moved point into.
Beside each other, the list and chat panes both stay visible, so the
screen only records which of them owns the keys."
  (when-let* (((eq this-command 'mouse-set-point))
              ((etaf-telega-model-chat etaf-telega--model))
              (runtime (etaf-telega--runtime (current-buffer)))
              (pane (etaf-host-ref-at runtime (point)
                                      (lambda (ref)
                                        (memq ref '(telega-list-pane telega-chat-pane))))))
    (let ((screen (if (eq pane 'telega-list-pane) 'root 'chat)))
      (unless (eq screen (etaf-telega-model-screen etaf-telega--model))
        (etaf-telega--show-screen screen)))))

(defun etaf-telega--track-point ()
  "Select the conversation or message under point."
  (when etaf-telega--model
    (etaf-telega--focus-clicked-pane)
    (cond
     (etaf-telega--root-active
      (when-let* ((id (etaf-telega--row-at 'chat)))
        (unless (equal id (etaf-telega-model-cursor etaf-telega--model))
          (etaf-telega-model-set-cursor etaf-telega--model id))))
     (etaf-telega--chat-active
      (let ((id (etaf-telega--row-at 'message)))
        (unless (equal id (etaf-telega-model-selected-id etaf-telega--model))
          (etaf-telega-model-set-selected etaf-telega--model id)))))))

(defun etaf-telega--step (ids current delta)
  "Return the id DELTA steps from CURRENT in IDS, clamped to its ends.
A nil CURRENT steps from past the last id."
  (let* ((index (or (cl-position current ids :test #'equal) (length ids)))
         (next (+ index delta)))
    (and ids (nth (max 0 (min (1- (length ids)) next)) ids))))

(defun etaf-telega--visible-chat-ids ()
  "Return the ids of the conversations the list pane shows."
  (mapcar (lambda (chat) (plist-get chat :id))
          (etaf-telega--list-chats (etaf-telega--model))))

(defun etaf-telega--move-chat (delta)
  "Move point DELTA conversations in the list."
  (let* ((model (etaf-telega--model))
         (ids (etaf-telega--visible-chat-ids))
         (current (or (etaf-telega--row-at 'chat) (etaf-telega-model-cursor model)))
         (target (if (member current ids)
                     (etaf-telega--step ids current delta)
                   (car ids))))
    (when target
      (etaf-telega-model-set-cursor model target)
      (etaf-telega--goto-ref (current-buffer) (etaf-telega--ref 'chat target)))))

(defun etaf-telega-next-chat ()
  "Move to the next conversation."
  (interactive)
  (etaf-telega--move-chat 1))

(defun etaf-telega-previous-chat ()
  "Move to the previous conversation."
  (interactive)
  (etaf-telega--move-chat -1))

(defun etaf-telega-next-unread-chat ()
  "Move to the next conversation with unread messages (M-g u on the list)."
  (interactive)
  (let* ((model (etaf-telega--model))
         (chats (etaf-telega--list-chats model))
         (ids (mapcar (lambda (chat) (plist-get chat :id)) chats))
         (current (or (etaf-telega--row-at 'chat) (etaf-telega-model-cursor model)))
         (start (or (cl-position current ids :test #'equal) -1))
         (target (cl-loop for offset from 1 to (length ids)
                          for id = (nth (mod (+ start offset) (length ids)) ids)
                          for chat = (cl-find id chats
                                              :key (lambda (item)
                                                     (plist-get item :id))
                                              :test #'equal)
                          when (and chat
                                    (or (> (or (plist-get chat :unread) 0) 0)
                                        (> (or (plist-get chat :mention) 0) 0)))
                          return id)))
    (unless target (user-error "没有未读会话"))
    (etaf-telega-model-set-cursor model target)
    (etaf-telega--goto-ref (current-buffer) (etaf-telega--ref 'chat target))))

(defun etaf-telega--move-message (delta)
  "Move point DELTA messages; past the newest message lands on the composer.
While the message search is open the movement walks its results."
  (let* ((model (etaf-telega--model))
         (ids (mapcar (lambda (message) (plist-get message :id))
                      (if (etaf-telega-model-chat-search model)
                          (etaf-telega--chat-search-results model)
                        (etaf-telega-model-messages model))))
         (current (etaf-telega--row-at 'message))
         (index (or (cl-position current ids :test #'equal) (length ids)))
         (next (+ index delta)))
    (cond
     ((null ids))
     ((>= next (length ids))
      (etaf-telega-model-set-selected model nil)
      (etaf-telega--goto-ref (current-buffer) 'telega-composer))
     (t
      (let ((target (nth (max 0 next) ids)))
        (when (and (< next 0) (not (etaf-telega-model-chat-search model)))
          (etaf-telega-load-more))
        (etaf-telega-model-set-selected model target)
        (etaf-telega--goto-ref (current-buffer)
                               (etaf-telega--ref 'message target)))))))

(defun etaf-telega-show-message (id)
  "Select the open chat's message ID, loading its page when not loaded."
  (interactive (list (or (etaf-telega--row-at 'message)
                         (user-error "先把光标移到一条消息上"))))
  (let ((model (etaf-telega--model)))
    (unless (etaf-telega-model-chat model)
      (user-error "先打开一个对话"))
    (etaf-telega--show-screen 'chat)
    (if (etaf-telega-model-message-by-id model id)
        (progn
          (etaf-telega-model-set-selected model id)
          (etaf-telega--goto-ref etaf-telega--buffer
                                 (etaf-telega--ref 'message id)))
      (setq etaf-telega--jump-target id)
      (etaf-telega-backend-around etaf-telega--backend id))))

(defun etaf-telega-jump-quote ()
  "Jump to the message quoted by the message under point (M-g q)."
  (interactive)
  (let* ((message (etaf-telega--message-at-point))
         (id (plist-get (plist-get message :reply) :id)))
    (unless id (user-error "这条消息没有引用"))
    (etaf-telega-show-message id)))

(defun etaf-telega-jump-pinned ()
  "Jump to the open conversation's pinned message (M-g p)."
  (interactive)
  (let* ((chat (etaf-telega-model-chat (etaf-telega--model)))
         (id (and chat (plist-get chat :pinned-id))))
    (unless id (user-error "这个对话没有置顶消息"))
    (etaf-telega-show-message id)))

(defun etaf-telega-jump-date (date)
  "Jump to the message nearest before DATE, a timestamp (M-g d).
Interactively read a date like 2026-10-05 or 2026-10-05 14:30."
  (interactive (list (etaf-telega--read-date)))
  (let ((model (etaf-telega--model)))
    (unless (etaf-telega-model-chat model)
      (user-error "先打开一个对话"))
    (setq etaf-telega--jump-target t)
    (etaf-telega-backend-date-message etaf-telega--backend date)))

(defun etaf-telega--read-date ()
  "Read a timestamp the client can jump to."
  (float-time
   (date-to-time
    (read-string "跳到日期（YYYY-MM-DD [HH:MM]）："
                 (format-time-string "%Y-%m-%d")))))

(defun etaf-telega--select-next-message (candidates)
  "Select the first CANDIDATE newer than the selected message.
With no newer candidate, select the oldest one, so repeated presses cycle."
  (let* ((model (etaf-telega--model))
         (current (etaf-telega-model-selected-id model))
         (target (or (cl-find-if (lambda (message)
                                   (> (or (plist-get message :id) 0)
                                      (or current 0)))
                                 candidates)
                     (car candidates))))
    (etaf-telega-show-message (plist-get target :id))))

(defun etaf-telega-next-unread-message ()
  "Select the next unread message in the open conversation (M-g u)."
  (interactive)
  (let* ((model (etaf-telega--model))
         (chat (etaf-telega-model-chat model))
         (last-read (or (and chat (plist-get chat :last-read)) 0))
         (candidates (cl-remove-if-not
                      (lambda (message)
                        (and (> (or (plist-get message :id) 0) last-read)
                             (not (plist-get message :outgoing))))
                      (etaf-telega-model-messages model))))
    (unless candidates (user-error "没有未读消息"))
    (etaf-telega--select-next-message candidates)))

(defun etaf-telega-next-mention ()
  "Select the next loaded message that mentions the account (M-g m)."
  (interactive)
  (let* ((model (etaf-telega--model))
         (username (plist-get (etaf-telega-model-me model) :username))
         (candidates
          (and (stringp username) (not (string-empty-p username))
               (let ((case-fold-search t)
                     (needle (regexp-quote (concat "@" username))))
                 (cl-remove-if-not
                  (lambda (message)
                    (string-match-p needle (or (plist-get message :text) "")))
                  (etaf-telega-model-messages model))))))
    (unless candidates (user-error "没有提及"))
    (etaf-telega--select-next-message candidates)))

(defun etaf-telega-next-outgoing ()
  "Select the next message sent by the account (M-g o)."
  (interactive)
  (let* ((model (etaf-telega--model))
         (candidates (cl-remove-if-not (lambda (message)
                                         (plist-get message :outgoing))
                                       (etaf-telega-model-messages model))))
    (unless candidates (user-error "没有自己发送的消息"))
    (etaf-telega--select-next-message candidates)))

(defun etaf-telega-next-message ()
  "Move to the next message, or to the composer after the newest one."
  (interactive)
  (etaf-telega--move-message 1))

(defun etaf-telega-previous-message ()
  "Move to the previous message, loading older history at the top."
  (interactive)
  (etaf-telega--move-message -1))

;;;; Keys

(defvar etaf-telega-root-native-keys
  '(("i" . telega-describe-chat) ("h" . telega-describe-chat)
    ("a" . telega-chat-add-member) ("o" . telega-transient-chat-operate)
    ("r" . telega-chat-toggle-read) ("d" . telega-transient-chat-delete)
    ("P" . telega-chat-toggle-pin) ("^" . telega-chat-toggle-pin)
    ("M" . telega-chat-toggle-muted) ("A" . telega-chat-toggle-archive)
    ("L" . telega-chat-leave)
    ("DEL" . telega-chat-delete))
  "Keys of the conversation list that run Telega's own chat commands.
Each command operates on the conversation under point.")

(defvar etaf-telega-chat-native-keys
  '(("c" . telega-msg-copy-dwim) ("d" . telega-transient-msg-delete)
    ("DEL" . telega-transient-msg-delete) ("f" . telega-transient-msg-forward)
    ("l" . telega-msg-copy-link) ("s" . telega-transient-msg-save)
    ("t" . telega-transient-msg-translate) ("!" . telega-msg-add-reaction)
    ("^" . telega-transient-msg-pin-toggle) ("*" . telega-msg-favorite-toggle)
    ("T" . telega-msg-open-thread-or-topic) ("=" . telega-msg-diff-edits)
    ("B" . telega-msg-ban-sender) ("R" . telega-msg-resend)
    ("o" . telega-transient-msg-operate) ("I" . telega-describe-message))
  "Keys of the chat buffer that run Telega's own message commands.
Each command operates on the message under point.")

(defun etaf-telega--native-map (keys map)
  "Bind every key of KEYS in MAP to `etaf-telega-native'."
  (dolist (entry keys)
    (define-key map (kbd (car entry)) #'etaf-telega-native))
  map)

(defvar etaf-telega-root-fastnav-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") #'etaf-telega-next-unread-chat)
    map)
  "Conversation navigation under `M-g', after Telega's root fastnav keys.")

(defvar etaf-telega-root-map
  (let ((map (make-sparse-keymap)))
    (etaf-telega--native-map etaf-telega-root-native-keys map)
    (dolist (pair '(("n" . etaf-telega-next-chat) ("p" . etaf-telega-previous-chat)
                    ("[" . etaf-telega-previous-filter)
                    ("]" . etaf-telega-next-filter)
                    ("j" . etaf-telega-chat-with)
                    ("RET" . etaf-telega-open-chat)
                    ("/" . etaf-telega-search) ("s" . etaf-telega-search)
                    ("J" . telega-chat-join-by-link)
                    ("N" . telega-chat-create) ("+" . telega-chat-create)
                    ("T" . etaf-telega-toggle-theme)
                    ("q" . bury-buffer) ("Q" . etaf-telega-close)))
      (define-key map (kbd (car pair)) (cdr pair)))
    (define-key map (kbd "M-g") etaf-telega-root-fastnav-map)
    map)
  "Keys of the conversation list screen, after Telega's root buffer.")

(defvar etaf-telega-chat-fastnav-map
  (let ((map (make-sparse-keymap)))
    (dolist (pair '(("q" . etaf-telega-jump-quote)
                    ("p" . etaf-telega-jump-pinned)
                    ("d" . etaf-telega-jump-date)
                    ("u" . etaf-telega-next-unread-message)
                    ("m" . etaf-telega-next-mention)
                    ("@" . etaf-telega-next-mention)
                    ("o" . etaf-telega-next-outgoing)))
      (define-key map (kbd (car pair)) (cdr pair)))
    map)
  "Message navigation under `M-g', after Telega's chat fastnav keys.")

(defvar etaf-telega-chat-map
  (let ((map (make-sparse-keymap)))
    (etaf-telega--native-map etaf-telega-chat-native-keys map)
    (dolist (pair '(("n" . etaf-telega-next-message)
                    ("p" . etaf-telega-previous-message)
                    ("i" . etaf-telega-compose) ("RET" . etaf-telega-activate)
                    ("j" . etaf-telega-chat-with)
                    (">" . etaf-telega-jump-newest) ("M->" . etaf-telega-jump-newest)
                    ("+" . etaf-telega-react)
                    ("r" . etaf-telega-reply) ("e" . etaf-telega-edit)
                    ("C-c C-k" . etaf-telega-cancel-compose)
                    ("g" . etaf-telega-reload-chat) ("M-<" . etaf-telega-load-more)
                    ("/" . etaf-telega-chat-search)
                    ("C-c i" . etaf-telega-toggle-details)
                    ("C-c C-s" . etaf-telega-toggle-silent)
                    ("C-c C-l" . etaf-telega-toggle-link-preview)
                    ("C-c C-m" . etaf-telega-toggle-markdown)
                    ("C-c C-t" . etaf-telega-schedule-send)
                    ("C-c C-d" . etaf-telega-clear-draft)
                    ("C-c C-a" . etaf-telega-attach)
                    ("C-c C-u" . etaf-telega-cancel-upload)
                    ("q" . etaf-telega-back) ("Q" . etaf-telega-close)))
      (define-key map (kbd (car pair)) (cdr pair)))
    (define-key map (kbd "M-g") etaf-telega-chat-fastnav-map)
    map)
  "Keys of the chat screen, after Telega's chat buffer.")

(defun etaf-telega--install-keys ()
  "Give the current screen's keys precedence over the mounted buffer's keys."
  (setq-local emulation-mode-map-alists
              (cons (list (cons 'etaf-telega--root-active etaf-telega-root-map)
                          (cons 'etaf-telega--chat-active etaf-telega-chat-map))
                    emulation-mode-map-alists)))

;;;; Conversation list commands

(defun etaf-telega--show-screen (screen)
  "Show SCREEN, `root' or `chat', and give its keys precedence."
  (etaf-telega-model-set-screen (etaf-telega--model) screen)
  (when (buffer-live-p etaf-telega--buffer)
    (with-current-buffer etaf-telega--buffer
      (setq etaf-telega--root-active (eq screen 'root)
            etaf-telega--chat-active (eq screen 'chat)))))

(defun etaf-telega-open-chat (id)
  "Open the conversation with ID on the chat screen."
  (interactive (list (or (etaf-telega--row-at 'chat)
                         (and etaf-telega--model
                              (etaf-telega-model-cursor etaf-telega--model))
                         (user-error "先把光标移到一个对话上"))))
  (let* ((model (etaf-telega--model))
         (chat (or (etaf-telega-model-chat-by-id model id)
                   (cl-find id (etaf-telega-model-search-results model)
                            :key (lambda (item) (plist-get item :id))
                            :test #'equal)
                   (user-error "没有这个对话：%s" id)))
         (previous (etaf-telega-model-chat model))
         (searching (etaf-telega-model-searching model)))
    (when (etaf-text-input-active-p (etaf-telega--runtime etaf-telega--buffer))
      (etaf-text-input-end t))
    (setq etaf-telega--history-end nil
          etaf-telega--older-pending nil
          etaf-telega--history-retries 0
          etaf-telega--jump-pending t
          etaf-telega--jump-target nil)
    (etaf-telega--reset-input-history)
    (etaf-batch
      (when searching
        (etaf-telega-model-add-recent model id)
        (etaf-telega-model-set-search model nil))
      (unless (equal (plist-get previous :id) id)
        (etaf-telega-model-set-chat-search model nil)
        (etaf-telega--save-draft (plist-get previous :id)
                                 (etaf-telega-model-draft model))
        (etaf-telega-model-set-draft model (or (plist-get chat :draft) "")))
      (etaf-telega-model-select model chat)
      (etaf-telega-model-set-cursor model id)
      (etaf-telega--show-screen 'chat))
    (etaf-telega-backend-select etaf-telega--backend id)
    id))

(defun etaf-telega--await-chat (id)
  "Wait briefly for the backend to list conversation ID."
  (let ((deadline (+ (float-time) 2)))
    (while (and (not (etaf-telega-model-chat-by-id (etaf-telega--model) id))
                (< (float-time) deadline))
      (sit-for 0.05))))

(defun etaf-telega-chat-with (id)
  "Open conversation ID, chosen by name when called interactively.
The backend offers its known conversations and contacts.  A contact whose
private chat is not loaded yet is created and opened once the refreshed
list carries it."
  (interactive
   (let* ((candidates (etaf-telega-backend-chat-candidates etaf-telega--backend))
          (choices (mapcar (lambda (item)
                             (cons (etaf-telega--plain-text
                                    (or (plist-get item :title) "?"))
                                   (plist-get item :id)))
                           candidates)))
     (unless choices (user-error "没有可用的会话"))
     (list (cdr (assoc-string (completing-read "会话：" choices nil t) choices)))))
  (unless (etaf-telega-model-chat-by-id (etaf-telega--model) id)
    (etaf-telega-backend-ensure-chat etaf-telega--backend id)
    (etaf-telega--await-chat id))
  (etaf-telega-open-chat id))

;;;; Search

(defun etaf-telega--end-editing ()
  "Leave the active text input, keeping its text."
  (when (etaf-text-input-active-p (etaf-telega--runtime etaf-telega--buffer))
    (etaf-text-input-end 'blur)))

(defun etaf-telega-search ()
  "Show the list search and begin typing its query (/ or s on the list).
On a compact chat screen the client first returns to the list."
  (interactive)
  (let ((model (etaf-telega--model))
        (runtime (etaf-telega--runtime etaf-telega--buffer)))
    (unless (etaf-host-ref-position runtime 'telega-list-pane)
      (etaf-telega-back))
    (etaf-telega-model-set-search model t (etaf-telega-model-search model))
    (with-current-buffer etaf-telega--buffer
      (etaf-text-input-begin runtime 'telega-search))))

(defun etaf-telega-search-exit ()
  "Leave the list search, clearing its query, and show the list again."
  (interactive)
  (etaf-telega--end-editing)
  (etaf-telega-model-set-search (etaf-telega--model) nil)
  (etaf-telega-model-set-search-results (etaf-telega--model) nil))

(defun etaf-telega--search-focused ()
  "Show the list search when its field begins editing."
  (let ((model (etaf-telega--model)))
    (unless (etaf-telega-model-searching model)
      (etaf-telega-model-set-search model t (etaf-telega-model-search model)))))

(defun etaf-telega--search-changed (query)
  "Track QUERY and ask the backend for conversations matching it.
The local substring filter already shows loaded matches; the backend search
fills in conversations the list has not loaded.  A stale answer, one whose
query is no longer current, is dropped."
  (let ((model (etaf-telega--model)))
    (etaf-batch
      (etaf-telega-model-set-search model t query)
      (etaf-telega-model-set-search-results model nil))
    (unless (string-empty-p (string-trim query))
      (let ((token query))
        (etaf-telega-backend-search-chats
         etaf-telega--backend query
         (lambda (chats)
           (when (and (eq model etaf-telega--model)
                      (equal token (etaf-telega-model-search model)))
             (etaf-telega-model-set-search-results model chats))))))))

(defun etaf-telega--search-submit (_query)
  "Open the first conversation the list search shows."
  (if-let* ((id (car (etaf-telega--visible-chat-ids))))
      (etaf-telega-open-chat id)
    (message "没有匹配的对话")))

(defun etaf-telega--search-cancelled (_query)
  "Leave the list search after its field was cancelled."
  (etaf-telega-model-set-search (etaf-telega--model) nil)
  (etaf-telega-model-set-search-results (etaf-telega--model) nil))

(defun etaf-telega--search-kind-select (kind)
  "Narrow the list search to conversations of KIND."
  (etaf-telega-model-set-search-kind (etaf-telega--model) kind))

(defun etaf-telega-search-clear-recent ()
  "Forget the recently opened search results."
  (interactive)
  (etaf-telega-model-clear-recent (etaf-telega--model)))

(defun etaf-telega-chat-search ()
  "Search the open conversation's loaded messages, typing the query (/)."
  (interactive)
  (let ((model (etaf-telega--model)))
    (unless (etaf-telega-model-chat model)
      (user-error "先打开一个对话"))
    (unless (etaf-telega-model-chat-search model)
      (etaf-telega-model-set-chat-search model ""))
    (with-current-buffer etaf-telega--buffer
      (etaf-text-input-begin (etaf-telega--runtime etaf-telega--buffer)
                             'telega-chat-search))))

(defun etaf-telega-chat-search-exit ()
  "Close the message search and show the whole history again."
  (interactive)
  (etaf-telega--end-editing)
  (etaf-telega-model-set-chat-search (etaf-telega--model) nil)
  (etaf-telega-model-set-chat-search-results (etaf-telega--model) nil))

(defun etaf-telega--chat-search-cancelled (_query)
  "Close the message search after its field was cancelled."
  (etaf-telega-model-set-chat-search (etaf-telega--model) nil)
  (etaf-telega-model-set-chat-search-results (etaf-telega--model) nil))

(defun etaf-telega--chat-search-changed (query)
  "Track the message search QUERY and ask the backend for matches.
Local matches over the loaded history show at once; the backend fills in
messages the client has not loaded.  A stale answer is dropped."
  (let ((model (etaf-telega--model)))
    (etaf-batch
      (etaf-telega-model-set-chat-search model query)
      (etaf-telega-model-set-chat-search-results model nil))
    (unless (string-empty-p (string-trim query))
      (let ((token query))
        (etaf-telega-backend-search-messages
         etaf-telega--backend query
         (lambda (messages)
           (when (and (eq model etaf-telega--model)
                      (equal token (etaf-telega-model-chat-search model)))
             (etaf-telega-model-set-chat-search-results model messages))))))))

(defun etaf-telega-toggle-details ()
  "Show or hide the open conversation's details pane (C-c i)."
  (interactive)
  (let ((model (etaf-telega--model)))
    (unless (etaf-telega-model-chat model)
      (user-error "先打开一个对话"))
    (etaf-telega-model-set-details model (not (etaf-telega-model-details model)))))

(defun etaf-telega--list-menu-select (key)
  "Run the list menu item KEY.
Commands that remount or close the client run after the menu's own event."
  (pcase key
    ('create (call-interactively #'telega-chat-create))
    ('join (call-interactively #'telega-chat-join-by-link))
    ('theme (run-at-time 0 nil #'etaf-telega-toggle-theme))
    ('close (run-at-time 0 nil #'etaf-telega-close))))

(defun etaf-telega--chat-menu-select (key)
  "Run the chat menu item KEY."
  (pcase key
    ('details (etaf-telega-toggle-details))
    ('search (etaf-telega-chat-search))
    ('newest (etaf-telega-jump-newest))
    ('reload (etaf-telega-reload-chat))
    ('back (etaf-telega-back))))

;;;; Filters

(defun etaf-telega-select-filter (key)
  "Show the conversations of filter KEY."
  (interactive
   (list (intern (completing-read "分组："
                                  (mapcar (lambda (entry) (symbol-name (car entry)))
                                          etaf-telega-filters)
                                  nil t))))
  (etaf-telega-model-set-filter (etaf-telega--model) key))

(defun etaf-telega--step-filter (delta)
  "Show the filter DELTA tabs away, staying at the strip's ends."
  (let* ((keys (mapcar #'car etaf-telega-filters))
         (index (or (cl-position (etaf-telega-model-filter (etaf-telega--model))
                                 keys)
                    0))
         (next (+ index delta)))
    (when (< -1 next (length keys))
      (etaf-telega-select-filter (nth next keys)))))

(defun etaf-telega-next-filter ()
  "Show the next conversation filter (] on the list); the ends hold."
  (interactive)
  (etaf-telega--step-filter 1))

(defun etaf-telega-previous-filter ()
  "Show the previous conversation filter ([ on the list); the ends hold."
  (interactive)
  (etaf-telega--step-filter -1))

(defun etaf-telega-native (command)
  "Run Telega's COMMAND on the conversation or message under point.
Called from a key of `etaf-telega-root-native-keys' or
`etaf-telega-chat-native-keys', COMMAND is that key's command."
  (interactive
   (let ((key (key-description (this-command-keys-vector))))
     (list (or (cdr (assoc key (if etaf-telega--chat-active
                                   etaf-telega-chat-native-keys
                                 etaf-telega-root-native-keys)))
               (user-error "%s 没有对应的 Telega 命令" key)))))
  (let ((model (etaf-telega--model)))
    (if etaf-telega--chat-active
        (let ((id (or (etaf-telega--row-at 'message)
                      (user-error "先把光标移到一条消息上"))))
          (etaf-telega-backend-native etaf-telega--backend command
                                      (plist-get (etaf-telega-model-chat model) :id)
                                      id))
      (etaf-telega-backend-native etaf-telega--backend command
                                  (or (etaf-telega--row-at 'chat)
                                      (etaf-telega-model-cursor model)
                                      (user-error "先把光标移到一个对话上"))))))

;;;; Chat buffer commands

(defun etaf-telega--open-content ()
  "Open the content of Telega's message at point like clicking it."
  (interactive)
  (telega-msg-open-content (telega-msg-at (point)) t))

(defconst etaf-telega--viewable-kinds '(photo video animation sticker voice audio)
  "Message kinds the in-buffer media viewer shows.")

(defun etaf-telega--media-ids ()
  "Return the ids of the open chat's viewable messages, oldest first."
  (cl-loop for message in (etaf-telega-model-messages (etaf-telega--model))
           when (and (memq (plist-get message :kind) etaf-telega--viewable-kinds)
                     (or (plist-get message :media)
                         (memq (plist-get message :kind) '(voice audio))))
           collect (plist-get message :id)))

(defun etaf-telega--show-media (message &optional source progress)
  "Show MESSAGE's media in the viewer, playing SOURCE once it is local.
Until the full file arrives the viewer shows the message's preview image
and the download PROGRESS, a fraction, when known."
  (let* ((model (etaf-telega--model))
         (id (plist-get message :id))
         (media (plist-get message :media))
         (video (memq (plist-get message :kind) '(video animation)))
         (audio (memq (plist-get message :kind) '(voice audio)))
         (ids (etaf-telega--media-ids))
         (index (cl-position id ids :test #'equal)))
    (etaf-telega-model-set-viewer
     model
     (list :id id
           :kind (cond ((and video source) 'video)
                       (audio 'audio)
                       (t 'image))
           :src (or source (plist-get media :src))
           :full (and source t)
           :duration (plist-get message :duration)
           :size (and (plist-get media :width) (plist-get media :height)
                      (cons (plist-get media :width) (plist-get media :height)))
           :title (etaf-telega--plain-text (or (plist-get message :sender) ""))
           :subtitle (concat (string-trim
                              (concat (or (plist-get message :date) "") " "
                                      (or (plist-get message :time) "")))
                             (if (and (not source) progress)
                                 (format " · 正在下载 %d%%" (round (* 100 progress)))
                               ""))
           :caption (let ((text (plist-get message :text)))
                      (and text (not (string-empty-p text)) text))
           :previous (and index (> index 0))
           :next (and index (< index (1- (length ids))))))))

(defun etaf-telega-open-media (id)
  "Preview message ID's photo, play its video or audio in the viewer.
Voice notes and audio files play in the same in-buffer viewer; other
attachments, such as files and stickers, open through Telega."
  (interactive (list (or (etaf-telega--row-at 'message)
                         (user-error "先把光标移到一条消息上"))))
  (let* ((model (etaf-telega--model))
         (message (or (etaf-telega-model-message-by-id model id)
                      (user-error "没有这条消息：%s" id))))
    (if (and (memq (plist-get message :kind) etaf-telega--viewable-kinds)
             (or (plist-get message :media)
                 (memq (plist-get message :kind) '(voice audio))))
        (progn
          (etaf-telega-model-set-selected model id)
          (etaf-telega--show-media message)
          (etaf-telega-backend-media
           etaf-telega--backend id
           (lambda (progress source)
             (when-let* ((model etaf-telega--model)
                         ((equal (plist-get (etaf-telega-model-viewer model) :id) id))
                         (current (etaf-telega-model-message-by-id model id)))
               (etaf-telega--show-media current source progress)))))
      (etaf-telega-backend-native etaf-telega--backend #'etaf-telega--open-content
                                  (plist-get (etaf-telega-model-chat model) :id)
                                  id))))

(defun etaf-telega--viewer-step (delta)
  "Show the viewable message DELTA steps from the viewer's message."
  (when-let* ((viewer (etaf-telega-model-viewer (etaf-telega--model)))
              (ids (etaf-telega--media-ids))
              (index (cl-position (plist-get viewer :id) ids :test #'equal))
              ((< -1 (+ index delta) (length ids))))
    (etaf-telega-open-media (nth (+ index delta) ids))))

(defun etaf-telega-close-media ()
  "Close the media viewer and return to the message it showed."
  (interactive)
  (let* ((model (etaf-telega--model))
         (id (plist-get (etaf-telega-model-viewer model) :id)))
    (etaf-telega-model-set-viewer model nil)
    (when id
      (etaf-telega--goto-ref etaf-telega--buffer (etaf-telega--ref 'message id)))))

(defun etaf-telega-react (id key)
  "Toggle reaction KEY, an emoji string, on message ID.
Interactively, pick the reaction for the message under point."
  (interactive
   (let ((id (or (etaf-telega--row-at 'message)
                 (user-error "先把光标移到一条消息上"))))
     (list id (completing-read "表情：" (mapcar #'car etaf-telega-reaction-choices)
                               nil t))))
  (etaf-telega--model)
  (etaf-telega-backend-react etaf-telega--backend id key))

(defun etaf-telega-swipe (direction)
  "Handle a two-finger swipe in DIRECTION, `left' or `right'.
A rightward swipe in a chat goes back to the conversation list; on the list
a swipe shows the neighbouring filter tab."
  (let ((model (etaf-telega--model)))
    (unless (etaf-telega-model-viewer model)
      (if (eq (etaf-telega-model-screen model) 'chat)
          (when (eq direction 'right) (etaf-telega-back))
        (etaf-telega--step-filter (if (eq direction 'left) 1 -1))))))

(defun etaf-telega-activate ()
  "Open the media of the message at point, or start typing elsewhere.
While the message search is open, activating a result jumps to it in the
history and closes the search."
  (interactive)
  (let* ((model (etaf-telega--model))
         (id (etaf-telega--row-at 'message))
         (message (and id (cl-find id (etaf-telega-model-messages model)
                                   :key (lambda (item) (plist-get item :id))
                                   :test #'equal))))
    (cond
     ((and id (etaf-telega-model-chat-search model)
           (cl-find id (etaf-telega--chat-search-results model)
                    :key (lambda (item) (plist-get item :id))
                    :test #'equal))
      ;; Close the search first, so the jump lands in the full history.
      (etaf-telega-chat-search-exit)
      (etaf-telega-show-message id))
     ((and message (not (memq (plist-get message :kind) '(nil text))))
      (etaf-telega-open-media id))
     (t (etaf-telega-compose)))))

(defun etaf-telega-jump-newest ()
  "Show the newest messages and select the newest one."
  (interactive)
  (let* ((model (etaf-telega--model))
         (newest (car (last (etaf-telega-model-messages model)))))
    (when (etaf-text-input-active-p (etaf-telega--runtime etaf-telega--buffer))
      (etaf-text-input-end t))
    (etaf-telega--show-newest)
    (when newest
      (etaf-telega-model-set-selected model (plist-get newest :id))
      (etaf-telega--goto-ref etaf-telega--buffer
                             (etaf-telega--ref 'message (plist-get newest :id))))))

(defconst etaf-telega--composer-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-p") #'etaf-telega-history-previous)
    (define-key map (kbd "M-n") #'etaf-telega-history-next)
    (define-key map (kbd "C-c C-s") #'etaf-telega-toggle-silent)
    (define-key map (kbd "C-c C-l") #'etaf-telega-toggle-link-preview)
    (define-key map (kbd "C-c C-m") #'etaf-telega-toggle-markdown)
    (define-key map (kbd "C-c C-t") #'etaf-telega-schedule-send)
    (define-key map (kbd "C-c C-d") #'etaf-telega-clear-draft)
    (define-key map (kbd "C-c C-a") #'etaf-telega-attach)
    (define-key map (kbd "C-c C-u") #'etaf-telega-cancel-upload)
    map)
  "Composer keys: sent texts, send options and attachments.")

(defun etaf-telega--reset-input-history ()
  "Stop browsing the open conversation's input history."
  (setq etaf-telega--input-index nil
        etaf-telega--input-stash nil))

(defun etaf-telega--remember-input (chat-id text)
  "Remember TEXT as CHAT-ID's latest sent text, newest first."
  (when (and chat-id (stringp text) (not (string-empty-p text)))
    (let ((texts (cons text (remove text (gethash chat-id etaf-telega--input-history)))))
      (puthash chat-id
               (if (> (length texts) etaf-telega--input-history-limit)
                   (cl-subseq texts 0 etaf-telega--input-history-limit)
                 texts)
               etaf-telega--input-history))))

(defun etaf-telega--save-draft (chat-id text)
  "Store TEXT as CHAT-ID's draft through the backend."
  (when chat-id
    (etaf-telega-backend-save-draft etaf-telega--backend chat-id text)))

(defun etaf-telega--save-current-draft ()
  "Store the open conversation's current draft through the backend."
  (when etaf-telega--model
    (etaf-telega--save-draft (plist-get (etaf-telega-model-chat etaf-telega--model) :id)
                             (etaf-telega-model-draft etaf-telega--model))))

(defun etaf-telega--set-composer (text)
  "Show TEXT in the composer, keeping it editable."
  (let ((runtime (etaf-telega--runtime etaf-telega--buffer)))
    (when (etaf-text-input-active-p runtime)
      (etaf-text-input-end 'blur))
    (etaf-telega-model-set-draft (etaf-telega--model) text)
    (etaf-telega--goto-ref etaf-telega--buffer 'telega-composer)
    (etaf-text-input-begin runtime 'telega-composer)))

(defun etaf-telega--history-step (delta)
  "Replace the composer text with the sent text DELTA positions away."
  (let* ((model (etaf-telega--model))
         (chat (etaf-telega-model-chat model))
         (texts (and chat (gethash (plist-get chat :id) etaf-telega--input-history))))
    (unless texts (user-error "没有输入历史"))
    (cond
     ((null etaf-telega--input-index)
      (when (> delta 0)
        (setq etaf-telega--input-stash (etaf-telega-model-draft model)
              etaf-telega--input-index 0)
        (etaf-telega--set-composer (car texts))))
     (t
      (let ((index (+ etaf-telega--input-index delta)))
        (cond
         ((< index 0)
          (setq etaf-telega--input-index nil)
          (etaf-telega--set-composer (or etaf-telega--input-stash "")))
         ((>= index (length texts))
          (etaf-telega--set-composer (nth (1- (length texts)) texts)))
         (t
          (setq etaf-telega--input-index index)
          (etaf-telega--set-composer (nth index texts)))))))))

(defun etaf-telega-history-previous ()
  "Replace the composer text with the previous sent text (M-p)."
  (interactive)
  (etaf-telega--history-step 1))

(defun etaf-telega-history-next ()
  "Replace the composer text with the next sent text (M-n)."
  (interactive)
  (etaf-telega--history-step -1))

(defun etaf-telega-clear-draft ()
  "Discard the composer's text and leave the input (C-c C-d)."
  (interactive)
  (let ((model (etaf-telega--model))
        (runtime (etaf-telega--runtime etaf-telega--buffer)))
    (when (etaf-text-input-active-p runtime)
      (etaf-text-input-end 'blur))
    (etaf-telega--reset-input-history)
    (etaf-telega-model-set-draft model "")
    (etaf-telega--save-current-draft)))

(defun etaf-telega--toggle-send-option (key)
  "Toggle the send option KEY for the next message."
  (let* ((model (etaf-telega--model))
         (options (etaf-telega-model-send-options model)))
    (etaf-telega-model-set-send-option model key (not (plist-get options key)))))

(defun etaf-telega-toggle-silent ()
  "Send the next message without a notification (C-c C-s)."
  (interactive)
  (etaf-telega--toggle-send-option 'silent))

(defun etaf-telega-toggle-link-preview ()
  "Disable the link preview for the next message (C-c C-l)."
  (interactive)
  (etaf-telega--toggle-send-option 'no-preview))

(defun etaf-telega-toggle-markdown ()
  "Parse the next message as Markdown into formatted text (C-c C-m)."
  (interactive)
  (etaf-telega--toggle-send-option 'markdown))

(defun etaf-telega-schedule-send (time)
  "Send the next message at TIME, a timestamp; nil sends it now.
Interactively read HH:MM or a full date; an empty answer clears the
schedule."
  (interactive (list (etaf-telega--read-schedule)))
  (etaf-telega-model-set-send-option (etaf-telega--model) 'at time))

(defun etaf-telega-clear-schedule ()
  "Send the next message immediately, cancelling the schedule."
  (interactive)
  (etaf-telega-model-set-send-option (etaf-telega--model) 'at nil))

(defun etaf-telega--read-schedule ()
  "Read the timestamp the next message should be sent at, or nil.
An empty answer clears the schedule; an HH:MM answer uses today, or
tomorrow when that time has already passed."
  (let ((text (string-trim
               (read-string "定时发送（HH:MM 或 YYYY-MM-DD HH:MM，留空取消）："))))
    (unless (string-empty-p text)
      (let ((time (date-to-time
                   (if (and (string-match-p ":" text)
                            (not (string-match-p " " text)))
                       (concat (format-time-string "%Y-%m-%d") " " text)
                     text))))
        (float-time (if (time-less-p time (current-time))
                        (time-add time 86400)
                      time))))))

(defun etaf-telega--file-kind (path)
  "Return the Telegram kind for the file at PATH from its extension."
  (pcase (downcase (or (file-name-extension path) ""))
    ((or "png" "jpg" "jpeg" "gif" "webp" "heic" "bmp") 'photo)
    ((or "mp4" "mov" "m4v" "mkv" "webm" "avi") 'video)
    ((or "ogg" "oga" "opus" "m4a" "mp3" "wav" "flac") 'voice)
    (_ 'document)))

(defun etaf-telega-attach (path)
  "Send the file at PATH, choosing its Telegram kind by extension (C-c C-a)."
  (interactive (list (read-file-name "附件：")))
  (etaf-telega-send-file-as (etaf-telega--file-kind path) path))

(defun etaf-telega-send-file-as (kind path)
  "Send the file at PATH as KIND, using the composer draft as its caption.
KIND is `photo', `video', `voice' or `document'; the draft stays until the
message is accepted and the pending upload shows in the status line."
  (interactive
   (list (intern (completing-read "附件类型：" '("photo" "video" "voice" "document")
                                  nil t))
         (read-file-name "附件：")))
  (let* ((model (etaf-telega--model))
         (chat (etaf-telega-model-chat model))
         (caption (etaf-telega-model-draft model)))
    (unless chat (user-error "先选择一个对话"))
    (etaf-telega--reset-input-history)
    (etaf-telega-model-set-upload model
                                  (list :path path
                                        :name (file-name-nondirectory path)
                                        :progress 0))
    (etaf-telega-model-set-status model (format "正在上传 %s…"
                                                (file-name-nondirectory path)))
    (etaf-telega-backend-send-file
     etaf-telega--backend (plist-get chat :id) kind path
     (and caption (not (string-empty-p (string-trim caption))) caption)
     (etaf-telega-model-send-options model))
    (etaf-telega--reset-send-options model)
    path))

(defun etaf-telega-cancel-upload ()
  "Cancel the file upload in progress (C-c C-u)."
  (interactive)
  (let* ((model (etaf-telega--model))
         (upload (etaf-telega-model-upload model)))
    (if (null upload)
        (user-error "没有正在上传的文件")
      (etaf-telega-backend-cancel-upload etaf-telega--backend
                                         (plist-get upload :path))
      (etaf-telega-model-set-upload model nil)
      (etaf-telega-model-set-status model "已取消上传"))))

(defun etaf-telega--on-upload (model event)
  "Show upload EVENT's progress in MODEL's status line and upload row."
  (let ((name (or (plist-get event :name) "文件"))
        (progress (or (plist-get event :progress) 0))
        (upload (etaf-telega-model-upload model)))
    (etaf-telega-model-set-upload
     model (list :path (plist-get upload :path)
                 :name name :progress progress))
    (etaf-telega-model-set-status
     model (format "正在上传 %s：%d%%" name (round (* 100 progress))))))

(defun etaf-telega-compose ()
  "Start typing in the composer."
  (interactive)
  (unless (etaf-telega-model-chat (etaf-telega--model))
    (user-error "先选择一个对话"))
  (etaf-telega--reset-input-history)
  (let ((runtime (etaf-telega--runtime etaf-telega--buffer)))
    (etaf-telega--goto-ref etaf-telega--buffer 'telega-composer)
    (etaf-text-input-begin runtime 'telega-composer)))

(defun etaf-telega--message-at-point ()
  "Return the message under point or signal a user error."
  (let* ((model (etaf-telega--model))
         (id (or (etaf-telega--row-at 'message) (etaf-telega-model-selected-id model))))
    (or (and id (etaf-telega-model-message-by-id model id))
        (user-error "先把光标移到一条消息上"))))

(defun etaf-telega-reply ()
  "Compose a reply to the message under point."
  (interactive)
  (let ((message (etaf-telega--message-at-point)))
    (etaf-telega-model-set-compose etaf-telega--model
                                   (list :kind 'reply :target message))
    (etaf-telega-compose)))

(defun etaf-telega-edit ()
  "Edit the outgoing message under point in the composer."
  (interactive)
  (let ((message (etaf-telega--message-at-point))
        (model etaf-telega--model))
    (unless (plist-get message :outgoing)
      (user-error "只能编辑自己发送的消息"))
    (etaf-batch
      (etaf-telega-model-set-compose
       model (list :kind 'edit :target message :draft (etaf-telega-model-draft model)))
      (etaf-telega-model-set-draft model (or (plist-get message :text) "")))
    (etaf-telega-compose)))

(defun etaf-telega-cancel-compose ()
  "Cancel the active reply or edit.
An edit restores the draft that was present before editing began."
  (interactive)
  (when (etaf-text-input-active-p (etaf-telega--runtime etaf-telega--buffer))
    (etaf-text-input-end t))
  (let* ((model (etaf-telega--model))
         (compose (etaf-telega-model-compose model)))
    (when compose
      (etaf-batch
        (when (eq (plist-get compose :kind) 'edit)
          (etaf-telega-model-set-draft model (or (plist-get compose :draft) "")))
        (etaf-telega-model-set-compose model nil))
      (etaf-telega--save-current-draft))))

(defun etaf-telega-send (text)
  "Send TEXT to the open chat, honouring the compose state.
A reply carries its quoted message; an edit replaces its target.  The draft
stays until the backend accepts it, so a failure keeps the text for retry."
  (let* ((model (etaf-telega--model))
         (compose (etaf-telega-model-compose model)))
    (unless (etaf-telega-model-chat model)
      (user-error "先选择一个对话"))
    (unless (string-empty-p (string-trim text))
      (etaf-telega--reset-input-history)
      (let ((options (etaf-telega-model-send-options model)))
        (pcase (plist-get compose :kind)
          ('reply (etaf-telega-backend-send etaf-telega--backend text
                                            (plist-get compose :target) options))
          ('edit (etaf-telega-backend-edit etaf-telega--backend
                                           (plist-get compose :target) text))
          (_ (etaf-telega-backend-send etaf-telega--backend text nil options)))
        (etaf-telega--reset-send-options model)))))

(defun etaf-telega--reset-send-options (model)
  "Clear MODEL's pending send options after a send."
  (etaf-telega-model-set-send-option model 'silent nil)
  (etaf-telega-model-set-send-option model 'no-preview nil)
  (etaf-telega-model-set-send-option model 'at nil)
  (etaf-telega-model-set-send-option model 'markdown nil))

(defun etaf-telega--keep-draft (text)
  "Keep TEXT as the draft after leaving the composer.
Leaving an edit restores the draft captured before editing."
  (when etaf-telega--model
    (etaf-telega--reset-input-history)
    (let ((compose (etaf-telega-model-compose etaf-telega--model)))
      (etaf-batch
        (etaf-telega-model-set-draft
         etaf-telega--model
         (if (eq (plist-get compose :kind) 'edit)
             (or (plist-get compose :draft) "")
           text))
        (etaf-telega-model-set-compose etaf-telega--model nil))
      (etaf-telega--save-current-draft))))

(defun etaf-telega-reload-chat ()
  "Reload the open chat's newest history page."
  (interactive)
  (let ((chat (etaf-telega-model-chat (etaf-telega--model))))
    (unless chat (user-error "先选择一个对话"))
    (setq etaf-telega--history-end nil
          etaf-telega--older-pending nil
          etaf-telega--history-retries 0)
    (etaf-telega-backend-select etaf-telega--backend (plist-get chat :id))))

(defun etaf-telega-back ()
  "Leave the chat for the conversation list, keeping its conversation at point."
  (interactive)
  (let ((model (etaf-telega--model)))
    (when (etaf-text-input-active-p (etaf-telega--runtime etaf-telega--buffer))
      (etaf-text-input-end t))
    (etaf-telega--save-current-draft)
    (etaf-telega--show-screen 'root)
    (or (and (etaf-telega-model-cursor model)
             (etaf-telega--goto-ref etaf-telega--buffer
                                    (etaf-telega--ref 'chat
                                                      (etaf-telega-model-cursor model))))
        (with-current-buffer etaf-telega--buffer (goto-char (point-min))))))

;;;; Information, theme and lifecycle

(defun etaf-telega-info ()
  "Return visible client facts without network I/O."
  (let* ((model etaf-telega--model)
         (chat (and model (etaf-telega-model-chat model))))
    (list :buffer (and (buffer-live-p etaf-telega--buffer) etaf-telega--buffer)
          :screen (and model (etaf-telega-model-screen model))
          :status (and model (etaf-telega-model-status model))
          :chats (length (and model (etaf-telega-model-chats model)))
          :filter (and model (etaf-telega-model-filter model))
          :cursor (and model (etaf-telega-model-cursor model))
          :chat (plist-get chat :id)
          :messages (length (and model (etaf-telega-model-messages model)))
          :selected (and model (etaf-telega-model-selected-id model))
          :compose (plist-get (and model (etaf-telega-model-compose model)) :kind)
          :draft (and model (etaf-telega-model-draft model))
          :history-end etaf-telega--history-end
          :viewer (plist-get (and model (etaf-telega-model-viewer model)) :id)
          :search (and model (etaf-telega-model-searching model)
                       (etaf-telega-model-search model))
          :chat-search (and model (etaf-telega-model-chat-search model))
          :details (and model (etaf-telega-model-details model))
          :editing (and (etaf-text-input-active-p
                         (etaf-telega--runtime etaf-telega--buffer))
                        t))))

(defun etaf-telega-toggle-theme ()
  "Cycle the client through the dark, light and native palettes."
  (interactive)
  (setq etaf-telega-theme (pcase (etaf-telega--theme-name)
                            ('dark 'light)
                            ('light 'native)
                            (_ 'dark)))
  (when-let* ((model etaf-telega--model)
              (runtime (etaf-telega--runtime etaf-telega--buffer)))
    (with-current-buffer etaf-telega--buffer
      (let ((point (point)))
        (etaf-unmount runtime)
        (etaf-mount etaf-telega--buffer (etaf-view (etaf-telega-app :model model)))
        (goto-char (min point (point-max))))))
  (message "etaf-telega 主题：%s" (etaf-telega--theme-name)))

;;;###autoload
(defun etaf-telega-open ()
  "Show the client, starting it when needed, and return its buffer."
  (interactive)
  (unless (and etaf-telega--model (buffer-live-p etaf-telega--buffer))
    (etaf-telega-close)
    (let ((buffer (get-buffer-create etaf-telega-buffer-name))
          (model (etaf-telega-model)))
      (setq etaf-telega--model model
            etaf-telega--backend (funcall etaf-telega-backend-create-function)
            etaf-telega--buffer buffer)
      ;; The shell is one window tall, so mount where the window is known.
      (pop-to-buffer-same-window buffer)
      (etaf-mount buffer (etaf-view (etaf-telega-app :model model)))
      (with-current-buffer buffer
        (setq-local cursor-type nil)
        (etaf-telega--install-keys)
        (add-hook 'post-command-hook #'etaf-telega--track-point nil t)
        (add-hook 'kill-buffer-hook #'etaf-telega--dispose nil t))
      (etaf-telega--show-screen 'root)
      (etaf-telega-backend-start etaf-telega--backend (etaf-telega--callbacks model))))
  (pop-to-buffer-same-window etaf-telega--buffer)
  etaf-telega--buffer)

(defun etaf-telega--dispose ()
  "Stop the backend and release the mounted buffer."
  (let ((backend etaf-telega--backend)
        (buffer etaf-telega--buffer))
    (setq etaf-telega--model nil
          etaf-telega--backend nil
          etaf-telega--buffer nil)
    (when backend (etaf-telega-backend-stop backend))
    (when-let* ((runtime (etaf-telega--runtime buffer)))
      (with-current-buffer buffer
        (when (etaf-text-input-active-p runtime)
          (etaf-text-input-end t)))
      (etaf-unmount runtime))))

;;;###autoload
(defun etaf-telega-close ()
  "Close the client buffer.
The backend stops observing without closing Telega's server or account."
  (interactive)
  (let ((buffer etaf-telega--buffer))
    (etaf-telega--dispose)
    (when (buffer-live-p buffer) (kill-buffer buffer))))

(provide 'etaf-telega-app)
;;; etaf-telega-app.el ends here
