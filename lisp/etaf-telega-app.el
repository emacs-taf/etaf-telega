;;; etaf-telega-app.el --- Application shell of etaf-telega -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The application layer owns the buffer, the command map and the backend
;; lifecycle.  A paint is a full projection of the model; while the composer
;; owns a native editing session, paints stay pending so the session keeps
;; the buffer text, exactly like every other ETAF application.
;;
;; Keys follow Telega's own habits: n/p move between messages, r replies,
;; e edits, d deletes, c copies, i starts typing, and C-c C-k cancels a reply
;; or an edit.  j/k (and C-n/C-p) move between conversations.

;;; Code:

(require 'cl-lib)
(require 'etaf)
(require 'etaf-ui)
(require 'etaf-telega-model)
(require 'etaf-telega-backend)
(require 'etaf-telega-views)

(defcustom etaf-telega-buffer-name "*ETAF Telegram*"
  "Buffer name of the mounted client.
Do not start it with \"*Telegram\": `telega-buffer-p' claims that prefix and
Telega's root-buffer cleanup kills every matching buffer."
  :type 'string
  :group 'etaf-telega)

(defvar etaf-telega-backend-create-function
  #'etaf-telega-backend-telega-create
  "Return a fresh backend for `etaf-telega-open'.
Tests bind this to a fake backend factory so no network is touched.")

(defvar-local etaf-telega--model nil
  "Model shown by the current buffer.")

(defvar-local etaf-telega--backend nil
  "Backend feeding the current buffer.")

(defvar-local etaf-telega--paint-pending nil
  "Non-nil when a paint was requested while the composer was editing.")

(defvar-local etaf-telega--older-pending nil
  "Non-nil while an older history page is in flight.")

(defun etaf-telega--window ()
  "Return the window showing the current buffer, if any."
  (get-buffer-window (current-buffer) t))

(defconst etaf-telega-split-threshold 110
  "Window width in columns at which the client splits its panes.")

(defun etaf-telega--split-p ()
  "Return non-nil when the wide master-detail layout fits the window."
  (and (etaf-telega--window)
       (>= (window-body-width (etaf-telega--window))
           etaf-telega-split-threshold)))

(defun etaf-telega--list-columns ()
  "Return the conversation column width of the wide layout."
  (max 40 (min 52 (floor (* (or (and (etaf-telega--window)
                                     (window-body-width (etaf-telega--window)))
                                80)
                          0.38)))))

(defun etaf-telega--width ()
  "Return the message cell width for the current window.
The wide layout reserves the conversation column, so messages wrap to the
remaining pane; the narrow layout owns the full width."
  (max 20 (- (or (and (etaf-telega--window) (window-body-width)) 80)
             (if (etaf-telega--split-p) (+ (etaf-telega--list-columns) 5) 4))))

(defun etaf-telega--list-width ()
  "Return the conversation-list width in character cells."
  (max 24 (- (if (etaf-telega--split-p)
                 (etaf-telega--list-columns)
               (or (and (etaf-telega--window) (window-body-width)) 84))
             2)))

(defun etaf-telega--runtime ()
  "Return the mounted runtime of the current buffer, if any."
  (etaf-runtime-for-buffer (current-buffer)))

(defun etaf-telega--chat-rows ()
  "Return the sidebar's visible chat row count.
Each conversation row is two lines tall; the remainder covers the title
bar, the filter tabs and their borders."
  (max 1 (/ (max 2 (- (or (and (etaf-telega--window)
                               (window-body-height (etaf-telega--window)))
                         24)
                      4))
            2)))

(defconst etaf-telega-filters
  '((main . "全部") (important . "重要") (online . "在线")
    (groups . "群组") (channels . "频道") (archived . "归档"))
  "Chat filters shown in the top tab strip.")

(defun etaf-telega--filter-chats (chats key)
  "Return CHATS matching filter KEY."
  (pcase key
    ('main (cl-remove-if (lambda (chat) (plist-get chat :archived)) chats))
    ('important (cl-remove-if-not (lambda (chat)
                                    (> (or (plist-get chat :mention) 0) 0))
                                  chats))
    ('online (cl-remove-if-not (lambda (chat) (plist-get chat :online)) chats))
    ('groups (cl-remove-if-not (lambda (chat)
                                 (eq (plist-get chat :kind) 'group)) chats))
    ('channels (cl-remove-if-not (lambda (chat)
                                   (eq (plist-get chat :kind) 'channel)) chats))
    ('archived (cl-remove-if-not (lambda (chat)
                                   (plist-get chat :archived)) chats))
    (_ chats)))

(defun etaf-telega--filter-items (chats)
  "Return the filter tab items over CHATS, with unread chat counts."
  (mapcar (lambda (entry)
            (let* ((matching (etaf-telega--filter-chats chats (car entry)))
                   (unread (length (cl-remove-if-not
                                    (lambda (chat)
                                      (> (or (plist-get chat :unread) 0) 0))
                                    matching))))
              (append (list :key (car entry) :label (cdr entry))
                      (when (> unread 0) (list :count unread)))))
          etaf-telega-filters))

(defun etaf-telega--chat-layout ()
  "Return the sidebar's visible chat slice.
The slice follows the selected conversation, so walking the list never
leaves the selection off screen."
  (let* ((model etaf-telega--model)
         (chats (if model
                    (etaf-telega--filter-chats (etaf-telega-model-chats model)
                                               (etaf-telega-model-filter model))
                  nil))
         (total (length chats))
         (visible (etaf-telega--chat-rows))
         (selected (and model (etaf-telega-model-cursor model)))
         (index (cl-position selected chats
                             :key (lambda (chat) (plist-get chat :id))
                             :test #'equal))
         (start (min (max 0 (if model (etaf-telega-model-chat-scroll model) 0))
                     (max 0 (- total visible)))))
    (when index
      (cond ((< index start) (setq start index))
            ((>= index (+ start visible)) (setq start (1+ (- index visible))))))
    (setq start (min (max 0 start) (max 0 (- total visible))))
    (when model (etaf-telega-model-set-chat-scroll model start))
    (list :total total :visible visible :start start :end (+ start visible)
          :chats (cl-subseq chats start (min total (+ start visible))))))

(defun etaf-telega--pane-rows ()
  "Return the message pane's visible row count.
The remainder covers the title bar and the composer."
  (max 3 (- (or (and (etaf-telega--window) (window-body-height (etaf-telega--window)))
                24)
            8)))

(defun etaf-telega--blocks-slice (blocks pane scroll)
  "Return the visible slice of BLOCKS for PANE rows with SCROLL rows above."
  (let* ((heights (mapcar (lambda (block) (plist-get block :height)) blocks))
         (total (apply #'+ heights))
         (bottom (- total scroll))
         (top (max 0 (- bottom pane)))
         (pos 0) start end)
    (dotimes (i (length blocks))
      (let ((next (+ pos (nth i heights))))
        (when (and (< pos bottom) (> next top))
          (setq start (or start i))
          (setq end (1+ i)))
        (setq pos next)))
    (list :start (or start 0) :end (or end 0)
          :visible (cl-subseq blocks (or start 0) (or end 0)))))

(defun etaf-telega--layout ()
  "Return current width, pane rows and the visible message block slice.
The viewport follows the newest message unless the model holds a scroll
offset; every value is clamped so the slice always fits the pane."
  (let* ((width (etaf-telega--width))
         (pane (etaf-telega--pane-rows))
         (model etaf-telega--model)
         (blocks (if model
                     (etaf-telega-message-blocks
                      (etaf-telega-model-messages model)
                      (etaf-telega-model-selected-id model) width)
                   nil))
         (heights (mapcar (lambda (block) (plist-get block :height)) blocks))
         (total-height (apply #'+ heights))
         (scroll (min (max 0 (if model (etaf-telega-model-scroll model) 0))
                      (max 0 (- total-height pane))))
         (slice (etaf-telega--blocks-slice blocks pane scroll)))
    (list :width width :pane pane :blocks blocks :heights heights
          :total-height total-height :scroll scroll
          :start (plist-get slice :start) :end (plist-get slice :end)
          :visible (plist-get slice :visible))))

(defvar-local etaf-telega--paint-depth 0
  "Depth of the current synchronous paint batch.")

(defvar-local etaf-telega--paint-dirty nil
  "Non-nil when a paint was requested inside a batch.")

(defmacro etaf-telega--with-paint-batch (&rest body)
  "Run BODY and paint the model at most once afterwards.
Backend callbacks and model writes inside the batch coalesce into the one
projection that shows their combined result."
  (declare (indent 0) (debug t))
  `(let ((etaf-telega--paint-depth (1+ etaf-telega--paint-depth)))
     (prog1 (progn ,@body)
       (setq etaf-telega--paint-depth (1- etaf-telega--paint-depth))
       (when (and (zerop etaf-telega--paint-depth) etaf-telega--paint-dirty)
         (setq etaf-telega--paint-dirty nil)
         (etaf-telega--paint-now)))))

(defun etaf-telega--paint ()
  "Request a projection of the model into the mounted runtime.
Inside a paint batch the request waits for the batch to finish."
  (if (> etaf-telega--paint-depth 0)
      (setq etaf-telega--paint-dirty t)
    (etaf-telega--paint-now)))

(defun etaf-telega--view ()
  "Return the current projection of the model."
  (let ((layout (etaf-telega--layout))
        (chat-layout (etaf-telega--chat-layout))
        (chats (and etaf-telega--model (etaf-telega-model-chats etaf-telega--model))))
    (etaf-telega-view etaf-telega--model
                      (plist-get layout :width)
                      (plist-get layout :visible)
                      (plist-get chat-layout :chats)
                      (etaf-telega--filter-items chats)
                      (etaf-telega-model-filter etaf-telega--model)
                      (etaf-telega-model-screen etaf-telega--model)
                      #'etaf-telega-select-filter
                      #'etaf-telega-back
                      #'etaf-telega--run-action
                      (etaf-telega--list-width)
                      (etaf-telega--split-p))))

(defun etaf-telega--paint-now ()
  "Reproject the model into the mounted runtime.
A native editing session owns the buffer text, so its paint waits until the
session ends."
  (when (and etaf-telega--model (etaf-telega--window))
    (let ((runtime (etaf-telega--runtime)))
      (when runtime
        (if (etaf-text-input-active-p runtime)
            (setq etaf-telega--paint-pending t)
          (setq etaf-telega--paint-pending nil)
          (etaf-mount (current-buffer) (etaf-telega--view)
                      (list :viewport-width (window-body-width (etaf-telega--window) t)
                            :viewport-height (window-body-height (etaf-telega--window))))
          (setq-local cursor-type nil))))))

(defun etaf-telega--callbacks (buffer)
  "Return backend callbacks that update BUFFER's model."
  (cl-labels ((report (update)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (funcall update)
                    (etaf-telega--paint))))
              (clear-draft-for (text)
                (when (equal (etaf-telega-model-draft etaf-telega--model) text)
                  (etaf-telega-model-set-draft etaf-telega--model ""))))
    (list :status (lambda (status)
                    (report (lambda ()
                              (etaf-telega-model-set-status etaf-telega--model status))))
          :me (lambda (me)
                (report (lambda () (etaf-telega-model-set-me etaf-telega--model me))))
          :chats (lambda (chats)
                   (report (lambda ()
                             (etaf-telega-model-set-chats etaf-telega--model chats))))
          :messages (lambda (messages)
                      (report (lambda ()
                                (etaf-telega-model-set-messages
                                 etaf-telega--model (reverse messages)))))
          :older (lambda (messages)
                   (report (lambda ()
                             (let* ((older (reverse messages))
                                    (added (apply #'+
                                                  (mapcar (lambda (block)
                                                            (plist-get block :height))
                                                          (etaf-telega-message-blocks
                                                           older nil
                                                           (etaf-telega--width))))))
                               (etaf-telega-model-prepend-messages etaf-telega--model older)
                               ;; A reader who scrolled away keeps their place;
                               ;; a follower still watches the newest rows.
                               (when (> (etaf-telega-model-scroll etaf-telega--model) 0)
                                 (etaf-telega-model-set-scroll
                                  etaf-telega--model
                                  (+ (etaf-telega-model-scroll etaf-telega--model) added)))
                               (setq etaf-telega--older-pending nil)))))
          :sent (lambda (message)
                  (report (lambda ()
                            (clear-draft-for (plist-get message :text))
                            (etaf-telega-model-set-compose etaf-telega--model nil)
                            (etaf-telega-model-append-message etaf-telega--model message)
                            (when (plist-get message :id)
                              (etaf-telega-model-set-selected
                               etaf-telega--model (plist-get message :id))))))
          :edited (lambda (message)
                    (report (lambda ()
                              (clear-draft-for (plist-get message :text))
                              (etaf-telega-model-set-compose etaf-telega--model nil)
                              (etaf-telega-model-replace-message
                               etaf-telega--model message))))
          :deleted (lambda (id)
                     (report (lambda ()
                               (etaf-telega-model-remove-message
                                etaf-telega--model id))))
          :message (lambda (message)
                     (report (lambda ()
                               (etaf-telega-model-append-message
                                etaf-telega--model message))))
          :describe (lambda (describe)
                      (report (lambda ()
                                (etaf-telega-model-set-describe
                                 etaf-telega--model describe))))
          :error (lambda (message)
                   (report (lambda ()
                             (etaf-telega-model-set-status etaf-telega--model
                                                           (format "错误：%s" message))))))))

(defun etaf-telega-open-chat (chat)
  "Select CHAT and load its recent history."
  (interactive)
  (etaf-telega--with-paint-batch
    (let ((model (or etaf-telega--model (user-error "当前不是 Telegram 客户端")))
          (id (plist-get chat :id)))
      (etaf-telega-model-select model chat)
      (etaf-telega-model-set-cursor model (plist-get chat :id))
      (etaf-telega-model-set-screen model 'chat)
      (setq etaf-telega--older-pending nil)
      (etaf-telega--paint)
      (etaf-telega-backend-select etaf-telega--backend id))))

(defun etaf-telega-select-chat (id)
  "Select the chat with ID and load its history."
  (interactive "nChat id: ")
  (let ((chat (and etaf-telega--model (etaf-telega-model-chat-by-id
                                       etaf-telega--model id))))
    (unless chat (user-error "没有这个对话：%s" id))
    (etaf-telega-model-set-cursor etaf-telega--model id)
    (etaf-telega-open-chat chat)))

(defun etaf-telega-info (&optional buffer)
  "Return visible client facts for BUFFER without network I/O."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((model etaf-telega--model)
           (runtime (etaf-telega--runtime))
           (chat (and model (etaf-telega-model-chat model)))
           (compose (and model (etaf-telega-model-compose model))))
      (list :status (and model (etaf-telega-model-status model))
            :chats (length (and model (etaf-telega-model-chats model)))
            :chat (plist-get chat :id)
            :messages (length (and model (etaf-telega-model-messages model)))
            :selected (and model (etaf-telega-model-selected-id model))
            :scroll (and model (etaf-telega-model-scroll model))
            :screen (and model (etaf-telega-model-screen model))
            :filter (and model (etaf-telega-model-filter model))
            :cursor (and model (etaf-telega-model-cursor model))
            :compose (plist-get compose :kind)
            :draft (and model (etaf-telega-model-draft model))
            :split (etaf-telega--split-p)
            :editing (and runtime (and (etaf-text-input-active-p runtime) t))))))

(defun etaf-telega--compose-focus ()
  "Focus the in-layout composer and begin native editing."
  (when-let* ((runtime (etaf-telega--runtime)))
    (etaf-text-input-begin runtime 'telega-composer)))

(defun etaf-telega--move-chat (delta)
  "Move the chat-list cursor by DELTA entries."
  (let* ((model (or etaf-telega--model (user-error "当前不是 Telegram 客户端")))
         (chats (etaf-telega--filter-chats (etaf-telega-model-chats model)
                                           (etaf-telega-model-filter model)))
         (cursor (etaf-telega-model-cursor model))
         (index (cl-position cursor chats
                             :key (lambda (chat) (plist-get chat :id))
                             :test #'equal))
         (next (nth (max 0 (min (1- (length chats))
                                (+ (or index -1) delta)))
                    chats)))
    (when next
      (etaf-telega-model-set-cursor model (plist-get next :id))
      (etaf-telega--paint))))

(defun etaf-telega-open-selected-chat ()
  "Open the conversation selected in the chat list."
  (interactive)
  (let* ((model (or etaf-telega--model (user-error "当前不是 Telegram 客户端")))
         (cursor (etaf-telega-model-cursor model))
         (chat (and cursor (etaf-telega-model-chat-by-id model cursor))))
    (when chat (etaf-telega-open-chat chat))))

(defun etaf-telega-return ()
  "Open the selected conversation, or reload the open one."
  (interactive)
  (if (eq (etaf-telega-model-screen etaf-telega--model) 'root)
      (etaf-telega-open-selected-chat)
    (etaf-telega-reload-chat)))

(defun etaf-telega-next-chat ()
  "Select the next chat in the list."
  (interactive)
  (etaf-telega--move-chat 1))

(defun etaf-telega-previous-chat ()
  "Select the previous chat in the list."
  (interactive)
  (etaf-telega--move-chat -1))

(defun etaf-telega-first-chat ()
  "Select the first conversation in the chat list."
  (interactive)
  (let* ((model (or etaf-telega--model (user-error "当前不是 Telegram 客户端")))
         (chats (etaf-telega-model-chats model)))
    (when chats (etaf-telega-open-chat (car chats)))))

(defun etaf-telega-last-chat ()
  "Select the last conversation in the chat list."
  (interactive)
  (let* ((model (or etaf-telega--model (user-error "当前不是 Telegram 客户端")))
         (chats (etaf-telega-model-chats model)))
    (when chats (etaf-telega-open-chat (car (last chats))))))

(defun etaf-telega-reload-chat ()
  "Reload the selected chat's history."
  (interactive)
  (let ((chat (etaf-telega-model-chat etaf-telega--model)))
    (when chat (etaf-telega-open-chat chat))))

(defun etaf-telega-load-more ()
  "Load an older page of the selected chat's history."
  (interactive)
  (let* ((model (or etaf-telega--model (user-error "当前不是 Telegram 客户端")))
         (messages (etaf-telega-model-messages model)))
    (unless (etaf-telega-model-chat model) (user-error "先选择一个对话"))
    (when (and messages (not etaf-telega--older-pending))
      (setq etaf-telega--older-pending t)
      (etaf-telega-backend-history etaf-telega--backend
                                   :before (plist-get (car messages) :id)))))

(defun etaf-telega--scroll-to-selection ()
  "Adjust the viewport so the selected message's block is visible."
  (let* ((layout (etaf-telega--layout))
         (blocks (plist-get layout :blocks))
         (heights (plist-get layout :heights))
         (selected (etaf-telega-model-selected-id etaf-telega--model))
         (pane (plist-get layout :pane))
         (total (plist-get layout :total-height))
         (index (cl-position selected blocks
                             :key (lambda (block)
                                    (plist-get (plist-get block :message) :id))
                             :test #'equal)))
    (when index
      (let ((top 0))
        (dotimes (i index) (setq top (+ top (nth i heights))))
        (let* ((bottom (+ top (nth index heights)))
               (view-bottom (- total (plist-get layout :scroll)))
               (view-top (max 0 (- view-bottom pane))))
          (cond ((< top view-top)
                 (etaf-telega-model-set-scroll
                  etaf-telega--model
                  (max 0 (- total (min total (+ top pane))))))
                ((> bottom view-bottom)
                 (etaf-telega-model-set-scroll
                  etaf-telega--model (max 0 (- total bottom))))))))))

(defun etaf-telega--move-message (delta)
  "Move the message selection by DELTA messages."
  (let ((model (or etaf-telega--model (user-error "当前不是 Telegram 客户端"))))
    (etaf-telega-model-move-selection model delta)
    (etaf-telega--scroll-to-selection)
    (etaf-telega--paint)))

(defun etaf-telega--scroll-to (offset)
  "Set the message viewport OFFSET, clamped to the loaded history."
  (let ((layout (etaf-telega--layout)))
    (etaf-telega-model-set-scroll
     etaf-telega--model
     (min (max 0 (or offset 0))
          (max 0 (- (plist-get layout :total-height) (plist-get layout :pane)))))
    (etaf-telega--paint)))

(defun etaf-telega-page-up (&optional rows)
  "Scroll the message viewport towards older messages by ROWS.
At the top of the loaded history, load an older page."
  (interactive)
  (let* ((layout (etaf-telega--layout))
         (step (or rows (max 1 (1- (plist-get layout :pane))))))
    (etaf-telega--scroll-to (+ (plist-get layout :scroll) step))
    (when (and (zerop (plist-get (etaf-telega--layout) :start))
               (plist-get (etaf-telega--layout) :blocks))
      (etaf-telega-load-more))))

(defun etaf-telega-page-down (&optional rows)
  "Scroll the message viewport towards newer messages by ROWS."
  (interactive)
  (let* ((layout (etaf-telega--layout))
         (step (or rows (max 1 (1- (plist-get layout :pane))))))
    (etaf-telega--scroll-to (- (plist-get layout :scroll) step))))

(defun etaf-telega-follow ()
  "Scroll the message viewport to the newest message."
  (interactive)
  (etaf-telega--scroll-to 0))

(defun etaf-telega-next-message ()
  "Select the next message in the conversation."
  (interactive)
  (etaf-telega--move-message 1))

(defun etaf-telega-previous-message ()
  "Select the previous message in the conversation."
  (interactive)
  (etaf-telega--move-message -1))

(defun etaf-telega-compose ()
  "Focus the composer for typing."
  (interactive)
  (etaf-telega--compose-focus))

(defun etaf-telega-selected-message ()
  "Return the selected message or signal a user error."
  (let* ((model (or etaf-telega--model (user-error "当前不是 Telegram 客户端")))
         (message (etaf-telega-model-selected-message model)))
    (or message (user-error "先选中一条消息"))))

(defun etaf-telega-reply ()
  "Compose a reply to the selected message."
  (interactive)
  (let ((message (etaf-telega-selected-message)))
    (etaf-telega-model-set-compose etaf-telega--model
                                   (list :kind 'reply :target message))
    (etaf-telega--paint)
    (etaf-telega--compose-focus)))

(defun etaf-telega-edit ()
  "Edit the selected outgoing message in the composer."
  (interactive)
  (let* ((message (etaf-telega-selected-message))
         (model etaf-telega--model))
    (unless (plist-get message :outgoing)
      (user-error "只能编辑自己发送的消息"))
    (etaf-telega-model-set-compose
     model (list :kind 'edit :target message :draft (etaf-telega-model-draft model)))
    (etaf-telega-model-set-draft model (or (plist-get message :text) ""))
    (etaf-telega--paint)
    (etaf-telega--compose-focus)))

(defun etaf-telega-cancel-compose ()
  "Cancel the active reply or edit.
An edit restores the draft that was present before editing began."
  (interactive)
  (let ((model etaf-telega--model)
        (compose (and etaf-telega--model (etaf-telega-model-compose etaf-telega--model))))
    (when compose
      (when (eq (plist-get compose :kind) 'edit)
        (etaf-telega-model-set-draft model (or (plist-get compose :draft) "")))
      (etaf-telega-model-set-compose model nil)
      (etaf-telega--paint))))

(defun etaf-telega-copy ()
  "Copy the selected message's text."
  (interactive)
  (let ((message (etaf-telega-selected-message)))
    (kill-new (or (plist-get message :text) ""))
    (etaf-telega-model-set-status etaf-telega--model "已复制消息")
    (etaf-telega--paint)))

(defun etaf-telega-delete ()
  "Delete the selected message after confirmation."
  (interactive)
  (let ((message (etaf-telega-selected-message)))
    (when (yes-or-no-p "删除这条消息？")
      (etaf-telega--with-paint-batch
        (etaf-telega-backend-delete etaf-telega--backend message)
        (when etaf-telega--paint-pending (etaf-telega--paint))))))

(defun etaf-telega-send (text)
  "Send TEXT to the selected chat, honouring the compose state.
A reply carries its quoted message; an edit replaces the selected message.
The draft stays until the backend accepts it, so a failure keeps the text
available for retry."
  (interactive)
  (unless (etaf-telega-model-chat etaf-telega--model)
    (user-error "先选择一个对话"))
  (etaf-telega--with-paint-batch
    (unless (string-empty-p (string-trim text))
      (let ((compose (etaf-telega-model-compose etaf-telega--model)))
        (pcase (plist-get compose :kind)
          ('reply (etaf-telega-backend-send etaf-telega--backend text
                                            (plist-get compose :target)))
          ('edit (etaf-telega-backend-edit etaf-telega--backend
                                           (plist-get compose :target) text))
          (_ (etaf-telega-backend-send etaf-telega--backend text)))))
    (when etaf-telega--paint-pending (etaf-telega--paint))))

(defun etaf-telega-keep-draft (text)
  "Handle leaving the composer with TEXT.
An edit restores the draft captured before editing; any other text stays
available as the next draft."
  (when etaf-telega--model
    (let ((compose (etaf-telega-model-compose etaf-telega--model)))
      (if (eq (plist-get compose :kind) 'edit)
          (etaf-telega-model-set-draft etaf-telega--model
                                       (or (plist-get compose :draft) ""))
        (etaf-telega-model-set-draft etaf-telega--model text))
      (when compose
        (etaf-telega-model-set-compose etaf-telega--model nil))))
  (when etaf-telega--paint-pending (etaf-telega--paint)))

(defun etaf-telega-select-filter (key)
  "Show the conversations of filter KEY and return to the list."
  (interactive)
  (let ((model (or etaf-telega--model (user-error "当前不是 Telegram 客户端"))))
    (etaf-telega-model-set-filter model key)
    (etaf-telega-model-set-screen model 'root)
    (etaf-telega-model-set-chat-scroll model 0)
    (etaf-telega--paint)))

(defun etaf-telega-back ()
  "Leave the current screen: profiles and menus return to the conversation."
  (interactive)
  (when etaf-telega--model
    (etaf-telega-model-set-screen
     etaf-telega--model
     (if (memq (etaf-telega-model-screen etaf-telega--model)
               '(describe actions))
         'chat
       'root))
    (etaf-telega--paint)))

(defun etaf-telega-describe ()
  "Show the selected conversation's profile."
  (interactive)
  (let* ((model (or etaf-telega--model (user-error "当前不是 Telegram 客户端")))
         (chat (or (etaf-telega-model-chat model)
                   (etaf-telega-model-chat-by-id model
                                                 (etaf-telega-model-cursor model)))))
    (unless chat (user-error "先选择一个对话"))
    (etaf-telega-model-set-screen model 'describe)
    (etaf-telega--paint)
    (etaf-telega-backend-describe etaf-telega--backend (plist-get chat :id))))

(defun etaf-telega-actions ()
  "Show the selected message's action menu."
  (interactive)
  (etaf-telega-selected-message)
  (etaf-telega-model-set-screen etaf-telega--model 'actions)
  (etaf-telega--paint))

(defun etaf-telega--chat-action (action)
  "Apply ACTION to the open conversation and refresh its profile."
  (let ((chat (etaf-telega-model-chat etaf-telega--model)))
    (when chat
      (etaf-telega-backend-chat-action etaf-telega--backend
                                       (plist-get chat :id) action)
      (when (eq (etaf-telega-model-screen etaf-telega--model) 'describe)
        (etaf-telega-backend-describe etaf-telega--backend (plist-get chat :id))))))

(defun etaf-telega-toggle-pin ()
  "Toggle the open conversation's pinned state."
  (interactive)
  (etaf-telega--chat-action 'pin))

(defun etaf-telega-toggle-mute ()
  "Toggle the open conversation's muted state."
  (interactive)
  (etaf-telega--chat-action 'mute))

(defun etaf-telega--run-action (action)
  "Run ACTION from the message menu or the conversation profile."
  (pcase action
    ((or 'reply 'edit 'delete 'copy)
     (etaf-telega-model-set-screen etaf-telega--model 'chat)
     (pcase action
       ('reply (etaf-telega-reply))
       ('edit (etaf-telega-edit))
       ('delete (etaf-telega-delete))
       ('copy (etaf-telega-copy))))
    ((or 'pin 'mute 'read) (etaf-telega--chat-action action))))

(defun etaf-telega-quit ()
  "Leave the current screen, or close the client from the chat list."
  (interactive)
  (if (eq (etaf-telega-model-screen etaf-telega--model) 'root)
      (etaf-telega-close)
    (etaf-telega-back)))

(defvar etaf-telega-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (pair '(("j" . etaf-telega-next-chat) ("k" . etaf-telega-previous-chat)
                    ("C-n" . etaf-telega-next-chat) ("C-p" . etaf-telega-previous-chat)
                    ("M-<" . etaf-telega-first-chat) ("M->" . etaf-telega-last-chat)
                    ("n" . etaf-telega-next-message) ("p" . etaf-telega-previous-message)
                    ("SPC" . etaf-telega-page-down) ("S-SPC" . etaf-telega-page-up)
                    ("C-v" . etaf-telega-page-down) ("M-v" . etaf-telega-page-up)
                    ("r" . etaf-telega-reply) ("e" . etaf-telega-edit)
                    ("o" . etaf-telega-actions) ("t" . etaf-telega-describe)
                    ("d" . etaf-telega-delete) ("c" . etaf-telega-copy)
                    ("i" . etaf-telega-compose)
                    ("RET" . etaf-telega-return)
                    ("g" . etaf-telega-reload-chat)
                    ("q" . etaf-telega-quit)))
      (define-key map (kbd (car pair)) (cdr pair)))
    map)
  "Keys the client owns outside the composer.")

(defvar etaf-telega--command-maps (list (cons 'etaf-telega--model etaf-telega-mode-map)))

;;;###autoload
(defun etaf-telega-open ()
  "Mount the application in its own buffer and return that buffer."
  (interactive)
  (let ((buffer (get-buffer-create etaf-telega-buffer-name)))
    (with-current-buffer buffer
      (setq-local etaf-telega--model (etaf-telega-model)
                  etaf-telega--backend (funcall etaf-telega-backend-create-function)
                  etaf-telega--paint-pending nil)
      (use-local-map etaf-telega-mode-map)
      (setq-local emulation-mode-map-alists
                  (cons 'etaf-telega--command-maps emulation-mode-map-alists)))
    ;; The client owns the current window; never let display-buffer split it.
    (pop-to-buffer buffer '((display-buffer-same-window)))
    (with-current-buffer buffer
      (etaf-mount buffer (etaf-telega--view)
                  (list :viewport-width (window-body-width (etaf-telega--window) t)
                        :viewport-height (window-body-height (etaf-telega--window))))
      (setq-local cursor-type nil)
      (add-hook 'kill-buffer-hook #'etaf-telega--dispose nil t)
      (etaf-telega-backend-start etaf-telega--backend (etaf-telega--callbacks buffer))
      (etaf-runtime-flush (etaf-runtime-for-buffer buffer)))
    buffer))

(defun etaf-telega--dispose ()
  "Release the backend and runtime owned by this buffer."
  (when etaf-telega--backend
    (etaf-telega-backend-stop etaf-telega--backend)
    (setq etaf-telega--backend nil))
  (when-let* ((runtime (etaf-runtime-for-buffer (current-buffer))))
    (etaf-unmount runtime)))

;;;###autoload
(defun etaf-telega-close ()
  "Unmount the application buffer and kill it.
The backend stops observing without closing Telega's server or account."
  (interactive)
  (let ((buffer (get-buffer etaf-telega-buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer (etaf-telega--dispose))
      (kill-buffer buffer))))

(defvar-local etaf-telega--resize-timer nil
  "Pending debounced repaint after a window resize.")

(defun etaf-telega--resize-repaint (buffer)
  "Repaint BUFFER's client after its window size settled.
The projection bakes cell counts into the View, so a resize has to reproject
instead of only refitting the tree."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq etaf-telega--resize-timer nil)
      (when (and etaf-telega--model (etaf-telega--window))
        (etaf-telega--paint-now)))))

(defun etaf-telega--schedule-resize-repaint ()
  "Repaint this client shortly after its window size settles."
  (when (timerp etaf-telega--resize-timer)
    (cancel-timer etaf-telega--resize-timer))
  (setq etaf-telega--resize-timer
        (run-at-time 0.15 nil #'etaf-telega--resize-repaint (current-buffer))))

(defun etaf-telega--window-size-changed (frame)
  "Repaint every mounted client displayed in FRAME after it settles."
  (dolist (window (window-list frame 'no-minibuf))
    (with-current-buffer (window-buffer window)
      (when (and (bound-and-true-p etaf-telega--model) etaf-telega--model)
        (etaf-telega--schedule-resize-repaint)))))

(add-hook 'window-size-change-functions #'etaf-telega--window-size-changed)

(provide 'etaf-telega-app)
;;; etaf-telega-app.el ends here
