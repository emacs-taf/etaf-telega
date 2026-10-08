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
  "Return the conversation column width of the wide layout.
A width stored by a divider drag wins over the proportional default; both are
clamped so the message pane keeps a usable minimum."
  (let* ((body (or (and (etaf-telega--window)
                        (window-body-width (etaf-telega--window)))
                   80))
         (stored (and etaf-telega--model
                      (etaf-telega-model-list-width etaf-telega--model))))
    (if stored
        (max 24 (min stored (max 24 (- body 40))))
      (max 40 (min 52 (floor (* body 0.38)))))))

(defun etaf-telega--list-ratio ()
  "Return the conversation column's fraction of the window body."
  (let* ((body (max 1 (or (and (etaf-telega--window)
                               (window-body-width (etaf-telega--window)))
                          80)))
         (ratio (/ (float (etaf-telega--list-columns)) body)))
    (max 0.1 (min 0.9 ratio))))

(defun etaf-telega--resize-list (gesture)
  "Apply splitter drag GESTURE to the conversation column width.
The divider reports pixel deltas; the resulting cell width is stored in the
model so both the sidebar and the message pane reproject with one paint."
  (when (eq (plist-get gesture :phase) 'move)
    (let* ((model (or etaf-telega--model
                      (user-error "当前不是 Telegram 客户端")))
           (window (etaf-telega--window))
           (delta (or (car-safe (plist-get gesture :delta)) 0))
           (char (max 1 (if window (frame-char-width (window-frame window)) 8)))
           (current (etaf-telega--list-columns))
           (body (max 1 (or (and window (window-body-width window)) 80)))
           (width (min (max 24 (+ current (round (/ (float delta) char))))
                       (max 24 (- body 40)))))
      (etaf-telega-model-set-list-width model width)
      (etaf-telega--paint))))

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

(defun etaf-telega--line-pixels (&optional window)
  "Return one display line's height in pixels for WINDOW."
  (let ((window (or window (etaf-telega--window))))
    (max 1 (if window (frame-char-height (window-frame window)) 16))))

(defun etaf-telega--body-pixels (&optional window)
  "Return WINDOW's body height in pixels."
  (let ((window (or window (etaf-telega--window))))
    (max 1 (if window (window-body-height window t) 400))))

(defun etaf-telega--chat-rows ()
  "Return the conversation column's visible chat row count.
The row budget is measured in pixels: every row is two text lines plus its
separator, and the title bar and filter tabs own the rest of the frame, so
the list never overflows the window."
  (let* ((window (etaf-telega--window))
         (line (etaf-telega--line-pixels window))
         (chrome (+ (* 4 line) 2))       ; title bar and tabs, each with a border
         (row (+ (* 2 line) 1))          ; two text lines plus separator
         (available (- (etaf-telega--body-pixels window) chrome)))
    (max 1 (floor available row))))

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
The slice starts at the model's chat scroll offset; moving the cursor or
scrolling the list adjusts that offset instead of forcing the selection
into view on every layout."
  (let* ((model etaf-telega--model)
         (chats (if model
                    (etaf-telega--filter-chats (etaf-telega-model-chats model)
                                               (etaf-telega-model-filter model))
                  nil))
         (total (length chats))
         (visible (etaf-telega--chat-rows))
         (start (min (max 0 (if model (etaf-telega-model-chat-scroll model) 0))
                     (max 0 (- total visible)))))
    (list :total total :visible visible :start start :end (+ start visible)
          :chats (cl-subseq chats start (min total (+ start visible))))))

(defun etaf-telega--scroll-chat-to-selection ()
  "Scroll the conversation column so the selected chat stays visible."
  (let* ((model etaf-telega--model)
         (chats (and model
                     (etaf-telega--filter-chats (etaf-telega-model-chats model)
                                                (etaf-telega-model-filter model))))
         (visible (etaf-telega--chat-rows))
         (index (cl-position (and model (etaf-telega-model-cursor model)) chats
                             :key (lambda (chat) (plist-get chat :id))
                             :test #'equal))
         (start (or (and model (etaf-telega-model-chat-scroll model)) 0)))
    (when index
      (cond ((< index start) (setq start index))
            ((>= index (+ start visible)) (setq start (1+ (- index visible))))))
    (when model
      (etaf-telega-model-set-chat-scroll
       model (min (max 0 start) (max 0 (- (length chats) visible)))))))

(defun etaf-telega--pane-rows ()
  "Return the message pane's visible row count.
The pane owns the frame minus the title bar and the composer; both are
measured in pixels so the newest message never slides below the window."
  (let* ((window (etaf-telega--window))
         (line (etaf-telega--line-pixels window))
         (chrome (+ (* 2 line) 1        ; title bar and its border
                    (* 3 line) 1))      ; composer padding and its border
         (available (- (etaf-telega--body-pixels window) chrome)))
    (max 3 (floor available line))))

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
  "Return the current projection of the model.
The panes receive lazy slices, so the runtime tracks the model refs inside
each pane and a change re-renders only that pane."
  (let ((buffer (current-buffer)))
    (etaf-telega-view etaf-telega--model
                      (etaf-telega--width)
                      (etaf-telega--list-width)
                      (etaf-telega--list-ratio)
                      (etaf-telega--split-p)
                      (lambda ()
                        (with-current-buffer buffer
                          (plist-get (etaf-telega--layout) :visible)))
                      (lambda ()
                        (with-current-buffer buffer
                          (etaf-telega--chat-layout)))
                      (lambda ()
                        (with-current-buffer buffer
                          (etaf-telega--filter-items
                           (etaf-telega-model-chats etaf-telega--model))))
                      (lambda ()
                        (with-current-buffer buffer
                          (or (etaf-telega-model-selected-message etaf-telega--model)
                              (list))))
                      #'etaf-telega-select-filter
                      #'etaf-telega-back
                      #'etaf-telega--run-action
                      #'etaf-telega--resize-list)))

(defvar-local etaf-telega--remount-pending nil
  "Non-nil when a rebuild was deferred by a native editing session.")

(defvar-local etaf-telega--surface-rebuild nil
  "Non-nil when the next paint must rebuild the mounted runtime.
A publication can fail when an edit landed outside the retained surface's
owned cells; a fresh mount is the sanctioned recovery.  The flag is consumed
once so a persistent failure cannot rebuild in a loop.")

(defun etaf-telega--repaint-later (buffer)
  "Request one repaint of BUFFER outside the current error path."
  (run-at-time
   0 nil
   (lambda (live)
     (when (buffer-live-p live)
       (with-current-buffer live
         (when (and etaf-telega--model (etaf-telega--window))
           (etaf-telega--paint)))))
   buffer))

(defun etaf-telega--remount ()
  "Rebuild the mounted view in place.
The runtime tracks model refs by itself, so a rebuild is only needed when
non-reactive state changed (the palette, an explicit reload, a resize).
A native editing session owns the buffer text, so its rebuild waits until
the session ends."
  (when (and etaf-telega--model (etaf-telega--window))
    (let ((runtime (etaf-telega--runtime)))
      (if (and runtime (etaf-text-input-active-p runtime))
          (setq etaf-telega--remount-pending t)
        (setq etaf-telega--paint-pending nil
              etaf-telega--remount-pending nil
              etaf-telega--surface-rebuild nil)
        (etaf-mount (current-buffer) (lambda () (etaf-telega--view)))
        (setq-local cursor-type nil)))))

(defun etaf-telega--paint-now ()
  "Reproject the model into the mounted runtime.
A mounted runtime already subscribes every view expression to the model
refs, so a paint drains its pending work instead of rebuilding the whole
runtime; only the first paint mounts.  A native editing session owns the
buffer text, so its paint waits until the session ends; a rebuild deferred
meanwhile lands before the next ordinary flush."
  (when (and etaf-telega--model (etaf-telega--window))
    (let ((runtime (etaf-telega--runtime)))
      (cond
       ((and runtime (etaf-text-input-active-p runtime))
        (setq etaf-telega--paint-pending t))
       ((or etaf-telega--remount-pending etaf-telega--surface-rebuild)
        (etaf-telega--remount))
       ((and runtime (etaf-runtime-mounted-p runtime))
        (setq etaf-telega--paint-pending nil)
        (condition-case error
            (etaf-runtime-flush runtime)
          (error
           ;; The retained surface refused an update (an edit outside its
           ;; owned cells invalidates it).  Rebuild once on the next turn
           ;; instead of failing every later publication.
           (unless etaf-telega--surface-rebuild
             (setq etaf-telega--surface-rebuild t)
             (etaf-telega--repaint-later (current-buffer)))
           (signal (car error) (cdr error)))))
       (t (etaf-telega--remount))))))

(defvar-local etaf-telega--history-end nil
  "Non-nil once the backend reported an empty older page for the chat.")

(defvar-local etaf-telega--history-fill-count 0
  "Older pages the fill walk loaded since the last chat selection.")

(defvar-local etaf-telega--history-retry-count 0
  "Times the fill walk retried a stalled newest page for this chat.")

(defvar-local etaf-telega--fill-active nil
  "Non-nil while the history fill walk is loading pages.
Backend paints stay deferred so one fill shows one final frame instead of
one frame per page.")

(defconst etaf-telega--history-fill-limit 40
  "Most older pages one fill walk loads before it gives up.")

(defconst etaf-telega--history-retry-limit 3
  "Times a stalled fill walk retries the newest page before giving up.")

(defconst etaf-telega--history-retry-delay 0.8
  "Seconds before a stalled fill walk asks for the newest page again.")

(defun etaf-telega--history-filled-p ()
  "Return non-nil when the loaded history already fills the pane."
  (let ((layout (ignore-errors (etaf-telega--layout))))
    (and layout
         (>= (or (plist-get layout :total-height) 0)
             (or (plist-get layout :pane) 0)))))

(defun etaf-telega--retry-history (chat-id)
  "Re-request CHAT-ID's newest page after a stalled fill walk."
  (when-let* ((model etaf-telega--model)
              (chat (etaf-telega-model-chat model)))
    (when (equal (plist-get chat :id) chat-id)
      (etaf-telega-backend-select etaf-telega--backend chat-id))))

(defun etaf-telega--schedule-history-retry (chat-id)
  "Retry CHAT-ID's newest page after a short delay.
TDLib can answer an opening request with only its locally cached page and
finish downloading the rest moments later, so a stalled fill waits for it."
  (when (and chat-id (< etaf-telega--history-retry-count
                         etaf-telega--history-retry-limit))
    (cl-incf etaf-telega--history-retry-count)
    (let ((buffer (current-buffer)))
      (run-at-time etaf-telega--history-retry-delay nil
                   (lambda ()
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (etaf-telega--retry-history chat-id))))))
    t))

(defun etaf-telega--fill-history ()
  "Load older pages until the pane is filled or the history ends.
Opening a conversation must show its history, not only the page TDLib
happened to have cached; an empty page ends the walk, with a few retries
of the newest page while a stalled backend may still be downloading it.
Backend paints are deferred until the walk settles, so one fill shows one
final frame instead of one frame per page."
  (let ((chat (etaf-telega-model-chat etaf-telega--model)))
    (cond
     ((null chat) nil)
     ((null (etaf-telega-model-messages etaf-telega--model))
      (when etaf-telega--fill-active
        (setq etaf-telega--fill-active nil)
        (etaf-telega--paint)))
     ((etaf-telega--history-filled-p)
      (setq etaf-telega--fill-active nil)
      (etaf-telega--paint))
     ((and (not etaf-telega--history-end)
           (< etaf-telega--history-fill-count etaf-telega--history-fill-limit)
           (not etaf-telega--older-pending))
      (setq etaf-telega--fill-active t)
      (cl-incf etaf-telega--history-fill-count)
      (etaf-telega-load-more))
     (etaf-telega--history-end
      (setq etaf-telega--fill-active nil)
      (etaf-telega--paint)
      (etaf-telega--schedule-history-retry (plist-get chat :id)))
     (t
      (setq etaf-telega--fill-active nil)
      (etaf-telega--paint)))))

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
the echo area and can hang Emacs.  The status line carries a clipped reason
instead, and a dead buffer ends the report."
  (condition-case nil
      (etaf-telega-model-set-status
       etaf-telega--model
       (format "回调错误：%s" (etaf-telega--clip-text (error-message-string error))))
    ;; The boundary itself must not raise: the buffer may be dead or the
    ;; model already released while this callback drains.
    (error nil)))

(defun etaf-telega--callbacks (buffer)
  "Return backend callbacks that update BUFFER's model."
  (cl-labels ((report (update)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (condition-case error
                        (progn
                          (funcall update)
                          (unless etaf-telega--fill-active
                            (etaf-telega--paint)))
                      ((error quit)
                       (etaf-telega--report-callback-failure error))))))
              (clear-draft-for (text)
                (when (equal (etaf-telega-model-draft etaf-telega--model) text)
                  (etaf-telega-model-set-draft etaf-telega--model ""))))
    (list :status (lambda (status)
                    (report (lambda ()
                              (etaf-telega-model-set-status
                               etaf-telega--model
                               (etaf-telega--clip-text status)))))
          :me (lambda (me)
                (report (lambda () (etaf-telega-model-set-me etaf-telega--model me))))
          :chats (lambda (chats)
                   (report (lambda ()
                             (etaf-telega-model-set-chats etaf-telega--model chats))))
          :messages (lambda (messages)
                      (report (lambda ()
                                (setq etaf-telega--history-end (null messages))
                                (etaf-telega-model-set-messages
                                 etaf-telega--model (reverse messages))
                                (etaf-telega--fill-history))))
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
                               (setq etaf-telega--older-pending nil
                                     etaf-telega--history-end (null messages))
                               (etaf-telega--fill-history)))))
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
                             (setq etaf-telega--fill-active nil)
                             (etaf-telega-model-set-status
                              etaf-telega--model
                              (format "错误：%s" (etaf-telega--clip-text message)))))))))

(defun etaf-telega-open-chat (chat)
  "Select CHAT and load its recent history."
  (interactive)
  (etaf-telega--with-paint-batch
    (let ((model (or etaf-telega--model (user-error "当前不是 Telegram 客户端")))
          (id (plist-get chat :id)))
      (etaf-telega-model-select model chat)
      (setq etaf-telega--history-end nil
            etaf-telega--history-fill-count 0
            etaf-telega--history-retry-count 0
            etaf-telega--fill-active t)
      (etaf-telega-model-set-cursor model (plist-get chat :id))
      (etaf-telega--scroll-chat-to-selection)
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
            :list-width (and (etaf-telega--split-p) (etaf-telega--list-columns))
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
      (etaf-telega--scroll-chat-to-selection)
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

(defun etaf-telega--selection-at-oldest-p ()
  "Return non-nil when the selected message is the oldest loaded one."
  (let ((model etaf-telega--model)
        (messages (etaf-telega-model-messages etaf-telega--model)))
    (and model messages
         (equal (etaf-telega-model-selected-id model)
                (plist-get (car messages) :id)))))

(defun etaf-telega--move-message (delta)
  "Move the message selection by DELTA messages.
Walking past the oldest loaded message loads an older page, like Telega's
chat buffer, so paging never strands the reader at a page boundary."
  (let ((model (or etaf-telega--model (user-error "当前不是 Telegram 客户端"))))
    (etaf-telega-model-move-selection model delta)
    (etaf-telega--scroll-to-selection)
    (when (and (< delta 0)
               (not etaf-telega--history-end)
               (etaf-telega--selection-at-oldest-p))
      (etaf-telega-load-more))
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

(defun etaf-telega-scroll-chats (delta)
  "Scroll the conversation column by DELTA rows.
Positive DELTA walks toward later conversations; the selected chat is not
moved, so the wheel browses the list without changing the selection."
  (let* ((model etaf-telega--model)
         (chats (and model
                     (etaf-telega--filter-chats (etaf-telega-model-chats model)
                                                (etaf-telega-model-filter model))))
         (visible (etaf-telega--chat-rows))
         (start (min (max 0 (+ (or (and model (etaf-telega-model-chat-scroll model)) 0)
                               delta))
                     (max 0 (- (length chats) visible)))))
    (when model
      (etaf-telega-model-set-chat-scroll model start)
      (etaf-telega--paint))))

(defun etaf-telega-scroll-messages (delta)
  "Scroll the message viewport by DELTA rows.
Positive DELTA walks toward newer messages; scrolling past the top of the
loaded history loads an older page."
  (let ((layout (etaf-telega--layout)))
    (etaf-telega--scroll-to
     (- (plist-get layout :scroll) (* delta 3)))
    (when (and (> delta 0)
               (zerop (plist-get (etaf-telega--layout) :start))
               (plist-get (etaf-telega--layout) :blocks))
      (etaf-telega-load-more))))

(defun etaf-telega--wheel (event delta)
  "Scroll the pane under EVENT by DELTA rows (positive = down)."
  (let* ((position (event-start event))
         (window (posn-window position)))
    (when (and (window-live-p window) (null (posn-area position)))
      (with-current-buffer (window-buffer window)
        (when etaf-telega--model
          (let* ((pixels (posn-x-y position))
                 (column (and (consp pixels)
                              (floor (/ (float (car pixels))
                                        (window-font-width window)))))
                 (screen (etaf-telega-model-screen etaf-telega--model)))
            (cond
             ((eq screen 'root) (etaf-telega-scroll-chats (* delta 3)))
             ((and (etaf-telega--split-p)
                   column
                   (< column (etaf-telega--list-width)))
              (etaf-telega-scroll-chats (* delta 3)))
             (t (etaf-telega-scroll-messages delta)))))))))

(defun etaf-telega-wheel-up (event)
  "Scroll the pane under EVENT towards older content."
  (interactive "e")
  (etaf-telega--wheel event -1))

(defun etaf-telega-wheel-down (event)
  "Scroll the pane under EVENT towards newer content."
  (interactive "e")
  (etaf-telega--wheel event 1))

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
        (when (or etaf-telega--paint-pending etaf-telega--remount-pending)
          (etaf-telega--paint))))))

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
    (when (or etaf-telega--paint-pending etaf-telega--remount-pending)
      (etaf-telega--paint))))

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
  (when (or etaf-telega--paint-pending etaf-telega--remount-pending)
    (etaf-telega--paint)))

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

(defun etaf-telega-toggle-theme ()
  "Switch the client between the dark and light palettes."
  (interactive)
  (setq etaf-telega-theme
        (if (eq (etaf-telega--theme-name) 'dark) 'light 'dark))
  (when etaf-telega--model
    (etaf-telega--remount))
  (message "etaf-telega 主题：%s" (if (eq (etaf-telega--theme-name) 'dark)
                                      "dark" "light")))

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
                    ("<wheel-up>" . etaf-telega-wheel-up)
                    ("<wheel-down>" . etaf-telega-wheel-down)
                    ("n" . etaf-telega-next-message) ("p" . etaf-telega-previous-message)
                    ("SPC" . etaf-telega-page-down) ("S-SPC" . etaf-telega-page-up)
                    ("C-v" . etaf-telega-page-down) ("M-v" . etaf-telega-page-up)
                    ("r" . etaf-telega-reply) ("e" . etaf-telega-edit)
                    ("o" . etaf-telega-actions) ("t" . etaf-telega-describe)
                    ("d" . etaf-telega-delete) ("c" . etaf-telega-copy)
                    ("i" . etaf-telega-compose)
                    ("RET" . etaf-telega-return)
                    ("g" . etaf-telega-reload-chat)
                    ("T" . etaf-telega-toggle-theme)
                    ("q" . etaf-telega-quit)))
      (define-key map (kbd (car pair)) (cdr pair)))
    (define-key map [wheel-up] #'etaf-telega-wheel-up)
    (define-key map [wheel-down] #'etaf-telega-wheel-down)
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
                  etaf-telega--paint-pending nil
                  etaf-telega--remount-pending nil
                  etaf-telega--surface-rebuild nil)
      (use-local-map etaf-telega-mode-map)
      (setq-local emulation-mode-map-alists
                  (cons 'etaf-telega--command-maps emulation-mode-map-alists)))
    ;; The client owns the current window; never let display-buffer split it.
    (pop-to-buffer buffer '((display-buffer-same-window)))
    (with-current-buffer buffer
      (etaf-mount buffer (lambda () (etaf-telega--view)))
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
        (etaf-telega--remount)))))

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
