;;; etaf-telega-views.el --- Views of etaf-telega -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Views are pure projections: they read refs and return structure.  The shell
;; provides the application palette to the catalog, so the composer field and
;; every later etaf-ui component render in the application colors.
;;
;; The message view keeps one row per message so selection, reply quotes and
;; future per-message affordances have a home; day separators group the rows
;; exactly like Telega's own chat buffer.

;;; Code:

(require 'cl-lib)
(require 'etaf)
(require 'etaf-ui)
(require 'etaf-telega-theme)

(declare-function etaf-telega-send "etaf-telega-app" (text))
(declare-function etaf-telega-keep-draft "etaf-telega-app" (text))
(declare-function etaf-telega-open-chat "etaf-telega-app" (chat))

(defconst etaf-telega-sidebar-width 28
  "Sidebar width in character cells.")

(defun etaf-telega--wrap (text width)
  "Wrap TEXT at WIDTH display cells, preserving empty lines."
  (let (lines)
    (dolist (line (split-string (replace-regexp-in-string "\t" "    " text t t) "\n" nil))
      (while (> (string-width line) width)
        (let* ((piece (truncate-string-to-width line width))
               (count (max 1 (length piece))))
          (push (substring line 0 count) lines)
          (setq line (substring line count))))
      (push line lines))
    (nreverse lines)))

(defun etaf-telega--one-line (text &optional limit)
  "Return TEXT's first line, truncated to LIMIT display cells."
  (let ((line (car (split-string (or text "") "\n" nil))))
    (if (and limit (> (string-width line) limit))
        (concat (truncate-string-to-width line (max 1 (- limit 1))) "…")
      line)))

(defun etaf-telega--separator-text (label width)
  "Return a one-line day separator LABEL centred in WIDTH cells."
  (let* ((label (format " %s " (or label "")))
         (inner (max 0 (- width (string-width label) 4)))
         (left (/ inner 2))
         (right (- inner left)))
    (concat "  " (make-string left ?─) label (make-string right ?─))))

(defun etaf-telega--initial (title)
  "Return TITLE's avatar initial."
  (let ((title (or title "")))
    (if (string-empty-p title) "?" (upcase (substring title 0 1)))))

(defun etaf-telega--message-body-text (message)
  "Return MESSAGE's body text, including its edited mark."
  (concat (or (plist-get message :text) "")
          (if (plist-get message :edited) "（已编辑）" "")))

(defun etaf-telega--message-stamp (message)
  "Return MESSAGE's Telega-style stamp: the time today, its date otherwise."
  (let ((date (plist-get message :date))
        (time (or (plist-get message :time) "")))
    (cond ((null date) time)
          ((equal date (format-time-string "%Y-%m-%d")) time)
          (t (let ((parts (split-string date "-")))
               (if (= 3 (length parts))
                   (format "%s.%s.%s" (nth 2 parts) (nth 1 parts)
                           (substring (nth 0 parts) 2))
                 date))))))

(defun etaf-telega-message-body (message width)
  "Return MESSAGE's wrapped body text at WIDTH."
  (mapconcat #'identity
             (etaf-telega--wrap (etaf-telega--message-body-text message)
                                (max 8 width))
             "\n"))

(defun etaf-telega-message-blocks (messages selected width)
  "Return display blocks for MESSAGES at WIDTH.
Each block is a plist with `:message', its display `:height' in rows (at
least the avatar column), `:selected' and the day `:separator' that opens
it.  The viewport slices blocks, so wrapping happens once per paint."
  (let (blocks last-date)
    (dolist (message messages)
      (let (separator)
        (when-let* ((date (plist-get message :date)))
          (unless (equal date last-date)
            (setq separator date
                  last-date date)))
        (let* ((lines (length (etaf-telega--wrap
                               (etaf-telega--message-body-text message)
                               (max 8 (- width 6)))))
               (content (+ 1 (if (plist-get message :reply) 1 0) (max 1 lines))))
          (push (list :message message :separator separator
                      :selected (equal (plist-get message :id) selected)
                      :height (+ (if separator 1 0) (max 3 content)))
                blocks))))
    (nreverse blocks)))

(defun etaf-telega--compose-label (compose)
  "Return COMPOSE's prompt line, or nil."
  (let ((target (plist-get compose :target)))
    (pcase (plist-get compose :kind)
      ('reply (format "回复 %s：%s　C-c C-k 取消回复"
                      (or (plist-get target :sender) "?")
                      (etaf-telega--one-line (plist-get target :text) 40)))
      ('edit (format "编辑消息：%s　C-c C-k 取消编辑"
                     (etaf-telega--one-line (plist-get target :text) 40))))))

(etaf-define-component etaf-telega-avatar (&key title)
  "Render a two-cell avatar block for TITLE."
  :view
  (flex :width '(ch 2) :height '(lh 2) :align-items 'center :justify-content 'center
        :background-color (etaf-telega-sender-color title)
    (text :color (etaf-telega-color :bg) :font-weight 'bold
          (expr (etaf-telega--initial title)))))

(etaf-define-component etaf-telega-chat-row (&key chat selected)
  "Render one CHAT row; pressing it opens the conversation."
  :view
  (flex :flex-direction 'row :align-items 'center :width 'stretch
        :padding '((lh 0) (ch 1)) :column-gap '(ch 1)
        :background-color (if selected (etaf-telega-color :accent-soft)
                            (etaf-telega-color :surface))
        :role 'button :tab-index 0
        :on-press (lambda () (etaf-telega-open-chat chat))
    (etaf-avatar :name (or (plist-get chat :title) "?")
                 :initials (etaf-telega--initial (plist-get chat :title))
                 :size '(ch 4)
                 :variant (if (plist-get chat :pinned) 'info 'success))
    (flex :flex-direction 'column :flex-grow 1 :flex-basis '(ch 0) :min-width '(ch 0)
      (flex :flex-direction 'row :align-items 'center :column-gap '(ch 1)
        (box :flex-grow 1 :flex-basis '(ch 0) :min-width '(ch 0)
             :height '(lh 1) :overflow 'hidden :wrap-mode 'none
          (text :color (etaf-telega-color :ink)
                :font-weight (if selected 'bold 'normal)
                (expr (or (plist-get chat :title) "?"))))
        (text :color (etaf-telega-color :muted) (expr (or (plist-get chat :time) ""))))
      (box :height '(lh 1) :overflow 'hidden :wrap-mode 'none
        (text :color (etaf-telega-color :muted)
              (expr (or (plist-get chat :preview) "")))))
    (expr (when (> (or (plist-get chat :unread) 0) 0)
            (etaf-node 'etaf-badge
                       (list :text (number-to-string (plist-get chat :unread))
                             :variant 'info)
                       nil)))))

(etaf-define-component etaf-telega-day-separator (&key label width)
  "Render a muted day separator between message groups."
  :view
  (box :height '(lh 1) :overflow 'hidden :wrap-mode 'none
    (text :color (etaf-telega-color :muted)
          (expr (etaf-telega--separator-text label width)))))

(etaf-define-component etaf-telega-message-block (&key block width)
  "Render one message BLOCK with its day separator and avatar."
  :view
  (flex :flex-direction 'column :width 'stretch
    (flex :if (plist-get block :separator)
      (etaf-telega-day-separator :label (plist-get block :separator)
                                 :width width))
    (flex :flex-direction 'row :align-items 'flex-start :width 'stretch
          :padding '((lh 0) (ch 1)) :column-gap '(ch 1)
          :background-color (when (plist-get block :selected)
                              (etaf-telega-color :accent-soft))
      (etaf-avatar :name (or (plist-get (plist-get block :message) :sender) "?")
                   :initials (etaf-telega--initial
                              (plist-get (plist-get block :message) :sender))
                   :size '(ch 4)
                   :variant (if (plist-get (plist-get block :message) :outgoing)
                                'info 'success))
      (flex :flex-direction 'column :flex-grow 1 :flex-basis '(ch 0) :min-width '(ch 0)
        (flex :flex-direction 'row :align-items 'center :width 'stretch
              :height '(lh 1) :column-gap '(ch 1)
          (box :flex-grow 1 :flex-basis '(ch 0) :min-width '(ch 0)
               :height '(lh 1) :overflow 'hidden :wrap-mode 'none
            (text :color (if (plist-get (plist-get block :message) :outgoing)
                             (etaf-telega-color :accent)
                           (etaf-telega-sender-color
                            (plist-get (plist-get block :message) :sender)))
                  :font-weight 'bold
                  (expr (or (plist-get (plist-get block :message) :sender) "?"))))
          (text :color (etaf-telega-color :muted)
                (expr (etaf-telega--message-stamp (plist-get block :message)))))
        (expr (when-let* ((reply (plist-get (plist-get block :message) :reply)))
                (list (etaf-node
                       'text (list :color (etaf-telega-color :muted))
                       (list (format "↩ %s：%s"
                                     (or (plist-get reply :sender) "?")
                                     (etaf-telega--one-line (plist-get reply :text) 36)))))))
        (text :color (etaf-telega-color :ink)
              (expr (etaf-telega-message-body (plist-get block :message)
                                              (max 8 (- width 6)))))))))

(etaf-define-component etaf-telega-shell
    (&key model blocks chats filters filter screen width on-filter on-back)
  "Render the client shell for MODEL.
BLOCKS is the visible message slice, CHATS the visible conversation slice,
SCREEN is `root' or `chat', and FILTERS/FILTER drive the filter tabs."
  :setup (etaf-theme-provide (etaf-telega-theme-defaults))
  :view
  (flex :flex-direction 'column :width 'stretch :height 'stretch
        :background-color (etaf-telega-color :bg)
    (flex :flex-direction 'row :align-items 'center
          :padding (list (list 'lh 1) (list 'ch 2)) :column-gap '(ch 2)
          :background-color (etaf-telega-color :surface-2)
          :border-bottom (list '(px 1) 'solid (etaf-telega-color :border))
      (flex :if (eq screen 'chat) :width '(ch 2) :flex-grow 0
            :flex-basis '(ch 2) :min-width '(ch 2)
        (etaf-button :label "‹" :variant 'ghost :on-press on-back))
      (text :color (etaf-telega-color :ink) :font-weight 'bold
            (expr (if (eq screen 'chat)
                      (or (plist-get (etaf-value (plist-get model :chat)) :title)
                          "对话")
                    "Telegram")))
      (box :flex-grow 1 :flex-basis '(ch 0) :min-width '(ch 0) "")
      (text :color (etaf-telega-color :muted)
            (expr (etaf-value (plist-get model :status)))))
    (flex :if (eq screen 'root) :flex-direction 'row :padding '((lh 0) (ch 2))
          :background-color (etaf-telega-color :surface)
          :border-bottom (list '(px 1) 'solid (etaf-telega-color :border))
      (etaf-tabs :items filters :active filter :on-change on-filter))
    (flex :if (eq screen 'root) :flex-direction 'column :flex-grow 1
          :flex-basis '(lh 0) :min-height '(lh 1) :width 'stretch :overflow 'hidden
      (expr (mapcar (lambda (chat)
                      (etaf-view
                       (etaf-telega-chat-row
                        :key (plist-get chat :id) :chat chat
                        :selected (equal (plist-get chat :id)
                                         (etaf-value (plist-get model :cursor))))))
                    chats)))
    (flex :if (eq screen 'chat) :flex-direction 'column :flex-grow 1
          :flex-basis '(lh 0) :min-height '(lh 1) :width 'stretch
      (flex :flex-direction 'column :flex-grow 1 :flex-basis '(lh 0)
            :min-height '(lh 1) :overflow 'hidden :wrap-mode 'none
            :padding '((lh 0) (ch 2))
        (expr (or (mapcar (lambda (block)
                            (etaf-view (etaf-telega-message-block
                                        :block block :width width)))
                          blocks)
                  (list (etaf-node
                         'text (list :color (etaf-telega-color :muted))
                         (list (if (etaf-value (plist-get model :chat))
                                   "" "选择一个对话")))))))
      (flex :flex-direction 'column :width 'stretch :height '(lh 4)
            :background-color (etaf-telega-color :surface)
            :border-top (list '(px 1) 'solid (etaf-telega-color :border))
            :padding '((lh 0) (ch 2))
        (expr (when-let* ((compose (etaf-value (plist-get model :compose))))
                (list (etaf-node
                       'text (list :color (etaf-telega-color :accent))
                       (list (etaf-telega--compose-label compose))))))
        (etaf-text-input
         :ref 'telega-composer :bind (plist-get model :draft)
         :label "消息" :hide-label t :rows 2
         :width (list 'ch (max 20 (- width 4)))
         :placeholder "发消息…" :submit-on-return t
         :on-submit #'etaf-telega-send
         :on-cancel #'etaf-telega-keep-draft)))))

(defun etaf-telega-view (model width blocks chats filters filter screen
                            on-filter on-back)
  "Return the application View for MODEL.
WIDTH is the message cell count, BLOCKS the visible message slice, CHATS
the visible conversation slice, FILTERS/FILTER the filter tabs and SCREEN
the active screen."
  (etaf-view (etaf-telega-shell :model model :blocks blocks :chats chats
                                :filters filters :filter filter :screen screen
                                :on-filter on-filter :on-back on-back
                                :width width)))

(provide 'etaf-telega-views)
;;; etaf-telega-views.el ends here
