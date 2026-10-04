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

(defun etaf-telega--initial (title)
  "Return TITLE's avatar initial."
  (let ((title (or title "")))
    (if (string-empty-p title) "?" (upcase (substring title 0 1)))))

(defun etaf-telega--message-body-text (message)
  "Return MESSAGE's body text, including its edited mark."
  (concat (or (plist-get message :text) "")
          (if (plist-get message :edited) "（已编辑）" "")))

(defun etaf-telega-message-rows (messages selected width)
  "Return the display rows for MESSAGES at WIDTH.
Each row is a plist whose `:kind' is `separator', `reply', `header' or
`body'; SELECTED is the selected message id.  The viewport renders a slice
of this list, so wrapping and grouping happen exactly once per paint."
  (let (rows last-date)
    (dolist (message messages)
      (when-let* ((date (plist-get message :date)))
        (unless (equal date last-date)
          (push (list :kind 'separator :label date :width width) rows)
          (setq last-date date)))
      (let ((chosen (equal (plist-get message :id) selected)))
        (when-let* ((reply (plist-get message :reply)))
          (push (list :kind 'reply :message message :selected chosen
                      :text (format "  ↩ %s：%s"
                                    (or (plist-get reply :sender) "?")
                                    (etaf-telega--one-line (plist-get reply :text) 36)))
                rows))
        (push (list :kind 'header :message message :selected chosen) rows)
        (dolist (line (etaf-telega--wrap (etaf-telega--message-body-text message)
                                         (max 8 (- width 2))))
          (push (list :kind 'body :message message :selected chosen
                      :text (concat "  " line))
                rows))))
    (nreverse rows)))

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
  "Render one CHAT row; pressing it selects the chat."
  :view
  (flex :flex-direction 'row :align-items 'center :width 'stretch
        :padding '((lh 0) (ch 1)) :column-gap '(ch 1)
        :background-color (if selected (etaf-telega-color :accent-soft)
                            (etaf-telega-color :surface))
        :role 'button :tab-index 0
        :on-press (if selected nil (lambda () (etaf-telega-open-chat chat)))
    (etaf-telega-avatar :title (plist-get chat :title))
    (flex :flex-direction 'column :flex-grow 1 :flex-basis '(px 0) :min-width '(ch 0)
      (flex :flex-direction 'row :align-items 'center :column-gap '(ch 1)
        (box :flex-grow 1 :flex-basis '(px 0) :min-width '(ch 0)
             :height '(lh 1) :overflow 'hidden :wrap-mode 'none
          (text :color (etaf-telega-color :ink)
                :font-weight (if selected 'bold 'normal)
                (expr (or (plist-get chat :title) "?"))))
        (text :color (etaf-telega-color :muted) (expr (or (plist-get chat :time) ""))))
      (box :height '(lh 1) :overflow 'hidden :wrap-mode 'none
        (text :color (etaf-telega-color :muted) (expr (or (plist-get chat :preview) "")))))
    (expr (when (> (or (plist-get chat :unread) 0) 0)
            (etaf-node 'etaf-badge
                       (list :text (number-to-string (plist-get chat :unread))
                             :variant 'info)
                       nil)))))

(defun etaf-telega--separator-text (label width)
  "Return a one-line day separator LABEL centred in WIDTH cells."
  (let* ((label (format " %s " (or label "")))
         (inner (max 0 (- width (string-width label) 4)))
         (left (/ inner 2))
         (right (- inner left)))
    (concat "  " (make-string left ?─) label (make-string right ?─))))

(etaf-define-component etaf-telega-day-separator (&key label width)
  "Render a muted day separator between message groups."
  :view
  (box :height '(lh 1) :overflow 'hidden :wrap-mode 'none
    (text :color (etaf-telega-color :muted)
          (expr (etaf-telega--separator-text label width)))))

(etaf-define-component etaf-telega-message-line (&key row width)
  "Render one display ROW of the message viewport."
  :render
  (let ((background (when (plist-get row :selected)
                      (etaf-telega-color :accent-soft))))
    (pcase (plist-get row :kind)
      ('separator
       (etaf-node 'box (list :height '(lh 1) :overflow 'hidden :wrap-mode 'none)
                  (list (etaf-node
                         'text (list :color (etaf-telega-color :muted))
                         (list (etaf-telega--separator-text (plist-get row :label) width))))))
      ('reply
       (etaf-node 'box (list :height '(lh 1) :overflow 'hidden :wrap-mode 'none
                             :background-color background)
                  (list (etaf-node
                         'text (list :color (etaf-telega-color :muted))
                         (list (plist-get row :text))))))
      ('header
       (let ((message (plist-get row :message)))
         (etaf-node 'box (list :height '(lh 1) :overflow 'hidden :wrap-mode 'none
                               :background-color background)
                    (list (etaf-node
                           'text (list :color (if (plist-get message :outgoing)
                                                  (etaf-telega-color :accent)
                                                (etaf-telega-sender-color
                                                 (plist-get message :sender)))
                                       :font-weight 'bold)
                           (list (or (plist-get message :sender) "?")))
                          (etaf-node
                           'text (list :color (etaf-telega-color :muted))
                           (list (format "  %s" (or (plist-get message :time) ""))))))))
      ('body
       (etaf-node 'box (list :height '(lh 1) :overflow 'hidden :wrap-mode 'none
                             :background-color background)
                  (list (etaf-node
                         'text (list :color (etaf-telega-color :ink))
                         (list (plist-get row :text)))))))))

(etaf-define-component etaf-telega-shell (&key model rows width)
  "Render the client shell for MODEL; ROWS is the visible message slice."
  :setup (etaf-theme-provide (etaf-telega-theme-defaults))
  :view
  (flex :flex-direction 'row :width 'stretch :height 'stretch
        :background-color (etaf-telega-color :bg)
    (flex :flex-direction 'column :width (list 'ch etaf-telega-sidebar-width)
          :height 'stretch :background-color (etaf-telega-color :surface)
          :border-right (list '(px 1) 'solid (etaf-telega-color :border))
      (flex :flex-direction 'row :align-items 'center
            :padding (list (list 'lh 1) (list 'ch 1)) :column-gap '(ch 1)
        (text :color (etaf-telega-color :ink) :font-weight 'bold "Telegram")
        (box :flex-grow 1 :flex-basis '(ch 0) :min-width '(ch 0) "")
        (text :color (etaf-telega-color :muted)
              (expr (or (plist-get (etaf-value (plist-get model :me)) :name) ""))))
      (flex :flex-direction 'column
        (expr (mapcar (lambda (chat)
                        (etaf-view
                         (etaf-telega-chat-row
                          :key (plist-get chat :id) :chat chat
                          :selected (equal (plist-get chat :id)
                                           (plist-get (etaf-value (plist-get model :chat)) :id)))))
                      (etaf-value (plist-get model :chats))))))
    (flex :flex-direction 'column :flex-grow 1 :flex-basis '(ch 0)
          :min-width '(ch 0)
      (flex :flex-direction 'row :align-items 'center
            :padding (list (list 'lh 1) (list 'ch 2))
            :background-color (etaf-telega-color :surface-2)
            :border-bottom (list '(px 1) 'solid (etaf-telega-color :border))
        (text :color (etaf-telega-color :ink) :font-weight 'bold
              (expr (or (plist-get (etaf-value (plist-get model :chat)) :title)
                        "选择对话")))
        (box :flex-grow 1 :flex-basis '(ch 0) :min-width '(ch 0) "")
        (text :color (etaf-telega-color :muted)
              (expr (etaf-value (plist-get model :status)))))
      (flex :flex-direction 'column :flex-grow 1 :flex-basis '(lh 0)
            :min-height '(lh 1) :overflow 'hidden :wrap-mode 'none
            :padding '((lh 0) (ch 2))
        (expr (or (mapcar (lambda (row)
                            (etaf-view (etaf-telega-message-line :row row :width width)))
                          rows)
                  (list (etaf-node
                         'text (list :color (etaf-telega-color :muted))
                         (list (if (etaf-value (plist-get model :chat)) "" "从左侧选择一个对话")))))))
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

(defun etaf-telega-view (model width rows)
  "Return the application View for MODEL, with WIDTH cells and visible ROWS.
ROWS is the viewport slice produced by `etaf-telega-message-rows'."
  (etaf-view (etaf-telega-shell :model model :rows rows :width width)))

(provide 'etaf-telega-views)
;;; etaf-telega-views.el ends here
