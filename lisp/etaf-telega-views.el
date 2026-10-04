;;; etaf-telega-views.el --- Views of etaf-telega -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Views are pure projections: they read refs and return structure.  The shell
;; provides the application palette to the catalog, so the composer field and
;; every later etaf-ui component render in the application colors.

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

(defun etaf-telega--message-lines (messages width)
  "Return display lines for MESSAGES wrapped at WIDTH."
  (let (lines)
    (dolist (message messages)
      (let ((prefix (format "%s %s: "
                            (if (plist-get message :outgoing) "我"
                              (or (plist-get message :sender) "?"))
                            (or (plist-get message :time) "--:--"))))
        (let ((text (or (plist-get message :text) "")))
          (dolist (line (etaf-telega--wrap text (max 8 (- width (string-width prefix)))))
            (push (concat prefix line) lines)
            (setq prefix (make-string (string-width prefix) ?\s))))))
    (nreverse lines)))

(etaf-define-component etaf-telega-chat-row (&key chat selected)
  "Render one CHAT row; pressing it selects the chat."
  :view
  (flex :flex-direction 'row :align-items 'center
        :padding '((lh 0) (ch 1))
        :background-color (if selected (etaf-telega-color :accent-soft)
                            (etaf-telega-color :surface))
        :role 'button :tab-index 0
        :on-press (if selected nil (lambda () (etaf-telega-open-chat chat)))
    (box :width '(ch 20) :height '(lh 1) :overflow 'hidden :wrap-mode 'none
      (text :color (etaf-telega-color :ink)
            :font-weight (if selected 'bold 'normal)
            (expr (or (plist-get chat :title) "?"))))
    (box :flex-grow 1 :flex-basis '(ch 0) :min-width '(ch 0) "")
    (expr (when (> (or (plist-get chat :unread) 0) 0)
            (etaf-node 'etaf-badge
                       (list :text (number-to-string (plist-get chat :unread))
                             :variant 'info)
                       nil)))))

(etaf-define-component etaf-telega-shell (&key model lines)
  "Render the client shell for MODEL; LINES is the wrapped message view."
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
      (box :flex-grow 1 :flex-basis '(lh 0) :min-height '(lh 1)
           :overflow 'hidden :wrap-mode 'none :padding '((lh 0) (ch 2))
        (text :color (etaf-telega-color :ink)
              (expr (mapconcat #'identity lines "\n"))))
      (box :width 'stretch :height '(lh 3) :overflow 'hidden
           :padding '((lh 0) (ch 2))
        (etaf-text-input
         :ref 'telega-composer :bind (plist-get model :draft)
         :label "消息" :hide-label t :rows 2 :width 'stretch
         :placeholder "发消息…" :submit-on-return t
         :on-submit #'etaf-telega-send
         :on-cancel #'etaf-telega-keep-draft)))))

(defun etaf-telega-view (model width)
  "Return the application View for MODEL, with WIDTH message cells."
  (let* ((chat (etaf-value (plist-get model :chat)))
         (messages (etaf-value (plist-get model :messages)))
         (lines (if chat
                    (or (etaf-telega--message-lines messages width) (list ""))
                  (list "" "从左侧选择一个对话"))))
    (etaf-view (etaf-telega-shell :model model :lines lines))))

(provide 'etaf-telega-views)
;;; etaf-telega-views.el ends here
