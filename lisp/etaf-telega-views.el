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

(declare-function etaf-telega-model-selected-message "etaf-telega-model" (model))

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

(defun etaf-telega--media-label (kind)
  "Return KIND's short placeholder label."
  (pcase kind
    ('photo "[图片]")
    ('video "[视频]")
    ('animation "[动图]")
    ('voice "[语音]")
    ('audio "[音频]")
    ('sticker "[贴纸]")
    ('file "[文件]")
    (_ "")))

(defun etaf-telega--format-size (size)
  "Return SIZE bytes as a short readable string."
  (cond ((or (null size) (zerop size)) "")
        ((< size 1024) (format "%dB" size))
        ((< size (* 1024 1024)) (format "%.0fK" (/ size 1024.0)))
        (t (format "%.1fM" (/ size (* 1024.0 1024.0))))))

(defun etaf-telega--message-extras (message width)
  "Return MESSAGE's extra display lines (media, file, link) wrapped at WIDTH."
  (let ((kind (plist-get message :kind))
        (file (plist-get message :file))
        (link (plist-get message :link))
        (width (max 8 width))
        lines)
    (when (and kind (not (eq kind 'text)))
      (push (if (and (eq kind 'file) file)
                (format "%s %s %s"
                        (etaf-telega--media-label kind)
                        (or (plist-get file :name) "")
                        (etaf-telega--format-size (plist-get file :size)))
              (etaf-telega--media-label kind))
            lines))
    (when link
      (push (format "链接 %s：%s"
                    (or (plist-get link :site) "")
                    (or (plist-get link :title) ""))
            lines))
    (cl-mapcan (lambda (line) (etaf-telega--wrap line width))
               (nreverse lines))))

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
               (extras (length (etaf-telega--message-extras
                                message (max 8 (- width 6)))))
               (content (+ 1 (if (plist-get message :reply) 1 0)
                           extras (max 1 lines))))
          (push (list :message message :separator separator
                      :selected (equal (plist-get message :id) selected)
                      :height (+ (if separator 1 0) (max 3 content)))
                blocks))))
    (nreverse blocks)))

(defun etaf-telega--compose-label (compose)
  "Return COMPOSE's prompt line, showing the input mode."
  (let ((target (plist-get compose :target)))
    (pcase (plist-get compose :kind)
      ('reply (format "» 回复 %s：%s　C-c C-k 取消"
                      (or (plist-get target :sender) "?")
                      (etaf-telega--one-line (plist-get target :text) 40)))
      ('edit (format "» 编辑消息：%s　C-c C-k 取消"
                     (etaf-telega--one-line (plist-get target :text) 40)))
      (_ "» 发送消息：RET 发送，Shift+RET 换行"))))

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
      (box :class "etaf-avatar" :role 'img :width '(ch 4) :height '(lh 3)
           :text-align 'center :padding-top '(lh 1) :font-weight 'bold
           :color (etaf-telega-color :bg)
           :background-color (etaf-telega-sender-color
                              (plist-get (plist-get block :message) :sender))
        (text (expr (etaf-telega--initial
                     (plist-get (plist-get block :message) :sender)))))
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
        (expr (mapcar (lambda (line)
                        (etaf-node 'text (list :color (etaf-telega-color :muted))
                                   (list line)))
                      (etaf-telega--message-extras (plist-get block :message)
                                                   (max 8 (- width 6)))))
        (text :color (etaf-telega-color :ink)
              (expr (etaf-telega-message-body (plist-get block :message)
                                              (max 8 (- width 6)))))))))

(etaf-define-component etaf-telega-describe-screen (&key describe on-action on-back)
  "Render DESCRIBE's chat profile with profile actions."
  :view
  (flex :flex-direction 'column :flex-grow 1 :flex-basis '(lh 0)
        :min-height '(lh 1) :width 'stretch :overflow 'hidden
        :padding '((lh 1) (ch 3)) :row-gap '(lh 1)
    (flex :flex-direction 'row :align-items 'center :column-gap '(ch 2)
      (etaf-avatar :name (or (plist-get describe :title) "?")
                   :size '(ch 6))
      (flex :flex-direction 'column :flex-grow 1 :flex-basis '(ch 0) :min-width '(ch 0)
        (text :color (etaf-telega-color :ink) :font-weight 'bold
              (expr (or (plist-get describe :title) "?")))
        (text :color (etaf-telega-color :muted)
              (expr (if (string-empty-p (or (plist-get describe :username) ""))
                        (pcase (plist-get describe :kind)
                          ('group "群组") ('channel "频道") (_ "私聊"))
                      (format "@%s" (plist-get describe :username)))))))
    (etaf-divider)
    (text :color (etaf-telega-color :muted)
          (expr (if (> (or (plist-get describe :members) 0) 0)
                    (format "成员：%d" (plist-get describe :members))
                  "成员：—")))
    (text :color (etaf-telega-color :ink)
          (expr (if (string-empty-p (or (plist-get describe :description) ""))
                    "暂无简介"
                  (plist-get describe :description))))
    (flex :flex-direction 'row :column-gap '(ch 1)
      (etaf-button :label (if (plist-get describe :pinned) "取消置顶" "置顶")
                   :variant 'secondary
                   :on-press (lambda () (funcall on-action 'pin)))
      (etaf-button :label (if (plist-get describe :muted) "取消静音" "静音")
                   :variant 'secondary
                   :on-press (lambda () (funcall on-action 'mute)))
      (etaf-button :label (if (> (or (plist-get describe :unread) 0) 0)
                              "标为已读" "标为未读")
                   :variant 'ghost
                   :on-press (lambda () (funcall on-action 'read))))
    (text :color (etaf-telega-color :muted) "q 返回")))

(etaf-define-component etaf-telega-actions-screen (&key message on-action on-back)
  "Render the selected MESSAGE's action menu."
  :view
  (flex :flex-direction 'column :flex-grow 1 :flex-basis '(lh 0)
        :min-height '(lh 1) :width 'stretch :overflow 'hidden
        :padding '((lh 1) (ch 3)) :row-gap '(lh 1)
    (text :color (etaf-telega-color :ink) :font-weight 'bold "消息操作")
    (text :color (etaf-telega-color :muted)
          (expr (etaf-telega--one-line (plist-get message :text) 60)))
    (flex :flex-direction 'row :column-gap '(ch 1) :flex-wrap 'wrap
      (etaf-button :label "回复" :variant 'secondary
                   :on-press (lambda () (funcall on-action 'reply)))
      (etaf-button :label "编辑" :variant 'secondary
                   :on-press (lambda () (funcall on-action 'edit)))
      (etaf-button :label "复制" :variant 'ghost
                   :on-press (lambda () (funcall on-action 'copy)))
      (etaf-button :label "删除" :variant 'ghost
                   :on-press (lambda () (funcall on-action 'delete))))
    (text :color (etaf-telega-color :muted) "q 返回")))

(etaf-define-component etaf-telega-shell
    (&key model blocks chats filters filter screen width
          on-filter on-back on-action)
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
      (flex :if (not (eq screen 'root)) :width '(ch 2) :flex-grow 0
            :flex-basis '(ch 2) :min-width '(ch 2)
        (etaf-button :label "‹" :variant 'ghost :on-press on-back))
      (text :color (etaf-telega-color :ink) :font-weight 'bold
            (expr (pcase screen
                    ('chat (or (plist-get (etaf-value (plist-get model :chat)) :title)
                               "对话"))
                    ('describe "资料")
                    ('actions "消息操作")
                    (_ "Telegram"))))
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
    (flex :if (eq screen 'describe) :flex-direction 'column :flex-grow 1
          :flex-basis '(lh 0) :min-height '(lh 1) :width 'stretch
      (etaf-telega-describe-screen
       :describe (or (etaf-value (plist-get model :describe)) (list))
       :on-action on-action :on-back on-back))
    (flex :if (eq screen 'actions) :flex-direction 'column :flex-grow 1
          :flex-basis '(lh 0) :min-height '(lh 1) :width 'stretch
      (etaf-telega-actions-screen
       :message (or (etaf-telega-model-selected-message model) (list))
       :on-action on-action :on-back on-back))
    (flex :if (eq screen 'chat) :flex-direction 'column :flex-grow 1
          :flex-basis '(lh 0) :min-height '(lh 1) :width 'stretch
      (flex :flex-direction 'column :flex-grow 1 :flex-basis '(lh 0)
            :min-height '(lh 1) :overflow 'hidden :wrap-mode 'none
            :justify-content 'flex-end
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
        (text :color (if (etaf-value (plist-get model :compose))
                         (etaf-telega-color :accent)
                       (etaf-telega-color :muted))
              (expr (etaf-telega--compose-label
                     (etaf-value (plist-get model :compose)))))
        (etaf-text-input
         :ref 'telega-composer :bind (plist-get model :draft)
         :label "消息" :hide-label t :rows 2
         :width (list 'ch (max 20 (- width 4)))
         :placeholder "发消息…" :submit-on-return t
         :on-submit #'etaf-telega-send
         :on-cancel #'etaf-telega-keep-draft)))))

(defun etaf-telega-view (model width blocks chats filters filter screen
                            on-filter on-back on-action)
  "Return the application View for MODEL.
WIDTH is the message cell count, BLOCKS the visible message slice, CHATS
the visible conversation slice, FILTERS/FILTER the filter tabs, SCREEN the
active screen and ON-ACTION the message/profile action dispatcher."
  (etaf-view (etaf-telega-shell :model model :blocks blocks :chats chats
                                :filters filters :filter filter :screen screen
                                :on-filter on-filter :on-back on-back
                                :on-action on-action
                                :width width)))

(provide 'etaf-telega-views)
;;; etaf-telega-views.el ends here
