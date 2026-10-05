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
  "Return TITLE's avatar initial, skipping leading punctuation."
  (let* ((title (or title ""))
         (char (cl-find-if (lambda (character)
                             (string-match-p "\\w" (char-to-string character)))
                           (string-to-list title))))
    (if char (upcase (char-to-string char)) "?")))

(defun etaf-telega--count-label (count)
  "Return COUNT as a compact badge label for narrow columns."
  (cond ((>= count 10000) (format "%.0fK" (/ count 1000.0)))
        ((>= count 1000) (format "%.1fK" (/ count 1000.0)))
        (t (number-to-string count))))

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
                      :height (+ (if separator 1 0) (max 2 content)))
                blocks))))
    (nreverse blocks)))

(defun etaf-telega--compose-label (compose)
  "Return COMPOSE's prompt line, or nil while plainly composing."
  (let ((target (plist-get compose :target)))
    (pcase (plist-get compose :kind)
      ('reply (format "» 回复 %s：%s　C-c C-k 取消"
                      (or (plist-get target :sender) "?")
                      (etaf-telega--one-line (plist-get target :text) 40)))
      ('edit (format "» 编辑消息：%s　C-c C-k 取消"
                     (etaf-telega--one-line (plist-get target :text) 40)))
      (_ nil))))

(etaf-define-component etaf-telega-chat-row (&key chat selected width)
  "Render one CHAT row; pressing it opens the conversation.
The row is exactly two lines: title over preview, with a coloured initial
chip, the timestamp and the unread badge in fixed right-aligned columns."
  :render
  (let* ((title (or (plist-get chat :title) "?"))
         (unread (or (plist-get chat :unread) 0))
         (muted (plist-get chat :muted))
         (limit (max 8 (- width 16))))
    (etaf-view
     (flex :flex-direction 'row :align-items 'center :width 'stretch
           :height '(lh 2) :padding '((lh 0) (ch 2)) :column-gap '(ch 1)
           :background-color (if selected (etaf-telega-color :accent-soft)
                               (etaf-telega-color :surface))
           :border-bottom (list '(px 1) 'solid (etaf-telega-color :hairline))
           :role 'button :tab-index 0
           :on-press (lambda () (etaf-telega-open-chat chat))
       (box :class "etaf-avatar" :role 'img :width '(ch 2) :height '(lh 1)
            :text-align 'center :font-weight 'bold
            :color (etaf-telega-color :bg)
            :background-color (etaf-telega-sender-color title)
         (text (expr (etaf-telega--initial title))))
       (flex :flex-direction 'column :flex-grow 1 :flex-basis '(ch 0) :min-width '(ch 0)
         (box :height '(lh 1) :overflow 'hidden :wrap-mode 'none
           (text :color (etaf-telega-color :ink)
                 :font-weight (if (and (> unread 0) (not muted)) 'bold 'normal)
                 (expr title)))
         (box :height '(lh 1) :overflow 'hidden :wrap-mode 'none
           (text :color (etaf-telega-color :muted)
                 (expr (etaf-telega--one-line (plist-get chat :preview) limit)))))
       (flex :flex-direction 'column :width '(ch 6) :row-gap '(lh 0)
         (box :height '(lh 1) :width 'stretch :text-align 'right
              :overflow 'hidden :wrap-mode 'none
           (text :color (etaf-telega-color :muted)
                 (expr (or (plist-get chat :time) ""))))
         (flex :flex-direction 'row :width 'stretch :justify-content 'flex-end
           (expr (when (> unread 0)
                   (if muted
                       (etaf-node 'text (list :color (etaf-telega-color :muted)
                                               :font-weight 'bold)
                                  (list (etaf-telega--count-label unread)))
                     (etaf-node 'etaf-badge
                                (list :text (etaf-telega--count-label unread)
                                      :variant 'info)
                                nil))))))))))

(etaf-define-component etaf-telega-day-separator (&key label)
  "Render the day LABEL as a centred chip between message groups."
  :view
  (flex :flex-direction 'row :justify-content 'center :width 'stretch
        :height '(lh 1)
    (box :padding-inline '(ch 1)
         :background-color (etaf-telega-color :surface-2)
         :color (etaf-telega-color :muted) :font-weight 'bold
      (text (expr label)))))

(etaf-define-component etaf-telega-message-block (&key block width)
  "Render one message BLOCK with its day separator and avatar."
  :view
  (flex :flex-direction 'column :width 'stretch
    (flex :if (plist-get block :separator)
      (etaf-telega-day-separator :label (plist-get block :separator)))
    (flex :flex-direction 'row :align-items 'flex-start :width 'stretch
          :padding '((lh 0) (ch 1)) :column-gap '(ch 1)
          :border-bottom (list '(px 1) 'solid (etaf-telega-color :hairline))
          :background-color (when (plist-get block :selected)
                              (etaf-telega-color :accent-soft))
      (box :class "etaf-avatar" :role 'img :width '(ch 2) :height '(lh 1)
           :text-align 'center :font-weight 'bold
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
  "Render DESCRIBE's chat profile with its facts and profile actions."
  :render
  (let* ((title (or (plist-get describe :title) "?"))
         (kind (pcase (plist-get describe :kind)
                 ('group "群组") ('channel "频道") (_ "私聊")))
         (username (or (plist-get describe :username) ""))
         (members (or (plist-get describe :members) 0))
         (description (or (plist-get describe :description) ""))
         (pinned (plist-get describe :pinned))
         (muted (plist-get describe :muted))
         (unread (or (plist-get describe :unread) 0)))
    (etaf-view
     (flex :flex-direction 'column :flex-grow 1 :flex-basis '(lh 0)
           :min-height '(lh 1) :width 'stretch :overflow 'hidden
           :padding '((lh 1) (ch 3)) :row-gap '(lh 1)
       (flex :flex-direction 'row :align-items 'center :column-gap '(ch 2)
         (box :class "etaf-avatar" :role 'img :width '(ch 5) :height '(lh 3)
              :text-align 'center :font-weight 'bold
              :color (etaf-telega-color :bg)
              :background-color (etaf-telega-sender-color title)
           (text (expr (format "\n%s" (etaf-telega--initial title)))))
         (flex :flex-direction 'column :flex-grow 1 :flex-basis '(ch 0)
               :min-width '(ch 0) :row-gap '(lh 0)
           (text :color (etaf-telega-color :ink) :font-weight 'bold
                 (expr title))
           (text :color (etaf-telega-color :muted)
                 (expr (if (string-empty-p username)
                           kind
                         (format "@%s" username))))))
       (etaf-divider)
       (flex :flex-direction 'column :row-gap '(lh 1)
         (flex :flex-direction 'row :column-gap '(ch 2)
           (box :width '(ch 6)
             (text :color (etaf-telega-color :muted) "类型"))
           (text :color (etaf-telega-color :ink) (expr kind)))
         (flex :flex-direction 'row :column-gap '(ch 2)
           (box :width '(ch 6)
             (text :color (etaf-telega-color :muted) "成员"))
           (text :color (etaf-telega-color :ink)
                 (expr (if (> members 0) (number-to-string members) "—")))))
       (flex :flex-direction 'column :row-gap '(lh 1)
         (text :color (etaf-telega-color :muted) "简介")
         (text :color (etaf-telega-color :ink)
               (expr (if (string-empty-p description) "暂无简介" description))))
       (flex :flex-direction 'row :column-gap '(ch 1)
         (etaf-button :label (if pinned "取消置顶" "置顶")
                      :variant 'secondary
                      :on-press (lambda () (funcall on-action 'pin)))
         (etaf-button :label (if muted "取消静音" "静音")
                      :variant 'secondary
                      :on-press (lambda () (funcall on-action 'mute)))
         (etaf-button :label (if (> unread 0) "标为已读" "标为未读")
                      :variant 'ghost
                      :on-press (lambda () (funcall on-action 'read))))
       (text :color (etaf-telega-color :muted) "q 返回")))))

(etaf-define-component etaf-telega-actions-screen (&key message on-action on-back)
  "Render the selected MESSAGE's action menu."
  :render
  (let ((preview (etaf-telega--one-line (plist-get message :text) 60)))
    (etaf-view
     (flex :flex-direction 'column :flex-grow 1 :flex-basis '(lh 0)
           :min-height '(lh 1) :width 'stretch :overflow 'hidden
           :padding '((lh 2) (ch 3)) :row-gap '(lh 1)
       (text :color (etaf-telega-color :ink) :font-weight 'bold "消息操作")
       (box :background-color (etaf-telega-color :surface-2)
            :padding '((lh 0) (ch 1))
         (text :color (etaf-telega-color :muted) (expr preview)))
       (etaf-divider)
       (flex :flex-direction 'column :row-gap '(lh 1)
         (etaf-button :label "回复" :variant 'ghost
                      :on-press (lambda () (funcall on-action 'reply)))
         (etaf-button :label "编辑" :variant 'ghost
                      :on-press (lambda () (funcall on-action 'edit)))
         (etaf-button :label "复制" :variant 'ghost
                      :on-press (lambda () (funcall on-action 'copy)))
         (etaf-button :label "删除" :variant 'ghost
                      :color (etaf-telega-color :danger)
                      :on-press (lambda () (funcall on-action 'delete))))
       (text :color (etaf-telega-color :muted) "q 返回")))))

(defun etaf-telega--chat-meta (chat)
  "Return CHAT's one-line meta text for the pane header."
  (let* ((kind (pcase (plist-get chat :kind)
                 ('group "群组") ('channel "频道") ('private "私聊") (_ nil)))
         (username (or (plist-get chat :username) ""))
         (parts (delq nil (list kind
                                (unless (string-empty-p username)
                                  (format "@%s" username))))))
    (if parts (mapconcat #'identity parts " · ") "")))

(etaf-define-component etaf-telega-filter-bar (&key filters filter on-filter)
  "Render the conversation filter tabs."
  :view
  (flex :flex-direction 'row :align-items 'center
        :height '(lh 2) :padding '((lh 0) (ch 2))
        :background-color (etaf-telega-color :surface)
        :border-bottom (list '(px 1) 'solid (etaf-telega-color :hairline))
    (etaf-tabs :items filters :active filter :on-change on-filter)))

(etaf-define-component etaf-telega-chat-list (&key model chats rows-width)
  "Render the conversation rows pane."
  :view
  (flex :flex-direction 'column :flex-grow 1 :flex-basis '(lh 0)
        :min-height '(lh 1) :width 'stretch :overflow 'hidden
    (expr (mapcar (lambda (chat)
                    (etaf-view
                     (etaf-telega-chat-row
                      :key (plist-get chat :id) :chat chat :width rows-width
                      :selected (equal (plist-get chat :id)
                                       (etaf-value (plist-get model :cursor))))))
                  chats))))

(etaf-define-component etaf-telega-chat-pane (&key model blocks width header)
  "Render the conversation pane: an optional title strip, the message
viewport and the composer.  In the wide layout HEADER names the open
conversation; the narrow layout already owns the shell title bar."
  :view
  (flex :flex-direction 'column :flex-grow 1 :flex-basis '(lh 0)
        :min-height '(lh 1) :width 'stretch :overflow 'hidden
    (flex :if header :flex-direction 'column :justify-content 'center
          :height '(lh 2) :padding '((lh 0) (ch 2))
          :background-color (etaf-telega-color :surface-2)
          :border-bottom (list '(px 1) 'solid (etaf-telega-color :hairline))
      (text :color (etaf-telega-color :ink) :font-weight 'bold
            (expr (or (plist-get (etaf-value (plist-get model :chat)) :title)
                      "对话")))
      (text :color (etaf-telega-color :muted)
            (expr (etaf-telega--chat-meta
                   (etaf-value (plist-get model :chat))))))
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
                                 ""
                               "选择一个对话")))))))
    (flex :flex-direction 'column :width 'stretch
          :background-color (etaf-telega-color :surface)
          :border-top (list '(px 1) 'solid (etaf-telega-color :hairline))
          :padding '((lh 1) (ch 2)) :row-gap '(lh 1)
      (expr (when-let* ((compose (etaf-value (plist-get model :compose)))
                        (label (etaf-telega--compose-label compose)))
              (etaf-node 'text (list :color (etaf-telega-color :accent))
                         (list label))))
      (etaf-text-input
       :ref 'telega-composer :bind (plist-get model :draft)
       :label "消息" :hide-label t :rows 1
       :width (list 'ch (max 20 width))
       :placeholder "发消息…" :submit-on-return t
       :on-submit #'etaf-telega-send
       :on-cancel #'etaf-telega-keep-draft))))

(etaf-define-component etaf-telega-welcome-pane (&key model)
  "Render the wide layout's idle pane beside the conversation list."
  :render
  (let* ((all (etaf-value (plist-get model :chats)))
         (total (length all))
         (unread (length (cl-remove-if-not
                          (lambda (chat) (> (or (plist-get chat :unread) 0) 0))
                          all))))
    (etaf-view
     (flex :flex-direction 'column :flex-grow 1 :flex-basis '(lh 0)
           :min-height '(lh 1) :width 'stretch :overflow 'hidden
           :justify-content 'center :row-gap '(lh 1)
       (box :width 'stretch :text-align 'center
         (text :color (etaf-telega-color :ink) :font-weight 'bold "Telegram"))
       (box :width 'stretch :text-align 'center
         (text :color (etaf-telega-color :muted)
               (expr (format "共 %d 个对话 · %d 个未读" total unread))))
       (box :width 'stretch :text-align 'center
         (text :color (etaf-telega-color :muted)
               "j/k 选择会话，RET 打开"))))))

(etaf-define-component etaf-telega-shell
    (&key model blocks chats filters filter screen width list-width split
          on-filter on-back on-action)
  "Render the client shell for MODEL.
BLOCKS is the visible message slice, CHATS the visible conversation slice,
SCREEN names the active screen and FILTERS/FILTER drive the filter tabs.
SPLIT selects the wide master-detail layout: the conversation column stays
visible at LIST-WIDTH cells while the active screen fills the right pane."
  :setup (etaf-theme-provide (etaf-telega-theme-defaults))
  :view
  (flex :flex-direction 'column :width 'stretch :height 'stretch
        :background-color (etaf-telega-color :bg)
    (flex :flex-direction 'row :align-items 'center
          :height '(lh 2) :padding '((lh 0) (ch 2)) :column-gap '(ch 2)
          :background-color (etaf-telega-color :surface-2)
          :border-bottom (list '(px 1) 'solid (etaf-telega-color :border))
      (flex :if (and (not split) (not (eq screen 'root)))
            :width '(ch 2) :flex-grow 0 :flex-basis '(ch 2) :min-width '(ch 2)
        (etaf-button :label "‹" :variant 'ghost :on-press on-back))
      (text :color (etaf-telega-color :ink) :font-weight 'bold
            (expr (if split
                      "Telegram"
                    (pcase screen
                      ('chat (or (plist-get (etaf-value (plist-get model :chat))
                                            :title)
                                 "对话"))
                      ('describe "资料")
                      ('actions "消息操作")
                      (_ "Telegram")))))
      (box :flex-grow 1 :flex-basis '(ch 0) :min-width '(ch 0) "")
      (text :color (etaf-telega-color :muted)
            (expr (etaf-value (plist-get model :status)))))
    ;; Wide layout: conversation column beside the active screen.
    (flex :if split :flex-direction 'row :flex-grow 1 :flex-basis '(lh 0)
          :min-height '(lh 1) :width 'stretch :overflow 'hidden
      (flex :flex-direction 'column :width (list 'ch list-width)
            :flex-grow 0 :flex-basis (list 'ch list-width)
            :min-width (list 'ch list-width)
            :height 'stretch :overflow 'hidden
            :background-color (etaf-telega-color :surface)
            :border-right (list '(px 1) 'solid (etaf-telega-color :hairline))
        (etaf-telega-filter-bar :filters filters :filter filter
                                :on-filter on-filter)
        (etaf-telega-chat-list :model model :chats chats
                               :rows-width (- list-width 2)))
      (flex :flex-direction 'column :flex-grow 1 :flex-basis '(ch 0)
            :min-width '(ch 0) :overflow 'hidden
        (flex :if (eq screen 'chat) :flex-direction 'column :flex-grow 1
              :flex-basis '(lh 0) :min-height '(lh 1) :width 'stretch
          (etaf-telega-chat-pane :model model :blocks blocks :width width
                                 :header t))
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
        (flex :if (eq screen 'root) :flex-direction 'column :flex-grow 1
              :flex-basis '(lh 0) :min-height '(lh 1) :width 'stretch
          (etaf-telega-welcome-pane :model model))))
    ;; Narrow layout: one screen owns the window.
    (flex :if (and (not split) (eq screen 'root)) :flex-direction 'column
          :flex-grow 1 :flex-basis '(lh 0) :min-height '(lh 1)
          :width 'stretch :overflow 'hidden
      (etaf-telega-filter-bar :filters filters :filter filter
                              :on-filter on-filter)
      (etaf-telega-chat-list :model model :chats chats
                             :rows-width list-width))
    (flex :if (and (not split) (eq screen 'describe)) :flex-direction 'column
          :flex-grow 1 :flex-basis '(lh 0) :min-height '(lh 1)
          :width 'stretch
      (etaf-telega-describe-screen
       :describe (or (etaf-value (plist-get model :describe)) (list))
       :on-action on-action :on-back on-back))
    (flex :if (and (not split) (eq screen 'actions)) :flex-direction 'column
          :flex-grow 1 :flex-basis '(lh 0) :min-height '(lh 1)
          :width 'stretch
      (etaf-telega-actions-screen
       :message (or (etaf-telega-model-selected-message model) (list))
       :on-action on-action :on-back on-back))
    (flex :if (and (not split) (eq screen 'chat)) :flex-direction 'column
          :flex-grow 1 :flex-basis '(lh 0) :min-height '(lh 1)
          :width 'stretch
      (etaf-telega-chat-pane :model model :blocks blocks :width width
                             :header nil))))

(defun etaf-telega-view (model width blocks chats filters filter screen
                            on-filter on-back on-action list-width split)
  "Return the application View for MODEL.
WIDTH is the message cell count, BLOCKS the visible message slice, CHATS
the visible conversation slice, FILTERS/FILTER the filter tabs, SCREEN the
active screen, ON-ACTION the message/profile action dispatcher,
LIST-WIDTH the conversation-list cell count and SPLIT the wide layout flag."
  (etaf-view (etaf-telega-shell :model model :blocks blocks :chats chats
                                :filters filters :filter filter :screen screen
                                :on-filter on-filter :on-back on-back
                                :on-action on-action
                                :width width :list-width list-width
                                :split split)))

(provide 'etaf-telega-views)
;;; etaf-telega-views.el ends here
