;;; etaf-telega-tests.el --- Acceptance tests for etaf-telega -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Public behaviour tests.  Every scenario drives the application through the
;; interfaces declared in `etaf-telega.el' and asserts what a person would see.
;; The fake backend replays fixed chats and messages; no network is touched.
;; Keep internals out: no private helpers, structures or call counts.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'etaf-telega)

(defun etaf-telega-test--backend (&optional responder)
  "Return a fake backend with two chats and one loaded history."
  (let ((histories (make-hash-table :test #'equal)))
    ;; Newest first, matching the backend contract and TDLib history order.
    (puthash 1 (list (list :id 12 :outgoing t :sender "我" :time "10:01" :text "在的")
                     (list :id 11 :sender "Alice" :time "10:00" :text "你好"))
             histories)
    (etaf-telega-backend-fake-create
     :chats (list (list :id 1 :title "Alice" :unread 2)
                  (list :id 2 :title "工作群" :unread 0))
     :histories histories :responder responder)))

(defun etaf-telega-test--open (&optional backend)
  "Open a client with BACKEND, or a fresh fake one."
  (let ((etaf-telega-backend-create-function
         (lambda () (or backend (etaf-telega-test--backend)))))
    (etaf-telega-open)))

(defmacro etaf-telega-test--with-client (var backend &rest body)
  "Open a client in VAR with BACKEND and close it afterwards even on failure."
  (declare (indent 2))
  `(let ((,var (etaf-telega-test--open ,backend)))
     (unwind-protect
         (progn ,@body)
       (when (buffer-live-p ,var)
         (with-current-buffer ,var (etaf-telega-close))))))

(defun etaf-telega-test--text (buffer)
  "Return BUFFER's visible text without properties."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest etaf-telega-tests-open-mounts-and-close-releases ()
  "Opening mounts live chats and closing kills the buffer and backend."
  (let ((backend (etaf-telega-test--backend)))
    (etaf-telega-test--with-client buffer nil
      (should (buffer-live-p buffer))
      (should (etaf-runtime-for-buffer buffer))
      (let ((text (etaf-telega-test--text buffer)))
        (should (string-match-p "Telegram" text))
        (should (string-match-p "就绪" text))
        (should (string-match-p "Alice" text))
        (should (string-match-p "工作群" text)))
      (should (equal (plist-get (etaf-telega-info buffer) :chats) 2)))
    (should (eq (etaf-telega-backend-fake-sent backend) nil))))

(ert-deftest etaf-telega-tests-select-chat-loads-history ()
  "Selecting a chat loads and shows its recent messages."
  (etaf-telega-test--with-client buffer nil
    (etaf-telega-select-chat 1)
    (let ((text (etaf-telega-test--text buffer)))
      (should (string-match-p "你好" text))
      (should (string-match-p "在的" text)))
    (should (equal (plist-get (etaf-telega-info buffer) :chat) 1))
    (should (equal (plist-get (etaf-telega-info buffer) :messages) 2))
    (with-current-buffer buffer
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (< (string-match-p "你好" text)
                   (string-match-p "在的" text)))))))

(ert-deftest etaf-telega-tests-composer-sends-and-clears-on-ack ()
  "Typing in the composer sends the text and clears it after acceptance."
  (let ((backend (etaf-telega-test--backend
                  (lambda (fake text)
                    (etaf-telega-backend-fake-complete fake text)))))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-select-chat 1)
      (with-current-buffer buffer
        (etaf-telega-compose)
        (should (plist-get (etaf-telega-info buffer) :editing))
        (insert "今晚见")
        (call-interactively (key-binding (kbd "RET"))))
      (should (equal (car (etaf-telega-backend-fake-sent backend)) "今晚见"))
      (should (equal (plist-get (etaf-telega-info buffer) :draft) ""))
      (should (string-match-p "今晚见" (etaf-telega-test--text buffer))))))

(ert-deftest etaf-telega-tests-draft-survives-until-ack ()
  "A pending send keeps the draft text and clears only after the ack."
  (let ((backend (etaf-telega-test--backend)))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-select-chat 1)
      (with-current-buffer buffer
        (etaf-telega-compose)
        (insert "未确认")
        (call-interactively (key-binding (kbd "RET"))))
      (should (equal (plist-get (etaf-telega-info buffer) :draft) "未确认"))
      (etaf-telega-backend-fake-complete backend "未确认")
      (should (equal (plist-get (etaf-telega-info buffer) :draft) "")))))

(ert-deftest etaf-telega-tests-chat-keys-move-the-selection ()
  "The chat keys move the list cursor without opening a conversation."
  (etaf-telega-test--with-client buffer nil
    (etaf-telega-select-chat 1)
    (with-current-buffer buffer (etaf-telega-back))
    (with-current-buffer buffer (call-interactively #'etaf-telega-next-chat))
    (should (equal (plist-get (etaf-telega-info buffer) :cursor) 2))
    (should (eq (plist-get (etaf-telega-info buffer) :screen) 'root))
    (with-current-buffer buffer (call-interactively #'etaf-telega-previous-chat))
    (should (equal (plist-get (etaf-telega-info buffer) :cursor) 1))))

(ert-deftest etaf-telega-tests-live-message-appends-to-the-open-chat ()
  "A message arriving while a chat is open is appended to its history."
  (let ((backend (etaf-telega-test--backend)))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-select-chat 1)
      (should (equal (plist-get (etaf-telega-info buffer) :messages) 2))
      (etaf-telega-backend-fake-deliver
       backend (list :id 13 :sender "Alice" :time "10:02" :text "晚上见"))
      (should (equal (plist-get (etaf-telega-info buffer) :messages) 3))
      (let ((text (etaf-telega-test--text buffer)))
        (should (string-match-p "晚上见" text))
        (should (< (string-match-p "在的" text)
                   (string-match-p "晚上见" text)))))))

(ert-deftest etaf-telega-tests-live-message-deduplicates-by-id ()
  "Delivering the same message twice still shows one copy."
  (let ((backend (etaf-telega-test--backend)))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-select-chat 1)
      (let ((message (list :id 13 :sender "Alice" :time "10:02" :text "晚上见")))
        (etaf-telega-backend-fake-deliver backend message)
        (etaf-telega-backend-fake-deliver backend (copy-tree message)))
      (should (equal (plist-get (etaf-telega-info buffer) :messages) 3)))))

(ert-deftest etaf-telega-tests-live-copy-of-the-draft-keeps-it-until-ack ()
  "Another client's copy of the draft text does not clear the draft."
  (let ((backend (etaf-telega-test--backend)))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-select-chat 1)
      (with-current-buffer buffer
        (etaf-telega-compose)
        (insert "草稿")
        (call-interactively (key-binding (kbd "RET"))))
      (should (equal (plist-get (etaf-telega-info buffer) :draft) "草稿"))
      (etaf-telega-backend-fake-deliver
       backend (list :id 14 :outgoing t :sender "我" :time "10:03" :text "草稿"))
      (should (equal (plist-get (etaf-telega-info buffer) :draft) "草稿"))
      (etaf-telega-backend-fake-complete backend "草稿")
      (should (equal (plist-get (etaf-telega-info buffer) :draft) "")))))

(ert-deftest etaf-telega-tests-first-and-last-chat-commands ()
  "The first/last chat commands jump to the ends of the list."
  (etaf-telega-test--with-client buffer nil
    (etaf-telega-select-chat 1)
    (with-current-buffer buffer (call-interactively #'etaf-telega-last-chat))
    (should (equal (plist-get (etaf-telega-info buffer) :chat) 2))
    (with-current-buffer buffer (call-interactively #'etaf-telega-first-chat))
    (should (equal (plist-get (etaf-telega-info buffer) :chat) 1))))

(ert-deftest etaf-telega-tests-message-keys-move-the-selection ()
  "The n/p keys move the message selection like Telega's chat buffer."
  (etaf-telega-test--with-client buffer nil
    (etaf-telega-select-chat 1)
    (should (equal (plist-get (etaf-telega-info buffer) :selected) 12))
    (with-current-buffer buffer (call-interactively #'etaf-telega-previous-message))
    (should (equal (plist-get (etaf-telega-info buffer) :selected) 11))
    (with-current-buffer buffer (call-interactively #'etaf-telega-next-message))
    (should (equal (plist-get (etaf-telega-info buffer) :selected) 12))))

(ert-deftest etaf-telega-tests-reply-quotes-the-selected-message ()
  "r arms a reply; the next send carries the quote and clears the mode."
  (let ((backend (etaf-telega-test--backend
                  (lambda (fake text) (etaf-telega-backend-fake-complete fake text)))))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-select-chat 1)
      (with-current-buffer buffer
        (call-interactively #'etaf-telega-previous-message)
        (call-interactively #'etaf-telega-reply)
        (should (eq (plist-get (etaf-telega-info buffer) :compose) 'reply))
        (insert "收到")
        (call-interactively (key-binding (kbd "RET"))))
      (should (equal (cdr (assoc "收到" (etaf-telega-backend-fake-replies backend))) 11))
      (should (null (plist-get (etaf-telega-info buffer) :compose)))
      (should (equal (plist-get (etaf-telega-info buffer) :draft) ""))
      (should (string-match-p "↩ Alice：你好" (etaf-telega-test--text buffer))))))

(ert-deftest etaf-telega-tests-edit-replaces-the-selected-message ()
  "e prefills the composer; sending replaces the message and marks it edited."
  (let ((backend (etaf-telega-test--backend)))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-select-chat 1)
      (with-current-buffer buffer
        (call-interactively #'etaf-telega-edit)
        (should (eq (plist-get (etaf-telega-info buffer) :compose) 'edit))
        (should (equal (plist-get (etaf-telega-info buffer) :draft) "在的"))
        (insert "！")
        (call-interactively (key-binding (kbd "RET"))))
      (should (null (plist-get (etaf-telega-info buffer) :compose)))
      (should (equal (plist-get (etaf-telega-backend-fake-message backend 12) :text)
                     "在的！"))
      (should (string-match-p "（已编辑）" (etaf-telega-test--text buffer))))))

(ert-deftest etaf-telega-tests-cancel-edit-restores-the-draft ()
  "C-c C-k on an edit restores the draft captured before editing."
  (etaf-telega-test--with-client buffer nil
    (etaf-telega-select-chat 1)
    (with-current-buffer buffer
      (etaf-telega-compose)
      (insert "草稿")
      (call-interactively (key-binding (kbd "C-c C-k"))))
    (should (equal (plist-get (etaf-telega-info buffer) :draft) "草稿"))
    (with-current-buffer buffer
      (call-interactively #'etaf-telega-edit)
      (should (equal (plist-get (etaf-telega-info buffer) :draft) "在的"))
      (insert "！")
      (call-interactively (key-binding (kbd "C-c C-k"))))
    (should (null (plist-get (etaf-telega-info buffer) :compose)))
    (should (equal (plist-get (etaf-telega-info buffer) :draft) "草稿"))))

(ert-deftest etaf-telega-tests-delete-removes-the-selected-message ()
  "d deletes the selected message and moves the selection."
  (let ((backend (etaf-telega-test--backend)))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-select-chat 1)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (with-current-buffer buffer (call-interactively #'etaf-telega-delete)))
      (should (equal (plist-get (etaf-telega-info buffer) :messages) 1))
      (should (equal (plist-get (etaf-telega-info buffer) :selected) 11))
      (should-not (etaf-telega-backend-fake-message backend 12)))))

(ert-deftest etaf-telega-tests-copy-yanks-the-selected-message ()
  "c copies the selected message's text to the kill ring."
  (etaf-telega-test--with-client buffer nil
    (etaf-telega-select-chat 1)
    (with-current-buffer buffer (call-interactively #'etaf-telega-copy))
    (should (equal (current-kill 0) "在的"))))

(ert-deftest etaf-telega-tests-sidebar-viewport-follows-the-chat-selection ()
  "A long chat list renders only the rows around the selection."
  (let* ((chats (cl-loop for n from 1 to 60
                         collect (list :id n :title (format "对话 %03d" n) :unread 0)))
         (backend (etaf-telega-backend-fake-create
                   :chats chats :histories (make-hash-table :test #'equal))))
    (etaf-telega-test--with-client buffer backend
      (with-current-buffer buffer (etaf-telega-back))
      (let ((text (etaf-telega-test--text buffer)))
        (should (string-match-p "对话 001" text))
        (should-not (string-match-p "对话 060" text)))
      (dotimes (_ 30)
        (with-current-buffer buffer (call-interactively #'etaf-telega-next-chat)))
      (let ((text (etaf-telega-test--text buffer)))
        (should (string-match-p "对话 031" text))
        (should-not (string-match-p "对话 001" text))
        (should-not (string-match-p "对话 060" text)))
      (with-current-buffer buffer (call-interactively #'etaf-telega-return))
      (should (eq (plist-get (etaf-telega-info buffer) :screen) 'chat)))))

(ert-deftest etaf-telega-tests-describe-screen-shows-the-profile ()
  "t shows the conversation profile and its actions stay on the screen."
  (let* ((chats (list (list :id 1 :title "Alice" :unread 2 :kind 'private
                            :username "alice" :members 0 :description "测试简介"
                            :pinned t :muted nil)))
         (backend (etaf-telega-backend-fake-create
                   :chats chats :histories (make-hash-table :test #'equal))))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-select-chat 1)
      (with-current-buffer buffer (call-interactively #'etaf-telega-describe))
      (should (eq (plist-get (etaf-telega-info buffer) :screen) 'describe))
      (let ((text (etaf-telega-test--text buffer)))
        (should (string-match-p "Alice" text))
        (should (string-match-p "@alice" text))
        (should (string-match-p "测试简介" text)))
      (with-current-buffer buffer (call-interactively #'etaf-telega-toggle-mute))
      (should (eq (plist-get (car (etaf-telega-backend-fake-chats backend)) :muted) t))
      (with-current-buffer buffer (call-interactively #'etaf-telega-back))
      (should (eq (plist-get (etaf-telega-info buffer) :screen) 'chat)))))

(ert-deftest etaf-telega-tests-message-actions-menu ()
  "o opens the action menu for the selected message."
  (etaf-telega-test--with-client buffer nil
    (etaf-telega-select-chat 1)
    (with-current-buffer buffer (call-interactively #'etaf-telega-actions))
    (should (eq (plist-get (etaf-telega-info buffer) :screen) 'actions))
    (let ((text (etaf-telega-test--text buffer)))
      (should (string-match-p "消息操作" text))
      (should (string-match-p "回复" text))
      (should (string-match-p "删除" text)))
    (with-current-buffer buffer (call-interactively #'etaf-telega-back))
    (should (eq (plist-get (etaf-telega-info buffer) :screen) 'chat))))

(ert-deftest etaf-telega-tests-media-and-link-messages-show-extras ()
  "Media and link messages carry placeholder lines."
  (let* ((histories (make-hash-table :test #'equal)))
    (puthash 1 (list (list :id 12 :sender "Alice" :time "10:01" :kind 'photo :text "")
                     (list :id 11 :sender "Alice" :time "10:00"
                           :link (list :site "Example" :title "标题") :text "看这个"))
             histories)
    (let ((backend (etaf-telega-backend-fake-create
                    :chats (list (list :id 1 :title "Alice" :unread 0))
                    :histories histories)))
      (etaf-telega-test--with-client buffer backend
        (etaf-telega-select-chat 1)
        (let ((text (etaf-telega-test--text buffer)))
          (should (string-match-p "\[图片\]" text))
          (should (string-match-p "链接 Example：标题" text)))))))

(ert-deftest etaf-telega-tests-important-and-online-filters ()
  "The importance and online filters narrow the chat list."
  (let* ((chats (list (list :id 1 :title "Alice" :unread 2 :kind 'private :online t)
                      (list :id 2 :title "工作群" :unread 0 :kind 'group :mention 3)
                      (list :id 3 :title "新闻频道" :unread 5 :kind 'channel)))
         (backend (etaf-telega-backend-fake-create
                   :chats chats :histories (make-hash-table :test #'equal))))
    (etaf-telega-test--with-client buffer backend
      (with-current-buffer buffer (etaf-telega-select-filter 'important))
      (let ((text (etaf-telega-test--text buffer)))
        (should (string-match-p "工作群" text))
        (should-not (string-match-p "Alice" text)))
      (with-current-buffer buffer (etaf-telega-select-filter 'online))
      (let ((text (etaf-telega-test--text buffer)))
        (should (string-match-p "Alice" text))
        (should-not (string-match-p "新闻频道" text))))))

(ert-deftest etaf-telega-tests-filter-tabs-switch-the-list ()
  "Filter tabs narrow the chat list inside the one client buffer."
  (let* ((chats (list (list :id 1 :title "Alice" :unread 2 :kind 'private)
                      (list :id 2 :title "工作群" :unread 0 :kind 'group)
                      (list :id 3 :title "新闻频道" :unread 5 :kind 'channel)
                      (list :id 4 :title "旧群" :unread 0 :kind 'group :archived t)))
         (backend (etaf-telega-backend-fake-create
                   :chats chats :histories (make-hash-table :test #'equal))))
    (etaf-telega-test--with-client buffer backend
      (with-current-buffer buffer (etaf-telega-select-filter 'groups))
      (let ((text (etaf-telega-test--text buffer)))
        (should (string-match-p "工作群" text))
        (should-not (string-match-p "Alice" text))
        (should-not (string-match-p "新闻频道" text)))
      (should (equal (plist-get (etaf-telega-info buffer) :filter) 'groups))
      (should (eq (plist-get (etaf-telega-info buffer) :screen) 'root))
      (with-current-buffer buffer (etaf-telega-select-filter 'channels))
      (should (string-match-p "新闻频道" (etaf-telega-test--text buffer)))
      (with-current-buffer buffer (etaf-telega-select-filter 'archived))
      (should (string-match-p "旧群" (etaf-telega-test--text buffer)))
      (with-current-buffer buffer (etaf-telega-select-filter 'main))
      (let ((text (etaf-telega-test--text buffer)))
        (should (string-match-p "Alice" text))
        (should-not (string-match-p "旧群" text))))))

(ert-deftest etaf-telega-tests-viewport-scrolls-and-pages-the-history ()
  "The pane shows the newest rows, follows, scrolls and pages at the top."
  (let* ((histories (make-hash-table :test #'equal))
         (all (cl-loop for n from 30 downto 1
                       collect (list :id n :sender "Alice" :time "10:00"
                                     :text (format "第 %d 条" n)))))
    (puthash 1 all histories)
    (let ((backend (etaf-telega-backend-fake-create
                    :chats (list (list :id 1 :title "Alice" :unread 0))
                    :histories histories)))
      (etaf-telega-test--with-client buffer backend
        (etaf-telega-select-chat 1)
        (dotimes (_ 5)
          (with-current-buffer buffer (call-interactively #'etaf-telega-load-more)))
        (should (equal (plist-get (etaf-telega-info buffer) :messages) 12))
        (let ((text (etaf-telega-test--text buffer)))
          (should (string-match-p "第 30 条" text))
          (should-not (string-match-p "第 19 条" text)))
        ;; Walking the selection to an older message scrolls it into view.
        (dotimes (_ 11)
          (with-current-buffer buffer (call-interactively #'etaf-telega-previous-message)))
        (should (string-match-p "第 19 条" (etaf-telega-test--text buffer)))
        ;; Scrolling at the top loads the next older page, and following
        ;; returns to the newest rows.
        (with-current-buffer buffer (call-interactively #'etaf-telega-page-up))
        (should (equal (plist-get (etaf-telega-info buffer) :messages) 14))
        (with-current-buffer buffer (call-interactively #'etaf-telega-follow))
        (should (string-match-p "第 30 条" (etaf-telega-test--text buffer)))))))

(ert-deftest etaf-telega-tests-day-separators-group-messages ()
  "Messages from different days carry their day labels."
  (let* ((histories (make-hash-table :test #'equal)))
    (puthash 1 (list (list :id 13 :sender "Alice" :time "10:02" :date "2026-10-05" :text "今天")
                     (list :id 12 :sender "Alice" :time "10:01" :date "2026-10-04" :text "昨天")
                     (list :id 11 :sender "Alice" :time "10:00" :date "2026-10-04" :text "前天"))
             histories)
    (let ((backend (etaf-telega-backend-fake-create
                    :chats (list (list :id 1 :title "Alice" :unread 0))
                    :histories histories)))
      (etaf-telega-test--with-client buffer backend
        (etaf-telega-select-chat 1)
        (let ((text (etaf-telega-test--text buffer)))
          (should (string-match-p "2026-10-04" text))
          (should (string-match-p "2026-10-05" text))
          (should (string-match-p "昨天" text)))
        (with-current-buffer buffer (call-interactively #'etaf-telega-load-more))
        (should (string-match-p "前天" (etaf-telega-test--text buffer)))))))

(ert-deftest etaf-telega-tests-load-more-prepends-older-history ()
  "Loading an older page prepends it and keeps the selection."
  (let* ((histories (make-hash-table :test #'equal)))
    (puthash 1 (list (list :id 14 :sender "Alice" :time "10:03" :text "四")
                     (list :id 13 :sender "Alice" :time "10:02" :text "三")
                     (list :id 12 :sender "Alice" :time "10:01" :text "二")
                     (list :id 11 :sender "Alice" :time "10:00" :text "一"))
             histories)
    (let ((backend (etaf-telega-backend-fake-create
                    :chats (list (list :id 1 :title "Alice" :unread 0))
                    :histories histories)))
      (etaf-telega-test--with-client buffer backend
        (etaf-telega-select-chat 1)
        (should (equal (plist-get (etaf-telega-info buffer) :messages) 2))
        (should (equal (plist-get (etaf-telega-info buffer) :selected) 14))
        (with-current-buffer buffer (call-interactively #'etaf-telega-load-more))
        (should (equal (plist-get (etaf-telega-info buffer) :messages) 4))
        (should (equal (plist-get (etaf-telega-info buffer) :selected) 14))
        (let ((text (etaf-telega-test--text buffer)))
          (should (< (string-match-p "一" text) (string-match-p "二" text))))
        (with-current-buffer buffer (call-interactively #'etaf-telega-load-more))
        (should (equal (plist-get (etaf-telega-info buffer) :messages) 4))))))

(ert-deftest etaf-telega-tests-opening-a-chat-reads-its-newest-page ()
  "Opening a chat accepts its newest page as read, like Telega's chatbuf."
  (let ((backend (etaf-telega-test--backend)))
    (etaf-telega-test--with-client buffer backend
      (should (equal (plist-get (car (etaf-telega-backend-fake-chats backend)) :unread) 2))
      (etaf-telega-select-chat 1)
      (should (equal (plist-get (car (etaf-telega-backend-fake-chats backend)) :unread) 0)))))

(ert-deftest etaf-telega-tests-chat-actions-update-the-stored-chat ()
  "The backend chat actions set read and toggle pin and mute."
  (let ((backend (etaf-telega-test--backend)))
    (etaf-telega-backend-chat-action backend 1 'pin)
    (should (eq t (plist-get (car (etaf-telega-backend-fake-chats backend)) :pinned)))
    (etaf-telega-backend-chat-action backend 1 'mute)
    (should (eq t (plist-get (car (etaf-telega-backend-fake-chats backend)) :muted)))
    (etaf-telega-backend-chat-action backend 1 'read)
    (should (equal (plist-get (car (etaf-telega-backend-fake-chats backend)) :unread) 0))))

(provide 'etaf-telega-tests)
(ert-deftest etaf-telega-tests-theme-selection-drives-the-palette ()
  "Pinning a theme changes the colors the views read, and the toggle flips it."
  (let ((etaf-telega-theme 'dark))
    (should (equal (etaf-telega-color :bg) "#0d1117"))
    (should (equal (etaf-telega-color :ink) "#e6edf3"))
    (let ((dark-sender (etaf-telega-sender-color "Alice")))
      (setq etaf-telega-theme 'light)
      (should (equal (etaf-telega-color :bg) "#ffffff"))
      (should-not (equal (etaf-telega-color :ink) "#e6edf3"))
      (should-not (equal (etaf-telega-sender-color "Alice") dark-sender))))
  (let ((etaf-telega-theme 'light))
    (call-interactively #'etaf-telega-toggle-theme)
    (should (eq etaf-telega-theme 'dark))))

;;; etaf-telega-tests.el ends here
