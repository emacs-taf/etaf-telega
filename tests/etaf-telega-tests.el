;;; etaf-telega-tests.el --- Acceptance tests for etaf-telega -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Public behaviour tests of the single-buffer client.  Every scenario drives
;; the application through the commands and keys declared in `etaf-telega.el'
;; and asserts what a person would see or what the backend receives.  The
;; fake backend replays fixed chats and messages; no network is touched.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'etaf-telega)

(defun etaf-telega-test--histories (&rest entries)
  "Return a history table from ENTRIES of (CHAT-ID . MESSAGES), newest first."
  (let ((histories (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (puthash (car entry) (cdr entry) histories))
    histories))

(defun etaf-telega-test--backend (&optional responder histories)
  "Return a fake backend with three chats and HISTORIES.
Chat 1 holds two messages unless HISTORIES replaces it.  RESPONDER is the
fake backend's send responder."
  (etaf-telega-backend-fake-create
   :chats (list (list :id 1 :title "Alice" :unread 2 :preview "在的"
                      :kind 'private :time "10:01")
                (list :id 2 :title "工作群" :unread 0 :preview "周报"
                      :kind 'group :time "09:30")
                (list :id 3 :title "每日新闻" :unread 5 :preview "头条"
                      :kind 'channel :time "08:00"))
   :histories (or histories
                  (etaf-telega-test--histories
                   (list 1
                         (list :id 12 :outgoing t :sender "我" :time "10:01"
                               :text "在的")
                         (list :id 11 :sender "Alice" :time "10:00"
                               :text "你好"))))
   :responder responder))

(defmacro etaf-telega-test--with-client (var backend &rest body)
  "Open a client in VAR on BACKEND, run BODY in it and always close it."
  (declare (indent 2))
  `(let* ((backend ,backend)
          (,var (let ((etaf-telega-backend-create-function (lambda () backend)))
                  (etaf-telega-open))))
     (unwind-protect
         (save-window-excursion
           (switch-to-buffer ,var)
           ,@body)
       (etaf-telega-close))))

(defun etaf-telega-test--keys (keys)
  "Press KEYS in the client like a person at the keyboard."
  (execute-kbd-macro (kbd keys)))

(defun etaf-telega-test--text ()
  "Return the client buffer's text without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun etaf-telega-test--info (key)
  "Return the visible client fact KEY."
  (plist-get (etaf-telega-info) key))

(defun etaf-telega-test--before-point-p (text)
  "Return non-nil when TEXT appears before point."
  (string-search text (buffer-substring-no-properties (point-min) (point))))

(ert-deftest etaf-telega-tests-open-lists-chats-and-close-releases ()
  "Opening shows the account's chats in one buffer; closing kills it."
  (let (client)
    (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
      (setq client buffer)
      (should (eq (etaf-telega-test--info :screen) 'root))
      (should (equal (etaf-telega-test--info :status) "就绪"))
      (should (= (etaf-telega-test--info :chats) 3))
      (let ((text (etaf-telega-test--text)))
        (dolist (title '("搜索" "Alice" "工作群" "每日新闻" "全部" "群组"))
          (should (string-search title text)))
        (should (< (string-search "Alice" text) (string-search "工作群" text)))))
    (should-not (buffer-live-p client))
    (should-not (etaf-telega-test--info :buffer))))

(ert-deftest etaf-telega-tests-filter-shows-matching-chats ()
  "A filter tab narrows the list and the main tab restores it."
  (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
    (etaf-telega-select-filter 'groups)
    (should (eq (etaf-telega-test--info :filter) 'groups))
    (let ((text (etaf-telega-test--text)))
      (should (string-search "工作群" text))
      (should-not (string-search "每日新闻" text)))
    (etaf-telega-select-filter 'main)
    (should (string-search "每日新闻" (etaf-telega-test--text)))))

(ert-deftest etaf-telega-tests-filter-keys-cycle ()
  "The [ and ] keys walk the list's filter tabs and narrow the list."
  (let ((backend (etaf-telega-backend-fake-create
                  :chats (list (list :id 1 :title "Alice" :unread 2 :mention 1
                                     :preview "在的" :kind 'private :time "10:01")
                               (list :id 2 :title "工作群" :unread 0 :online t
                                     :preview "周报" :kind 'group :time "09:30")))))
    (etaf-telega-test--with-client buffer backend
      (should (eq (etaf-telega-test--info :filter) 'main))
      (should (string-search "Alice" (etaf-telega-test--text)))
      (etaf-telega-test--keys "]")
      (should (eq (etaf-telega-test--info :filter) 'important))
      (let ((text (etaf-telega-test--text)))
        (should (string-search "Alice" text))
        (should-not (string-search "工作群" text)))
      (etaf-telega-test--keys "]")
      (should (eq (etaf-telega-test--info :filter) 'online))
      (let ((text (etaf-telega-test--text)))
        (should (string-search "工作群" text))
        (should-not (string-search "Alice" text)))
      (etaf-telega-test--keys "[")
      (should (eq (etaf-telega-test--info :filter) 'important))
      (etaf-telega-test--keys "[")
      (should (eq (etaf-telega-test--info :filter) 'main)))))

(ert-deftest etaf-telega-tests-list-keys-move-and-open ()
  "n and p walk the list and RET opens the conversation under point."
  (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
    (should (equal (etaf-telega-test--info :cursor) 1))
    (etaf-telega-test--keys "n")
    (should (equal (etaf-telega-test--info :cursor) 2))
    (etaf-telega-test--keys "n")
    (should (equal (etaf-telega-test--info :cursor) 3))
    (etaf-telega-test--keys "p")
    (should (equal (etaf-telega-test--info :cursor) 2))
    (should (eq (etaf-telega-test--info :screen) 'root))
    (etaf-telega-test--keys "RET")
    (should (eq (etaf-telega-test--info :screen) 'chat))
    (should (equal (etaf-telega-test--info :chat) 2))))

(ert-deftest etaf-telega-tests-chat-with-opens-by-name ()
  "Known conversations are offered by name and the chosen one opens."
  (let ((backend (etaf-telega-test--backend)))
    (etaf-telega-test--with-client buffer backend
      (should (equal (mapcar (lambda (item) (plist-get item :title))
                             (etaf-telega-backend-chat-candidates backend))
                     '("Alice" "工作群" "每日新闻")))
      (etaf-telega-chat-with 3)
      (should (eq (etaf-telega-test--info :screen) 'chat))
      (should (equal (etaf-telega-test--info :chat) 3))
      (should (string-search "每日新闻" (etaf-telega-test--text))))))

(ert-deftest etaf-telega-tests-open-chat-shows-history-and-lands-on-composer ()
  "Opening a chat replaces the list with its history, oldest first."
  (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
    (etaf-telega-open-chat 1)
    (should (eq (etaf-telega-test--info :screen) 'chat))
    (should (= (etaf-telega-test--info :messages) 2))
    (let ((text (etaf-telega-test--text)))
      (should (string-search "Alice" text))
      (should-not (string-search "工作群" text))
      (should (< (string-search "你好" text) (string-search "在的" text))))
    (should (etaf-telega-test--before-point-p "在的"))
    (should-not (etaf-telega-test--info :editing))))

(ert-deftest etaf-telega-tests-back-returns-to-the-list ()
  "q leaves the chat for the list with that conversation under point."
  (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
    (etaf-telega-open-chat 2)
    (etaf-telega-test--keys "q")
    (should (eq (etaf-telega-test--info :screen) 'root))
    (should (equal (etaf-telega-test--info :cursor) 2))
    (should (string-search "每日新闻" (etaf-telega-test--text)))
    (etaf-telega-test--keys "RET")
    (should (equal (etaf-telega-test--info :chat) 2))))

(ert-deftest etaf-telega-tests-message-keys-walk-history-and-composer ()
  "p selects older messages and n past the newest returns to the composer."
  (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
    (etaf-telega-open-chat 1)
    (etaf-telega-test--keys "p")
    (should (equal (etaf-telega-test--info :selected) 12))
    (etaf-telega-test--keys "p")
    (should (equal (etaf-telega-test--info :selected) 11))
    (should-not (etaf-telega-test--before-point-p "你好"))
    (etaf-telega-test--keys "n")
    (should (equal (etaf-telega-test--info :selected) 12))
    (etaf-telega-test--keys "n")
    (should-not (etaf-telega-test--info :selected))
    (should (etaf-telega-test--before-point-p "在的"))))

(ert-deftest etaf-telega-tests-jump-newest-selects-the-newest-message ()
  "> shows the newest message from anywhere in the history."
  (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
    (etaf-telega-open-chat 1)
    (etaf-telega-test--keys "p p")
    (should (equal (etaf-telega-test--info :selected) 11))
    (etaf-telega-test--keys ">")
    (should (equal (etaf-telega-test--info :selected) 12))))

(ert-deftest etaf-telega-tests-composer-sends-and-clears-on-ack ()
  "Typing in the composer sends the text and clears it after acceptance."
  (let ((backend (etaf-telega-test--backend
                  (lambda (fake text) (etaf-telega-backend-fake-complete fake text)))))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-open-chat 1)
      (etaf-telega-test--keys "i")
      (should (etaf-telega-test--info :editing))
      (etaf-telega-test--keys "今晚见")
      (etaf-telega-test--keys "RET")
      (should (equal (etaf-telega-backend-fake-sent backend) '("今晚见")))
      (should (equal (etaf-telega-test--info :draft) ""))
      (should (= (etaf-telega-test--info :messages) 3))
      (let ((text (etaf-telega-test--text)))
        (should (< (string-search "在的" text) (string-search "今晚见" text)))))))

(ert-deftest etaf-telega-tests-input-history-recalls-sent-texts ()
  "M-p and M-n recall the texts sent in the open conversation."
  (let ((backend (etaf-telega-test--backend
                  (lambda (fake text) (etaf-telega-backend-fake-complete fake text)))))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-open-chat 1)
      (etaf-telega-test--keys "i 第一条 RET")
      (etaf-telega-test--keys "i 第二条 RET")
      (should (equal (etaf-telega-test--info :draft) ""))
      (etaf-telega-test--keys "i")
      (etaf-telega-test--keys "M-p")
      (should (equal (etaf-telega-test--info :draft) "第二条"))
      (etaf-telega-test--keys "M-p")
      (should (equal (etaf-telega-test--info :draft) "第一条"))
      (etaf-telega-test--keys "M-n")
      (should (equal (etaf-telega-test--info :draft) "第二条"))
      (etaf-telega-test--keys "M-n")
      (should (equal (etaf-telega-test--info :draft) "")))))

(ert-deftest etaf-telega-tests-draft-survives-until-ack ()
  "A pending send keeps the draft and clears only after acceptance."
  (let ((backend (etaf-telega-test--backend)))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-open-chat 1)
      (etaf-telega-compose)
      (etaf-telega-test--keys "未确认")
      (etaf-telega-test--keys "RET")
      (should (equal (etaf-telega-test--info :draft) "未确认"))
      (etaf-telega-backend-fake-complete backend "未确认")
      (should (equal (etaf-telega-test--info :draft) "")))))

(ert-deftest etaf-telega-tests-leaving-the-composer-keeps-the-draft ()
  "Cancelling plain typing keeps the text as the chat's draft."
  (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
    (etaf-telega-open-chat 1)
    (etaf-telega-test--keys "i")
    (etaf-telega-test--keys "草稿")
    (etaf-telega-test--keys "C-c C-k")
    (should-not (etaf-telega-test--info :editing))
    (should (equal (etaf-telega-test--info :draft) "草稿"))
    (should (string-search "草稿" (etaf-telega-test--text)))))

(ert-deftest etaf-telega-tests-reply-quotes-the-selected-message ()
  "r arms a reply to the message under point and the sent message quotes it."
  (let ((backend (etaf-telega-test--backend
                  (lambda (fake text) (etaf-telega-backend-fake-complete fake text)))))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-open-chat 1)
      (etaf-telega-test--keys "p p r")
      (should (eq (etaf-telega-test--info :compose) 'reply))
      (should (string-search "回复 Alice：你好" (etaf-telega-test--text)))
      (etaf-telega-test--keys "收到")
      (etaf-telega-test--keys "RET")
      (should (equal (etaf-telega-backend-fake-replies backend) '(("收到" . 11))))
      (should-not (etaf-telega-test--info :compose))
      (let ((text (etaf-telega-test--text)))
        (should-not (string-search "回复 Alice" text))
        (should (string-match-p "Alice[ \n]*你好\\(.\\|\n\\)*收到" text))))))

(ert-deftest etaf-telega-tests-edit-replaces-and-cancel-restores-the-draft ()
  "e edits an outgoing message; cancelling an edit restores the old draft."
  (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
    (etaf-telega-open-chat 1)
    (etaf-telega-test--keys "i")
    (etaf-telega-test--keys "原草稿")
    (etaf-telega-test--keys "C-c C-k p e")
    (should (eq (etaf-telega-test--info :compose) 'edit))
    (should (equal (etaf-telega-test--info :draft) "在的"))
    (etaf-telega-test--keys "C-c C-k")
    (should-not (etaf-telega-test--info :compose))
    (should (equal (etaf-telega-test--info :draft) "原草稿"))
    (etaf-telega-test--keys "p e")
    (etaf-telega-test--keys "C-e ！")
    (etaf-telega-test--keys "RET")
    (should-not (etaf-telega-test--info :compose))
    (should (string-match-p "在的！ +已编辑" (etaf-telega-test--text)))))

(ert-deftest etaf-telega-tests-edit-refuses-incoming-messages ()
  "Only the account's own messages can be edited."
  (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
    (etaf-telega-open-chat 1)
    (etaf-telega-test--keys "p p")
    (should-error (etaf-telega-edit) :type 'user-error)
    (should-not (etaf-telega-test--info :compose))))

(ert-deftest etaf-telega-tests-live-changes-update-the-open-chat ()
  "Arrivals append once, updates replace in place and deletions remove."
  (let ((backend (etaf-telega-test--backend)))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-open-chat 1)
      (let ((message (list :id 13 :sender "Alice" :time "10:02" :text "晚上见")))
        (etaf-telega-backend-fake-deliver backend message)
        (etaf-telega-backend-fake-deliver backend (copy-tree message)))
      (should (= (etaf-telega-test--info :messages) 3))
      (should (< (string-search "在的" (etaf-telega-test--text))
                 (string-search "晚上见" (etaf-telega-test--text))))
      (etaf-telega-backend-fake-update
       backend (list :id 11 :sender "Alice" :time "10:00" :text "你好呀" :edited t))
      (should (string-match-p "你好呀[ \n]*已编辑 10:00" (etaf-telega-test--text)))
      (etaf-telega-backend-fake-remove backend 12)
      (should (= (etaf-telega-test--info :messages) 2))
      (should-not (string-search "在的" (etaf-telega-test--text))))))

(ert-deftest etaf-telega-tests-load-more-prepends-older-pages ()
  "Older pages load above the history until the chat's first message."
  (let ((backend (etaf-telega-test--backend
                  nil
                  (etaf-telega-test--histories
                   (cons 1 (cl-loop for id from 15 downto 11
                                    collect (list :id id :sender "Alice"
                                                  :time "10:00"
                                                  :text (format "第%d条" id))))))))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-open-chat 1)
      (cl-loop repeat 5 until (etaf-telega-test--info :history-end)
               do (etaf-telega-load-more))
      (should (etaf-telega-test--info :history-end))
      (should (= (etaf-telega-test--info :messages) 5))
      (let ((text (etaf-telega-test--text)))
        (should (< (string-search "第11条" text) (string-search "第13条" text)
                   (string-search "第15条" text)))))))

(ert-deftest etaf-telega-tests-jump-loads-and-selects-a-message ()
  "Jumping to an unloaded message loads its page and selects it."
  (let ((backend (etaf-telega-test--backend
                  nil
                  (etaf-telega-test--histories
                   (cons 1 (cl-loop for id from 50 downto 11
                                    collect (list :id id :sender "Alice"
                                                  :time "10:00"
                                                  :text (format "第%d条" id))))))))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-open-chat 1)
      (should (< (etaf-telega-test--info :messages) 40))
      (should-not (string-search "第11条" (etaf-telega-test--text)))
      (etaf-telega-show-message 11)
      (should (equal (etaf-telega-test--info :selected) 11))
      (should (string-search "第11条" (etaf-telega-test--text))))))

(ert-deftest etaf-telega-tests-jumps-reach-quoted-pinned-and-dated-messages ()
  "Quoted, pinned and dated jumps select the right message."
  (let* ((histories
          (etaf-telega-test--histories
           (list 1
                 (list :id 13 :sender "Alice" :time "09:00" :date "2026-10-05"
                       :text "今天")
                 (list :id 12 :outgoing t :sender "我" :time "10:00" :date "2026-10-04"
                       :text "在的"
                       :reply (list :id 11 :sender "Alice" :kind 'text :text "你好"))
                 (list :id 11 :sender "Alice" :time "10:00" :date "2026-10-03"
                       :text "你好"))))
         (backend (etaf-telega-backend-fake-create
                   :chats (list (list :id 1 :title "Alice" :unread 0 :preview "在的"
                                      :kind 'private :time "10:01" :pinned-id 13))
                   :histories histories)))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-open-chat 1)
      (etaf-telega-test--keys "p p")
      (etaf-telega-jump-quote)
      (should (equal (etaf-telega-test--info :selected) 11))
      (etaf-telega-jump-pinned)
      (should (equal (etaf-telega-test--info :selected) 13))
      (etaf-telega-jump-date (float-time (date-to-time "2026-10-04 10:00")))
      (should (equal (etaf-telega-test--info :selected) 12)))))

(ert-deftest etaf-telega-tests-next-unread-chat-moves-to-the-unread-one ()
  "M-g u on the list selects the next conversation with unread messages."
  (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
    (should (equal (etaf-telega-test--info :cursor) 1))
    (etaf-telega-test--keys "M-g u")
    (should (equal (etaf-telega-test--info :cursor) 3))
    (etaf-telega-test--keys "M-g u")
    (should (equal (etaf-telega-test--info :cursor) 1))))

(ert-deftest etaf-telega-tests-fastnav-walks-unread-mentions-and-outgoing ()
  "M-g u/m/o select unread, mentioning and outgoing messages."
  (let* ((histories
          (etaf-telega-test--histories
           (list 1
                 (list :id 15 :outgoing t :sender "我" :time "10:04" :text "收到")
                 (list :id 14 :sender "Alice" :time "10:03" :text "在吗 @fixture")
                 (list :id 13 :sender "Alice" :time "10:02" :text "新消息")
                 (list :id 12 :sender "Alice" :time "10:01" :text "已读")
                 (list :id 11 :outgoing t :sender "我" :time "10:00" :text "在的"))))
         (backend (etaf-telega-test--backend nil histories)))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-open-chat 1)
      (etaf-telega-test--keys "M-g u")
      (should (equal (etaf-telega-test--info :selected) 12))
      (etaf-telega-test--keys "M-g u")
      (should (equal (etaf-telega-test--info :selected) 13))
      (etaf-telega-test--keys "M-g m")
      (should (equal (etaf-telega-test--info :selected) 14))
      (etaf-telega-test--keys "M-g o")
      (should (equal (etaf-telega-test--info :selected) 15)))))

(ert-deftest etaf-telega-tests-day-separators-label-each-date ()
  "Each new date opens with a day label; today reads as 今天."
  (let ((today (format-time-string "%Y-%m-%d")))
    (etaf-telega-test--with-client buffer
        (etaf-telega-test--backend
         nil (etaf-telega-test--histories
              (list 1 (list :id 12 :sender "Alice" :date today :time "09:00" :text "早")
                    (list :id 11 :sender "Alice" :date "2020-01-02" :time "22:00"
                          :text "晚安"))))
      (etaf-telega-open-chat 1)
      (let ((text (etaf-telega-test--text)))
        (should (< (string-search "2020-01-02" text) (string-search "晚安" text)
                   (string-search "今天" text) (string-search "早" text)))))))

(ert-deftest etaf-telega-tests-activate-opens-media-or-composes ()
  "RET on a media message opens it through Telega; elsewhere it composes."
  (let ((backend (etaf-telega-test--backend
                  nil (etaf-telega-test--histories
                       (list 1 (list :id 12 :sender "Alice" :time "10:01" :text "看看"
                                     :kind 'photo)
                             (list :id 11 :sender "Alice" :time "10:00" :text "你好"))))))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-open-chat 1)
      (should (string-search "看看" (etaf-telega-test--text)))
      (etaf-telega-test--keys "p RET")
      (should (equal (cdr (car (etaf-telega-backend-fake-natives backend))) '(1 12)))
      (should-not (etaf-telega-test--info :editing))
      (etaf-telega-test--keys "p RET")
      (should (= (length (etaf-telega-backend-fake-natives backend)) 1))
      (should (etaf-telega-test--info :editing)))))

(defun etaf-telega-test--image ()
  "Return a temporary SVG file standing in for downloaded media."
  (let ((file (make-temp-file "etaf-telega-test" nil ".svg")))
    (with-temp-file file
      (insert "<svg xmlns='http://www.w3.org/2000/svg' width='4' height='3'>"
              "<rect width='4' height='3' fill='red'/></svg>"))
    file))

(ert-deftest etaf-telega-tests-viewer-pages-and-closes-in-the-buffer ()
  "RET on a photo opens the in-buffer viewer; n and p page; q closes it."
  (let* ((image (etaf-telega-test--image))
         (media (list :src image :width 4 :height 3))
         (backend (etaf-telega-test--backend
                   nil (etaf-telega-test--histories
                        (list 1 (list :id 12 :sender "Alice" :time "10:01"
                                      :text "第二张" :kind 'photo :media media)
                              (list :id 11 :sender "Alice" :time "10:00"
                                    :text "第一张" :kind 'photo :media media))))))
    (unwind-protect
        (etaf-telega-test--with-client buffer backend
          (etaf-telega-open-chat 1)
          (etaf-telega-test--keys "p RET")
          (should (equal (etaf-telega-test--info :viewer) 12))
          (should-not (etaf-telega-backend-fake-natives backend))
          (etaf-telega-test--keys "p")
          (should (equal (etaf-telega-test--info :viewer) 11))
          (etaf-telega-test--keys "n")
          (should (equal (etaf-telega-test--info :viewer) 12))
          (etaf-telega-test--keys "q")
          (should-not (etaf-telega-test--info :viewer))
          (should (eq (etaf-telega-test--info :screen) 'chat)))
      (delete-file image))))

(ert-deftest etaf-telega-tests-react-toggles-a-visible-reaction ()
  "Reacting shows the emoji and its count; reacting again removes it."
  (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
    (etaf-telega-open-chat 1)
    (should-not (string-search "👍" (etaf-telega-test--text)))
    (etaf-telega-react 11 "👍")
    (should (string-search "👍" (etaf-telega-test--text)))
    (etaf-telega-react 11 "👍")
    (should-not (string-search "👍" (etaf-telega-test--text)))))

(ert-deftest etaf-telega-tests-own-messages-align-with-everyone-else ()
  "Outgoing and incoming messages start in the same column."
  (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
    (etaf-telega-open-chat 1)
    (cl-flet ((column (text)
                (save-excursion
                  (goto-char (point-min))
                  (search-forward text)
                  (- (current-column) (length text)))))
      (should (= (column "你好") (column "在的"))))))

(ert-deftest etaf-telega-tests-swipes-go-back-and-change-filters ()
  "Swiping right leaves a chat; swiping on the list changes the filter."
  (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
    (etaf-telega-open-chat 1)
    (etaf-telega-swipe 'right)
    (should (eq (etaf-telega-test--info :screen) 'root))
    (let ((filter (etaf-telega-test--info :filter)))
      (etaf-telega-swipe 'left)
      (should-not (equal (etaf-telega-test--info :filter) filter))
      (etaf-telega-swipe 'right)
      (should (equal (etaf-telega-test--info :filter) filter)))))

(ert-deftest etaf-telega-tests-printing-keys-never-edit-the-history ()
  "Unbound printing keys on a message leave the history text unchanged."
  (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
    (etaf-telega-open-chat 1)
    (etaf-telega-test--keys "p")
    (let ((before (etaf-telega-test--text)))
      (etaf-telega-test--keys "z")
      (should (equal (etaf-telega-test--text) before))
      (should-not (etaf-telega-test--info :editing)))))

(ert-deftest etaf-telega-tests-telega-keys-run-native-commands ()
  "Telega's own keys run Telega's commands on the chat or message at point."
  (let ((backend (etaf-telega-test--backend)))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-test--keys "n P")
      (should (equal (car (etaf-telega-backend-fake-natives backend))
                     '(telega-chat-toggle-pin 2 nil)))
      (etaf-telega-open-chat 1)
      (etaf-telega-test--keys "p c")
      (should (equal (car (etaf-telega-backend-fake-natives backend))
                     '(telega-msg-copy-dwim 1 12))))))

(ert-deftest etaf-telega-tests-chat-action-keys-run-native-commands ()
  "M, A and L run Telega's mute, archive and leave on the chat under point."
  (let ((backend (etaf-telega-test--backend)))
    (etaf-telega-test--with-client buffer backend
      (etaf-telega-test--keys "n")
      (should (equal (etaf-telega-test--info :cursor) 2))
      (etaf-telega-test--keys "M")
      (should (equal (car (etaf-telega-backend-fake-natives backend))
                     '(telega-chat-toggle-muted 2 nil)))
      (etaf-telega-test--keys "A")
      (should (equal (car (etaf-telega-backend-fake-natives backend))
                     '(telega-chat-toggle-archive 2 nil)))
      (etaf-telega-test--keys "L")
      (should (equal (car (etaf-telega-backend-fake-natives backend))
                     '(telega-chat-leave 2 nil))))))

(ert-deftest etaf-telega-tests-theme-switch-repaints-the-client ()
  "Toggling the theme swaps the palette and keeps the open chat."
  (let ((etaf-telega-theme 'dark))
    (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
      (etaf-telega-open-chat 1)
      (let ((dark (etaf-telega-color :bg)))
        (etaf-telega-toggle-theme)
        (should (eq etaf-telega-theme 'light))
        (should-not (equal dark (etaf-telega-color :bg)))
        (should (eq (etaf-telega-test--info :screen) 'chat))
        (should (string-search "在的" (etaf-telega-test--text)))))))

(ert-deftest etaf-telega-tests-search-finds-opens-and-remembers-chats ()
  "/ searches the list; RET opens the first match, which becomes recent."
  (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
    (etaf-telega-test--keys "/")
    (should (equal (etaf-telega-test--info :search) ""))
    (should (string-search "常用联系人" (etaf-telega-test--text)))
    (etaf-telega-test--keys "工作")
    (should (equal (etaf-telega-test--info :search) "工作"))
    (let ((text (etaf-telega-test--text)))
      (should (string-search "工作群" text))
      (should-not (string-search "每日新闻" text)))
    (etaf-telega-test--keys "RET")
    (should (eq (etaf-telega-test--info :screen) 'chat))
    (should (equal (etaf-telega-test--info :chat) 2))
    (should-not (etaf-telega-test--info :search))
    (etaf-telega-test--keys "q /")
    (let ((text (etaf-telega-test--text)))
      (should (string-match-p "最近\\(.\\|\n\\)*工作群" text)))
    (etaf-telega-search-clear-recent)
    (should (string-search "打开的搜索结果会列在这里" (etaf-telega-test--text)))
    (etaf-telega-search-exit)
    (should-not (etaf-telega-test--info :search))
    (should (string-search "每日新闻" (etaf-telega-test--text)))))

(ert-deftest etaf-telega-tests-chat-search-and-details-pane ()
  "/ in a chat narrows the history; C-c i shows and hides the details."
  (etaf-telega-test--with-client buffer (etaf-telega-test--backend)
    (etaf-telega-open-chat 1)
    (etaf-telega-test--keys "C-c C-k /")
    (should (equal (etaf-telega-test--info :chat-search) ""))
    (etaf-telega-test--keys "你好")
    (let ((text (etaf-telega-test--text)))
      (should (string-search "你好" text))
      (should-not (string-search "在的" text)))
    (etaf-telega-chat-search-exit)
    (should-not (etaf-telega-test--info :chat-search))
    (should (string-search "在的" (etaf-telega-test--text)))
    (should-not (etaf-telega-test--info :details))
    (etaf-telega-test--keys "C-c i")
    (should (etaf-telega-test--info :details))
    (should (string-search "对话信息" (etaf-telega-test--text)))
    (etaf-telega-test--keys "C-c i")
    (should-not (etaf-telega-test--info :details))))

(provide 'etaf-telega-tests)
;;; etaf-telega-tests.el ends here
