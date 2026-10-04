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
  "The chat keys select the next and previous conversations."
  (etaf-telega-test--with-client buffer nil
    (etaf-telega-select-chat 1)
    (with-current-buffer buffer (call-interactively #'etaf-telega-next-chat))
    (should (equal (plist-get (etaf-telega-info buffer) :chat) 2))
    (with-current-buffer buffer (call-interactively #'etaf-telega-previous-chat))
    (should (equal (plist-get (etaf-telega-info buffer) :chat) 1))))

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
;;; etaf-telega-tests.el ends here
