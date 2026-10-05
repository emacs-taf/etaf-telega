;;; etaf-telega-gui-scenarios.el --- GUI acceptance for the Telegram client -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Run inside an owned graphical Emacs instance.  Drives the real client
;; through its declared public API with the offline backend, using command-loop
;; keys and shared control activation; Telega's account is never touched and no
;; frame is raised.
;;; Code:
(require 'cl-lib)
(require 'etaf-telega)
(require 'emacs-gui-verifier)

(defvar etaf-telega-gui--buffer nil)
(defvar etaf-telega-gui--backend nil)
(defvar etaf-telega-gui--hold nil
  "When non-nil the fake backend withholds its send acknowledgement.")

(defun etaf-telega-gui--responder (backend text)
  "Acknowledge TEXT for BACKEND unless the scenario holds the receipt."
  (unless etaf-telega-gui--hold
    (etaf-telega-backend-fake-complete backend text)))

(defun etaf-telega-gui--backend-create ()
  "Return a fake backend with two chats and a short history."
  (let ((histories (make-hash-table :test #'equal)))
    (puthash 1 (list (list :id 12 :outgoing t :sender "我" :time "10:01" :date "2026-10-04" :text "在的")
                     (list :id 11 :sender "Alice" :time "10:00" :date "2026-10-04" :text "你好"))
             histories)
    (puthash 2 (list (list :id 21 :sender "Bob" :time "09:30" :date "2026-10-04" :text "周报收到"))
             histories)
    (setq etaf-telega-gui--backend
          (etaf-telega-backend-fake-create
           :chats (list (list :id 1 :title "Alice" :unread 2 :preview "晚上见" :time "10:02"
                              :kind 'private)
                        (list :id 2 :title "工作群" :unread 0 :preview "周报收到" :time "09:30"
                              :kind 'group)
                        (list :id 3 :title "每日新闻" :unread 5 :preview "头条速览" :time "08:00"
                              :kind 'channel))
           :histories histories :responder #'etaf-telega-gui--responder))))

(defun etaf-telega-gui--main ()
  "Select the client window."
  (if-let* ((window (get-buffer-window etaf-telega-gui--buffer t)))
      (select-window window)
    (switch-to-buffer etaf-telega-gui--buffer)))

(defun etaf-telega-gui--info ()
  "Return the client facts of the owned buffer."
  (with-current-buffer etaf-telega-gui--buffer (etaf-telega-info)))

(defun etaf-telega-gui--idle (_)
  "Check settled client facts."
  (and (stringp (plist-get (etaf-telega-gui--info) :status))
       (not (string-prefix-p "错误" (plist-get (etaf-telega-gui--info) :status)))))

(defun etaf-telega-gui--text ()
  "Return the owned buffer's visible text."
  (with-current-buffer etaf-telega-gui--buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun etaf-telega-gui--geometry ()
  "Check the complete canvas and its window fit."
  (let* ((window (get-buffer-window etaf-telega-gui--buffer))
         (size (and window (window-text-pixel-size window)))
         (editing (with-current-buffer etaf-telega-gui--buffer
                    (and (etaf-runtime-for-buffer (current-buffer))
                         (etaf-text-input-active-p (etaf-runtime-for-buffer (current-buffer)))))))
    (and window
         (or editing (etaf-gui-verifier-canvas-filled-p etaf-telega-gui--buffer))
         (or editing (<= (car size) (window-body-width window t)))
         (= (window-hscroll window) 0)
         (let ((info (etaf-telega-gui--info)))
           (and (integerp (plist-get info :chats))
                (<= 1 (plist-get info :chats)))))))

(defun etaf-telega-gui--action (id execute &optional ready screenshot)
  "Build the verifier action ID with EXECUTE, READY and SCREENSHOT."
  (etaf-gui-verifier-action-create
   :id id :screenshot screenshot
   :execute (lambda (context)
              (etaf-telega-gui--main) (funcall execute context)
              (setf (etaf-gui-verifier-context-target-buffer context)
                    (window-buffer (selected-window))))
   :settled-p
   (lambda (_context)
     (with-current-buffer etaf-telega-gui--buffer
       (and (or (not ready) (funcall ready nil))
            (etaf-telega-gui--geometry))))
   :assertions
   (lambda (_context)
     (with-current-buffer etaf-telega-gui--buffer
       (append (list (etaf-gui-verifier-assert
                      "no client error"
                      (not (string-prefix-p "错误" (plist-get (etaf-telega-info) :status)))))
               (etaf-gui-verifier-line-height-assertions etaf-telega-gui--buffer)
               (list (etaf-gui-verifier-assert
                      "composer lives inside the conversation screen"
                      (let ((info (etaf-telega-info)))
                        (or (not (eq (plist-get info :screen) 'chat))
                            (let* ((runtime (etaf-runtime-for-buffer etaf-telega-gui--buffer))
                                   (bounds (and runtime
                                                (etaf-host-ref-bounds runtime 'telega-composer))))
                              (and runtime (consp bounds))))))))))))

(defun etaf-telega-gui--type (text)
  "Enter TEXT into the active composer session."
  (insert text))

(defun etaf-telega-gui-run (directory)
  "Run the offline product workflow and save evidence to DIRECTORY."
  (let* ((original-frame (selected-frame))
         (frame (make-frame '((name . "Telegram / GUI acceptance") (no-focus-on-map . t))))
         (scenario
          (etaf-gui-verifier-scenario-create
           :name "etaf-telega-product" :claim "Telega-style conversation: selection, reply, edit, live stream"
           :initialize
           (lambda (context)
             (select-frame frame) (delete-frame original-frame t)
             (set-frame-size frame 140 44)
             (setq etaf-telega-gui--hold nil)
             (let ((etaf-telega-backend-create-function
                    (lambda () (etaf-telega-gui--backend-create) etaf-telega-gui--backend)))
               (setq etaf-telega-gui--buffer (etaf-telega-open))
               (with-current-buffer etaf-telega-gui--buffer (etaf-telega-select-chat 1)))
             (setf (etaf-gui-verifier-context-target-buffer context) etaf-telega-gui--buffer))
           :actions
           (append
            (list
             (etaf-telega-gui--action "chat-open" #'ignore #'etaf-telega-gui--idle t)
             (etaf-telega-gui--action
              "chat-back" (lambda (_) (execute-kbd-macro (kbd "q")))
              (lambda (_) (eq (plist-get (etaf-telega-gui--info) :screen) 'root)) t)
             (etaf-telega-gui--action
              "filter-groups" (lambda (_) (etaf-telega-select-filter 'groups))
              (lambda (_) (and (eq (plist-get (etaf-telega-gui--info) :filter) 'groups)
                               (string-match-p "工作群" (etaf-telega-gui--text)))))
             (etaf-telega-gui--action
              "filter-main" (lambda (_) (etaf-telega-select-filter 'main))
              (lambda (_) (and (eq (plist-get (etaf-telega-gui--info) :filter) 'main)
                               (string-match-p "Alice" (etaf-telega-gui--text)))))
             (etaf-telega-gui--action
              "chat-open-again" (lambda (_) (execute-kbd-macro (kbd "RET")))
              (lambda (_) (eq (plist-get (etaf-telega-gui--info) :screen) 'chat)))
             (etaf-telega-gui--action
              "select-previous-message" (lambda (_) (execute-kbd-macro (kbd "p")))
              (lambda (_) (equal (plist-get (etaf-telega-gui--info) :selected) 11)) t)
             (etaf-telega-gui--action
              "reply" (lambda (_)
                        (execute-kbd-macro (kbd "r"))
                        (unless (eq (plist-get (etaf-telega-gui--info) :compose) 'reply)
                          (error "Reply mode did not arm"))
                        (etaf-telega-gui--type "收到，明早见")
                        (execute-kbd-macro (kbd "RET")))
              (lambda (_)
                (let ((info (etaf-telega-gui--info)))
                  (and (null (plist-get info :compose))
                       (equal (plist-get info :draft) "")
                       (string-match-p "↩ Alice：你好" (etaf-telega-gui--text)))))
              t)
             (etaf-telega-gui--action
              "edit" (lambda (_)
                       (execute-kbd-macro (kbd "e"))
                       (unless (eq (plist-get (etaf-telega-gui--info) :compose) 'edit)
                         (error "Edit mode did not arm"))
                       (etaf-telega-gui--type "！")
                       (execute-kbd-macro (kbd "RET")))
              (lambda (_)
                (and (null (plist-get (etaf-telega-gui--info) :compose))
                     (string-match-p "（已编辑）" (etaf-telega-gui--text)))))
             (etaf-telega-gui--action
              "cancel-edit-restores-draft"
              (lambda (_)
                (execute-kbd-macro (kbd "e"))
                (etaf-telega-gui--type "不要这个")
                (execute-kbd-macro (kbd "C-c C-k")))
              (lambda (_)
                (let ((info (etaf-telega-gui--info)))
                  (and (null (plist-get info :compose))
                       (equal (plist-get info :draft) ""))))
              t)
             (etaf-telega-gui--action
              "copy" (lambda (_)
                       (execute-kbd-macro (kbd "p"))
                       (execute-kbd-macro (kbd "p"))
                       (execute-kbd-macro (kbd "c")))
              (lambda (_) (equal (current-kill 0) "你好")))
             (etaf-telega-gui--action
              "delete" (lambda (_)
                         (execute-kbd-macro (kbd "d yes RET")))
              (lambda (_)
                (and (not (string-match-p "\n  你好" (etaf-telega-gui--text)))
                     (= (plist-get (etaf-telega-gui--info) :messages) 2))))
             (etaf-telega-gui--action
              "live-message-appends"
              (lambda (_)
                (etaf-telega-backend-fake-deliver
                 etaf-telega-gui--backend
                 (list :id 13 :sender "Alice" :time "10:02" :date "2026-10-05" :text "好的，明早见")))
              (lambda (_) (string-match-p "好的，明早见" (etaf-telega-gui--text)))
              t)
             (etaf-telega-gui--action
              "composer-draft" (lambda (_)
                                 (execute-kbd-macro (kbd "i"))
                                 (etaf-telega-gui--type "未发送的草稿")
                                 (execute-kbd-macro (kbd "C-c C-k")))
              (lambda (_)
                (let ((info (etaf-telega-gui--info)))
                  (and (not (plist-get info :editing))
                       (equal (plist-get info :draft) "未发送的草稿"))))
              t)
             (etaf-telega-gui--action
              "late-receipt-keeps-draft"
              (lambda (_)
                (setq etaf-telega-gui--hold t)
                (execute-kbd-macro (kbd "i"))
                (execute-kbd-macro (kbd "RET")))
              (lambda (_)
                (let ((info (etaf-telega-gui--info)))
                  (and (not (plist-get info :editing))
                       (equal (plist-get info :draft) "未发送的草稿")))))
             (etaf-telega-gui--action
              "confirm-clears-draft"
              (lambda (_)
                (setq etaf-telega-gui--hold nil)
                (etaf-telega-backend-fake-complete
                 etaf-telega-gui--backend "未发送的草稿"))
              (lambda (_) (equal (plist-get (etaf-telega-gui--info) :draft) "")) t)
             (etaf-telega-gui--action
              "next-chat" (lambda (_)
                            (execute-kbd-macro (kbd "q"))
                            (execute-kbd-macro (kbd "j")))
              (lambda (_)
                (and (eq (plist-get (etaf-telega-gui--info) :screen) 'root)
                     (equal (plist-get (etaf-telega-gui--info) :cursor) 2))))
             (etaf-telega-gui--action
              "previous-chat" (lambda (_) (execute-kbd-macro (kbd "k")))
              (lambda (_) (equal (plist-get (etaf-telega-gui--info) :cursor) 1)))
             (etaf-telega-gui--action
              "last-chat" (lambda (_) (execute-kbd-macro (kbd "M->")))
              (lambda (_) (equal (plist-get (etaf-telega-gui--info) :cursor) 3)))
             (etaf-telega-gui--action
              "first-chat" (lambda (_) (execute-kbd-macro (kbd "M-<")))
              (lambda (_) (equal (plist-get (etaf-telega-gui--info) :cursor) 1)))
             (etaf-telega-gui--action
              "reload-history" (lambda (_) (execute-kbd-macro (kbd "g")))
              (lambda (_) (= (plist-get (etaf-telega-gui--info) :messages) 1))))
            (mapcar
             (lambda (size)
               (etaf-telega-gui--action
                (format "layout-%dx%d" (car size) (cadr size))
                (lambda (_) (set-frame-size frame (car size) (cadr size)))
                (lambda (_) (and (= (frame-width frame) (car size))
                                 (= (frame-height frame) (cadr size)))) t))
             '((120 40) (80 30) (140 44))))
           :completion (lambda (_) (etaf-telega-gui--idle nil)))))
    (unwind-protect
        (etaf-gui-verifier-run scenario directory)
      (when (buffer-live-p etaf-telega-gui--buffer)
        (with-current-buffer etaf-telega-gui--buffer
          (let ((text (buffer-substring-no-properties (point-min) (point-max)))
                (info (etaf-telega-info)))
            (with-temp-file (expand-file-name "client.txt" directory)
              (insert (format "%S\n\n%s" info text))))
          (etaf-telega-close))))))
(provide 'etaf-telega-gui-scenarios)
;;; etaf-telega-gui-scenarios.el ends here
