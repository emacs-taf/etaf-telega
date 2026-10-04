;;; performance-scenarios.el --- Chat publication workloads -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Offline client workloads driven through the public application API with the
;; declared fake backend.  Measurements cover key/callback-to-redisplay only;
;; native input latency and visual appearance stay with their own gates.
;;; Code:
(require 'etaf-telega)
(declare-function taf-benchmark-register "performance-driver" (id &rest properties))

(defun etaf-telega-benchmark--backend (count)
  "Return an offline backend whose first chat holds COUNT messages."
  (let ((histories (make-hash-table :test #'equal)))
    (puthash 1 (cl-loop for n from count downto 1
                        collect (list :id n :sender "Alice" :time "10:00"
                                      :text (format "第 %d 条消息，检查消息列表的排版与滚动。" n)))
             histories)
    (puthash 2 nil histories)
    (etaf-telega-backend-fake-create
     :chats (list (list :id 1 :title "Alice" :unread 0)
                  (list :id 2 :title "工作群" :unread 0))
     :histories histories)))

(defun etaf-telega-benchmark--open (backend)
  "Open the client over BACKEND and select its populated chat."
  (let ((etaf-telega-backend-create-function (lambda () backend)))
    (let ((buffer (etaf-telega-open)))
      (with-current-buffer buffer (etaf-telega-select-chat 1))
      buffer)))

(defun etaf-telega-benchmark--cleanup (buffer)
  "Close the client in BUFFER when it is still alive."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer (etaf-telega-close))))

(taf-benchmark-register
 "telega.chat.append" :version 1 :modes '(batch gui) :budget-ms 50
 :impact 'local :isolation 'series
 :description "Append a live message to the selected conversation"
 :setup
 (lambda (_mode)
   (let* ((backend (etaf-telega-benchmark--backend 50))
          (buffer (etaf-telega-benchmark--open backend)))
     (list :buffer buffer
           :ready (lambda ()
                    (with-current-buffer buffer
                      (and (equal (plist-get (etaf-telega-info) :chat) 1)
                           (>= (plist-get (etaf-telega-info) :messages) 50))))
           :invoke (lambda (sample)
                     (with-current-buffer buffer
                       (etaf-telega-backend-fake-deliver
                        backend (list :id (+ 10000 sample) :sender "Alice"
                                      :time "10:01"
                                      :text (format "新增第 %d 条消息。" sample)))
                       (redisplay t)))
           :verify (lambda ()
                     (with-current-buffer buffer
                       (if (> (plist-get (etaf-telega-info) :messages) 50)
                           t "the live message was not appended")))
           :cleanup (lambda () (etaf-telega-benchmark--cleanup buffer))))))

(taf-benchmark-register
 "telega.chat.select" :version 1 :modes '(batch gui) :budget-ms 80
 :impact 'local :isolation 'series
 :description "Switch conversations and republish each loaded history"
 :setup
 (lambda (_mode)
   (let* ((backend (etaf-telega-benchmark--backend 60))
          (buffer (etaf-telega-benchmark--open backend)))
     (list :buffer buffer
           :ready (lambda ()
                    (with-current-buffer buffer
                      (member (plist-get (etaf-telega-info) :chat) '(1 2))))
           :invoke (lambda (sample)
                     (with-current-buffer buffer
                       (etaf-telega-select-chat (if (= (% sample 2) 0) 1 2))
                       (redisplay t)))
           :verify (lambda ()
                     (with-current-buffer buffer
                       (let ((info (etaf-telega-info)))
                         (if (and (member (plist-get info :chat) '(1 2))
                                  (= (plist-get info :messages)
                                     (if (= (plist-get info :chat) 1) 60 0)))
                             t "the selected history does not match its chat"))))
           :cleanup (lambda () (etaf-telega-benchmark--cleanup buffer))))))

(provide 'performance-scenarios)
;;; performance-scenarios.el ends here
