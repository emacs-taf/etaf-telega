;;; etaf-telega-model.el --- State of etaf-telega -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The model owns plain data and reactive sources.  It never touches a buffer,
;; the network or storage; views read the refs and commands mutate them.
;; Chats and messages are plain plists so a backend can replace a record
;; without depending on any mutable object identity.

;;; Code:

(require 'cl-lib)
(require 'etaf)

(defun etaf-telega-model ()
  "Create the application model."
  (list :status (etaf-ref "正在连接 Telegram…" :name 'etaf-telega-status)
        :me (etaf-ref nil :name 'etaf-telega-me)
        :chats (etaf-ref nil :name 'etaf-telega-chats)
        :chat (etaf-ref nil :name 'etaf-telega-chat)
        :messages (etaf-ref nil :name 'etaf-telega-messages)
        :draft (etaf-ref "" :name 'etaf-telega-draft)))

(defun etaf-telega-model-status (model)
  "Return MODEL's connection status."
  (etaf-value (plist-get model :status)))

(defun etaf-telega-model-me (model)
  "Return MODEL's own account projection."
  (etaf-value (plist-get model :me)))

(defun etaf-telega-model-chats (model)
  "Return MODEL's chat projections."
  (etaf-value (plist-get model :chats)))

(defun etaf-telega-model-chat (model)
  "Return MODEL's selected chat projection."
  (etaf-value (plist-get model :chat)))

(defun etaf-telega-model-messages (model)
  "Return MODEL's messages for the selected chat."
  (etaf-value (plist-get model :messages)))

(defun etaf-telega-model-draft (model)
  "Return MODEL's composer text."
  (etaf-value (plist-get model :draft)))

(defun etaf-telega-model-set-status (model status)
  "Set MODEL's STATUS."
  (setf (etaf-value (plist-get model :status)) status))

(defun etaf-telega-model-set-me (model me)
  "Set MODEL's account projection ME."
  (setf (etaf-value (plist-get model :me)) me))

(defun etaf-telega-model-set-chats (model chats)
  "Replace MODEL's chats with CHATS."
  (setf (etaf-value (plist-get model :chats)) chats))

(defun etaf-telega-model-select (model chat)
  "Select CHAT in MODEL and clear the message list until it loads."
  (setf (etaf-value (plist-get model :chat)) chat
        (etaf-value (plist-get model :messages)) nil))

(defun etaf-telega-model-set-messages (model messages)
  "Replace MODEL's messages with MESSAGES."
  (setf (etaf-value (plist-get model :messages)) messages))

(defun etaf-telega-model-append-message (model message)
  "Append MESSAGE to MODEL's message list.
A message whose non-nil id is already present replaces its earlier copy, so a
confirmation delivered twice still shows once."
  (let* ((id (plist-get message :id))
         (messages (etaf-telega-model-messages model)))
    (setf (etaf-value (plist-get model :messages))
          (if (and id (cl-find id messages
                               :key (lambda (item) (plist-get item :id))
                               :test #'equal))
              (cl-mapcar (lambda (existing)
                           (if (equal (plist-get existing :id) id)
                               message existing))
                         messages)
            (append messages (list message))))))

(defun etaf-telega-model-set-draft (model text)
  "Set MODEL's composer draft to TEXT."
  (setf (etaf-value (plist-get model :draft)) text))

(defun etaf-telega-model-chat-by-id (model id)
  "Return MODEL's chat projection with ID, or nil."
  (cl-find id (etaf-telega-model-chats model)
           :key (lambda (chat) (plist-get chat :id)) :test #'equal))

(provide 'etaf-telega-model)
;;; etaf-telega-model.el ends here
