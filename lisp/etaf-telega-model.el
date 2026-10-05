;;; etaf-telega-model.el --- State of etaf-telega -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The model owns plain data and reactive sources.  It never touches a buffer,
;; the network or storage; views read the refs and commands mutate them.
;; Chats and messages are plain plists so a backend can replace a record
;; without depending on any mutable object identity.
;;
;; Besides status, chats, history and the draft, the model holds what Telega
;; keeps in point and its input prompt: the selected message and the composer's
;; compose state, which is either a reply to, or an edit of, one message.

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
        :selected (etaf-ref nil :name 'etaf-telega-selected)
        :compose (etaf-ref nil :name 'etaf-telega-compose)
        :scroll (etaf-ref 0 :name 'etaf-telega-scroll)
        :chat-scroll (etaf-ref 0 :name 'etaf-telega-chat-scroll)
        :cursor (etaf-ref nil :name 'etaf-telega-cursor)
        :screen (etaf-ref 'root :name 'etaf-telega-screen)
        :describe (etaf-ref nil :name 'etaf-telega-describe)
        :filter (etaf-ref 'main :name 'etaf-telega-filter)
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

(defun etaf-telega-model-selected-id (model)
  "Return MODEL's selected message id, or nil."
  (etaf-value (plist-get model :selected)))

(defun etaf-telega-model-compose (model)
  "Return MODEL's compose state, a plist with `:kind' and `:target'."
  (etaf-value (plist-get model :compose)))

(defun etaf-telega-model-scroll (model)
  "Return MODEL's message viewport offset, in display rows above the newest."
  (or (etaf-value (plist-get model :scroll)) 0))

(defun etaf-telega-model-set-scroll (model offset)
  "Set MODEL's viewport OFFSET, in display rows above the newest."
  (setf (etaf-value (plist-get model :scroll)) (max 0 (or offset 0))))

(defun etaf-telega-model-chat-scroll (model)
  "Return MODEL's chat list offset, the first visible chat index."
  (or (etaf-value (plist-get model :chat-scroll)) 0))

(defun etaf-telega-model-set-chat-scroll (model offset)
  "Set MODEL's chat list OFFSET, the first visible chat index."
  (setf (etaf-value (plist-get model :chat-scroll)) (max 0 (or offset 0))))

(defun etaf-telega-model-cursor (model)
  "Return MODEL's selected conversation id in the chat list."
  (etaf-value (plist-get model :cursor)))

(defun etaf-telega-model-set-cursor (model id)
  "Select the conversation with ID in MODEL's chat list."
  (setf (etaf-value (plist-get model :cursor)) id))

(defun etaf-telega-model-screen (model)
  "Return MODEL's current screen, `root' or `chat'."
  (or (etaf-value (plist-get model :screen)) 'root))

(defun etaf-telega-model-set-screen (model screen)
  "Switch MODEL to SCREEN, `root' or `chat'."
  (setf (etaf-value (plist-get model :screen)) screen))

(defun etaf-telega-model-describe (model)
  "Return MODEL's chat profile, or nil."
  (etaf-value (plist-get model :describe)))

(defun etaf-telega-model-set-describe (model describe)
  "Set MODEL's chat profile to DESCRIBE."
  (setf (etaf-value (plist-get model :describe)) describe))

(defun etaf-telega-model-filter (model)
  "Return MODEL's active chat filter key."
  (or (etaf-value (plist-get model :filter)) 'main))

(defun etaf-telega-model-set-filter (model key)
  "Set MODEL's active chat filter to KEY."
  (setf (etaf-value (plist-get model :filter)) key))

(defun etaf-telega-model-message-by-id (model id)
  "Return MODEL's message with ID, or nil."
  (cl-find id (etaf-telega-model-messages model)
           :key (lambda (message) (plist-get message :id)) :test #'equal))

(defun etaf-telega-model-selected-message (model)
  "Return MODEL's selected message, or nil."
  (let ((id (etaf-telega-model-selected-id model)))
    (and id (etaf-telega-model-message-by-id model id))))

(defun etaf-telega-model-set-status (model status)
  "Set MODEL's STATUS."
  (setf (etaf-value (plist-get model :status)) status))

(defun etaf-telega-model-set-me (model me)
  "Set MODEL's account projection ME."
  (setf (etaf-value (plist-get model :me)) me))

(defun etaf-telega-model-set-chats (model chats)
  "Replace MODEL's chats with CHATS.
The list cursor keeps its conversation when it is still present and falls
back to the newest one."
  (let ((current (etaf-telega-model-cursor model)))
    (setf (etaf-value (plist-get model :chats)) chats
          (etaf-value (plist-get model :cursor))
          (if (and current (cl-find current chats
                                    :key (lambda (chat) (plist-get chat :id))
                                    :test #'equal))
              current
            (and chats (plist-get (car chats) :id))))))

(defun etaf-telega-model-select (model chat)
  "Select CHAT in MODEL.
The loaded history, the message selection, the viewport and any compose
state clear until the backend reports the chat's messages."
  (setf (etaf-value (plist-get model :chat)) chat
        (etaf-value (plist-get model :messages)) nil
        (etaf-value (plist-get model :selected)) nil
        (etaf-value (plist-get model :scroll)) 0
        (etaf-value (plist-get model :compose)) nil))

(defun etaf-telega-model-set-messages (model messages)
  "Replace MODEL's messages with MESSAGES and follow the newest one."
  (setf (etaf-value (plist-get model :messages)) messages
        (etaf-value (plist-get model :selected))
        (and messages (plist-get (car (last messages)) :id))
        (etaf-value (plist-get model :scroll)) 0))

(defun etaf-telega-model-set-selected (model id)
  "Select the message with ID in MODEL."
  (setf (etaf-value (plist-get model :selected)) id))

(defun etaf-telega-model-move-selection (model delta)
  "Move MODEL's message selection by DELTA messages."
  (let* ((messages (etaf-telega-model-messages model))
         (current (etaf-telega-model-selected-id model))
         (index (cl-position current messages
                             :key (lambda (message) (plist-get message :id))
                             :test #'equal))
         (index (or index (max 0 (1- (length messages)))))
         (next (max 0 (min (1- (length messages)) (+ index delta))))
         (message (nth next messages)))
    (when message (etaf-telega-model-set-selected model (plist-get message :id)))))

(defun etaf-telega-model-set-compose (model compose)
  "Set MODEL's compose state to COMPOSE, a `(:kind :target)' plist or nil."
  (setf (etaf-value (plist-get model :compose)) compose))

(defun etaf-telega-model-prepend-messages (model messages)
  "Prepend older MESSAGES, in reading order, to MODEL's history."
  (when messages
    (setf (etaf-value (plist-get model :messages))
          (append messages (etaf-telega-model-messages model)))))

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

(defun etaf-telega-model-replace-message (model message)
  "Replace MODEL's message with MESSAGE's id."
  (when (plist-get message :id)
    (setf (etaf-value (plist-get model :messages))
          (cl-mapcar (lambda (existing)
                       (if (equal (plist-get existing :id) (plist-get message :id))
                           message existing))
                     (etaf-telega-model-messages model)))))

(defun etaf-telega-model-remove-message (model id)
  "Remove the message with ID from MODEL.
The selection moves to the nearest remaining message."
  (let* ((messages (etaf-telega-model-messages model))
         (index (cl-position id messages :key (lambda (item) (plist-get item :id))
                             :test #'equal))
         (rest (cl-remove id messages :key (lambda (item) (plist-get item :id))
                          :test #'equal)))
    (setf (etaf-value (plist-get model :messages)) rest)
    (when (equal (etaf-telega-model-selected-id model) id)
      (setf (etaf-value (plist-get model :selected))
            (and rest (plist-get (nth (min (max 0 (1- (or index 0)))
                                          (1- (length rest)))
                                      rest)
                                 :id))))))

(defun etaf-telega-model-set-draft (model text)
  "Set MODEL's composer draft to TEXT."
  (setf (etaf-value (plist-get model :draft)) text))

(defun etaf-telega-model-chat-by-id (model id)
  "Return MODEL's chat projection with ID, or nil."
  (cl-find id (etaf-telega-model-chats model)
           :key (lambda (chat) (plist-get chat :id)) :test #'equal))

(provide 'etaf-telega-model)
;;; etaf-telega-model.el ends here
