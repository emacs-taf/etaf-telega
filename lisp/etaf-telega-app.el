;;; etaf-telega-app.el --- Application shell of etaf-telega -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The application layer owns the buffer, the command map and the backend
;; lifecycle.  A paint is a full projection of the model; while the composer
;; owns a native editing session, paints stay pending so the session keeps
;; the buffer text, exactly like every other ETAF application.

;;; Code:

(require 'cl-lib)
(require 'etaf)
(require 'etaf-ui)
(require 'etaf-telega-model)
(require 'etaf-telega-backend)
(require 'etaf-telega-views)

(defcustom etaf-telega-buffer-name "*ETAF Telegram*"
  "Buffer name of the mounted client.
Do not start it with \"*Telegram\": `telega-buffer-p' claims that prefix and
Telega's root-buffer cleanup kills every matching buffer."
  :type 'string
  :group 'etaf-telega)

(defvar etaf-telega-backend-create-function
  #'etaf-telega-backend-telega-create
  "Return a fresh backend for `etaf-telega-open'.
Tests bind this to a fake backend factory so no network is touched.")

(defvar-local etaf-telega--model nil
  "Model shown by the current buffer.")

(defvar-local etaf-telega--backend nil
  "Backend feeding the current buffer.")

(defvar-local etaf-telega--paint-pending nil
  "Non-nil when a paint was requested while the composer was editing.")

(defun etaf-telega--window ()
  "Return the window showing the current buffer, if any."
  (get-buffer-window (current-buffer) t))

(defun etaf-telega--width ()
  "Return the message width for the current window."
  (max 20 (- (or (and (etaf-telega--window) (window-body-width)) 80)
             etaf-telega-sidebar-width 6)))

(defun etaf-telega--paint ()
  "Reproject the model into the mounted runtime.
A native editing session owns the buffer text, so its paint waits until the
session ends."
  (when (and etaf-telega--model (etaf-telega--window))
    (let ((runtime (etaf-runtime-for-buffer (current-buffer))))
      (when runtime
        (if (etaf-text-input-active-p runtime)
            (setq etaf-telega--paint-pending t)
          (setq etaf-telega--paint-pending nil)
          (etaf-mount (current-buffer)
                      (etaf-telega-view etaf-telega--model (etaf-telega--width))
                      (list :viewport-width (window-body-width (etaf-telega--window) t)
                            :viewport-height (window-body-height (etaf-telega--window))))
          (setq-local cursor-type nil))))))

(defun etaf-telega--callbacks (buffer)
  "Return backend callbacks that update BUFFER's model."
  (cl-labels ((report (update)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (funcall update)
                    (etaf-telega--paint)))))
    (list :status (lambda (status)
                    (report (lambda ()
                              (etaf-telega-model-set-status etaf-telega--model status))))
          :me (lambda (me)
                (report (lambda () (etaf-telega-model-set-me etaf-telega--model me))))
          :chats (lambda (chats)
                   (report (lambda ()
                             (etaf-telega-model-set-chats etaf-telega--model chats))))
          :messages (lambda (messages)
                      (report (lambda ()
                                (etaf-telega-model-set-messages
                                 etaf-telega--model (reverse messages)))))
          :sent (lambda (message)
                  (report (lambda ()
                            (let ((text (plist-get message :text)))
                              (when (equal (etaf-telega-model-draft etaf-telega--model) text)
                                (etaf-telega-model-set-draft etaf-telega--model "")))
                            (etaf-telega-model-append-message etaf-telega--model message))))
          :message (lambda (message)
                     (report (lambda ()
                               (etaf-telega-model-append-message
                                etaf-telega--model message))))
          :error (lambda (message)
                   (report (lambda ()
                             (etaf-telega-model-set-status etaf-telega--model
                                                           (format "错误：%s" message))))))))

(defun etaf-telega-open-chat (chat)
  "Select CHAT and load its recent history."
  (interactive)
  (let ((model (or etaf-telega--model (user-error "当前不是 Telegram 客户端")))
        (id (plist-get chat :id)))
    (etaf-telega-model-select model chat)
    (etaf-telega--paint)
    (etaf-telega-backend-select etaf-telega--backend id)))

(defun etaf-telega-select-chat (id)
  "Select the chat with ID and load its history."
  (interactive "nChat id: ")
  (let ((chat (and etaf-telega--model (etaf-telega-model-chat-by-id
                                       etaf-telega--model id))))
    (unless chat (user-error "没有这个对话：%s" id))
    (etaf-telega-open-chat chat)))

(defun etaf-telega-info (&optional buffer)
  "Return visible client facts for BUFFER without network I/O."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((model etaf-telega--model)
           (runtime (etaf-runtime-for-buffer (current-buffer)))
           (chat (and model (etaf-telega-model-chat model))))
      (list :status (and model (etaf-telega-model-status model))
            :chats (length (and model (etaf-telega-model-chats model)))
            :chat (plist-get chat :id)
            :messages (length (and model (etaf-telega-model-messages model)))
            :draft (and model (etaf-telega-model-draft model))
            :editing (and runtime (and (etaf-text-input-active-p runtime) t))))))

(defun etaf-telega--select-relative (delta)
  "Move the chat selection by DELTA entries."
  (let* ((model (or etaf-telega--model (user-error "当前不是 Telegram 客户端")))
         (chats (etaf-telega-model-chats model))
         (current (etaf-telega-model-chat model))
         (index (cl-position current chats :test (lambda (a b)
                                                   (equal (plist-get a :id)
                                                          (plist-get b :id)))))
         (next (nth (max 0 (min (1- (length chats))
                                (+ (or index -1) delta)))
                    chats)))
    (when next (etaf-telega-open-chat next))))

(defun etaf-telega-next-chat ()
  "Select the next chat in the list."
  (interactive)
  (etaf-telega--select-relative 1))

(defun etaf-telega-previous-chat ()
  "Select the previous chat in the list."
  (interactive)
  (etaf-telega--select-relative -1))

(defun etaf-telega-first-chat ()
  "Select the first conversation in the chat list."
  (interactive)
  (let* ((model (or etaf-telega--model (user-error "当前不是 Telegram 客户端")))
         (chats (etaf-telega-model-chats model)))
    (when chats (etaf-telega-open-chat (car chats)))))

(defun etaf-telega-last-chat ()
  "Select the last conversation in the chat list."
  (interactive)
  (let* ((model (or etaf-telega--model (user-error "当前不是 Telegram 客户端")))
         (chats (etaf-telega-model-chats model)))
    (when chats (etaf-telega-open-chat (car (last chats))))))

(defun etaf-telega-reload-chat ()
  "Reload the selected chat's history."
  (interactive)
  (let ((chat (etaf-telega-model-chat etaf-telega--model)))
    (when chat (etaf-telega-open-chat chat))))

(defun etaf-telega-edit ()
  "Focus the composer for typing."
  (interactive)
  (let ((runtime (etaf-runtime-for-buffer (current-buffer))))
    (when runtime (etaf-text-input-begin runtime 'telega-composer))))

(defun etaf-telega-send (text)
  "Send TEXT to the selected chat.
The draft stays until the backend accepts the message, so a failure keeps the
text available for retry."
  (interactive)
  (unless (etaf-telega-model-chat etaf-telega--model)
    (user-error "先选择一个对话"))
  (unless (string-empty-p (string-trim text))
    (etaf-telega-backend-send etaf-telega--backend text))
  (when etaf-telega--paint-pending (etaf-telega--paint)))

(defun etaf-telega-keep-draft (text)
  "Keep TEXT as the draft after leaving the composer."
  (when etaf-telega--model
    (etaf-telega-model-set-draft etaf-telega--model text))
  (when etaf-telega--paint-pending (etaf-telega--paint)))

(defvar etaf-telega-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (pair '(("j" . etaf-telega-next-chat) ("k" . etaf-telega-previous-chat)
                    ("n" . etaf-telega-next-chat) ("p" . etaf-telega-previous-chat)
                    ("C-n" . etaf-telega-next-chat) ("C-p" . etaf-telega-previous-chat)
                    ("M-<" . etaf-telega-first-chat) ("M->" . etaf-telega-last-chat)
                    ("RET" . etaf-telega-reload-chat)
                    ("i" . etaf-telega-edit) ("e" . etaf-telega-edit)
                    ("g" . etaf-telega-reload-chat)
                    ("q" . etaf-telega-close)))
      (define-key map (kbd (car pair)) (cdr pair)))
    map)
  "Keys the client owns outside the composer.")

(defvar etaf-telega--command-maps (list (cons 'etaf-telega--model etaf-telega-mode-map)))

;;;###autoload
(defun etaf-telega-open ()
  "Mount the application in its own buffer and return that buffer."
  (interactive)
  (let ((buffer (get-buffer-create etaf-telega-buffer-name)))
    (with-current-buffer buffer
      (setq-local etaf-telega--model (etaf-telega-model)
                  etaf-telega--backend (funcall etaf-telega-backend-create-function)
                  etaf-telega--paint-pending nil)
      (use-local-map etaf-telega-mode-map)
      (setq-local emulation-mode-map-alists
                  (cons 'etaf-telega--command-maps emulation-mode-map-alists)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (etaf-mount buffer (etaf-telega-view etaf-telega--model (etaf-telega--width))
                  (list :viewport-width (window-body-width (etaf-telega--window) t)
                        :viewport-height (window-body-height (etaf-telega--window))))
      (setq-local cursor-type nil)
      (add-hook 'kill-buffer-hook #'etaf-telega--dispose nil t)
      (etaf-telega-backend-start etaf-telega--backend (etaf-telega--callbacks buffer))
      (etaf-runtime-flush (etaf-runtime-for-buffer buffer)))
    buffer))

(defun etaf-telega--dispose ()
  "Release the backend and runtime owned by this buffer."
  (when etaf-telega--backend
    (etaf-telega-backend-stop etaf-telega--backend)
    (setq etaf-telega--backend nil))
  (when-let* ((runtime (etaf-runtime-for-buffer (current-buffer))))
    (etaf-unmount runtime)))

;;;###autoload
(defun etaf-telega-close ()
  "Unmount the application buffer and kill it.
The backend stops observing without closing Telega's server or account."
  (interactive)
  (let ((buffer (get-buffer etaf-telega-buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer (etaf-telega--dispose))
      (kill-buffer buffer))))

(provide 'etaf-telega-app)
;;; etaf-telega-app.el ends here
