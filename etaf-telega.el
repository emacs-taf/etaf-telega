;;; etaf-telega.el --- Telegram client with an ETAF interface on Telega's TDLib backend -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: ETAF contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (etaf "0.2.1") (etaf-ui "0.1.0") (ebox "3.0.0"))
;; Keywords: convenience
;; URL: https://forgejo.vhkd.top/geekinney/etaf-telega

;;; Commentary:

;; Public API
;;
;; Load this package with (require 'etaf-telega) after adding this repository
;; root and the ETAF, ETAF UI, Ebox and ECSS roots to `load-path'.  The client
;; renders chats and messages with ETAF and edits the composer in place; the
;; Telega package owns the account, the TDLib database and the server process.
;; Tests and offline use never need Telega: the backend is replaceable through
;; `etaf-telega-backend-create-function'.
;;
;; Only the interfaces listed below are supported for use by other packages.
;; All unlisted symbols and implementation features are internal, including
;; names without a double hyphen.  This Commentary is the authoritative export
;; list; implementations and precise docstrings remain beside their
;; definitions.
;;
;; Function: etaf-telega-open ()
;;;###autoload (autoload 'etaf-telega-open "etaf-telega" nil t)
;; Mount the client in its own buffer and return that buffer.  Starting uses
;; the configured backend; the Telega backend starts Telega's server without
;; displaying Telega's own buffers and never touches the saved account.
;; Function: etaf-telega-close ()
;;;###autoload (autoload 'etaf-telega-close "etaf-telega" nil t)
;; Stop observing the backend, release the runtime and kill the client buffer.
;; Function: etaf-telega-select-chat (id)
;; Select the chat ID and load its recent history.
;; Function: etaf-telega-next-chat ()
;; Select the next conversation in the chat list.
;; Function: etaf-telega-previous-chat ()
;; Select the previous conversation in the chat list.
;; Function: etaf-telega-first-chat ()
;; Select the first conversation in the chat list.
;; Function: etaf-telega-last-chat ()
;; Select the last conversation in the chat list.
;; Function: etaf-telega-reload-chat ()
;; Reload the selected conversation's recent history.
;; Function: etaf-telega-edit ()
;; Focus the in-layout composer and begin native editing.
;; Function: etaf-telega-send (text)
;; Send TEXT to the selected chat; the draft stays until the backend accepts it.
;; Function: etaf-telega-info (&optional buffer)
;; Return visible client facts (status, chat count, selection, message count,
;; draft, editing session) without network I/O.
;; Variable: etaf-telega-backend-create-function
;; Return a fresh backend for `etaf-telega-open'.  Bind it to a fake backend
;; factory for tests and offline demonstrations.
;; Function: etaf-telega-backend-fake-create (&key chats histories responder)
;; Return an offline backend seeded with CHATS and HISTORIES.  RESPONDER, when
;; non-nil, is called with (BACKEND TEXT) after each send so a caller can
;; acknowledge through `etaf-telega-backend-fake-complete'.
;; Function: etaf-telega-backend-fake-sent (backend)
;; Return, newest first, the texts BACKEND accepted but has not confirmed.
;; Function: etaf-telega-backend-fake-complete (backend text)
;; Confirm TEXT as accepted by BACKEND, exactly like a server acknowledgement.
;; Function: etaf-telega-backend-fake-deliver (backend message)
;; Report MESSAGE as newly arrived in BACKEND's selected chat.
;; Variable: etaf-telega-buffer-name
;; Buffer name of the mounted client.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp"
                                          (file-name-directory
                                           (or load-file-name buffer-file-name))))
(require 'etaf-telega-app)
(require 'etaf-telega-backend)

(provide 'etaf-telega)
;;; etaf-telega.el ends here
