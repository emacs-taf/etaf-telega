;;; etaf-telega.el --- Telegram client with an ETAF interface on Telega's TDLib backend -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: ETAF contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (etaf "0.2.1") (etaf-ui "0.1.0") (ebox "3.0.0"))
;; Keywords: convenience
;; URL: https://forgejo.vhkd.top/emacs-taf/etaf-telega

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
;; Variable: etaf-telega-split-threshold
;; Window body width in columns at which the client splits into the
;; conversation column and the message pane.  Below it one screen owns the
;; window.  The divider between the panes is draggable; a dragged width is
;; remembered per session and clamped so the message pane stays usable.
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
;; Function: etaf-telega-open-selected-chat ()
;; Open the conversation selected in the chat list.
;; Function: etaf-telega-return ()
;; Open the selected conversation, or reload the open one.
;; Function: etaf-telega-select-filter (key)
;; Show the conversations of filter KEY (main, groups, channels, archived).
;; Function: etaf-telega-describe ()
;; Show the selected conversation's profile screen.
;; Function: etaf-telega-actions ()
;; Show the selected message's action menu screen.
;; Function: etaf-telega-toggle-pin ()
;; Toggle the open conversation's pinned state.
;; Function: etaf-telega-toggle-mute ()
;; Toggle the open conversation's muted state.
;; Function: etaf-telega-back ()
;; Return from a profile or action screen to the conversation, or from the
;; conversation to the chat list.
;; Function: etaf-telega-quit ()
;; Leave the conversation screen, or close the client from the list.
;; Function: etaf-telega-next-chat ()
;; Select the next conversation in the chat list.
;; Function: etaf-telega-previous-chat ()
;; Select the previous conversation in the chat list.
;; Function: etaf-telega-first-chat ()
;; Select the first conversation in the chat list.
;; Function: etaf-telega-last-chat ()
;; Select the last conversation in the chat list.
;; Function: etaf-telega-load-more ()
;; Load an older page of the selected chat's history.
;; Function: etaf-telega-page-up (&optional rows)
;; Scroll the message viewport towards older messages; at the top of the
;; loaded history it loads an older page.
;; Function: etaf-telega-page-down (&optional rows)
;; Scroll the message viewport towards newer messages.
;; Function: etaf-telega-follow ()
;; Scroll the message viewport to the newest message.
;; Function: etaf-telega-scroll-chats (delta)
;; Scroll the conversation column by DELTA rows (positive walks towards later
;; conversations) without moving the selected chat; the wheel uses it.
;; Function: etaf-telega-scroll-messages (delta)
;; Scroll the message viewport by DELTA rows; scrolling past the top of the
;; loaded history loads an older page.
;; Function: etaf-telega-wheel-up (event)
;; Scroll the pane under EVENT towards older content (sidebar or messages).
;; Function: etaf-telega-wheel-down (event)
;; Scroll the pane under EVENT towards newer content (sidebar or messages).
;; Function: etaf-telega-reload-chat ()
;; Reload the selected conversation's recent history.
;; Function: etaf-telega-compose ()
;; Focus the in-layout composer and begin native editing.
;; Function: etaf-telega-next-message ()
;; Select the next message in the conversation (Telega's n).
;; Function: etaf-telega-previous-message ()
;; Select the previous message in the conversation (Telega's p).
;; Function: etaf-telega-reply ()
;; Compose a reply to the selected message; the next send quotes it.
;; Function: etaf-telega-edit ()
;; Edit the selected outgoing message in the composer.
;; Function: etaf-telega-cancel-compose ()
;; Cancel the active reply or edit; an edit restores its previous draft.
;; Function: etaf-telega-copy ()
;; Copy the selected message's text to the kill ring.
;; Function: etaf-telega-delete ()
;; Delete the selected message after confirmation.
;; Function: etaf-telega-send (text)
;; Send TEXT to the selected chat; a reply carries its quote and an edit
;; replaces the selected message.  The draft stays until the backend accepts
;; the change.
;; Function: etaf-telega-info (&optional buffer)
;; Return visible client facts (status, chat count, selection, message count,
;; viewport offset, draft, editing session) without network I/O.
;; Function: etaf-telega-toggle-theme ()
;; Switch the open client between the dark and light palettes and repaint.
;; Function: etaf-telega-color (key)
;; Return the active palette's color for KEY; every view reads its colors
;; through this function so a theme switch stays consistent.
;; Function: etaf-telega-sender-color (name)
;; Return the active palette's stable accent color for sender NAME.
;; Variable: etaf-telega-theme
;; Palette selection: `auto' follows the frame's background mode, `dark' and
;; `light' pin a maintained palette.
;; Variable: etaf-telega-backend-create-function
;; Return a fresh backend for `etaf-telega-open'.  Bind it to a fake backend
;; factory for tests and offline demonstrations.
;; Function: etaf-telega-backend-start (backend callbacks)
;; Connect BACKEND and report status, account and chats through CALLBACKS.
;; Function: etaf-telega-backend-stop (backend)
;; Stop observing the account without closing Telega's server or data.
;; Function: etaf-telega-backend-select (backend chat-id)
;; Open the chat with CHAT-ID and load its newest history page.
;; Function: etaf-telega-backend-history (backend &key before limit)
;; Load messages older than BEFORE, the oldest loaded message id.
;; Function: etaf-telega-backend-send (backend text &optional reply-to)
;; Send TEXT, replying to the message REPLY-TO when non-nil.
;; Function: etaf-telega-backend-edit (backend message text)
;; Replace the text of MESSAGE.
;; Function: etaf-telega-backend-delete (backend message)
;; Delete MESSAGE.
;; Function: etaf-telega-backend-read (backend message-ids)
;; Accept MESSAGE-IDS in the selected chat as read.
;; Function: etaf-telega-backend-chat-action (backend chat-id action &optional value)
;; Apply ACTION to CHAT-ID: read/unread set the state, pin/mute/archive toggle.
;; Function: etaf-telega-backend-describe (backend chat-id)
;; Report the chat's profile (title, username, kind, members, description,
;; pinned, muted, unread) through the `:describe' callback.
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
;; Function: etaf-telega-backend-fake-message (backend id)
;; Return BACKEND's stored projection of the message with ID, or nil.
;; Function: etaf-telega-backend-fake-replies (backend)
;; Return the (TEXT . MESSAGE-ID) replies BACKEND recorded for its sends.
;; Function: etaf-telega-backend-fake-chats (backend)
;; Return BACKEND's stored chat projections.
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
