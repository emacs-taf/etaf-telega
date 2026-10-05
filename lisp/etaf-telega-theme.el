;;; etaf-telega-theme.el --- Palettes of etaf-telega -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Two maintained palettes behind one accessor.  `etaf-telega-color' reads the
;; palette selected by `etaf-telega-theme', so a theme switch can never leak a
;; half-updated set of colors and every view follows it automatically.

;;; Code:

(defcustom etaf-telega-theme 'auto
  "Palette used by the client.
`auto' follows the frame's background mode; `dark' and `light' pin one of
the maintained palettes.  `etaf-telega-toggle-theme' switches between the
two pinned palettes while the client is open."
  :type '(choice (const :tag "Follow the frame" auto)
                 (const :tag "Dark" dark)
                 (const :tag "Light" light))
  :group 'etaf-telega)

(defconst etaf-telega-themes
  '((dark
     (:bg . "#0d1117")
     (:surface . "#161b22")
     (:surface-2 . "#1c2430")
     (:ink . "#e6edf3")
     (:muted . "#9198a1")
     (:accent . "#4493f8")
     (:accent-soft . "#152c4d")
     (:border . "#30363d")
     (:hairline . "#1f242c")
     (:danger . "#f85149")
     (:ok . "#3fb950")
     (:senders "#f778ba" "#79c0ff" "#d2a8ff" "#7ee787"
               "#ffa657" "#ff7b72" "#a5d6ff" "#e3b341"))
    (light
     (:bg . "#ffffff")
     (:surface . "#ffffff")
     (:surface-2 . "#f3f4f6")
     (:ink . "#111827")
     (:muted . "#6b7280")
     (:accent . "#2563eb")
     (:accent-soft . "#dbeafe")
     (:border . "#e5e7eb")
     (:hairline . "#eef2f7")
     (:danger . "#dc2626")
     (:ok . "#16a34a")
     (:senders "#d6336c" "#1d4ed8" "#7c3aed" "#15803d"
               "#b45309" "#b91c1c" "#0e7490" "#a16207")))
  "Maintained palettes: semantic colors plus sender accents per theme.")

(defun etaf-telega--theme-name ()
  "Return the effective palette name."
  (let ((choice etaf-telega-theme))
    (if (memq choice '(dark light))
        choice
      (if (eq (frame-parameter nil 'background-mode) 'dark) 'dark 'light))))

(defun etaf-telega--palette ()
  "Return the effective palette."
  (or (alist-get (etaf-telega--theme-name) etaf-telega-themes)
      (alist-get 'dark etaf-telega-themes)))

(defun etaf-telega-color (key)
  "Return the active palette's color for KEY."
  (or (alist-get key (etaf-telega--palette))
      (if (eq (etaf-telega--theme-name) 'dark) "#e6edf3" "#111827")))

(defun etaf-telega-sender-color (name)
  "Return the active palette's stable sender color for NAME."
  (let* ((colors (or (alist-get :senders (etaf-telega--palette))
                     (alist-get :senders (alist-get 'dark etaf-telega-themes))))
         (hash 0))
    (dolist (char (string-to-list (or name "")))
      (setq hash (mod (+ (* hash 31) char) 1000003)))
    (nth (mod hash (length colors)) colors)))

(defun etaf-telega-theme-defaults ()
  "Return the ETAF UI token plist for the active palette.
The application passes the result to `etaf-theme-provide' before mounting,
otherwise etaf-ui Components (buttons, badges, inputs, ranges) keep their
own light defaults and the app looks half-themed.  Keep this list in step
with the components the app actually uses."
  (let ((accent (etaf-telega-color :accent)))
    (list :ui-fg (etaf-telega-color :ink)
          :ui-bg (etaf-telega-color :bg)
          :ui-border (etaf-telega-color :border)
          :ui-muted-fg (etaf-telega-color :muted)
          :ui-panel-bg (etaf-telega-color :surface-2)
          :ui-panel-fg (etaf-telega-color :ink)
          :ui-disabled-fg (etaf-telega-color :muted)
          :ui-disabled-bg (etaf-telega-color :surface)
          :ui-disabled-border (etaf-telega-color :border)
          :ui-accent accent
          :ui-accent-fg "#FFFFFF"
          :ui-accent-soft-bg (etaf-telega-color :accent-soft)
          :ui-button-primary-fg "#FFFFFF"
          :ui-button-primary-bg accent
          :ui-button-secondary-fg accent
          :ui-button-secondary-bg (etaf-telega-color :accent-soft)
          :ui-button-ghost-fg (etaf-telega-color :muted)
          :ui-button-ghost-bg (etaf-telega-color :surface)
          :ui-range-track (etaf-telega-color :border)
          :ui-range-fill accent
          :ui-success-fg (etaf-telega-color :ok)
          :ui-success-bg (etaf-telega-color :surface-2)
          :ui-success-border (etaf-telega-color :border)
          :ui-danger-fg (etaf-telega-color :muted)
          :ui-danger-bg (etaf-telega-color :surface-2)
          :ui-danger-border (etaf-telega-color :border)
          :ui-warning-fg (etaf-telega-color :muted)
          :ui-warning-bg (etaf-telega-color :surface-2)
          :ui-warning-border (etaf-telega-color :border)
          :ui-info-fg "#FFFFFF"
          :ui-info-bg accent
          :ui-info-border accent)))

(provide 'etaf-telega-theme)
;;; etaf-telega-theme.el ends here
