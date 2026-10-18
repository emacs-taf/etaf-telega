;;; etaf-telega-theme.el --- Palettes of etaf-telega -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Two maintained palettes behind one accessor.  `etaf-telega-color' reads the
;; palette selected by `etaf-telega-theme', so a theme switch can never leak a
;; half-updated set of colors and every view follows it automatically.

;;; Code:

(require 'color)
(require 'cl-lib)

(defcustom etaf-telega-theme 'auto
  "Palette used by the client.
`auto' follows the frame's background mode; `dark' and `light' pin one of
the maintained palettes, and `native' derives every color from the current
Emacs theme's faces without painting any background of its own.
`etaf-telega-toggle-theme' cycles the three while the client is open."
  :type '(choice (const :tag "Follow the frame" auto)
                 (const :tag "Dark" dark)
                 (const :tag "Light" light)
                 (const :tag "Current Emacs theme" native))
  :group 'etaf-telega)

(defconst etaf-telega-themes
  '((dark
     (:bg . "#191a1c")
     (:surface . "#191a1c")
     (:surface-2 . "#202225")
     (:canvas . "#17181a")
     (:ink . "#e9eef4")
     (:muted . "#8796a8")
     (:accent . "#5eacef")
     (:accent-soft . "#2b5278")
     (:border . "#303338")
     (:hairline . "#26282b")
     (:chat-bg . "#17181a")
     (:bubble-in . "#25272b")
     (:bubble-out . "#2b5278")
     (:selected . "#2b5278")
     (:danger . "#ef5b5b")
     (:ok . "#4fbf67")
     (:senders "#f48fb1" "#64b5f6" "#ce93d8" "#81c784"
               "#ffb74d" "#ef9a9a" "#4dd0e1" "#fff176"))
    (light
     (:bg . "#ffffff")
     (:surface . "#ffffff")
     (:surface-2 . "#f3f5f8")
     (:canvas . "#e9eef4")
     (:ink . "#111827")
     (:muted . "#6b7280")
     (:accent . "#2481cc")
     (:accent-soft . "#dbeafe")
     (:border . "#e3e8ee")
     (:hairline . "#eef2f6")
     (:chat-bg . "#e9eef4")
     (:bubble-in . "#ffffff")
     (:bubble-out . "#d9ecfb")
     (:selected . "#d9ecfb")
     (:danger . "#dc2626")
     (:ok . "#16a34a")
     (:senders "#d6336c" "#1d4ed8" "#7c3aed" "#15803d"
               "#b45309" "#b91c1c" "#0e7490" "#a16207")))
  "Maintained palettes: semantic colors plus sender accents per theme.
`:canvas' is the chat background color drawn by gaps and idle rules.")

(defun etaf-telega--theme-name ()
  "Return the effective palette name: `dark', `light' or `native'."
  (let ((choice etaf-telega-theme))
    (if (memq choice '(dark light native))
        choice
      (if (eq (frame-parameter nil 'background-mode) 'dark) 'dark 'light))))

(defun etaf-telega--blend (from to ratio)
  "Return the color RATIO of the way from color FROM to color TO."
  (let ((a (color-name-to-rgb from))
        (b (color-name-to-rgb to)))
    (if (and a b)
        (apply #'color-rgb-to-hex
               (append (cl-mapcar (lambda (x y) (+ x (* ratio (- y x)))) a b)
                       (list 2)))
      to)))

(defun etaf-telega--native-palette ()
  "Return a palette derived from the current faces.
The background keys are nil, so the frame's own background shows through;
tints are blends of the default background with its foreground or links."
  (let* ((dark (eq (frame-parameter nil 'background-mode) 'dark))
         (bg (or (face-background 'default nil t) (if dark "#000000" "#ffffff")))
         (fg (or (face-foreground 'default nil t) (if dark "#ffffff" "#000000")))
         (accent (or (face-foreground 'link nil t) (if dark "#5eacef" "#2481cc"))))
    `((:bg) (:surface) (:chat-bg)
      (:canvas . ,bg)
      (:surface-2 . ,(etaf-telega--blend bg fg 0.06))
      (:ink . ,fg)
      (:muted . ,(or (face-foreground 'shadow nil t) (etaf-telega--blend bg fg 0.55)))
      (:accent . ,accent)
      (:accent-soft . ,(etaf-telega--blend bg accent 0.22))
      (:border . ,(etaf-telega--blend bg fg 0.16))
      (:hairline . ,(etaf-telega--blend bg fg 0.07))
      (:bubble-in . ,(etaf-telega--blend bg fg 0.08))
      (:bubble-out . ,(etaf-telega--blend bg accent 0.22))
      (:selected . ,(etaf-telega--blend bg accent 0.2))
      (:danger . ,(or (face-foreground 'error nil t) "#ef5b5b"))
      (:ok . ,(or (face-foreground 'success nil t) "#4fbf67"))
      (:senders . ,(cdr (assq :senders (alist-get (if dark 'dark 'light)
                                                  etaf-telega-themes)))))))

(defun etaf-telega--palette ()
  "Return the effective palette."
  (let ((name (etaf-telega--theme-name)))
    (if (eq name 'native)
        (etaf-telega--native-palette)
      (or (alist-get name etaf-telega-themes)
          (alist-get 'dark etaf-telega-themes)))))

(defun etaf-telega-color (key)
  "Return the active palette's color for KEY.
The native palette returns nil for background keys, leaving them unpainted."
  (let ((entry (assq key (etaf-telega--palette))))
    (if entry
        (cdr entry)
      (if (eq (etaf-telega--theme-name) 'dark) "#e9eef4" "#111827"))))

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
  (let ((accent (etaf-telega-color :accent))
        (surface (or (etaf-telega-color :surface) (etaf-telega-color :canvas))))
    (list :ui-fg (etaf-telega-color :ink)
          :ui-bg (or (etaf-telega-color :bg) (etaf-telega-color :canvas))
          :ui-border (etaf-telega-color :border)
          :ui-muted-fg (etaf-telega-color :muted)
          :ui-panel-bg (etaf-telega-color :surface-2)
          :ui-panel-fg (etaf-telega-color :ink)
          :ui-panel-border (etaf-telega-color :border)
          :ui-divider-bg (etaf-telega-color :border)
          :ui-picker-item-bg (etaf-telega-color :surface-2)
          :ui-picker-item-fg (etaf-telega-color :ink)
          :ui-picker-item-muted-fg (etaf-telega-color :muted)
          :ui-search-bg (etaf-telega-color :surface-2)
          :ui-search-fg (etaf-telega-color :ink)
          :ui-disabled-fg (etaf-telega-color :muted)
          :ui-disabled-bg surface
          :ui-disabled-border (etaf-telega-color :border)
          :ui-accent accent
          :ui-accent-fg "#FFFFFF"
          :ui-accent-soft-bg (etaf-telega-color :accent-soft)
          :ui-button-primary-fg "#FFFFFF"
          :ui-button-primary-bg accent
          :ui-button-secondary-fg accent
          :ui-button-secondary-bg (etaf-telega-color :accent-soft)
          :ui-button-ghost-fg (etaf-telega-color :muted)
          :ui-button-ghost-bg surface
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
