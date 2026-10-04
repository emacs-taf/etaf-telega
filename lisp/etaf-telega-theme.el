;;; etaf-telega-theme.el --- Semantic palette of etaf-telega -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; One palette read through `etaf-telega-color', so a palette change can never
;; leak a half-updated set of colors.  Replace the values with your own and
;; keep every view reading colors through this function.

;;; Code:

(defconst etaf-telega-palette
  '((:bg . "#0d1117")
    (:surface . "#161b22")
    (:surface-2 . "#1c2430")
    (:ink . "#e6edf3")
    (:muted . "#9198a1")
    (:accent . "#4493f8")
    (:accent-soft . "#152c4d")
    (:border . "#30363d")
    (:ok . "#3fb950"))
  "Semantic palette of the application.")

(defconst etaf-telega-sender-colors
  '("#f778ba" "#79c0ff" "#d2a8ff" "#7ee787" "#ffa657" "#ff7b72" "#a5d6ff" "#e3b341")
  "Stable sender colors for incoming messages and avatars.")

(defun etaf-telega-sender-color (name)
  "Return the stable display color for sender NAME."
  (let ((hash 0))
    (dolist (char (string-to-list (or name "")))
      (setq hash (mod (+ (* hash 31) char) 1000003)))
    (nth (mod hash (length etaf-telega-sender-colors))
         etaf-telega-sender-colors)))

(defun etaf-telega-color (key)
  "Return the palette color for KEY."
  (or (alist-get key etaf-telega-palette)
      (if (eq (frame-parameter nil 'background-mode) 'dark) "#e6edf3" "#1f2328")))

(defun etaf-telega-theme-defaults ()
  "Return the ETAF UI token plist for this palette.
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
          :ui-warning-border (etaf-telega-color :border))))

(provide 'etaf-telega-theme)
;;; etaf-telega-theme.el ends here
