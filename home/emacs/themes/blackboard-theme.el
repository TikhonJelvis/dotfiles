;; Modified Blackboard Color Theme for Emacs.
;;
;; MIT License Copyright (c) 2008 JD Huntington <jdhuntington at gmail dot com>
;;                       (c) 2020 Tikhon Jelvis <tikhon@jelv.is>
;; Credits due to the excellent TextMate Blackboard theme
;;
;; All patches welcome

;; --------------
;; This porting makes blackboard no longer rely on color-theme package,
;; since Emacs has a native theme mechanism from Emacs 24.

;; How to use:
;; First, add a local directory to custome-theme-load-path,
;; (add-to-list 'custom-theme-load-path "~/home/$USER/drop/the/theme/to")
;; Then drop this theme into it,
;; M-x load-theme, then choose blackboard, it should work
;; Or, simple use (load-theme 'blackboard t) to enable the theme from start.

;;; blackboard-theme

;;; Code
(deftheme blackboard
  "Based on Color theme by JD Huntington, which based off the TextMate Blackboard theme, created 2008-11-27")

(require 'color)
(defun org-color (n)
   "Org mode color palette (based on
learnui.design/tools/data-color-picker.html + blackboard)."
   (nth n '("#6699ff"
            "#ad8dfa"
            "#e37ee5"
            "#ff70c4"
            "#ff6b9b"
            "#ff766f"
            "#ff8c41"
            "#ffa600"
            "#8da6ce"
            "#d8fa3c")))

(let*
    ((bg "#0C1021")
     (fg "#F8F8F8"))
  (custom-theme-set-faces
   `blackboard

   `(default ((t (:background ,bg  :foreground ,fg))))

   `(bold ((t (:bold t))))
   `(bold-italic ((t (:bold t))))

   `(border-glyph ((t (nil))))

   `(buffers-tab ((t (:background ,bg :foreground ,fg))))

   `(font-lock-builtin-face ((t (:foreground "#94bff3"))))
   `(font-lock-comment-face ((t (:italic t :foreground "#AEAEAE"))))
   `(font-lock-constant-face ((t (:foreground "#D8FA3C"))))
   `(font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
   `(font-lock-function-name-face ((t (:foreground "#FF6400"))))
   `(font-lock-keyword-face ((t (:foreground "#FBDE2D"))))
   `(font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
   `(font-lock-reference-face ((t (:foreground "SlateBlue"))))
   `(font-lock-string-face ((t (:foreground "#61CE3C"))))
   `(font-lock-type-face ((t (:foreground "#8DA6CE"))))
   `(font-lock-variable-name-face ((t (:foreground "#FF6400"))))
   `(font-lock-warning-face ((t (:bold t :foreground "pink"))))

   `(font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
   `(font-lock-regexp-grouping-construct ((t (:foreground "red"))))

   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 15)))))
   `(company-scrollbar-fg ((t (:background "DarkOrange"))))
   `(company-tooltip-selection ((t (:background ,(color-lighten-name bg 20)))))
   `(company-tooltip-common ((t (:inherit font-lock-builtin-face))))
   `(company-tooltip-annotation ((t (:inherit font-lock-builtin-face))))

   `(org-hide ((t (:foreground "#2e3436"))))

   `(org-level-1 ((t (:bold nil :foreground ,(org-color 0) :height 1.3))))
   `(org-level-2 ((t (:bold nil :foreground ,(org-color 1) :height 1.2))))
   `(org-level-3 ((t (:bold nil :foreground ,(org-color 5) :height 1.1))))
   `(org-level-4 ((t (:bold nil :foreground ,(org-color 7) :height 1.0))))
   `(org-level-5 ((t (:bold nil :foreground ,(org-color 8) :height 1.0))))
   `(org-level-6 ((t (:bold nil :foreground ,(org-color 9) :height 1.0))))

   `(org-date ((t (:underline t :foreground "magenta3"))))
   `(org-footnote  ((t (:underline t :foreground "magenta3"))))
   `(org-link ((t (:foreground "skyblue2" :background "#2e3436"))))
   `(org-special-keyword ((t (:foreground "brown"))))
   `(org-verbatim ((t (:foreground "#eeeeec" :underline t :slant italic))))
   `(org-block ((t (:foreground "#bbbbbc"))))
   `(org-quote ((t (:inherit org-block :slant italic))))
   `(org-verse ((t (:inherit org-block :slant italic))))
   `(org-todo ((t (:bold t :foreground ,(org-color 6)))))
   `(org-done ((t (:bold t :foreground "#61CE3C"))))
   `(org-warning ((t (:bold t :foreground ,(org-color 7)))))

   `(org-agenda-done ((t (:foreground ,(color-desaturate-name (org-color 8) 40)))))
   `(org-scheduled-today ((t (:foreground ,(color-lighten-name (org-color 1) 20)))))
   `(org-scheduled ((t (:foreground ,(org-color 8)))))
   `(org-agenda-date ((t (:foreground ,(org-color 1)))))
   `(org-agenda-date-weekend ((t (:weight normal :foreground ,(org-color 2)))))
   `(org-agenda-date-today ((t (:weight bold :foreground ,(org-color 7)))))
   `(org-agenda-structure ((t (:weight bold :foreground ,(org-color 0)))))
   `(org-agenda-structure-filter ((t (:inherit (org-warning org-agenda-structure)))))

   `(mode-line ((t (:background "DarkOrange" :foreground "black" :box nil))))
   `(mode-line-inactive ((t (:box nil))))
   `(powerline-inactive0 ((t (:background "gray30" :foreground "gray80"))))
   `(powerline-inactive1 ((t (:background "gray20" :foreground "gray80"))))
   `(powerline-inactive2 ((t (:background "gray30" :foreground "gray80"))))

   `(gui-element ((t (:background "#D4D0C8" :foreground "black"))))
   `(region ((t (:background "#253B76"))))
   `(highlight ((t (:background "#222222"))))
   `(highline-face ((t (:background "SeaGreen"))))
   `(italic ((t (nil))))
   `(left-margin ((t (nil))))
   `(text-cursor ((t (:background "yellow" :foreground "black"))))
   `(toolbar ((t (nil))))
   `(underline ((nil (:underline nil))))
   `(fringe ((t (:background nil))))

   ;; Language-specific tweaks
   `(scala-font-lock:var-face ((t (:inherit font-lock-variable-name-face :bold t))))))

(provide-theme 'blackboard)

;;; blackboard-theme.el ends here.
