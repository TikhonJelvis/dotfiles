;;; cfdg-mode.el --- major mode for Context-Free Art files

;; Copyright (C) Brent Sanders 2008
;;
;; Author: cfdg-mode@thoughtnoise.net
;; Keywords: languages
;; Version: 1.1

;;; Commentary:

;; Major mode for editing Context-Free Art gramar files, usually
;; ending with '.cfdg'.

;; This package provides the following features:
;;  * Syntax coloring (via font-lock)

;;; Installation:

;;  put this file into your load path and the following into your ~/.emacs:
;;    (autoload `cfdg-mode "cfdg-mode" nil t)
;;    (add-to-list 'auto-mode-alist '("\\.cfdg\\'" . cfdg-mode))

;;; Code:

(require 'generic)

(define-generic-mode cfdg-mode
  ;; comment-list
  (list "#" "//" '("/*" . "*/"))

  ;; keyword-list
  nil ;; none - handled below

  ;; font lock list
  (list
   '("\\(rule\\|include\\|startshape\\|background\\)[ \011]" 1 'font-lock-keyword-face)
   '("include[ \011]+\\([a-zA-Z0-9\-_\.]+\\)" 1 'font-lock-type-face)
   '("\\(CIRCLE\\|SQUARE\\|TRIANGLE\\)" 1 'font-lock-builtin-face)
   '("\\(rule\\|startshape\\)[ \011]+\\([a-zA-Z0-9_]+\\)" 2 'font-lock-function-name-face)
   '("\\(rule\\|startshape\\)[ \011]+\\([a-zA-Z0-9_]+\\)[ \011]+\\([0-9\.]+\\)" 3 'font-lock-constant-face)
   '("\\([a-zA-Z0-9_]+\\)[ \011]*\\({\\|\\[\\)" 1 'font-lock-function-name-face)
   '("\\(sat\\|size\\|rotate\\|flip\\|skew\\|hue\\|saturation\\|brightness\\|alpha\\|x\\|y\\|z\\|s\\|r\\|f\\|h\\|b\\|a\\)[ \011]+\\([-]*[0-9]+\\)" 1 'font-lock-variable-name-face)
   '("\\([0-9]+[ \011]*\\*\\)[ \011]*{" 1 'font-lock-constant-face)
   )

  ;; auto-mode-list
  (list "\\.cfdg\'")

  ;; function list
  ()

  "Major mode for editing Context-Free Art files.")

(provide 'cfdg-mode)
;;; cfdg-mode.el ends here
