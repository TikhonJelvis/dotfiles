;;; array-forth-mode.el

;; Copyright (C) 2012 Tikhon Jelvis
;; Author: Tikhon Jelvis <tikhon@berkeley.edu>
;; Version: 0.0.1
;; Keywords: forth, colorforth, arrayforth 

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a mode for editing arrayForth files. This is the dialect
;; of forth used by GreenArray chips. Normal forth syntax is used
;; internally and exported to the colorForth format via some external
;; Python scripts. The syntax coloring reflects the colorForth style
;; colors arrayForth uses. 

;;; Code:

;; TODO: Add customization options!
(defgroup array-forth nil
  "Customization options for `array-forth-mode'.")

(defvar array-forth-mode-syntax-table (make-syntax-table)
  "Syntax table for `array-forth-mode'.")

(defcustom array-forth-trim-markers nil
  "Controls whether punctuation is hidden in favor of color."
  :group 'array-forth
  :type 'boolean)

(defface array-forth-red-face '((t (:foreground "red")))
  "The face corresponding to colorForth's red color which denotes
  word definitions."
  :group 'array-forth-faces)
(defface array-forth-magenta-face '((t (:foreground "magenta")))
  "The face corresponding to colorForth's red color which denotes
  something or other."
  :group 'array-forth-faces)
(defface array-forth-green-face '((t (:foreground "green")))
  "The face corresponding to colorForth's green color which
  denotes normal code, or something."
  :group 'array-forth-faces)
(defface array-forth-yellow-face '((t (:foreground "yellow")))
  "The face corresponding to colorForth's white color which
  denotes something like compile-time code."
  :group 'array-forth-faces)
(defface array-forth-white-face '((t (:foreground "white")))
  "The face corresponding to colorForth's white color which
  denotes comments."
  :group 'array-forth-faces)
(defface array-forth-blue-face '((t (:foreground "#77AAFF")))
  "the face corresponding to colorForth's blue color which
  denotes words that only affect displaying code."
  :group 'array-forth-faces)

(defvar array-forth-font-lock-keywords '()
  "The syntax highlighting for arrayForth.")

(defun trim (start end &optional group)
  "Hides the first `start' and last `end' characters of the last
  match. The optional argument `group' specifies which matched
  group to use. "
  (when array-forth-trim-markers
    (when start (compose-region (match-beginning group) (+ (match-beginning group) start) ""))
    (when end (compose-region (- (match-end group) end) (match-end group) ""))))

(defun process (start end face &optional group)
  "Trims the current match with `trim' and sets the specified face."
  (setq group (or group 0))
  (trim start end group)
  (put-text-property (match-beginning group) (match-end group) 'font-lock-face face)
  nil)

(defun darken-channel (channel)
  "Darkens a single RGB color channel."
  (if (> channel 20) (- (/ channel 256) 20) channel))

(defun darken (color)
  "Returns a darker version of the color if possible, or the same color otherwise."
  (let ((values (color-values color)))
    (apply 'format (cons "#%02X%02X%02X" (mapcar 'darken-channel values)))))

(defun add-face (faces)
  "Returns a list with the given faces and the last match's faces."
  (let ((last (get-text-property (match-beginning 0) 'font-lock-face)))
    (if (listp last) (append faces last) (append faces (list last)))))

(defun modify-face-for-hex (&optional group)
  "Modifies the face of the current match to display hexidecimal
  numbers by making it bold, italic and darker."
  (setq group (or group 0))
  (let* ((last (get-text-property (match-beginning group) 'font-lock-face))
         (foreground (darken (face-attribute last :foreground))))
    (add-face `(italic bold (:foreground ,foreground)))))

;; TODO: move all this to the defvar:
(setq array-forth-font-lock-keywords
      '((": [^ ]+\\>\\(.*\\)"  (1 (process nil nil 'array-forth-green-face 1)))
        (": [^ ]+\\>"          (0 (process 2   nil 'array-forth-red-face)))
        ("var [^ ]+\\>"        (0 (process 4   nil 'array-forth-magenta-face)))
        ("| [^ ]+\\>"          (0 (process 2   nil 'array-forth-blue-face)))
        ("( [^)]*)"            (0 (process 2   1   'array-forth-white-face)))
        ("\\\\[^\n]*$"         (0 (process 1   nil 'array-forth-white-face)))
        ("\\[ .* \\]"          (0 (process 2   2   'array-forth-yellow-face)))
        ("\\$[0-9]+"           (0 (process 1   nil (modify-face-for-hex))))))

;;;###autoload
(define-derived-mode array-forth-mode fundamental-mode "Array-Forth"
  "A major mode for editing arrayForth and colorForth files."
  :syntax-table array-forth-mode-syntax-table
  :group 'array-forth
  (set (make-local-variable 'font-lock-defaults) '(array-forth-font-lock-keywords)))

;; (define-key array-forth-mode-map (kbd ";") 'array-forth-electric-semicolon)

(provide 'array-forth-mode)
