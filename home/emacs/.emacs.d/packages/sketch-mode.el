;;; derived-mode-ex.el --- example of a CC Mode derived mode for a new language

;; Author:     Tikhon Jelvis <tikhon@jelv.is>
;; Maintainer: Tikhon Jelvis <tikhon@jelv.is>
;; Created:    May 2013
;; Version:    0.1.0
;; Keywords:   sketch synthesizer

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a simple editing mode for the sketch frontend language.

;; Right now, it just adds support for sketch-specific keywords and
;; constructs.

;; This code is heavily based on the example cc-derived mode provided
;; with cc-mode and on rust-mode.

;;; Code:

(require 'cc-mode)

(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  (c-add-language 'sketch-mode 'java-mode))

;; Sketch has the following primitive types and no booleans:
(c-lang-defconst c-primitive-type-kwds
  sketch (append '("bit" "int" "char" "double" "float" "void" "fun" "generator")
                 (delete "boolean"
                         (append (c-lang-const c-primitive-type-kwds)
                                 nil))))

(c-lang-defconst c-block-stmt-2-kwds
  sketch '("for" "if" "else" "while" "do" "minrepeat" "repeat" "implements"))

(c-lang-defconst c-constant-kwds
  sketch '("true" "false" "??" "null"))

(c-lang-defconst c-simple-stmt-kwds
  sketch '("assert" "new" "break" "continue" "return"))

(c-lang-defconst c-modifier-kwds
  sketch '("final harness"))

(c-lang-defconst c-type-modifier-kwds
  sketch '("ref"))

(c-lang-defconst c-ref-list-kwds
  sketch '("include" "package"))

(c-lang-defconst c-case-kwds
  sketch nil)
(c-lang-defconst c-label-kwds
  sketch nil)
(c-lang-defconst c-type-prefix-kwds
  sketch nil)
(c-lang-defconst c-type-list-kwds
  sketch nil)
(c-lang-defconst c-class-decl-kwds
  sketch '("struct"))
(c-lang-defconst c-bitfield-kwds
  sketch nil)
(c-lang-defconst c-nonsymbol-sexp-kwds
 rust nil)

(c-lang-defconst c-cpp-matchers
  sketch (cons
      '(eval . (list "^\\s *\\(pragma\\)\\>\\(.*\\)"
		     (list 1 c-preprocessor-face-name)
		     '(2 font-lock-string-face)))
      (c-lang-const c-cpp-matchers)))

(defcustom sketch-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in SKETCH mode.
Each list item should be a regexp matching a single identifier.")

(defconst sketch-font-lock-keywords-1 (c-lang-const c-matchers-1 sketch)
  "Minimal highlighting for SKETCH mode.")

(defconst sketch-font-lock-keywords-2 (c-lang-const c-matchers-2 sketch)
  "Fast normal highlighting for SKETCH mode.")

(defconst sketch-font-lock-keywords-3 (c-lang-const c-matchers-3 sketch)
  "Accurate normal highlighting for SKETCH mode.")

(defvar sketch-font-lock-keywords sketch-font-lock-keywords-3
  "Default expressions to highlight in SKETCH mode.")

(defvar sketch-mode-syntax-table nil
  "Syntax table used in sketch-mode buffers.")
(or sketch-mode-syntax-table
    (setq sketch-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table sketch))))

(defvar sketch-mode-abbrev-table nil
  "Abbreviation table used in sketch-mode buffers.")

(defvar sketch-mode-map (let ((map (c-make-inherited-keymap)))
		      ;; Add bindings which are only useful for sketch
		      map)
  "Keymap used in sketch-mode buffers.")

(easy-menu-define sketch-menu sketch-mode-map "SKETCH Mode Commands"
		  ;; Can use `sketch' as the language for `c-mode-menu'
		  ;; since its definition covers any language.  In
		  ;; this case the language is used to adapt to the
		  ;; nonexistence of a cpp pass and thus removing some
		  ;; irrelevant menu alternatives.
		  (cons "SKETCH" (c-lang-const c-mode-menu sketch)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sketch\\'" . sketch-mode))

;;;###autoload
(defun sketch-mode ()
  "Major mode for editing sketch frontend code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `sketch-mode-hook'.

Key bindings:
\\{sketch-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table sketch-mode-syntax-table)
  (setq major-mode 'sketch-mode
	mode-name "sketch"
	local-abbrev-table sketch-mode-abbrev-table
	abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars sketch-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'sketch-mode)
  (easy-menu-add sketch-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'sketch-mode-hook)
  (c-update-modeline))


(provide 'sketch-mode)

;;; derived-mode-ex.el ends here

