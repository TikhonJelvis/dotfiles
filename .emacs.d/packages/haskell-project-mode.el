;;; haskell-project-mode.el

;; Copyright (C) 2012 Tikhon Jelvis
;; Author: Tikhon Jelvis <tikhon@berkeley.edu>
;; Version: 0.1.0
;; Keywords: haskell, cabal, cabal-dev

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

;; This is a mode for working with Haskell projects. I loosely define
;; "Haskell project" as any directory containing a .cabal file and
;; some Haskell sources. 
;; 
;; Right now, this sets the M-x compile command to use cabal and, if
;; you're using cabal-dev or cabal sandboxes, creates a custom
;; inferior haskell buffer using cabal-dev ghci or cabal repl as
;; appropriate.
;;
;; If you're using cabal sandboxes, this also creates yet another
;; inferior haskell buffer for your test code. It assumes you named
;; your test suite "test" and calles `cabal repl test'. I'm also going
;; to add a command for quickly jumping between your source code and
;; its corresponding test file.

;;; Code:

(defcustom haskell-test-directory "test"
  "The default directory haskell-project-mode looks for
  tests. Currently, this always assumes your test suite is named
  'test' in your .cabal file.")

(define-minor-mode haskell-project-mode
  "A mode for working with Haskell projects."
  nil
  " λP"
  nil
  (require 'haskell-cabal)
  (let ((project-dir (haskell-cabal-find-dir)))
    (when project-dir
      (message (concat "cabal directory: " project-dir))
      (let* ((cabal-dev? (member "cabal-dev" (directory-files project-dir)))
             (cabal-sandbox? (member "cabal.sandbox.config" (directory-files project-dir)))
             (cabal-sandbox-test? (and cabal-sandbox? (string-match (format "\\<%s\\>" haskell-test-directory) (buffer-file-name))))
             (cabal-bin (if cabal-dev? "cabal-dev" "cabal"))
             (command (format "cd %s; %s install" project-dir cabal-bin)))
        (set (make-local-variable 'compile-command) command)
        (when (or cabal-dev? cabal-sandbox?)
          (let* ((project-name (file-name-nondirectory (directory-file-name project-dir)))
                 (project-inf-buffer-name (if cabal-sandbox-test?
                                              (format "haskell test – %s" project-name)
                                              (format "haskell – %s" project-name)))
                 (ghci-command (cond (cabal-dev? "ghci-dev")
                                     (cabal-sandbox-test? "ghci-test")
                                     (t "ghci-sandbox")))
                 (project-inf-command (list ghci-command project-dir))
                 (message (format "%s %s" ghci-command project-dir))
                 (project-inf-buffer (make-comint project-inf-buffer-name ghci-command nil project-dir)))
            (message (format "cabal-dev: %s" project-name))
            (set (make-local-variable 'inferior-haskell-buffer) project-inf-buffer)
            (set (make-local-variable 'haskell-program-name) ghci-command)
            (with-current-buffer project-inf-buffer
              (inferior-haskell-mode)
              (run-hooks 'inferior-haskell-mode))))))))

(provide 'haskell-project-mode)
