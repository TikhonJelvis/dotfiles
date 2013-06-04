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
;; some Haskell sources. For now, all I'm doing is setting the M-x
;; compile command correctly for the cabal file; in the future, I'll
;; probably add some other things like a command to run tests
;; automatically and maybe a per-project inferior Haskell buffer.

;;; Code:

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
             (cabal-bin (if cabal-dev? "cabal-dev" "cabal"))
             (command (format "cd %s; %s install" project-dir cabal-bin)))
        (set (make-local-variable 'compile-command) command)
        (when cabal-dev?
          (let* ((project-name (file-name-nondirectory (directory-file-name project-dir)))
                 (project-inf-buffer-name (format "haskell – %s" project-name))
                 (project-inf-command (list "ghci-dev" project-dir))
                 (message (format "ghci-dev %s" project-dir))
                 (project-inf-buffer (make-comint project-inf-buffer-name "ghci-dev" nil project-dir)))
            (message (format "cabal-dev: %s" project-name))
            (set (make-local-variable 'inferior-haskell-buffer) project-inf-buffer)
            (set (make-local-variable 'haskell-program-name) "ghci-dev")
            (with-current-buffer project-inf-buffer
              (inferior-haskell-mode)
              (run-hooks 'inferior-haskell-mode))))))))

(provide 'haskell-project-mode)
