;;; esl.el --- Live-load ESL's tree-sitter grammar and mode  -*- lexical-binding: t; -*-

;; The ESL Emacs mode and tree-sitter grammar live in the esmcompiler
;; repo (esl/emacs, esl/tree-sitter-esl).  Rather than copying them
;; into the dotfiles or building them with Nix, this file loads them
;; straight from a checkout.
;;
;; Re-evaluating this file (or M-x esl-reload) picks up the latest
;; mode *and* grammar in the running Emacs: no home-manager build, no
;; restart.  The grammar is compiled from the committed
;; src/parser.c with the system C compiler.

(defcustom esl-checkout-directory
  (expand-file-name "~/Programming/work/esm/o9.apex.esmcompiler/esl")
  "The esl/ directory of the esmcompiler checkout to load ESL from."
  :type 'directory
  :group 'esl-ts)

(defvar esl-grammar-cache-directory
  (expand-file-name "esl-ts" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Where compiled ESL grammar libraries are kept.")

(defun esl--grammar-library ()
  "Compile the ESL grammar if needed, and return its library base name.

Libraries are named by a hash of parser.c.  The OS caches a loaded
library by path, so a fresh path is what makes a changed grammar
load into an already-running Emacs."
  (let* ((src (expand-file-name "tree-sitter-esl/src" esl-checkout-directory))
         (parser (expand-file-name "parser.c" src))
         (hash (with-temp-buffer
                 (insert-file-contents-literally parser)
                 (substring (secure-hash 'sha1 (current-buffer)) 0 12)))
         (base (format "libtree-sitter-esl-%s" hash))
         (suffix (if (eq system-type 'darwin) ".dylib" ".so"))
         (library (expand-file-name (concat base suffix)
                                    esl-grammar-cache-directory)))
    (unless (file-exists-p library)
      (make-directory esl-grammar-cache-directory t)
      (let* ((tmp (make-temp-file "esl-grammar-" nil suffix))
             (out (generate-new-buffer " *esl grammar build*"))
             (status (call-process "cc" nil out nil
                                   "-fPIC" "-shared" "-O2" "-I" src
                                   parser "-o" tmp)))
        (if (zerop status)
            (progn (rename-file tmp library t)
                   ;; Old builds are dead weight.
                   (dolist (old (directory-files esl-grammar-cache-directory t
                                                 "\\`libtree-sitter-esl-"))
                     (unless (equal old library) (delete-file old))))
          (delete-file tmp)
          (error "Compiling the ESL grammar failed:\n%s"
                 (with-current-buffer out (buffer-string))))
        (kill-buffer out)))
    base))

;;;###autoload
(defun esl-reload ()
  "Load the latest ESL grammar and `esl-ts-mode' from the checkout.
Buffers already in `esl-ts-mode' are re-initialized."
  (interactive)
  (let ((mode-file (expand-file-name "emacs/esl-ts-mode.el"
                                     esl-checkout-directory)))
    (if (not (file-exists-p mode-file))
        (message "esl: no ESL checkout at %s (see `esl-checkout-directory')"
                 esl-checkout-directory)
      (add-to-list 'treesit-extra-load-path esl-grammar-cache-directory)
      (setf (alist-get 'esl treesit-load-name-override-list)
            (list (esl--grammar-library) "tree_sitter_esl"))
      (load mode-file nil t)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'esl-ts-mode)
            (esl-ts-mode))))
      (message "esl: loaded grammar and mode from %s" esl-checkout-directory))))

(require 'treesit)
(esl-reload)

;;; esl.el ends here
