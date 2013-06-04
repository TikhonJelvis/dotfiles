
;;; hie.el --- minor mode Haskell-In-Emacs (ide like stuff)
     
;; Copyright (C) 2012 Christopher Monsanto.
     
;; Author: Christopher Monsanto <chris@monsan.to>
;; Maintainer: Christopher Monsanto <chris@monsan.to>
;; Created: 15 Mar 2012
;; Version: 0.1
;; Keywords: haskell intellisense autocomplete
;; License: GPLv3
;; 

(require 'cl)
(require 'haskell-mode)

;

(defvar hie-modules-dir "~/.hie/"
  "Where should we look for global definitions?")

(defvar hie-modules-cache-dir "~/.hie-cache/"
  "Where should we cache stuff?")

(defvar hie-update-interval 0.5
  "How long should you be idle before we update?")

;

(defvar hie-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c h") 'hie-show-signature)
    (define-key map (kbd "C-c j") 'hie-jump-to-definition)
    map)
  "hie-mode keymap")

(define-minor-mode hie-mode "Toggle hie mode." nil " hie" hie-mode-map
  (hie-setup-buffer))

;

(defun hie-new-hash ()
  "Create a hash table."
  (make-hash-table :test 'equal))

(defvar hie-import-defs-hash (hie-new-hash)
  "A hash table mapping hieimports to module hashes. We are not editing any of these files in Emacs. (Think system libraries).

If you DO edit this, you can clear all.")

(defvar hie-buffer-idents-hash nil
  "A hash table mapping local identifiers to hiedefs.")
(make-variable-buffer-local 'hie-buffer-idents-hash)

(defvar hie-buffer-module-sxhash nil
  "This is here to avoid setting up and destroying over and over...")
(make-variable-buffer-local 'hie-buffer-module-sxhash)

(defvar hie-buffer-imports nil
  "This is here to avoid setting up and destroying over and over...")
(make-variable-buffer-local 'hie-buffer-imports)

(defvar hie-buffer-check nil)
(make-variable-buffer-local 'hie-buffer-check)

;
; Structures
;

(defstruct hieimport
  name list alias is-qualified is-hidden)

(defstruct hiedef
  name is-instance parent file line column signature help)

(defstruct hiemod
  name defs imports exports)

;
; Main interactives
;

(defun hie-global-defshash ()
  (if hie-mode
      hie-buffer-idents-hash
    hie-buffer-idents-hash))

(defun hie-initial-text ()
  (when (gethash (haskell-ident-at-point)
		 (hie-global-defshash))
    (haskell-ident-at-point)))

(defvar hie-read-ident-history nil)
(defun hie-read-ident (prompt &optional filter)
  (let ((name (completing-read prompt
				(hie-global-defshash)
				filter 
				t
				(hie-initial-text)
				'hie-read-ident-history)))
    (gethash name (hie-global-defshash))))

(defun hie-jump (def) 
  (find-file (hiedef-file def)) 
  (goto-char (point-min))
  (forward-line (1- (hiedef-line def)))
  (forward-char (1- (hiedef-column def))))

(defun hie-jump-to-definition ()
  "Jump to the given definition."
  (interactive)
  (let ((def (hie-read-ident "Jump to def: " (lambda (key def) (not (hiedef-is-instance def))))))
    (when def
      (hie-jump def))))

(defun hie-show-signature ()
  "Show the signature in the mode line."
  (interactive)
  (let ((def (hie-read-ident "Signature: ")))
    (when def 
      (display-message-or-buffer (hie-make-help def)))))

(defun hie-make-help (def) 
  (if (and (hiedef-signature def) (hiedef-help def))
      (concat (hiedef-signature def) "\n\n" (hiedef-help def))
    (or (hiedef-signature def) (hiedef-help def) "")))

;

(defun hie-load-file (file)
  "Create completion table, etc for given module."
  (setq *hie-load* nil)
  (load file t t)
  *hie-load*)

(defun hie-write-file (filename mod)
  (with-temp-file filename
    (insert (format "(setq *hie-load* %s)" (prin1-to-string mod)))))

(defun hie-is-exportable-module (mod)
  (not (member (hiemod-name mod) (list nil "Main"))))

(defun hie-write-module (mod)
  (hie-write-file (concat hie-modules-dir (hiemod-name mod)) mod)
					; Delete the cache stuff
  (condition-case nil
      (delete-file (concat hie-modules-cache-dir (hiemod-name mod)))
    ('error nil))
  (condition-case nil
      (delete-file (concat hie-modules-cache-dir (hiemod-name mod) ".elc"))
    ('error nil)))

(defun hie-dump-defs (name defs)
  
  (with-temp-file (concat hie-modules-cache-dir name)
    (insert (format "(setq *hie-load* (list %s))"
		    (mapconcat 'identity 
			       (loop for def in defs
				     collect (prin1-to-string def)) " "))))
  (byte-compile-file (concat hie-modules-cache-dir name)))

(defun hie-import-defs (import)
  ; Try globals
  (let ((defs (gethash import hie-import-defs-hash 'missing))
	(name (hieimport-name import)))
    (cond
     ((eq defs 'recursive-hack) nil)
     ((and (eq defs 'missing) (not (hieimport-alias import))) ; if we dont have the raw version, make it
      (let ((defs (hie-load-file (concat hie-modules-cache-dir name))))
	(if defs
	    (puthash import defs hie-import-defs-hash) 
	  (let ((mod (hie-load-file
		      (concat hie-modules-dir name))))
	    (if mod
		(progn
		  (puthash (make-hieimport :name name) 'recursive-hack hie-import-defs-hash)
		  (let ((defs (hie-resolve-exports-defs
			       (hiemod-exports mod)
			       (hie-resolve-imports-defs (hiemod-imports mod) (hiemod-defs mod))))) 
		    (hie-dump-defs name defs)
		    (puthash (make-hieimport :name name) defs hie-import-defs-hash)))
	      (puthash (make-hieimport :name name) nil hie-import-defs-hash))))))
     ((eq defs 'missing) ; if this isn't the raw version, import the raw version and fix it
      (let ((defs (hie-import-defs (make-hieimport :name name)))) 
	(puthash import (if defs (hie-fix-raw-import-defs defs import) nil) hie-import-defs-hash)))
     (t defs))))

(defun hie-fix-raw-import-defs (base-defs import)
  (let* ((defs (if (hieimport-is-hidden import)
		   (reduce (lambda (rest exp)
			     (hie-filter-defs nil exp rest))
			   (hieimport-list import) :initial-value base-defs)
		 (loop for export in (hieimport-list import)
		       append (hie-filter-defs t export base-defs))))
	 (alias-defs (hie-alias-defs (hieimport-alias import) defs)))
    (if (hieimport-is-qualified import)
	alias-defs
      (append defs alias-defs))))

(defun hie-resolve-exports-defs (exports defs)
  (hie-filter-prefixes (loop for export in exports append
			     (hie-filter-defs t export defs))))

(defun hie-resolve-imports-defs (imports defs)
  (append defs
	  (loop for import in imports 
		append (hie-import-defs import))))

(defun hie-has-prefix (prefix string)
  (eq 0 (string-match
	 (concat "^" (regexp-quote prefix) "\\.") string)))

(defun hie-filter-prefixes (defs)
  (loop for def in defs
	collect
	(progn
	  (setq def (copy-hiedef def)) 
	  (setf (hiedef-name def)
		(replace-regexp-in-string "[A-Za-z0-9]+\\." "" (hiedef-name def)))
	  def)))

(defun hie-filter-defs (they-select export defs)
  (destructuring-bind (tag name) export
    (cond
     ((eq tag 'id)
      (loop for def in defs
	    if (eq they-select (equal (hiedef-name def) name))
	    collect def))
     ((eq tag 'all)
      (loop for def in defs
	    if (eq they-select (or (equal (hiedef-name def) name)
				   (equal (hiedef-parent def) name)))
	    collect def))
     ((eq tag 'mod)
      (loop for def in defs
	    if (eq they-select (hie-has-prefix
				name
				(hiedef-name def)))
	    collect def)))))

(defun hie-alias-defs (new-scope defs)
  "Strips out anything not in the namespace old-mod, and renames the rest to new-mod."
  (loop for def in defs
	collect (progn
		  (setq def (copy-hiedef def)) 
		  (setf (hiedef-name def)
			(concat new-scope "." (hiedef-name def)))
		  def)))

; I'll make this "multithreaded" once I get lexically scoped variables... not everyone uses Emacs 24 I guess. :(
(defun hie-run ()
  "Runs hie and loads the module it generates."
  (let ((file (make-temp-file "hie")))
    (call-process-region (point-min) (point-max) "hie" nil (list :file file) nil (buffer-file-name))
    (hie-load-file file)))	    
;

(defun hie-populate-defshash (defs defshash)
  (loop for def in defs do (puthash (hiedef-name def) def defshash)))

(defun hie-setup-buffer ()
  "Prepare the buffer for hie."
  (interactive)
  (let* ((mod (hie-run))
	 (sx (sxhash mod)))
    (setq hie-buffer-check nil)
    (when (and mod (not (eq hie-buffer-module-sxhash sx)))
      (setq hie-buffer-module-sxhash sx)

;; local business
      (setq hie-buffer-idents-hash (hie-new-hash))
      (loop for import in (hiemod-imports mod)
	    do (hie-populate-defshash (hie-import-defs import) hie-buffer-idents-hash))
      (hie-populate-defshash (hiemod-defs mod) hie-buffer-idents-hash)

      (setq hie-buffer-imports (loop for import in (hiemod-imports mod) collect (hieimport-name import)))
      
      ;; global business
     
      mod)))

(defun hie-save-buffer (mod)
  (hie-write-module mod)
  
  (loop for buffer in (buffer-list)
	    unless (equal buffer (current-buffer)) do
	    (with-current-buffer buffer
	      (when (and hie-mode (member (hiemod-name mod) hie-buffer-imports))
		(setq hie-buffer-check t)
		(setq hie-buffer-module-sxhash 0))))
      
  (loop for import being the hash-keys in hie-import-defs-hash
	do (when (equal (hieimport-name import) (hiemod-name mod))
	     (remhash import hie-import-defs-hash))))

;; Autocomplete stuff

(defun hie-ac-candidates ()
  (let (candidates)
    (maphash
     (lambda (name def)
       (unless (hiedef-is-instance def)
	 (setq candidates (cons name candidates))))
     hie-buffer-idents-hash)
    candidates))

(defun hie-source-prefix ()
  (let ((p (point)))
    (save-excursion
      (beginning-of-line) 
      (unless (looking-at "^[ \t]*import")
	(goto-char p)
	(ac-prefix-symbol))))) 

(defun hie-ac-document (name)
  (let ((def (gethash name hie-buffer-idents-hash)))
    (hie-make-help def)))

(defvar ac-source-hie
  '((candidates . hie-ac-candidates)
    (prefix . hie-source-prefix)
    (document . hie-ac-document)))

(defvar ac-source-hie-no-quick-help
  '((candidates . hie-ac-candidates)
    (prefix . hie-source-prefix)))

(defun hie-ac-module-candidates ()
  (directory-files hie-modules-dir nil "^[^.]" t))

(defvar ac-source-hie-modules
  '((candidates . hie-ac-module-candidates)
    (prefix . "^[ \t]*import[ \t]*\\(.*\\)")
    (cache)))

;

(defun hie-try-idle-update ()
  "Try to update the current buffer when we're idle."
  (when (and hie-mode hie-buffer-check) 
    (hie-setup-buffer)))

(defun hie-try-save-update ()
  "Try to update the current buffer when we're idle."
  (when hie-mode 
    (let ((mod (hie-setup-buffer)))
      (when (and mod (hie-is-exportable-module mod))
	(hie-save-buffer mod)))))

(defun hie-allow-update (b e l)
  (setq hie-buffer-check t))

(defvar hie-idle-timer nil)

(when hie-idle-timer
  (cancel-timer hie-idle-timer))
(setq hie-idle-timer (run-with-idle-timer hie-update-interval t 'hie-try-idle-update))

(add-hook 'after-save-hook 'hie-try-save-update)
(add-hook 'after-change-functions 'hie-allow-update)

(provide 'hie)
