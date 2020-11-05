                                        ; CUSTOM-SET
;; Put custom-set variables in a different file:
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

                                        ; PERSONAL PACKAGES
(add-to-list 'load-path "~/.emacs.d/packages")

                                        ; MAC-SPECIFIC SETTINGS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)

  (set-face-attribute 'default nil :height 150))

                                        ; UTILITY FUNCTIONS
(defun easy-move ()
  "Lets me navigate without using the control key. This only
makes sense in read-only buffers, obviously."
  (local-set-key (kbd "n") 'next-line)
  (local-set-key (kbd "p") 'previous-line)
  (local-set-key (kbd "l") 'recenter-top-bottom))

(defun quit ()
  "Prompts to save unsaved buffers and then kills the emacs server."
  (interactive)
  (save-some-buffers)
  (recentf-save-list)
  (kill-emacs))

(defun kill-current-buffer-process ()
  "Kills the process running in the current buffer."
  (interactive)
  (kill-process (current-buffer)))
(global-set-key (kbd "C-c C-k") 'kill-current-buffer-process)

;; A list of opposite boolean pairs.
(defvar bools '(("true" . "false") ("True" . "False") ("#t" . "#f")
                ("yes" . "no") ("Yes" . "No")))

(defun flip-bool-at-point ()
  "Flips the boolean literal at point, changing true to false and
vice-versa."
  (interactive)
  (let* ((true  (cdr (assoc  (current-word) bools)))
         (false (car (rassoc (current-word) bools)))
         (wrd (cond (true true)
                    (false false)
                    (t (current-word)))))
    (save-excursion
      (forward-word)
      (backward-kill-word 1)
      (insert wrd))))
(global-set-key (kbd "C-c C-b") 'flip-bool-at-point)
(global-set-key (kbd "C-c b") 'flip-bool-at-point)

;; Insert TODO comments programmatically:
(defun todo-comment ()
  "Inserts an empty TODO comment or makes an existing comment
into a TODO."
  (interactive)
  (when (not (region-active-p))
    (comment-dwim nil)
    (unless (equal (current-word) "TODO") (insert "TODO: "))))
(global-set-key (kbd "C-c t") 'todo-comment)

(defun file-name-at-point (add-to-kill-ring)
  "Prompts the user for a file path using the standard C-x C-f
interface and inserts it at point."
  (interactive "P")
  (let ((action (if add-to-kill-ring 'kill-new 'insert))
        (path (if ido-mode
                  (ido-read-file-name "file path: ")
                (read-file-name "file path: "))))
    (apply action (list path))))
(global-set-key (kbd "C-c f") 'file-name-at-point)

					; GENERAL STUFF
;; Have compile scroll to the end by default.
(setq-default compilation-scroll-output 'foo-bar)

;; Using X-resources on KDE leads to the wrong cursor color in new
;; frames :/
(setq inhibit-x-resources t)

(use-package windmove
  :init
  ;; Better commands for window management:
  ;; swap-with taken from emacsd-tile 0.1 by marius a. eriksen
  ;; (https://gist.github.com/287633)
  (defun swap-with (dir)
    (interactive)
    (let ((other-window (windmove-find-other-window dir)))
      (when other-window
        (let* ((this-window  (selected-window))
               (this-buffer  (window-buffer this-window))
               (other-buffer (window-buffer other-window))
               (this-start   (window-start this-window))
               (other-start  (window-start other-window)))
          (set-window-buffer this-window  other-buffer)
          (set-window-buffer other-window this-buffer)
          (set-window-start  this-window  other-start)
          (set-window-start  other-window this-start)))))

  :bind (("C-M-S-N" . (lambda () (interactive) (swap-with 'down)))
         ("C-M-S-P" . (lambda () (interactive) (swap-with 'up)))
         ("C-M-S-B" . (lambda () (interactive) (swap-with 'left)))
         ("C-M-S-F" . (lambda () (interactive) (swap-with 'right)))

         ("M-N" . windmove-down)
         ("M-P" . windmove-up)
         ("M-B" . windmove-left)
         ("M-F" . windmove-right)))

(use-package ido
  :config
  (ido-mode t)

  ;; Set C-x C-b to switching buffer—for some reason, I always hit by
  ;; accident. It's annoying!
  :bind ("C-x C-b" . ido-switch-buffer)

  :custom
  (ido-default-buffer-method 'selected-window)
  (ido-auto-merge-work-directories-length -1))

;; I don't like tabs very much:
(setq-default indent-tabs-mode nil)

;; Unique buffer names:
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-separator " • "))

;; Get rid of the annoying splash screen:
(setq inhibit-splash-screen t)

;; Have a list of recent files:
(add-hook 'recentf-dialog-mode-hook 'easy-move)
(recentf-mode 1)
(recentf-open-files nil "*Recent Files*")
(setq recentf-max-saved-items 100000)
(global-set-key (kbd "C-x C-a") 'recentf-open-files) ;; Open recent files easily

                                        ; APPEARANCE
(setq ring-bell-function 'ignore)
(show-paren-mode 1)
(column-number-mode t)
(transient-mark-mode -1)
(setq mark-even-if-inactive t)
(setq-default truncate-lines t)

;; For enabling color themes:
(setq custom-theme-directory "~/.emacs.d/themes/")
(setq custom-safe-themes t)
(load-theme 'blackboard t)

;; Change company-mode colors to match blackboard:
(use-package company
  :ensure t
  :after color
  :hook (emacs-lisp-mode . company-mode)
  :config
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 15)))))
     `(company-scrollbar-fg ((t (:background "DarkOrange"))))
     `(company-tooltip-selection ((t (:background ,(color-lighten-name bg 20)))))
     `(company-tooltip-common ((t (:inherit font-lock-builtin-face))))
     `(company-tooltip-annotation ((t (:inherit font-lock-builtin-face)))))))

;; Adds icons to company popups.
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;;Make the window simpler:
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; mac-specific: menu-bar-mode needed for fullscreen, for some reason?
(if (eq system-type 'darwin)
    (menu-bar-mode 1)
  (menu-bar-mode -1))

(fringe-mode 0)

;; No $ displayed for truncated lines
(set-display-table-slot standard-display-table 0 ?\ )

;; Fill to 80 characters by default:
(setq fill-column 80)

;; Not sure why this needs to be in a hook, but it didn't initialize
;; correctly otherwise.
(defun emoji-fonts-hook (frame)
  ;; Basic color Emoji support with Noto:
  (set-fontset-font t 'symbol "Noto Color Emoji" frame))
(add-hook 'after-make-frame-functions #'emoji-fonts-hook)


;; Icons that I can use in dired, buffer mode lines... etc
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))


;; Prettier mode line
(use-package powerline
  :ensure t)
(load-file "~/.emacs.d/mode-line.el")

                                        ; KEY REBINDINGS
;; Do nothing on C-x C-c:
(global-unset-key (kbd "C-x C-c"))
;; I'm phasing C-x o out:
(global-set-key (kbd "C-x o") 'other-frame)

(global-set-key (kbd "C-S-b") 'list-buffers)

;; Make complete tag not be alt-tab!
(global-set-key (kbd "M-s-<return>") 'complete-tag)

;; Some nice keyboard shortcuts:
(global-set-key (kbd "C-x 5 3") 'make-frame-command)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-j") 'compile)
(global-set-key (kbd "C-c C-a") 'align-regexp)
(global-set-key (kbd "M-#") 'ispell-complete-word)
(global-set-key (kbd "M-j") 'next-error)
(global-set-key (kbd "M-J") 'flycheck-next-error)

;; C-w remap:
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Get rid of column editing which I trigger by accident and find
;; incredibly annoying:
(global-unset-key (kbd "<f2>"))

                                        ; INPUT MODES
(setq default-input-method "TeX")
(toggle-input-method)

;; My quail customizations
(load-file ".emacs.d/quail-rules.el")

					; DIRED
;; Has to be above JABBER settings because it has a conflicting
;; keybinding :(.
(use-package dired-x
  ;; Automatically omit “uninteresting” files from the listing. (Toggled
  ;; with M-o.)
  :hook (dired-mode . dired-omit-mode))

;; Simplify the dired view by hiding permissions, users, date
;; modified... etc
(setq dired-hide-details-hide-symlink-targets nil)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Automatically update dired buffers when the directory changes:
(add-hook 'dired-mode-hook 'auto-revert-mode)

                                        ; IMAGES
;; Set the background for image-previews to an light color. This makes
;; reading diagrams with transparent backgrounds easier.
(defun set-buffer-background (color)
  "Set the background color of the current buffer to COLOR.

This uses the `buffer-face' minor mode."
  (interactive "sColor:")
  (buffer-face-set `(:background ,color))
  (buffer-face-mode 1))

(defun image-preview-set-background-color ()
  "Set the background color to a nice light background for images
  to make it easier to read diagrams with transparent
  backgrounds."
  (set-buffer-background "#cadbf2"))
(add-hook 'image-mode-hook 'image-preview-set-background-color)

                                        ; FLYSPELL
;; If the aspell executable is not available, check two things:
;;
;;   1. Does Emacs see the right PATH variable? See EXEC PATH section
;;   above.
;;
;;   2. Is aspell installed to your Nix user profile? Run home-manager
;;   switch to make sure.
(use-package flyspell
  :init
  (defun flyspell-color-hook ()
    (set-face-attribute 'flyspell-duplicate nil
                        :foreground nil
                        :underline "dark orange"
                        :bold nil)
    (set-face-attribute 'flyspell-incorrect nil
                        :foreground nil
                        :underline "red"
                        :bold nil))
  :custom
  (ispell-program-name "~/.nix-profile/bin/aspell")
  (ispell-personal-dictionary "~/Programming/dotfiles/.aspell.en.pws")

  :config
  (add-hook 'flyspell-mode-hook 'flyspell-color-hook))

                                        ; FLYCHECK
(use-package flycheck
  :ensure t
  :hook (python-mode . flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled)))


                                        ; NIX
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package direnv
  :ensure t

  :custom
  (direnv-show-paths-in-summary nil)
  (direnv-always-show-summary nil)

  :config
  (direnv-mode)
  (add-to-list 'direnv-non-file-modes 'shell-mode))


                                        ; JSON
;; Set the indent level to 4 for JSON files, making it buffer local to not
;; change .js files.
(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :init
  (defun json-indent-hook ()
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 4))
  :config
  (add-hook 'json-mode-hook 'json-indent-hook))

                                        ; YAML
(use-package yaml-mode
  :ensure t)

                                        ; YASNIPPET
(use-package yasnippet
  :ensure t)

                                        ; PROJECTILE
(use-package projectile
  :ensure t)

                                        ; LSP
(use-package lsp-mode
  :ensure t
  :custom
  (lsp-eldoc-hook nil)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-alignment 'window)
  :bind
  (:map lsp-mode-map
   ("C-c C-d" . lsp-ui-doc-show)))

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-sideline-enable nil))

(use-package dap-mode
  :ensure t
  :init
  (setq gud-key-prefix (kbd "C-c C-s")))

                                        ; MAGIT
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-clone))

  :custom
  (magit-commit-ask-to-stage 'stage)
  (magit-clone-set-remote.pushDefault t)
  ;; Don't revert my window layout when I q out of a Magit buffer
  (magit-bury-buffer-function 'magit-mode-quit-window)

  :config
  ;; Improve ergonomics of Git commit message buffers
  (defun my-git-commit-setup-hook ()
    (visual-line-mode 1)
    (visual-fill-column-mode 1))
  (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-auto-fill)
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  (add-hook 'git-commit-setup-hook 'my-git-commit-setup-hook))

                                        ; ORG-MODE
(use-package prog-mode)

(defun org-mode-prettify-hook ()
  "Configure prettify-symbols to replace todo/consider/done with
  pretty Unicode characters."
  (push '("TODO" . "") prettify-symbols-alist)
  (push '("FOLLOW-UP" . "") prettify-symbols-alist)
  (push '("CONSIDER" . "") prettify-symbols-alist)
  (push '("INVESTIGATE" . "") prettify-symbols-alist)
  (push '("DONE" . "✔") prettify-symbols-alist)
  (push '("CANCELED" . "") prettify-symbols-alist)
  (push '("PROJECT" . "") prettify-symbols-alist)
  (prettify-symbols-mode 1))

(use-package org
  :after prog-mode

  :custom
  (org-todo-keywords
   '((type "TODO" "CONSIDER" "FOLLOW-UP" "INVESTIGATE" "|" "DONE" "CANCELED")
     (sequence "PROJECT" "|" "DONE")))
  (org-capture-templates
   '(("t" "Todo" entry (file "Tasks.org")
      "* TODO %?\n  SCHEDULED: %T\n:PROPERTIES:\n:CREATED: %U\n:END:")))
  (org-refile-targets
   '((org-agenda-files :maxlevel . 3)))

  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)

         :map org-mode-map
         ("M-{" . outline-previous-visible-heading)
         ("M-}" . outline-next-visible-heading)
         ("C-c C-," . org-promote-subtree)
         ("C-c C-." . org-demote-subtree))

  :hook (org-insert-heading . org-insert-with-timestamp)

  :init
  (defun org-insert-with-timestamp ()
    (when (member "CREATED" (org-buffer-property-keys))
      (let* ((fmt (concat "[" (substring (cdr org-time-stamp-formats) 1 -1) "]"))
             (timestamp (format-time-string fmt (current-time))))
        (org-set-property "CREATED" timestamp))))

  :config
  (add-hook 'org-mode-hook 'org-mode-prettify-hook)

  (defun unset-agenda-binding () (local-unset-key (kbd "C-c C-a")))
  (add-hook 'comint-mode-hook 'unset-agenda-binding)

  ;; My core *.org files are stored in Dropbox unless I'm on a work
  ;; computer. (Only place I would use macOS!)
  (when (not (eq system-type 'darwin))
    (setq org-directory "~/Dropbox/org"))

  (setq org-default-notes-file (concat org-directory "/Notes.org"))

  ;; Set up the templates I use (triggered by typing < followed by a
  ;; letter followed by <TAB>)
  (add-to-list 'org-structure-template-alist
               '("h" "#+BEGIN_SRC haskell\n?\n#+END_SRC"))
  (add-to-list 'org-structure-template-alist
               '("p" ":PROPERTIES:\n:CREATED: ?\n:END:"))

  ;; Spellcheck my org mode files.
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'auto-fill-mode)

  ;; Allow markup in the middle of words.
  (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))

(use-package ox-reveal
  :ensure t
  :bind (:map org-mode-map
         ("M-S" . org-reveal-export-current-subtree)
         ("M-R" . org-reveal-export-to-html))
  :config
  ;; Configuring title page formatting with #+OPTION is too fiddly, so
  ;; we want to override the elisp variable instead
  (put 'org-reveal-title-slide 'safe-local-variable 'stringp)
  (add-to-list 'org-structure-template-alist
               '("f" "#+ATTR_REVEAL: :frag roll-in")))

(use-package el-patch
  :ensure t
  :config
  (setq el-patch-enable-use-package-integration t))

(define-advice org-agenda-format-item (:filter-args (&rest args) fontify-org)
  "Force fontify ageda item. (hack)

Source: https://www.reddit.com/r/orgmode/comments/i3upt6/prettifysymbolsmode_not_working_with_orgagenda/"
  (cl-multiple-value-bind (extra txt level category tags dotime remove-re habitp) (car args)
    (with-temp-buffer
      (cl-letf (((symbol-function 'yant/process-att-abbrev) #'identity)
		((symbol-function 'yant/process-att-id-abbrev) #'identity)) ;; expanding sometimes causes errors when attempting to access ancestors
	(org-mode)
        (setq txt (replace-regexp-in-string "[ \t]*:[[:alnum:]_@#%:]+:[ 	]*$" "" txt))
	(insert "* "
		txt
		"\t"
		(or (and tags (s-join ":" `(nil ,@(cl-remove-duplicates tags) nil)))
		    "")
		"\n")
	(font-lock-fontify-buffer)
	(goto-char (point-min))
	(looking-at "^\\* \\(\\([^\t]+\\)[ 	]+\\(:\\([[:alnum:]_@#%:]+\\):\\)*\\)[ 	]*$")
	(setq txt (match-string 2))
	(setq tags (and tags (s-split ":" (match-string 3) 't))))
      (list extra txt level category tags dotime remove-re habitp))))

(use-package org-agenda
  :after el-patch

  :bind (("C-c a" . org-agenda)
         :map org-agenda-mode-map
         ("k" . org-capture))

  :custom
  (org-agenda-scheduled-leaders '("" " %2d×"))
  (org-agenda-prefix-format
   '((agenda . " %i %-7t% s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
  (org-agenda-remove-times-when-in-prefix 'beg)
  (org-agenda-remove-tags t)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1200 1600 2000)
     "" "┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈"))
  (org-agenda-current-time-string "◀ ┈┈┈┈┈┈┈┈ now ┈┈┈┈┈┈┈┈")

  (org-agenda-window-setup 'other-window)

  :config
  (defun org-agenda-custom-date-format (date)
    (concat "\n" (org-agenda-format-date-aligned date)))
  (setq org-agenda-format-date 'org-agenda-custom-date-format)

  (setq org-agenda-files
        (list (concat org-directory "/Tasks.org")
              (concat org-directory "/Books.org")))


  :config/el-patch
  (el-patch-feature org-agenda)

  ;; calling `org-agenda-highlight-todo' breaks 'composition text property of
  ;; todo keywords, which breaks pretty-symbols fontifications fixing the
  ;; function to keep 'composition
  (defun org-agenda-highlight-todo (x)
    (let ((org-done-keywords org-done-keywords-for-agenda)
	  (case-fold-search nil)
	  re
          (el-patch-add composition-property))
      (if (eq x 'line)
	  (save-excursion
	    (beginning-of-line 1)
	    (setq re (org-get-at-bol 'org-todo-regexp))
	    (goto-char (or (text-property-any (point-at-bol) (point-at-eol) 'org-heading t) (point)))
	    (when (looking-at (concat "[ \t]*\\.*\\(" re "\\) +"))
	      (add-text-properties (match-beginning 0) (match-end 1)
				   (list 'face (org-get-todo-face 1)))
              (el-patch-add (setq composition-property (plist-get (text-properties-at (match-beginning 1)) 'composition)))
	      (let ((s (buffer-substring (match-beginning 1) (match-end 1))))
		(delete-region (match-beginning 1) (1- (match-end 0)))
		(goto-char (match-beginning 1))
		(insert (format org-agenda-todo-keyword-format s))
                (el-patch-add (add-text-properties (match-beginning 1) (match-end 1) (list 'composition composition-property))))))
	(let ((pl (text-property-any 0 (length x) 'org-heading t x)))
	  (setq re (get-text-property 0 'org-todo-regexp x))
	  (when (and re
		     ;; Test `pl' because if there's no heading content, there's
		     ;; no point matching to highlight.  Note that if we didn't
		     ;; test `pl' first, and there happened to be no keyword
		     ;; from `org-todo-regexp' on this heading line, then the
		     ;; `equal' comparison afterwards would spuriously succeed
		     ;; in the case where `pl' is nil -- causing an
		     ;; args-out-of-range error when we try to add text
		     ;; properties to text that isn't there.
		     pl
		     (equal
                      (string-match (concat "\\(\\.*\\)" re "\\( +\\)") x pl)
                      pl))
	    (add-text-properties
	     (or (match-end 1) (match-end 0))
             (match-end 0)
             (list 'face (org-get-todo-face (match-string 2 x)))
             x)
	    (when (match-end 1)
	      (setq x
		    (concat
		     (substring x 0 (match-end 1))
		     (format org-agenda-todo-keyword-format (match-string 2 x))

		     ;; Remove `display' property as the icon could
		     ;; leak on the white space.
		     (org-add-props " "
                         (org-plist-delete (text-properties-at 0 x) 'display))
		     (substring x (match-end 3)))))))
	x)))

  (add-hook 'org-agenda-mode-hook 'org-mode-prettify-hook))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode))

                                        ; SHELL BUFFERS
(use-package comint
  ;; Stop Emacs from expanding things like !! in history
  :custom (comint-input-autoexpand 'nil)

  :bind (("C-c s" . new-shell)
         :map comint-mode-map
         ("C-c C-k" . comint-clear-buffer))

  :config
  (defun format-shell-name (s)
    (subst-char-in-string ?' 8242 s))

  (defun find-useful-directory-name (dir)
    "Starting with the given directory and moving up in the
filesystem hierarchy, find the first directory that isn't a
really common name like 'src' or 'bin'."
    (let ((common-names '("src" "bin"))
          (base (file-name-base (directory-file-name dir)))
          (up (file-name-directory (directory-file-name dir))))
      (if (member base common-names)
          (find-useful-directory-name up)
        base)))

  (defun new-shell (name)
    "Opens a new shell buffer with the given name in
asterisks (*name*) in the current directory with and changes the
prompt to name>."
    (interactive "sName: ")
    (when (equal name "")
      (setq name (find-useful-directory-name default-directory)))
    (setq name (format-shell-name name))
    (pop-to-buffer (concat "<*" name "*>"))
    (unless (eq major-mode 'shell-mode)
      (shell (current-buffer))

      (comint-simple-send
       (get-buffer-process (current-buffer))
       "export TERM='xterm-256color'")
      (comint-simple-send
       (get-buffer-process (current-buffer))
       (format "export PAGER=%s"
               (expand-file-name "~/local/bin/epage")))
      (comint-simple-send
       (get-buffer-process (current-buffer))
       (concat "export PS1='"
               "\033[31m" name
               "\033[37m" ":"
               "\033[32m" "\\W"
               "\033[37m" ">"
               "\033[0m"
               "'"))

      (sleep-for 0 200)
      (comint-send-input)
      (comint-clear-buffer))))

(use-package xterm-color
  :ensure t

  :config
  (defun from-face (face)
    (face-attribute face :foreground))
  (setq xterm-color-names
        `[,(from-face 'default)
          ,(from-face 'font-lock-builtin-face)
          ,(from-face 'font-lock-variable-name-face)
          ,(from-face 'font-lock-constant-face)
          ,(from-face 'font-lock-string-face)
          ,(from-face 'font-lock-function-name-face)
          ,(from-face 'font-lock-keyword-face)
          ,(from-face 'font-lock-preprocessor-face)
          ])

  (defun shell-mode-xterm-colors ()
    ;; Disable font-lock in shell-mode to improve performance;
    ;; xterm-colors will handle coloring the output.
    (font-lock-mode -1)
    (make-local-variable 'font-lock-function)
    (setq font-lock-function (lambda (_) nil))

    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))
  (add-hook 'shell-mode-hook #'shell-mode-xterm-colors))

(use-package ansi-color
  :custom
  (ansi-color-names-vector
   ["white" "orange red" "green" "yellow" "pale blue" "magenta" "cyan" "tan"]))

;; A mode to handle buffers gotten from stdout:
(use-package stdout-mode)

                                        ; ELISP
(use-package elisp-mode)
(use-package paredit
  :ensure t
  :hook (emacs-lisp-mode . paredit-mode))

                                        ; JENKINSFILES
(use-package jenkinsfile-mode
  :ensure t
  :mode "Jenkinsfile\\'")

                                        ; PYTHON
(use-package pyvenv
  :ensure t)

(use-package auto-virtualenv
  :ensure t
  :after pyvenv
  :hook (python-mode . auto-virtualenv-set-virtualenv))

(use-package python-docstring
  :ensure t)

(use-package python-pytest
  :ensure t
  :bind
  (:map python-mode-map
   ("M-S" . python-pytest-dispatch)))

(defun my-python-lsp-hook ()
  (require 'lsp-python-ms)
  (lsp))
(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . my-python-lsp-hook)
  :init
  (setq lsp-python-ms-executable (executable-find "python-language-server")))

(defun my-flycheck-pycheckers-hook ()
  (flycheck-add-next-checker 'lsp '(t . python-pycheckers)))
(use-package flycheck-pycheckers
  :ensure t
  :after lsp-python-ms
  :hook
  (flycheck-mode . flycheck-pycheckers-setup)
  (python-mode . my-flycheck-pycheckers-hook))

                                        ; THETA

;;; Theta currently only makes sense at work.
(when (eq system-type 'darwin)
  (use-package theta-mode
    :mode "\\.theta\\'"))

                                        ; HASKELL
(use-package haskell-mode
  :ensure t
  :custom
  (haskell-process-type 'cabal-new-repl)
  (haskell-process-args-cabal-new-repl '("--ghc-option=-ferror-spans"))

  (haskell-font-lock-symbols nil)

  ;; Wrap haskell-mode's comamnds in a nix-shell by default:
  (haskell-process-wrapper-function
   (lambda (argv)
     (append (list "nix-shell" "-I" "." "--command")
             (list (mapconcat 'identity argv " ")))))

  :bind  (:map haskell-mode-map
          ("C-c C-s" . haskell-save-and-format)
          ("C-c C-r" . my-haskell-load-and-run))

  :init
  (defun haskell-save-and-format ()
    "Formats the import statements using haskell-stylish and saves
the current file."
    (interactive)
    (save-buffer)
    (haskell-mode-stylish-buffer)
    (save-buffer))

  (defun my-haskell-load-and-run ()
    "Loads and runs the current Haskell file."
    (interactive)
    (let ((start-buffer (current-buffer)))
      (inferior-haskell-load-and-run "main")
      (sleep-for 0 100)
      (end-of-buffer)
      (pop-to-buffer start-buffer)))

  :config
  (require 'haskell-indentation)

  (put 'haskell-process-wrapper-function 'safe-local-variable 'functionp)
  (put 'haskell-process-args-cabal-repl 'safe-local-variable 'listp)
  (put 'haskell-process-type 'safe-local-variable 'symbolp)

  (setq inferior-haskell-find-project-root nil)

  (defun my-inferior-haskell-mode-hook ()
    (local-set-key (kbd "C-a") 'haskell-interactive-mode-beginning)
    (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))
  (add-hook 'inferior-haskell-mode-hook 'my-inferior-haskell-mode-hook)

  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'font-lock-mode))

                                        ; RUST
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook (rust-mode . flycheck-mode))

(use-package cargo
  :ensure t
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode)

  :custom
  (cargo-process--custom-path-to-bin "cargo")
  (cargo-process--rustc-cmd "rustc")

  :config
  (defun my-cargo-process-hook ()
    (defun my-advice-compilation-filter (f proc string)
      (funcall f proc (xterm-color-filter string)))
    (advice-add 'compilation-filter :around #'my-advice-compilation-filter))
  (add-hook 'cargo-process-mode-hook #'my-cargo-process-hook))

(use-package flycheck-rust
  :ensure t
  :after rust-mode
  :hook (rust-mode . flycheck-rust-setup))

                                        ; SKETCH
(use-package sketch-mode
  :mode "\\.sk\\'"
  :after mmm-mode
  :config
  ;;; Add support for sketch files embedded in Haskell:
  (mmm-add-classes
   '((haskell-sketch
      :submode java-mode
      :front   "\\[sketch|\n?"
      :back    "|\\]"))))

                                        ; ARRAYFORTH
(use-package array-forth-mode
  :mode "\\.\\(cfs\\|forth\\)\\'"
  :custom (array-forth-trim-markers t))

                                        ; MARKDOWN
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"

  :custom
  (markdown-enable-math t)

  :config
  (defun my-markdown-hook ()
    (message "My Markdown hook!")
    (flyspell-mode)
    (visual-line-mode 1)
    (visual-fill-column-mode 1)
    (flyspell-buffer)
    (local-unset-key (kbd "C-M-b"))
    (local-unset-key (kbd "C-M-f")))
  (add-hook 'markdown-mode-hook 'my-markdown-hook))

                                        ; LATEX

(use-package tex-mode
  :config
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'autofill-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))

(global-auto-revert-mode t)
(use-package doc-view
  :custom (doc-view-continuous 1)
  :config
  (defun my-doc-view-hook ()
    "Make documents refresh faster."
    (set (make-local-variable 'auto-revert-interval) 0.5)
    (auto-revert-set-timer))
  (add-hook 'doc-view-mode-hook 'my-doc-view-hook))

                                        ; CFDG
(use-package cfdg-mode :mode "\\.cfdg\\'")

                                        ; TPL
(use-package tpl-mode)

                                        ; CS 164
(use-package cs164-mode
  :custom
  (cs164-base-directory "~/Documents/cs/164/p/2/cs164sp12/pa2/")
  (cs164-grammar "cs164b.grm"))

                                        ; WEB DEVELOPMENT
(use-package browse-url
  :custom
  (browse-url-generic-program "firefox")
  (browse-url-browser-function 'browse-url-generic))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"

  :custom
  (js2-basic-offset 2))

(use-package css-mode
  :mode "\\(\\.css\\|\\.less\\)\\'"

  :custom
  (css-indent-offset 2))

(use-package sgml-mode
  :mode "\\(\\.php\\|\\.html\\)\\'")

                                        ; COMMANDS
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)
