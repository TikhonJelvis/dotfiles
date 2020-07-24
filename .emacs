                                        ; CUSTOM-SET
;; Put custom-set variables in a different file:
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

                                         ; PACKAGE INITIALIZATION
(add-to-list 'load-path "~/.emacs.d/packages")

;; Configure package management:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; Make sure all the selected packages are installed. This ensured I
;; have the same set of Emacs packages available across all my
;; machines.
(package-install-selected-packages)

                                        ; MAC-SPECIFIC SETTINGS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

                                        ; EXEC PATH
;; Make sure Emacs sees executables from Nix correctly.
(require 'exec-path-from-shell)
(let ((nix-vars '("NIX_LINK"
                  "NIX_PATH"
                  "SSL_CERT_FILE")))
  (exec-path-from-shell-initialize) ; $PATH, $MANPATH and set exec-path
  (mapcar 'exec-path-from-shell-copy-env nix-vars))

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

(global-set-key (kbd "C-M-S-N") (lambda () (interactive) (swap-with 'down)))
(global-set-key (kbd "C-M-S-P") (lambda () (interactive) (swap-with 'up)))
(global-set-key (kbd "C-M-S-B") (lambda () (interactive) (swap-with 'left)))
(global-set-key (kbd "C-M-S-F") (lambda () (interactive) (swap-with 'right)))

(global-set-key (kbd "M-N") 'windmove-down)
(global-set-key (kbd "M-P") 'windmove-up)
(global-set-key (kbd "M-B") 'windmove-left)
(global-set-key (kbd "M-F") 'windmove-right)

(ido-mode t)
(setq ido-default-buffer-method 'selected-window)

;; I don't like tabs very much:
(setq-default indent-tabs-mode nil)

;; Unique buffer names:
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator " • ")

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
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 15)))))
   `(company-scrollbar-fg ((t (:background "DarkOrange"))))
   `(company-tooltip-selection ((t (:background ,(color-lighten-name bg 20)))))
   `(company-tooltip-common ((t (:inherit font-lock-builtin-face))))
   `(company-tooltip-annotation ((t (:inherit font-lock-builtin-face))))))

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

;; Icons that I can use in dired, buffer mode lines... etc
(require 'all-the-icons)

;; Prettier mode line
(load-file "~/.emacs.d/mode-line.el")

                                        ; KEY REBINDINGS
;; Do nothing on C-x C-c:
(global-unset-key (kbd "C-x C-c"))
;; I'm phasing C-x o out:
(global-set-key (kbd "C-x o") 'other-frame)

;; Set C-x C-b to switching buffer—for some reason, I always hit by
;; accident. It's annoying!
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-S-b") 'list-buffers)

;; Make complete tag not be alt-tab!
(global-set-key (kbd "M-s-<return>") 'complete-tag)

;; Some nice keyboard shortcuts:
(global-set-key (kbd "C-x 5 3") 'make-frame-command)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-j") 'compile)
(global-set-key (kbd "C-c C-a") 'align-regexp)
(global-set-key (kbd "M-#") 'ispell-complete-word)

;; C-w remap:
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Get rid of column editing which I trigger by accident and find
;; incredibly annoying:
(global-unset-key (kbd "<f2>"))

                                        ; INPUT MODES
(setq default-input-method "TeX")

					; DIRED
;; Has to be above JABBER settings because it has a conflicting
;; keybinding :(.
(require 'dired-x)

;; Automatically omit “uninteresting” files from the listing. (Toggled
;; with M-o.)
(add-hook 'dired-mode-hook 'dired-omit-mode)

;; Simplify the dired view by hiding permissions, users, date
;; modified... etc
(setq dired-hide-details-hide-symlink-targets nil)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Automatically update dired buffers when the directory changes:
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Display icons by dired files
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

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
;;   2. Is aspell installed to your Nix user profile? Run nix/switch
;;   to make sure.
(setq ispell-program-name "~/.nix-profile/bin/aspell")

(add-hook 'flyspell-mode-hook '(lambda ()
				(set-face-attribute 'flyspell-duplicate nil
						    :foreground nil
						    :underline "dark orange"
						    :bold nil)
				(set-face-attribute 'flyspell-incorrect nil
						    :foreground nil
						    :underline "red"
						    :bold nil)))


                                        ; JSON
;; Set the indent level to 4 for JSON files, making it buffer local to not
;; change .js files.
(defun json-indent-hook ()
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 4))
(add-hook 'json-mode-hook 'json-indent-hook)

                                        ; MAGIT
(require 'magit)
(customize-set-variable 'magit-commit-ask-to-stage 'stage)
(global-set-key (kbd "C-x g") 'magit-status)

(defun my-git-commit-setup-hook ()
  (visual-line-mode 1)
  (visual-fill-column-mode 1))

(remove-hook 'git-commit-setup-hook 'git-commit-turn-on-auto-fill)
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
(add-hook 'git-commit-setup-hook 'my-git-commit-setup-hook)

                                        ; ORG-MODE
(require 'org)

;; Making Org mode prettier

(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

(defun org-mode-prettify-hook ()
  "Configure prettify-symbols to replace todo/consider/done with
  pretty Unicode characters."
  (push '("TODO" . "☛") prettify-symbols-alist)
  (push '("CONSIDER" . "❓") prettify-symbols-alist)
  (push '("DONE" . "✔") prettify-symbols-alist)
  (prettify-symbols-mode 1))
(add-hook 'org-mode-hook 'org-mode-prettify-hook)
(add-hook 'org-agenda-mode-hook 'org-mode-prettify-hook)

(setq org-agenda-scheduled-leaders '("" " %2d×"))

(setq org-agenda-prefix-format
      '((agenda . " %i %-8t% s")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))
(setq org-agenda-remove-times-when-in-prefix 'beg)
(setq org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        " ∘ " "┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈"))
(setq org-agenda-current-time-string "◀ ┈┈┈┈┈┈┈┈ now ┈┈┈┈┈┈┈┈")

;; Extra states I use
(setq org-todo-keywords '((sequence "TODO" "|" "DONE")
                          (sequence "CONSIDER" "TODO" "|" "DONE")
                          (sequence "PROJECT" "|" "DONE")))


;; Globally accessible org commands
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(defun unset-agenda-binding () (local-unset-key (kbd "C-c C-a")))
(add-hook 'comint-mode-hook 'unset-agenda-binding)

;; Agenda configuration
(setq org-agenda-window-setup 'other-window)

;; My core *.org files are stored in Dropbox unless I'm on a work
;; computer. (Only place I would use macOS!)
(when (not (eq system-type 'darwin))
  (setq org-directory "~/Dropbox/org"))

(setq org-agenda-files
      (list (concat org-directory "/Tasks.org")
            (concat org-directory "/Books.org")))
(setq org-default-notes-file (concat org-directory "/Notes.org"))

(setq org-capture-templates
      '(("t" "Todo" entry (file "Tasks.org")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:")))

;; Spellcheck my org mode files.
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

;; Allow markup in the middle of words.
(setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
(setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

;; Configuring title page formatting with #+OPTION is too fiddly, so
;; we want to override the elisp variable instead
(put 'org-reveal-title-slide 'safe-local-variable 'stringp)


(defun my-org-mode-hook ()
  (local-set-key (kbd "M-{") 'outline-previous-visible-heading)
  (local-set-key (kbd "M-}") 'outline-next-visible-heading)
  (local-set-key (kbd "C-c C-,") 'org-promote-subtree)
  (local-set-key (kbd "C-c C-.") 'org-demote-subtree))
(add-hook 'org-mode-hook 'my-org-mode-hook)

                                        ; SHELL BUFFERS
;; Stop Emacs from expanding things like !! in history
(setq comint-input-autoexpand 'nil)

;; Clear comint buffers with C-c C-k. A lot more useful than the
;; standard binding of C-c C-k killing the buffer's process!
(defun my-comint-hook ()
  (local-set-key (kbd "C-c C-k") 'comint-clear-buffer))
(add-hook 'comint-mode-hook 'my-comint-hook)

;; I want an easy command for opening new shells:
(defun new-shell (name)
  "Opens a new shell buffer with the given name in
asterisks (*name*) in the current directory with and changes the
prompt to name>."
  (interactive "sName: ")
  (when (equal name "")
    (setq name (file-name-base (directory-file-name default-directory))))
  (pop-to-buffer (concat "<*" name "*>"))
  (unless (eq major-mode 'shell-mode)
    (shell (current-buffer))
    (comint-simple-send (get-buffer-process (current-buffer))
                        "export PAGER=epage")
    (comint-simple-send (get-buffer-process (current-buffer))
                        (concat "export PS1=\"\033[33m" name "\033[0m:\033[35m\\W\033[0m>\""))


    (sleep-for 0 200)
    (comint-send-input)
    (comint-clear-buffer)))
(global-set-key (kbd "C-c s") 'new-shell)

;; ANSI colors in shell mode would be nice by default:
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq ansi-color-names-vector ["white" "orange red" "green" "yellow" "pale blue" "magenta" "cyan" "tan"])

;; A mode to handle buffers gotten from stdout:
(require 'stdout-mode)

                                        ; DIRENV

;; Our nix environment is quite large so the summary messages that direnv-mode
;; provides can be a bit annoying. You may add these lines to suppress the
;; message once you confirmed that everything works.
(setq direnv-show-paths-in-summary nil)
(setq direnv-always-show-summary nil)

                                        ; ELISP
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

                                        ; JENKINSFILES
(require 'jenkinsfile-mode)
(add-to-list 'auto-mode-alist '("Jenkinsfile" . jenkinsfile-mode))

                                        ; PYTHON
(setq enable-local-eval t)
(put 'python-shell-interpreter 'safe-local-variable t)
(put 'python-shell-interpreter-args 'safe-local-variable 'stringp)

(elpy-enable)

                                        ; THETA

;;; Theta currently only makes sense at work.
(when (eq system-type 'darwin)
  (load "~/Programming/theta/emacs/theta-mode.el")
  (require 'theta-mode)
  (add-to-list 'auto-mode-alist '("\\.theta" . theta-mode)))

                                        ; HASKELL
;; Load Haskell mode:
(require 'haskell-mode)
(require 'haskell-indentation)

(setq haskell-process-type 'cabal-new-repl)
(setq haskell-process-args-cabal-new-repl '("--ghc-option=-ferror-spans"))

;; Wrap haskell-mode's comamnds in a nix-shell by default:
(setq haskell-process-wrapper-function
      (lambda (argv)
        (append (list "nix-shell" "-I" "." "--command")
                (list (mapconcat 'identity argv " ")))))

(put 'haskell-process-wrapper-function 'safe-local-variable 'functionp)
(put 'haskell-process-args-cabal-repl 'safe-local-variable 'listp)

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
    (inferior-haskell-load-and-run inferior-haskell-run-command)
    (sleep-for 0 100)
    (end-of-buffer)
    (pop-to-buffer start-buffer)))
(setq inferior-haskell-run-command "main")

(defun my-haskell-mode-hook ()
  (local-set-key (kbd "C-c C-s") 'haskell-save-and-format)
  (local-set-key (kbd "C-c C-r") 'my-haskell-load-and-run))

(defun my-inferior-haskell-mode-hook ()
  (local-set-key (kbd "C-a") 'haskell-interactive-mode-beginning)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
(add-hook 'inferior-haskell-mode-hook 'my-inferior-haskell-mode-hook)
(setq haskell-font-lock-symbols nil)
(setq delete-selection-mode nil)

(setq inferior-haskell-find-project-root nil)

;; hpaste integration
(load "hpaste/hpaste")
(require 'hpaste)

                                        ; SKETCH
(require 'sketch-mode)
(add-to-list 'auto-mode-alist '("\\.sk" . sketch-mode))

;;; Add support for sketch files embedded in Haskell:
(mmm-add-classes
 '((haskell-sketch
    :submode java-mode
    :front   "\\[sketch|\n?"
    :back    "|\\]")))
;; (mmm-add-mode-ext-class nil nil 'haskell-sketch)

                                        ; FORTH
(defun forth-load-current-file ()
  "Loads the currently visited file into a running forth process."
  (interactive)
  (save-buffer)
  (let ((name (buffer-file-name)))
    (unless (and forth-process-buffer
                 (get-buffer forth-process-buffer)
                 (get-buffer-process forth-process-buffer))
      (run-forth forth-program-name))
    (forth-load-file name)
    (pop-to-buffer forth-process-buffer)
    (end-of-buffer)))
(add-hook 'forth-mode-hook
          '(lambda () (local-set-key (kbd "C-c C-l") 'forth-load-current-file)))

                                        ; ARRAYFORTH
(require 'array-forth-mode)
(add-to-list 'auto-mode-alist '("\\.\\(cfs\\|forth\\)" . array-forth-mode))
(setq array-forth-trim-markers t)

                                        ; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(defun my-markdown-hook ()
  (message "My Markdown hook!")
  (flyspell-mode)
  (visual-line-mode 1)
  (visual-fill-column-mode 1)
  (flyspell-buffer)
  (local-unset-key (kbd "C-M-b"))
  (local-unset-key (kbd "C-M-f")))
(add-hook 'markdown-mode-hook 'my-markdown-hook)
(setq markdown-enable-math t)

                                        ; LATEX
(defun my-LaTeX-hook ()
  "Turn on wrapping and spell-check for LaTeX documents."
  (flyspell-mode)
  (auto-fill-mode))
(add-hook 'LaTeX-mode-hook 'my-LaTeX-hook)

;; PDF Mode by default:
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode) ;;pdf mode by default

;; Continuous scroll when viewing pdfs:
(setq doc-view-continuous 1)

;; Automatically update pdfs (and other buffers):
(global-auto-revert-mode t)
(defun my-doc-view-hook ()
  "Make documents refresh faster."
  (set (make-local-variable 'auto-revert-interval) 0.5)
  (auto-revert-set-timer))
(add-hook 'doc-view-mode-hook 'my-doc-view-hook)

;; Pandoc stuff:
(setq pandoc-binary "pandoc")

                                        ; CFDG
(require 'cfdg-mode)
(add-to-list 'auto-mode-alist '("\\.cfdg" . cfdg-mode))

                                        ; TPL
(require 'tpl-mode)

                                        ; CS 164
(require 'cs164-mode)
(setq cs164-base-directory "~/Documents/cs/164/p/2/cs164sp12/pa2/")
(setq cs164-grammar "cs164b.grm")

                                        ; WEB DEVELOPMENT
;; Make the default browser googly chrome:
(setq browse-url-generic-program "firefox"
      browse-url-browser-function 'browse-url-generic)

;; Make JS-2 mode the default:
(add-to-list 'auto-mode-alist '("\\.js" . javascript-mode))
(setq js2-basic-offset 2)
(setq js-indent-level 2)

;; Edit .less files with css mode:
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

(setq css-indent-offset 2)

;; I don't do much php, so let's edit it with html mode:
(add-to-list 'auto-mode-alist '("\\.php$" . html-mode))

                                        ; COMMANDS
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)
