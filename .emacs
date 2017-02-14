                                         ; PACKAGE INITIALIZATION
(add-to-list 'load-path "~/.emacs.d/packages")

;; Configure package management:
(require 'package)
(add-to-list 'package-archives 
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

                                        ; TERRIBLE MAC HACKS
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(require 'exec-path-from-shell)
(let ((nix-vars '("NIX_LINK"
                  "NIX_PATH"
                  "SSL_CERT_FILE")))
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize) ; $PATH, $MANPATH and set exec-path
    (mapcar 'exec-path-from-shell-copy-env nix-vars)))

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

;; Flyspell stuff
(setq ispell-program-name "/Users/z0028sn/.nix-profile/bin/aspell")
(add-hook 'flyspell-mode-hook '(lambda ()
				(set-face-attribute 'flyspell-duplicate nil
						    :foreground nil
						    :underline "dark orange"
						    :bold nil)
				(set-face-attribute 'flyspell-incorrect nil
						    :foreground nil
						    :underline "red"
						    :bold nil)))

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

;; Some minor preferences:
;; (setq visible-bell 'nil)
(setq ring-bell-function 'ignore)
(show-paren-mode 1)
(column-number-mode t)
(transient-mark-mode -1)
(setq mark-even-if-inactive t)
(setq-default truncate-lines t)

;; I don't like tabs very much:
(setq-default indent-tabs-mode nil)

;; For enabling color themes:
(setq custom-theme-directory "~/.emacs.d/themes/")
(setq custom-safe-themes t)
(load-theme 'blackboard)

;;Make the window simpler:
(tool-bar-mode -1)
(scroll-bar-mode -1) 
(menu-bar-mode 1) ;; mac-specific: menu-bar-mode needed for fullscreen, for some reason?
(fringe-mode 0)

;; No $ displayed for truncated lines
(set-display-table-slot standard-display-table 0 ?\ )

;; Fill to 80 characters by default:
(setq fill-column 80)

;; Now make it prettier:
(require 'powerline)
(powerline-default-theme)

(setq powerline-default-separator 'box)
(setq powerline-utf-8-separator-left #x25E3)
(setq powerline-utf-8-separator-right #x25E2)
(setq powerline-height 17)
(setq powerline-gui-use-vcs-glyph 'nil)

(set-face-attribute 'mode-line nil
                    :foreground "Black"
                    :background "DarkOrange"
                    :box nil)

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

;; make text-mode the default major mode
(setq default-major-mode 'text-mode)

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
(global-set-key (kbd "M-<return>") 'complete-tag)

;; Some nice keyboard shortcuts:
(global-set-key (kbd "C-x 5 3") 'make-frame-command)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-j") 'compile)
(global-set-key (kbd "C-c a") 'align-regexp)
(global-set-key (kbd "M-#") 'ispell-complete-word)

;; C-w remap:
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Get rid of column editing which I trigger by accident and find incredibly annoying:
(global-unset-key (kbd "<f2>"))

					; DIRED
;; Has to be above JABBER settings because it has a conflicting keybinding :(.
(require 'dired-x)

;; Automatically omit "uninteresting" files from the listing. (Toggled with M-o.)
(add-hook 'dired-mode-hook 'dired-omit-mode)

                                        ; ORG-MODE
;; Spellcheck my org mode files.
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

;; Allow markup in the middle of words.
;; (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
;; (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
;; (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

(defun my-org-mode-hook ()
  (local-set-key (kbd "M-{") 'outline-previous-visible-heading)
  (local-set-key (kbd "M-}") 'outline-next-visible-heading))
(add-hook 'org-mode-hook 'my-org-mode-hook)

                                        ; SHELL BUFFERS
;; Stop Emacs from expanding things like !! in history
(setq comint-input-autoexpand 'nil)

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
    (sleep-for 0 200)
    (delete-region (point-min) (point-max))
    (comint-simple-send (get-buffer-process (current-buffer)) 
                      (concat "export PS1=\"\033[33m" name "\033[0m:\033[35m\\W\033[0m>\""))))
(global-set-key (kbd "C-c s") 'new-shell)

;; ANSI colors in shell mode would be nice by default:
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq ansi-color-names-vector ["white" "light red" "green" "yellow" "pale blue" "magenta" "cyan" "tan"])

;; A mode to handle buffers gotten from stdout:
(require 'stdout-mode)

                                        ; ELISP
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

                                        ; HASKELL

;; Load Haskell mode:
(require 'haskell)
(require 'haskell-indentation)

;; Don't use stack for running Haskell projects:
(setq haskell-process-type 'cabal-repl)

;; Wrap haskell-mode's comamnds in a nix-shell by default:
(setq haskell-process-wrapper-function
      (lambda (argv)
        (append (list "nix-shell" "-I" "." "--command" )
                (list (mapconcat 'identity argv " ")))))

(add-to-list 'safe-local-variable-values
             '(haskell-process-wrapper-function . (lambda (argv)
                                                    (append (list "nix-shell" "-I" "." "--command" )
                                                            (list (mapconcat 'identity argv " "))))))

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
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

;;; More custom font lock symbols:
;; (add-to-list 'haskell-font-lock-symbols-alist '("Nat" . ?ℕ))

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
(add-hook 'inferior-haskell-mode-hook 'my-inferior-haskell-mode-hook)
(setq haskell-font-lock-symbols nil)
(setq delete-selection-mode nil)

(setq inferior-haskell-find-project-root nil)

                                        ; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(defun my-markdown-hook ()
  (message "My Markdown hook!")
  (flyspell-mode)
  (visual-line-mode 1)
  (flyspell-buffer)
  (local-unset-key (kbd "C-M-b"))
  (local-unset-key (kbd "C-M-f"))  )
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

                                        ; CUSTOM-SET STUFF
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(package-selected-packages
   (quote
    (powerline wgrep yaml-mode paredit ox-reveal nix-mode markdown-mode jabber haskell-mode exec-path-from-shell elm-mode bash-completion)))
 '(send-mail-function (quote sendmail-send-it))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 141 :width normal :foundry "nil" :family "DejaVu Sans Mono"))))
 '(erc-input-face ((t (:foreground "cornflower blue"))))
 '(erc-my-nick-face ((t (:foreground "CornflowerBlue" :weight bold))))
 '(flycheck-error ((t (:underline "red"))))
 '(flycheck-warning ((t (:underline "darkorange"))))
 '(flymake-errline ((t (:background "#00000000" :underline "red"))))
 '(flymake-warnline ((t (:background "#00000000" :underline "dark orange"))))
 '(sgml-namespace ((t (:inherit font-lock-builtin-face)))))

                                        ; COMMANDS
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)
