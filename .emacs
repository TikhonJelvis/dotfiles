                                         ; PACKAGE INITIALIZATION
(add-to-list 'load-path "~/.emacs.d/packages")

;; Configure package management:
(require 'package)
(add-to-list 'package-archives 
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

                                        ; SECRETS
;; Load my secrets file that contains passwords, keys and so on.
(load "~/secrets.el")

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

;; Take screenshots with a keystroke. You need the window ID, which
;; can be found via `xwininfo -display :0'.
;; 
;; You can preview the animation with `animate -delay 35 *.png' and
;; actually create it with `convert -delay 35 *.png out.gif'.
(defun screenshot-frame ()
  "Take a screenshot of 400x200 pixels of the Emacs frame."
  (interactive)
  (shell-command-to-string
   "sleep 1;import -window 0x5e000c3 +repage /home/tikhon/Documents/tmp/frames/`date +%s`.png"))
(global-set-key (kbd "<f8>") 'screenshot-frame)

(defun my-screenshot-hook ()
  (screenshot-frame))

(defun start-screenshots ()
  "Start taking screenshots after every single command."
  (interactive)
  (add-hook 'post-command-hook 'my-screenshot-hook))

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

(defcustom git-grep-command "git --no-pager grep --no-color --line-number <C> <R>"
  "The command to run with M-x git-grep.")
(defun git-grep (regexp)
  "Search for the given regexp using `git grep' in the current directory."
  (interactive "sRegexp: ")
  (unless (boundp 'grep-find-template) (grep-compute-defaults))
  (let ((old-command grep-find-template))
    (grep-apply-setting 'grep-find-template git-grep-command)
    (rgrep regexp "*" "")
    (grep-apply-setting 'grep-find-template old-command)))

                                        ; GENERAL STUFF
;; Auto-complete stuff
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(ac-flyspell-workaround)

(global-set-key (kbd "M-TAB") 'auto-complete)

;; Have compile scroll to the end by default.
(setq-default compilation-scroll-output 'foo-bar)

;; Flymake stuff
(setq flymake-cursor-error-display-delay 0.1)

;; Flyspell stuff
(setq ispell-program-name "/run/current-system/sw/bin/aspell")

;; Emerge settings (I still sometimes use emerge with git mergetool)
(setq emerge-diff-options "--ignore-all-space")

;; Flyspell stuff
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
(setq visible-bell t)
(show-paren-mode 1)
(column-number-mode t)
(transient-mark-mode -1)
(setq mark-even-if-inactive t)
(setq-default truncate-lines t)

;; Auto-indenting on RET:
(add-hook 'c-mode-common-hook '(lambda ()
                                 (local-set-key (kbd "RET") 
                                                'newline-and-indent)))
(add-hook 'python-mode-hook '(lambda ()
			       (local-set-key (kbd "RET") 
					      'newline-and-indent)))

;; I don't like tabs very much:
(setq-default indent-tabs-mode nil)

;; Auto indent pasted code in some modes:
(defvar indent-paste-modes '(emacs-lisp-mode lisp-mode clojure-mode scheme-mode
                             ruby-mode rspec-mode python-mode c-mode
                             c++-mode objc-mode latex-mode plain-tex-mode
                             css-mode less-css-mode))

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode indent-paste-modes)
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;; For enabling color themes:
(setq custom-theme-directory "~/.emacs.d/themes/")
(setq custom-safe-themes t)
(load-theme 'blackboard)

;;Make the window simpler:
(tool-bar-mode -1)
(scroll-bar-mode -1) 
(menu-bar-mode -1)
(fringe-mode 0)

;; No $ displayed for truncated lines
(set-display-table-slot standard-display-table 0 ?\ ) 

;; For some reason, I need to set the cursor color explicitly.
(add-to-list 'default-frame-alist '(cursor-color . "#AEAEAE"))

;; Fill to 80 characters by default:
(setq fill-column 80)

;; Now make it prettier:
(require 'powerline)
(set-face-attribute 'mode-line nil
                    :foreground "Black"
                    :background "DarkOrange"
                    :box nil)
(setq powerline-arrow-shape 'diagonal)
(setq-default mode-line-format '("%e"
  (:eval
   (concat
    (powerline-rmw 'left nil)
    (powerline-buffer-id 'left nil powerline-color1)
    (powerline-minor-modes 'left powerline-color1)
    (powerline-narrow 'left powerline-color1 powerline-color2)
    (powerline-vc 'center powerline-color2)
    (powerline-make-fill powerline-color2)
    (powerline-row 'right powerline-color1 powerline-color2)
    (powerline-make-text ":" powerline-color1)
    (powerline-column 'right powerline-color1)
    (powerline-percent 'right nil powerline-color1)
    (powerline-make-text "  " nil)))))

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

;; Get rid of column editing which I trigger by accident and find
;; incredibly annoying:
(global-unset-key (kbd "<f2>"))

                                        ; DIRED
(require 'dired-x)

;; Automatically omit "uninteresting" files from the listing. (Toggled with M-o.)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
(global-unset-key (kbd "C-x C-j"))

                                        ; MMM-MODE
(require 'mmm-mode)

(setq mmm-global-mode 'maybe)           ; Load mmm-mode when appropriate
(set-face-background 'mmm-default-submode-face "#2C3041")

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
;; I want an easy command for opening new shells:
(defun new-shell (name)
  "Opens a new shell buffer with the given name in
asterisks (*name*) in the current directory with and changes the
prompt to name>."
  (interactive "sName: ")
  (pop-to-buffer (concat "*" name "*"))
  (unless (eq major-mode 'shell-mode)
    (shell (current-buffer))
    (sleep-for 0 200)
    (delete-region (point-min) (point-max))
    (comint-simple-send (get-buffer-process (current-buffer)) 
                        (concat "export PS1=\"\033[33m" name "\033[0m:\033[35m\\W\033[0m>\""))))
(global-set-key (kbd "C-c s") 'new-shell)

;; ANSI colors in shell mode would be nice by default:
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq ansi-color-names-vector ["white" "orange red" "green" "yellow" "pale blue" "magenta" "cyan" "tan"])

;; A mode to handle buffers gotten from stdout:
(require 'stdout-mode)

                                        ; ELISP
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

                                        ; HASKELL
;; Load Haskell mode:
(require 'haskell)
(require 'haskell-indentation)

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
(setq haskell-font-lock-symbols t)
(setq delete-selection-mode nil)

(setq inferior-haskell-find-project-root nil)

;; hpaste integration
(load "hpaste/hpaste")
(require 'hpaste)

                                        ; OCAML

(setq tuareg-font-lock-symbols t)

(add-to-list 'auto-mode-alist '("\\.atd" . tuareg-mode))

                                        ; JAVA
;; use tabs (I guess that's what Eclipse does by default?)
(defun my-java-indent-tabs-hook ()
  (setq c-basic-offset 2
        tab-width 2
        indent-tabs-mode nil))
(add-hook 'java-mode-hook 'my-java-indent-tabs-hook)

;; ignore warnings in *compilation* buffer:
(setq compilation-skip-threshold 2)

;;; display errors and warnings at point
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

                                        ; XML
(defun my-nxml-hook ()
  (local-unset-key (kbd "M-{"))
  (local-unset-key (kbd "M-}")))
(add-hook 'nxml-mode-hook 'my-nxml-hook)

                                        ; ARDUINO
(add-to-list 'auto-mode-alist '("\\.pde" . java-mode))

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
  (pandoc-mode)
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

                                        ; CUSTOM-SET STUFF
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(package-selected-packages
   (quote
    (nix-mode typescript-mode paredit mmm-mode haskell-mode auto-complete)))
 '(send-mail-function (quote sendmail-send-it))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 112 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
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
