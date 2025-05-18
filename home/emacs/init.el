                                        ; PERFORMANCE
;; I precompile my packages with Nix, so deferred compilation is
;; unnecessary and can be slow/buggy.
(setq comp-deferred-compilation nil)

;; This setting *seems* to make Org Agenda commands faster, but I did
;; not measure it very carefully.
;;
;; In profiling, it reduced GC from 53% to 22% for some *quick* tests:
;; opening an agenda and moving back and forth between the weeks.
(setq gc-cons-percentage 0.5)

;; LSP mode suggests increasing gc-cons-threshold. Not 100% how this
;; interact with gc-cons-percentage, but seems like the worst case is
;; that the larger of the two settings always dominates, which should
;; be fine for performance.
;;
;; Unlike gc-cons-percentage, I haven't done any measurements *or*
;; benchmarks with setting.
(setq gc-cons-threshold 100000000)

;; Some LSP server processes send large (1–3M) responses. This setting
;; lets Emacs process responses up to 3M at a time.
(setq read-process-output-max (* 3 1024 1024))

                                        ; CUSTOM-SET
(setq custom-file (dotfile "emacs/custom.el"))
(load custom-file)

                                        ; PERSONAL PACKAGES
(add-to-list 'load-path "~/.emacs.d/packages")
(add-to-list 'load-path (dotfile "emacs/packages"))

                                        ; WORK
(defun is-work ()
  "Am I on a work computer?"
  (string= (system-name) "tikhon-nixos-mercury"))

                                        ; MAC-SPECIFIC SETTINGS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)

  (global-set-key (kbd "C-M-c") 'toggle-frame-fullscreen))

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
interface and inserts it at point.

Enters or returns the expanded absolute path to the chosen file."
  (interactive "P")
  (let ((action (if add-to-kill-ring 'kill-new 'insert))
        (path (expand-file-name (read-file-name "file path: "))))
    (funcall action path)))
(global-set-key (kbd "C-c f") 'file-name-at-point)


					; GENERAL STUFF
;; Have compile scroll to the end by default.
(setq-default compilation-scroll-output 'foo-bar)

;; Using X-resources on KDE leads to the wrong cursor color in new
;; frames :/
(setq inhibit-x-resources t)

;; I don't like tabs very much:
(setq-default indent-tabs-mode nil)

;; Hack for typing backticks with QMK keyboard config
;;
;; QMK uses super + ESC to type a `, but Emacs reads that as s-`
(global-set-key (kbd "s-`") (lambda () (interactive) (insert "`")))

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

;; Variable-width typeface for prose
(defface prose
  '((t (:inherit default :weight normal :width normal :family "Junction" :height 1.1)))
  "A variable-width for non-code prose—paragraphs of text, markdown... etc.")
(defvar-local prose-original-line-spacing 0.0
  "Line spacing set in this buffer *before* `prose-mode' was
turned on. `prose-mode' sets `line-spacing' to a different value,
and this variable lets `prose-mode' reset `line-spacing' when
it's turned off again.")
(defvar-local prose-mode nil "Is `prose-mode' active in the current buffer?")
(defun prose-mode (&optional arg)
  "Minor mode that sets the buffer's default face to `prose'."
  (interactive "P")
  (message "arg: %s" arg)
  (if (or arg (not prose-mode))
      (progn
        (setq prose-original-line-spacing line-spacing)
        (setq line-spacing 0.2)
        (setq prose-mode t))
    (setq line-spacing prose-original-line-spacing)
    (setq prose-mode nil))

  (buffer-face-mode-invoke 'prose 'toggle (called-interactively-p 'interactive)))

;; Trying out a variable-width font for programming by default.
(add-hook 'prog-mode-hook #'variable-pitch-mode)
(add-hook 'dired-mode #'variable-pitch-mode)

;; Keep using the monospace font for Haskell to support space-based alignment.
(defun monospace-mode ()
  "Explicitly turn off `variable-pitch-mode' for the buffer."
  (variable-pitch-mode -1))
(add-hook 'haskell-mode-hook #'monospace-mode)
(add-hook 'nix-mode-hook #'monospace-mode)
(add-hook 'theta-mode-hook #'monospace-mode)

;; Change font size based on resolution
;;
;; Code based on
;; https://www.reddit.com/r/emacs/comments/dpc2aj/readjusting_fontsize_according_to_monitor/f5uasez/
(defun frame-pixel-density (&optional frame)
  "Return the pixel density (in px/mm) for FRAME's display. Use the
current frame if FRAME is nil."
  (unless frame (setq frame (selected-frame)))
  (let* ((attrs (frame-monitor-attributes frame))
         (mm (apply 'max (cdr (assoc 'mm-size attrs))))
         (px (apply 'max (cdddr (assoc 'geometry attrs)))))
    (/ (float px) mm)))

(defun frame-monitor-name (&optional frame)
  "Return the name of FRAME's display. Use the current frame if
FRAME is nil."
  (unless frame (setq frame (selected-frame)))
  (frame-monitor-attribute 'name frame))

(defvar basis-font-size 110
  "The font size that works well on my 27” 1440p display (with a
pixel density of ≈4.29). Resolution-based font-size adjustment
will try to keep the actual font size the same across different
screens.")

(defvar basis-font-size-override
  '((("tikhon-nixos-x1" "eDP-1") . 45)
    (("tikhon-nixos-framework" "eDP-1") . 80)
    (("tikhon-nixos-mercury" "eDP-1") . 80)
    (("tikhon-nixos-mercury" "combined screen") . 160)
    ;; HDMI connection (for 1080p monitor in Tahoe)
    (("tikhon-nixos-framework" "DP-3") . 120))
  "Specific monitors for which I want a different font size
configured. This is an alist mapping hostname (`system-name') +
output name to a value to use for `basis-font-size' on that
monitor.")

(defun adjusted-font-size (&optional frame)
  "Return the adjusted font size for the given frame, based on
the resolution of the frame's current display.

My 27” 1440p display has a pixel density of ≈4.29 and works well
at the basis font size, so I use that as my basis and change it
proportionately."
  (unless frame (setq frame (selected-frame)))
  (let* ((monitor-key (list system-name (frame-monitor-name frame)))
         (override (assoc monitor-key basis-font-size-override))
         (basis (if override (cdr override) basis-font-size)))
    (round (* (frame-pixel-density frame) (/ basis 4.29)))))

(defun auto-adjust-font-size (&optional frame)
  "Automatically set the font size based on the resolution of the
frame's current display. See `adjusted-font-size' for details. "
  (interactive)
  (unless frame (setq frame (selected-frame)))
  (set-face-attribute 'default frame :height (adjusted-font-size frame)))

(defun auto-adjust-size-if-frame-changed (frame)
  "Call `auto-adjust-font-size' only if the frame has changed
size. Designed to work with `window-size-change-functions'."
  (when (frame-size-changed-p)
    (auto-adjust-font-size frame)))

;; For some reason, my font size adjusting code was pretty
;; inconsistent on macOS, and I don't feel like debugging it. I just
;; end up adjusting the size manually when I hook my macbook up to a
;; new display anyway...
(unless (eq system-type 'darwin)
  (add-hook 'focus-in-hook #'auto-adjust-font-size)
  (add-hook 'after-make-frame-functions #'auto-adjust-font-size)
  (add-hook 'window-size-change-functions #'auto-adjust-size-if-frame-changed))

;; For enabling color themes:
(setq custom-theme-directory (dotfile "emacs/themes"))
(setq custom-safe-themes t)
(load-theme 'blackboard t)

;;; the theme definition for my version of blackboard uses color
;;; functions like `color-lighten-name' which depend on the selected
;;; frame
;;;
;;; this was causing incorrect behavior when Emacs is started up
;;; /without/ a frame (ie starting a server before launching a
;;; client), so color calculations would be performed incorrectly (!)
;;;
;;; as a hacky workaround, we can just (re)load the theme the first
;;; time that we create a new frame
(defvar frame-created nil "Has a frame been created before?")
(defun load-theme-after-frame-hook ()
  "Load my default theme when a frame is created for the first time by a server."
  (unless frame-created
    (load-theme 'blackboard t))
  (setq frame-created t))
(add-hook 'server-after-make-frame-hook #'load-theme-after-frame-hook)

;;Make the window simpler:
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; mac-specific: menu-bar-mode needed for fullscreen, for some reason?
(if (eq system-type 'darwin)
    (menu-bar-mode 1)
  (menu-bar-mode -1))

(fringe-mode 12)

;; No $ displayed for truncated lines
(set-display-table-slot standard-display-table 0 ?\ )

;; Fill to 80 characters by default:
(setq fill-column 80)

(load-file (dotfile "emacs/fonts.el"))

;; Icons that I can use in dired, buffer mode lines... etc
(use-package nerd-icons
  :ensure t
  :custom
  ;; the Nerd Font package was refactored in NixOS 25.05, and the
  ;; default "Symbols Nerd Font Mono" stopped being installed (not
  ;; sure if this is intentional or a bug)
  ;;
  ;; As a fix, we can swap to "Symbols Nerd Font" which is provided in
  ;; Nixpkgs by pkgs.nerd-fonts.symbols-only
  (nerd-icons-font-family "Symbols Nerd Font")
  :config
  (let ((file-type-overrides
         '(("hs" nerd-icons-devicon "nf-dev-haskell" :face nerd-icons-blue)
           ("chs" nerd-icons-devicon "nf-dev-haskell" :face nerd-icons-blue)
           ("lhs" nerd-icons-devicon "nf-dev-haskell" :face nerd-icons-blue)
           ("hsc" nerd-icons-devicon "nf-dev-haskell" :face nerd-icons-blue)

           ("http" nerd-icons-octicon "nf-oct-globe" :face nerd-icons-lblue)

           ("json" nerd-icons-sucicon "nf-seti-json" :face nerd-icons-dyellow)
           ("cfg" nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-dyellow)
           ("toml" nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-dyellow)
           ("lock" nerd-icons-faicon "nf-fa-lock" :face nerd-icons-dyellow))))
    (setq nerd-icons-extension-icon-alist (append file-type-overrides nerd-icons-extension-icon-alist)))
  (add-to-list 'nerd-icons-regexp-icon-alist
               '("^\\." nerd-icons-octicon "nf-oct-gear" :face nerd-icons-lsilver)))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode)
  :config
  (set-face-attribute 'nerd-icons-dired-dir-face nil :foreground "#BBBBBB"))

;; Prettier mode line
(use-package powerline
  :ensure t)
(load-file (dotfile "emacs/mode-line.el"))


;; Make visual-line-mode configurable to fill-column. Not great, but
;; gets the job done...
(use-package visual-fill-column
  :ensure t)


;; Preview colors for color literals
(use-package rainbow-mode
  :ensure t)

                                        ; KEY REBINDINGS
;; Do nothing on C-x C-c:
(global-unset-key (kbd "C-x C-c"))
;; I'm phasing C-x o out:
(global-set-key (kbd "C-x o") 'other-frame)

;; I ≈never suspend Emacs on purpose, but I hit the default bindings
;; by accident pretty often :(
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "C-S-b") 'list-buffers)

;; Some nice keyboard shortcuts:
(global-set-key (kbd "C-x 5 3") 'make-frame-command)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-j") 'compile)
(global-set-key (kbd "C-c C-a") 'align-regexp)
(global-set-key (kbd "M-#") 'ispell-complete-word)
(global-set-key (kbd "M-j") 'next-error)
(global-set-key (kbd "C-c C-o") 'browse-url-at-point)

;; C-w remap:
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Get rid of column editing which I trigger by accident and find
;; incredibly annoying:
(global-unset-key (kbd "<f2>"))

;; Turn off "secondary selection"—I only ever trigger it by
;; accident...
(global-unset-key (kbd "M-<mouse-1>"))
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-unset-key (kbd "M-<drag-mouse-1>"))

                                        ; WINDOW MANAGEMENT
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

(defun my-balance-windows-advice (window)
  "Advice that runs after a function, balances the windows, and
returns the same value as the function."
  (balance-windows)
  window)

;; Automatically rebalance windows whenever a new window is created.
(advice-add 'split-window :filter-return #'my-balance-windows-advice)
(advice-add 'delete-window :filter-return #'my-balance-windows-advice)

                                        ; COMPLETION AND NAVIGATION
(use-package consult
  :ensure t)

(use-package posframe
  :ensure t
  :demand t

  :init
  (defun posframe-background-color ()
    (let ((bg (face-attribute 'default :background)))
      (color-lighten-name bg 100)))
  (defun posframe-border-color ()
    (face-attribute 'default :foreground))

  ;;; Without explicitly passing in (face-attribute 'default :font),
  ;;; posframe windows were not working correctly with my
  ;;; auto-adjust-font-size function.
  (defun display-posframe (buffer &rest extra-settings)
    "Display a posframe with my default settings, correctly adjusted
to the current frame's font size.

EXTRA-SETTINGS provides extra arguments to `posframe-show',
overriding defaults. For example, to use a different
':min-width', you can do:

(display-posframe my-handler :min-width new-width)"
    (let* ((font (face-attribute 'default :font))
           (settings
            (list :min-height 15
                  :min-width 50

                  :left-fringe (frame-parameter (selected-frame) 'left-fringe)
                  :right-fringe (frame-parameter (selected-frame) 'right-fringe)

                  :font font
                  :background-color (posframe-background-color)

                  :internal-border-width 1
                  :internal-border-color (posframe-border-color))))
      (frame-root-window
       (apply 'posframe-show buffer (org-combine-plists settings extra-settings)))))

  (defun display-posframe-bottom (buffer _alist)
    (display-posframe buffer
     :poshandler #'posframe-poshandler-frame-bottom-center

     ;; (frame-width) sometimes returns an incorrect number for newly
     ;; opened frames, and this alternate calculation seems to solve
     ;; the problem.
     ;;
     ;; The - 2 adjusts for some inconsistencies in the
     ;; calculation—the frame is too wide otherwise.
     :width (- (/ (frame-pixel-width) (window-font-width)) 2)))

  (defun display-posframe-center (buffer _alist)
    (display-posframe buffer :poshandler #'posframe-poshandler-window-center)))

(use-package vertico
  :ensure t
  :custom
  (enable-recursive-minibuffers t)
  (completion-ignore-case t)
  :config
  (load-file (dotfile "emacs/jump-shortcuts.el"))
  (when (is-work)
    (load-file (dotfile "emacs/work-shortcuts.el"))
    (add-to-list 'shortcuts-sources #'shortcuts-mercury))
  (unless (eq system-type 'darwin)
    (add-to-list 'shortcuts-core-shortcuts '("Dropbox" . "~/Dropbox"))
    (add-to-list 'shortcuts-core-shortcuts `("init.el" . ,(dotfile "emacs/init.el")))
    (add-to-list 'shortcuts-sources #'shortcuts-org-agenda-files)
    (add-to-list 'shortcuts-sources #'shortcuts-org-notes))
  (global-set-key (kbd "C-x j") 'jump-to-shortcut)
  :init
  (vertico-mode 1))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)
              ("C-j" . vertico-exit-input))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-posframe
  :ensure t
  :custom
  (vertico-posframe-parameters
   '((left-fringe . 8)
     (right-fringe . 8)))
  :init
  (vertico-posframe-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :bind (("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))


                                        ; INPUT MODES
(setq default-input-method "TeX")
(toggle-input-method)

;; My quail customizations
(load-file (dotfile "emacs/quail-rules.el"))

					; DIRED
(use-package dired-aux
  :config
  (add-to-list 'dired-compress-files-alist '("\\.tar\\'" . "tar -cf - %i > %o")))

;; Has to be above JABBER settings because it has a conflicting
;; keybinding :(.
(use-package dired-x
  ;; Automatically omit “uninteresting” files from the listing.
  :hook (dired-mode . dired-omit-mode)
  :config
  (add-to-list 'dired-omit-extensions ".map"))

(setq dired-dwim-target t)

;; Simplify the dired view by hiding permissions, users, date
;; modified... etc
(setq dired-hide-details-hide-symlink-targets nil)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Automatically update dired buffers when the directory changes:
(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq dired-auto-revert-buffer 't)

;; Don't ask about copying or deleting directories recursively:
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

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

                                        ; NETWORK REQUESTS
(use-package restclient
  :ensure t
  :mode ("\\.http" . restclient-mode)
  :config
  (add-to-list 'restclient-content-type-modes '("application/json" . json-mode))
  (add-to-list 'restclient-content-type-modes '("text/html" . mhtml-mode)))

(use-package request
  :ensure t
  :config
  (defun url-buffer-name (url-string)
    "Return a buffer name based on the given URL.

If the URL has a path to a file or directory, this uses the file
or directory name. If it doesn't, it uses the entire URL."
    (let* ((url (url-generic-parse-url url-string))
           (path (url-filename url))
           (filename (file-name-nondirectory path))
           (dirname (file-name-nondirectory (directory-file-name path)))
           (name
            (cond ((member path '("" "/")) (url-domain url))
                  ((equal filename "") dirname)
                  (t filename))))
      (concat "*" name "*")))

  (defun content-type-mode (response)
    "Choose a mode based on the response's content-type, if
possible. This uses the modes defined in
`restclient-content-type-modes'."
    (let* ((content-type
            (request-response-header response "content-type")))
      (cdr (assoc-string content-type restclient-content-type-modes t))))

  (defun download-file (url)
    "Download the given URL asynchronously, popping open the
content in a buffer once ready."
    (interactive
     (list
      (let ((url (thing-at-point 'url)))
        (if url (read-string (format "url (%s): " url) nil nil url)
          (read-string "url: ")))))
    (request url
      :success (cl-function
                (lambda (&key response &key data &allow-other-keys)
                  (pop-to-buffer (url-buffer-name (request-response-url response)))
                  (erase-buffer)
                  (insert data)
                  (let ((mode (content-type-mode response)))
                    (if mode (apply mode '()) (normal-mode))))))))

                                        ; PDF
(global-auto-revert-mode t)

(use-package pdf-view
  :bind (:map pdf-view-mode-map
              ("g" . revert-buffer-quick)))

(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

                                        ; SCREENSHOT
(unless (eq system-type 'darwin) (use-package screenshot))

                                        ; FLYSPELL
;; If the aspell executable is not available, check two things:
;;
;;   1. Does Emacs see the right PATH variable? See EXEC PATH section
;;   above.
;;
;;   2. Is aspell installed to your Nix user profile? Run home-manager
;;   switch to make sure.
(use-package flyspell
  :demand t

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
  (ispell-personal-dictionary (dotfile ".aspell.en.pws"))

  :config
  (add-hook 'flyspell-mode-hook 'flyspell-color-hook)
  (ispell-start-process))

                                        ; FLYCHECK
(use-package flycheck
  :ensure t
  :demand t

  :hook (python-mode . flycheck-mode)


  :bind (("M-J" . flycheck-next-error)
         ("M-K" . flycheck-previous-error)
         ("M-L" . flycheck-display-error-at-point))

  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))

  :config
  (setq flycheck-executable-find
        (lambda (cmd) (direnv-update-environment default-directory) (executable-find cmd))))

  ;; Modified from flycheck.el to not display the error message
  ;; automatically.
  (defun flycheck-next-error (&optional n reset)
    "Visit the N-th error from the current point *without* displaying
the error message.

N is the number of errors to advance by, where a negative N
advances backwards.  With non-nil RESET, advance from the
beginning of the buffer, otherwise advance from the current
position."
    (interactive "P")
    (when (consp n)
      ;; Universal prefix argument means reset
      (setq reset t n nil))
    (flycheck-next-error-function n reset))

(use-package flycheck-posframe
  :ensure t
  :after (flycheck posframe)
  :hook (flycheck-mode . flycheck-posframe-mode)

  :init
  (defun flycheck-posframe-hide-posframe ()
    "Hide the flycheck posframe if it is visible."
    (posframe-hide flycheck-posframe-buffer))

  (define-advice keyboard-quit (:before () hide-flycheck-posframe-on-quit)
    (flycheck-posframe-hide-posframe))

  :config
  (flycheck-posframe-configure-pretty-defaults)

  ;; override existing flymake function to disable automatic popups on
  ;; a timer
  (defun flycheck-maybe-display-error-at-point-soon ()
    "Doesn't do anything in order to disable displaying error
messages under the cursor automatically. Call
`flycheck-display-error-at-point' manually instead.")

  (defun flycheck-posframe-format-error (err)
  "Formats ERR for display."
  (propertize (concat
               (flycheck-posframe-get-prefix-for-error err)
               (flycheck-error-format-message-and-id err)
               "\n")
              'face
              `(:inherit ,(flycheck-posframe-get-face-for-error err))) )

  (defun flycheck-posframe-show-posframe (errors)
    "Display ERRORS using my custom posframe settings (see
`display-posframe')."
    (posframe-hide flycheck-posframe-buffer)
    (when (and errors
               (not (run-hook-with-args-until-success 'flycheck-posframe-inhibit-functions)))
      (let ((poshandler
             (intern (format "posframe-poshandler-%s" flycheck-posframe-position)))
            (formatted-message
             (concat "\n" (flycheck-posframe-format-errors errors))))
        (unless (functionp poshandler)
          (setq poshandler nil))
        (flycheck-posframe-check-position)
        (display-posframe flycheck-posframe-buffer)

        (display-posframe
         flycheck-posframe-buffer
         :string formatted-message
         :min-height (length (string-lines formatted-message))
         :position (point)
         :poshandler poshandler
         :hidehandler #'flycheck-posframe-hidehandler)))))


                                        ; CODE FORMATTING
(use-package format-all
  :ensure t
  :hook (prog-mode . format-all-mode)
  :init
  (put 'format-all-formatters 'safe-local-variable 'listp))


                                        ; NIX
;; Make sure Emacs sees executables from Nix correctly.
(use-package exec-path-from-shell
  :ensure t
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (let ((nix-vars '("NIX_LINK"
                    "NIX_PATH"
                    "NIX_SSL_CERT_FILE"
                    "SSL_CERT_FILE")))
    (exec-path-from-shell-initialize) ; $PATH, $MANPATH and set exec-path
    (mapcar 'exec-path-from-shell-copy-env nix-vars)))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :hook (nix-mode . nix-auto-format)
  :init
  (defun nix-auto-format ()
    "Turn on `format-all-mode' if nixpkgs-fmt is in the PATH in this
buffer."
    (when (executable-find "nixpkgs-fmt")
      (setq-local format-all-formatters '(("Nix" nixpkgs-fmt)))
      (format-all-mode t))))

(use-package direnv
  :ensure t

  :custom
  (direnv-show-paths-in-summary nil)
  (direnv-always-show-summary nil)

  :config
  (direnv-mode)
  (add-to-list 'direnv-non-file-modes 'shell-mode))


                                        ; JSON
(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :bind
  (:map json-mode-map
        ("C-." . hs-toggle-hiding))
  :init
  (defun json-indent-hook ()
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2))
  :config
  (add-hook 'json-mode-hook 'json-indent-hook)
  (add-hook 'json-mode-hook 'hs-minor-mode))

                                        ; SYSTEM
(use-package journalctl-mode
  :ensure t
  :custom
  (journalctl-chunk-size 2500))

                                        ; YAML
(use-package yaml-mode
  :ensure t)

                                        ; DOCKER
(use-package docker-compose-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

                                        ; BAZEL + STARLARK
(use-package bazel
  :ensure t
  :mode "\\.star\\'")

                                        ; COMPANY
;; Change company-mode colors to match blackboard:
(use-package company
  :ensure t
  :after color
  :bind (:map lsp-mode-map
              ("M-RET" . company-complete)
              :map emacs-lisp-mode-map
              ("M-RET" . company-complete))
  :hook
  (emacs-lisp-mode . company-mode)
  :custom
  (company-idle-delay nil))

;; Adds icons to company popups.
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)

  :config
  (define-advice company-complete (:before (&rest args) adjust-company-box-font-size)
    "company-box popups are techincally separate frames. They don't
inherit the default face from their parent frame and the
company-box code explicitly disables
`after-make-frame-functions', so the company popup does not
automatically adjust its font size for different resolutions.

As a workaround, I globally adjust the size of company-specific
faces each time before company-complete is called."
    (let ((height (adjusted-font-size (selected-frame))))
      (set-face-attribute 'company-tooltip nil :height height)
      (set-face-attribute 'company-tooltip-annotation nil :height height))))

                                        ; YASNIPPET
(use-package yasnippet
  :ensure t
  :demand t

  :hook
  (python-mode . yas-minor-mode)
  (yaml-mode . yas-minor-mode)
  (haskell-mode . yas-minor-mode))

                                        ; PROJECTILE
(use-package projectile
  :ensure t)

                                        ; LSP
(use-package treemacs
  :ensure t)

(use-package lsp-mode
  :ensure t
  :demand t
  :after treemacs

  :custom
  (lsp-eldoc-hook nil)
  (lsp-diagnostics-provider :flycheck)

  (lsp-headerline-breadcrumb-icons-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-modeline-diagnostics-enable nil)

  :custom-face
  (lsp-lsp-flycheck-info-unnecessary-face
   ((t (:underline (:color "#3366FF" :style line)))))

  :bind
  (:map lsp-mode-map
        ("C-c C-d" . lsp-ui-doc-show)
        ("M-A" . lsp-execute-code-action)
        ("M-D" . lsp-avy-lens)
        ("M-?" . lsp-find-references))

  :config
  (lsp-diagnostics-mode 1)
  (let ((patterns '("[/\\\\]\\.venv\\'"
                    "[/\\\\]\\.mypy_cache\\'"
                    "[/\\\\]\\.pytest_cache\\'")))
    (dolist (p patterns)
      (add-to-list 'lsp-file-watch-ignored-directories p))))

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-sideline-enable t)

  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-position 'at-point))

(use-package dap-mode
  :ensure t
  :init
  (setq gud-key-prefix (kbd "C-c C-s"))

  :config
  (require 'dap-python))

                                        ; MAGIT
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-clone)

         :map magit-file-section-map
         ("RET" . magit-diff-visit-file-other-window))

  :custom
  (magit-commit-ask-to-stage 'stage)
  (magit-clone-set-remote.pushDefault t)
  ;; Don't revert my window layout when I q out of a Magit buffer
  (magit-bury-buffer-function 'magit-mode-quit-window)

  :config
  ;; file type icons inside Magit buffers
  (setopt magit-format-file-function #'magit-format-file-nerd-icons)

  ;; Improve ergonomics of Git commit message buffers
  (defun my-git-commit-setup-hook ()
    (visual-line-mode 1)
    (visual-fill-column-mode 1))
  (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-auto-fill)
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  (add-hook 'git-commit-setup-hook 'my-git-commit-setup-hook))

(use-package git-modes
  :ensure t)

(use-package forge
  :ensure t
  :after magit

  :bind
  (:map magit-mode-map
        ("C-c C-o" . my-forge-browse-dwim))

  :init
  (defun my-forge-url-at-point (arg)
    "Return the URL of the appropriate magit object at point.

When a point has both a branch and a commit, this prioritizes the
URL for the commit."
    (if-let* ((topic (forge-topic-at-point)))
        (forge-get-url topic)

      (if-let* ((commit (magit-commit-at-point)))
          (forge--format (forge-get-repository 'stub)
                         'commit-url-format
                         `((?r . ,(magit-rev-hash commit))))

        (if-let* ((branch (magit-branch-at-point)))
            ;; Code borrowed from `forge-browse-remote'
            (let (remote)
              (if (magit-remote-branch-p branch)
                  (let ((cons (magit-split-branch-name branch)))
                    (setq remote (car cons))
                    (setq branch (cdr cons)))
                (or (setq remote (or (magit-get-push-remote branch)
                                     (magit-get-upstream-remote branch)))
                    (user-error "Cannot determine remote for %s" branch)))
              (forge--format remote 'branch-url-format
                             `((?r . ,branch))))
          (when-let* ((topic (forge-current-topic)))
            (forge-get-url topic))))))

  (defun my-forge-browse-dwim (arg)
    "A version of `forge-browse-dwim' that, if given a numeric
argument, pushes the URL onto the kill ring instead of browsing.

Unlike the normal `forge-browse-dwim', this prioritizes commits
over branches.

If there is no appropriate object at point, this function
silently does nothing."
    (interactive "P")
    (when-let* ((url (my-forge-url-at-point arg)))
      (if arg (kill-new url) (browse-url url))))

  :config
  (add-to-list
   'forge-alist
   '("git.target.com"
     "git.target.com/api/v3"
     "target"
     forge-github-repository)))

(use-package pr-review
  :ensure t)

(use-package smerge-mode
  :custom
  (smerge-command-prefix (kbd "C-.")))

                                        ; ORG-MODE
(use-package prog-mode
  :demand t)

(defun org-mode-prettify-hook ()
  "Configure prettify-symbols to replace todo/consider/done with
  pretty Unicode characters."
  (push '("TODO" . "📝") prettify-symbols-alist)
  (push '("FOLLOW-UP" . "➡") prettify-symbols-alist)
  (push '("CONSIDER" . "❔") prettify-symbols-alist)
  (push '("INVESTIGATE" . "🔎") prettify-symbols-alist)
  (push '("DONE" . "☑") prettify-symbols-alist)
  (push '("CANCELED" . "❌") prettify-symbols-alist)
  (push '("PROJECT" . "📂") prettify-symbols-alist)
  (prettify-symbols-mode 1))

(use-package org
  :after prog-mode
  :demand t

  :custom
  (org-todo-keywords
   '((type "TODO" "CONSIDER" "FOLLOW-UP" "INVESTIGATE" "|" "DONE" "CANCELED")
     (sequence "PROJECT" "|" "DONE")))
  (org-capture-templates
   '(("t" "Todo" entry (file "Tasks.org")
      "* TODO %?\n  SCHEDULED: %T\n:PROPERTIES:\n:CREATED: %U\n:END:")
     ("n" "Note" entry (file "misc.org")
      "* ")
     ("l" "Link" entry (file "links.org")
      "* ")))
  (org-refile-targets
   '((org-agenda-files :maxlevel . 3)))
  (org-log-done 'time)

  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)

         :map org-mode-map
         ("C-M-n" . outline-next-visible-heading)
         ("C-M-p" . outline-previous-visible-heading)
         ("M-}" . forward-paragraph)
         ("M-{" . backward-paragraph)
         ("C-c C-," . org-promote-subtree)
         ("C-c C-." . org-demote-subtree)
         ("C-c C-;" . org-insert-structure-template)
         ("C-c C-o" . tikhon/org-open-at-point))

  :hook
  ((org-insert-heading . tikhon/org-insert-with-timestamp)
   (org-after-refile-insert . org-update-parent-todo-statistics))


  :init
  (defun tikhon/org-insert-with-timestamp ()
    (when (member "CREATED" (org-buffer-property-keys))
      (let* ((fmt (concat "[" (substring (cdr org-time-stamp-formats) 1 -1) "]"))
             (timestamp (format-time-string fmt (current-time))))
        (org-set-property "CREATED" timestamp))))

  ;; Based on Navidot's code at the Emacs Stack Exhange:
  ;; https://emacs.stackexchange.com/a/60555/17
  (defun tikhon/org-copy-link-address-at-point ()
    "Copy the address of the link at point. Does nothing if there is no link
at point."
    (interactive)
    (let* ((link (org-element-lineage (org-element-context) '(link) t))
           (type (org-element-property :type link))
           (url (concat type ":" (org-element-property :path link))))
      (when link
        (message "Copied URL: %s" url)
        (kill-new url))))

  (defun tikhon/org-open-at-point (arg)
    "My version of `org-open-at-point'. Normally does same behavior as the
normal version, but with an argument calls `my-org-copy-link-address-at-point'
instead."
    (interactive "P")
    (if arg
        (tikhon/org-copy-link-address-at-point)
      (org-open-at-point)))

  ;; TODO: Run this function automatically?
  ;;
  ;; Not sure if I'd do this on a timer, as a cron job, on Emacs
  ;; startup...
  ;;
  ;; For now I'll just run it manually and it'll be fine.
  (defun org-archive-tasks-older-than (time &optional match)
    "Archive all top-level done tasks (ie not subtasks inside
projects) that were scheduled at least TIME before now.

Example: archive all TODO=\"DONE\" tasks older than 30 days:

(org-archive-tasks-older-than (days-to-time 30))

Example: archive all tasks tagged work older than 30
days (regardless of TODO status):

(org-archive-old-tasks (days-to-time 30) \"+work\")"
    (setq match (or match "+TODO=\"DONE\""))
    (org-map-entries
     (lambda ()
       (org-archive-subtree)

       ;; org-map-entries moves to the end of the line after
       ;; processing each entry
       ;;
       ;; since we are removing the current entry by archiving, this
       ;; implicitly skips the next entry; we have to set
       ;; org-map-continue-from to avoid this
       (setq org-map-continue-from
             (org-element-property :begin (org-element-at-point))))
     (format "%s+LEVEL=1+SCHEDULED<=\"%s\""
             match
             (format-time-string "<%Y-%m-%d %H:%M>"
                                 (time-subtract (current-time) time)))
     nil))

  :config
  (add-hook 'org-mode-hook 'org-mode-prettify-hook)

  (defun unset-agenda-binding () (local-unset-key (kbd "C-c C-a")))
  (add-hook 'comint-mode-hook 'unset-agenda-binding)

  ;; My core *.org files are stored in Dropbox unless I'm on a work
  ;; computer. (Only place I would use macOS!)
  (when (not (eq system-type 'darwin))
    (setq org-directory "~/Dropbox/org"))

  (setq org-default-notes-file (concat org-directory "/Notes.org"))

  (require 'org-tempo)

  ;; Spellcheck my org mode files.
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'auto-fill-mode)

  ;; Allow markup in the middle of words.
  (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  ;; Configuring title page formatting with #+OPTION is too fiddly, so
  ;; we want to override the elisp variable instead
  (put 'org-reveal-title-slide 'safe-local-variable 'stringp))

(use-package org-ql
  :ensure t)

(use-package org-ql-view)

(use-package htmlize
  :ensure t)

(use-package ox-reveal
  :ensure t
  :after org htmlize
  :demand t
  :bind (:map org-mode-map
         ("M-S" . org-reveal-export-current-subtree)
         ("M-R" . org-reveal-export-to-html))
  :config
  ;; Configuring title page formatting with #+OPTION is too fiddly, so
  ;; we want to override the elisp variable instead
  (put 'org-reveal-title-slide 'safe-local-variable 'stringp))

(use-package el-patch
  :ensure t
  :config
  (setq el-patch-enable-use-package-integration t))

(use-package org-agenda
  :after (org el-patch)

  :bind (("C-c a" . org-agenda)
         :map org-agenda-mode-map
         ("k" . org-capture))

  :custom
  (org-agenda-files
   (list (concat org-directory "/Tasks.org")
         (concat org-directory "/Projects.org")))

  (org-agenda-format-date 'org-agenda-custom-date-format)

  (org-agenda-scheduled-leaders '("" "%2d×"))
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
     ""
     ;; Hack: THREE-PER-EM SPACE inserted at beginning of string so
     ;; that Org Agenda doesn't trim the leading whitespace:
     "                       •••"))
  ;; Note: Using a bunch of NARROW NO-BREAK SPACE around 🕐 to align
  ;; with ••• in the agenda view
  (org-agenda-current-time-string "──────────────      🕐     ──────────────")

  (org-agenda-window-setup 'other-window)

  ;; I don't use blocked tasks (yet?), so disabling this should
  ;; improve performance of agenda commands.
  (org-agenda-dim-blocked-tasks nil)

  ;; Custom agenda commands:
  ;;  * p to list projects
  ;;  * P to list projects with a given tag
  (org-agenda-custom-commands
   '(("p" "Projects" todo "PROJECT")
     ("P" "Projects (with tag)" (lambda (arg) (call-interactively #'org-agenda-project-for-tag)))))

  :init
  (defun org-agenda-custom-date-format (date)
    (concat "\n" (org-agenda-format-date-aligned date)))

  (defun org-agenda-project-for-tag (tag)
    "Search for PROJECT todo entries with the given tag."
    (interactive (list (completing-read "tag:" #'org-tags-completion-function)))
    (org-tags-view t (format "%s/PROJECT" tag)))

  (defun org-read-add-default-time
      (&optional with-time to-time from-string prompt
                 default-time default-input inactive)
    "Filters the inputs to `org-read-date', setting the current time
as the default input if one was not already specified."
    (let ((new-default (or default-input (format-time-string "%H:%M"))))
      (list with-time to-time from-string prompt
            default-time new-default inactive)))

  (define-advice org-read-date (:filter-args (args) default-current-time)
    "Use the current time as the default input for `org-read-date'
unless one was provided."
    (apply 'org-read-add-default-time args))

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

  (defface org-agenda-default
    '((t (:inherit default :family "Roboto Mono" :weight normal)))
    "The default face to use when displaying the Org Agenda.")
  (defun org-agenda-default-face-mode ()
    "Minor mode that sets the buffer's default face to `org-agenda-default'."
    (interactive (list (or current-prefix-arg 'toggle)))
    (setq-local line-spacing 0.2)
    (buffer-face-mode-invoke 'org-agenda-default t
                             (called-interactively-p 'interactive)))
  (add-hook 'org-agenda-mode-hook #'org-agenda-default-face-mode)

  (add-hook 'org-agenda-mode-hook #'org-mode-prettify-hook))

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

  (defun shell-prompt (name directory)
    "Returns a shell prompt string for the given shell name and
directory in the format that the PS1 environment variable
expects.

This function makes it easy to have different prompts in
different context (eg explicitly mark out remote shells vs local
ones)."
    (setq name (format-shell-name name))
    (if (file-remote-p directory)
        (concat "\\033[35m" "ssh"
                "\\033[37m" ":"
                "\\033[31m" name
                "\\033[37m" ":"
                "\\033[32m" "\\W"
                "\\033[37m" ">"
                "\\033[0m")
      (concat "\\033[31m" name
              "\\033[37m" ":"
              "\\033[32m" "\\W"
              "\\033[37m" ">"
              "\\033[0m")))

  (defun new-shell (prefix name)
    "Opens a new shell buffer with the given name in
asterisks (*name*) in the current directory with and changes the
prompt to name:directory> or ssh:name:directory> as approrpiate.

If the buffer already exists, pops to that buffer without
restarting the process. With a numeric argument, starts a new
process regardless."
    (interactive "P\nsName: ")
    (when (equal name "")
      (setq name (find-useful-directory-name default-directory)))
    (pop-to-buffer (concat "<*" name "*>"))

    (unless (and (not prefix) (get-buffer-process (current-buffer)))
      (shell (current-buffer))

      (let* ((process (get-buffer-process (current-buffer)))
             (remote (file-remote-p default-directory)))

        (require 'cl-lib)
        (cl-flet ((send (str) (comint-simple-send process str)))
          ;; XXX Hack for BR
          (when remote (send "source ~/.bash_profile"))

          (send "export TERM='xterm-256color'")
          (send "export IN_EMACS=true")
          (send (concat "export PS1=\"" (shell-prompt name default-directory) "\""))

          (unless remote
            (send (format "export PAGER=%s" (expand-file-name "~/local/bin/epage")))
            (send "direnv allow 2> /dev/null"))

          (if remote (sleep-for 1 0) (sleep-for 0 200))
          (comint-send-input)
          (comint-clear-buffer))))))

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
          ,(from-face 'font-lock-warning-face)
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
(use-package elisp-mode
  :init
  (defun emacs-lisp-link-docstring-identifier ()
    "Surround the next identifier with ` and '."
    (interactive)
    (if (use-region-p)
        (markdown-wrap-or-insert "`" "'" nil (region-beginning) (region-end))
      (markdown-wrap-or-insert "`" "'" 'word nil nil)))
  :bind
  (:map emacs-lisp-mode-map
        ("C-M-;" . emacs-lisp-link-docstring-identifier)))

(use-package paredit
  :ensure t
  :hook (emacs-lisp-mode . paredit-mode)
        (lisp-data-mode . paredit-mode))

                                        ; PYTHON
(use-package python-docstring
  :ensure t
  :hook (python-mode . python-docstring-mode)
  :custom
  (python-docstring-sentence-end-double-space nil))

(use-package python-pytest
  :ensure t
  :bind
  (:map python-mode-map
        ("M-S" . python-pytest-dispatch)))

(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode))

(defun my-python-hook ()
  ;; Make forward- and backward-sexp functions work as they do in
  ;; other modes
  (setq forward-sexp-function nil)

  (direnv-update-environment default-directory)

  (require 'lsp-pyright)
  (lsp))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . my-python-hook))

                                        ; HIVE
(add-to-list 'auto-mode-alist '("\\.hql\\'" . sql-mode))

                                        ; R
(use-package ess
  :ensure t
  :custom
  (ess-indent-offset 2))

                                        ; THETA

(use-package theta-mode
  :mode "\\.theta\\'")

                                        ; HASKELL
(use-package haskell-mode
  :ensure t
  :after flycheck

  :custom
  (haskell-process-type 'cabal-repl)
  (haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans"))

  (haskell-font-lock-symbols nil)

  (haskell-process-wrapper-function 'identity)

  :bind  (:map haskell-mode-map
               ("C-M-;" . haskell-doc-comment)
               ("C-c C-s" . haskell-save-and-format)
               ("C-c C-r" . my-haskell-load-and-run)
               ("M-." . haskell-find-definition)
               ("M-?" . lsp-find-references)
               ("C-c C-e" . haskell-add-language-pragma)
               :map haskell-cabal-mode-map
               ("C-c C-s" . haskell-cabal-save-and-format))

  :init
  (defun insert-at-start (str)
    "Insert the given string at the beginning of the line the point is on,
collapsing any extra spaces after the inserted string."
    (save-excursion
      (beginning-of-line)
      (insert str)
      (just-one-space)))

  (defun haskell-doc-comment ()
    "Insert the first line of a Haddock documentation
comment (-- |). If the region is active, comment the entire region
as a documentation comment."
    (interactive)
    (if (use-region-p)
        (let ((end-line (line-number-at-pos (- (region-end) 1))))
          (save-excursion
            (goto-char (region-beginning))
            (insert-at-start "-- | ")
            (next-line)
            (while (<= (line-number-at-pos (point)) end-line)
              (insert-at-start "-- ")
              (next-line))))
      (insert-at-start "-- | ")
      (beginning-of-line)
      (forward-char 5)))

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

  (defun haskell-find-definition ()
    "Jump to the definition at point, using LSP mode if it's on and
Haskell mode if it's not."
    (interactive)
    (if lsp-mode
        (call-interactively 'lsp-find-definition)
      (call-interactively 'haskell-mode-jump-to-def-or-tag)))

  (defun haskell-add-language-pragma (extension)
    "Prompt for a GHC extension, then add a pragma for it at the top
of the file."
    (interactive
     (list (completing-read "Extension: " haskell-ghc-supported-extensions)))
    (save-excursion
      (beginning-of-buffer)
      (insert (format "{-# LANGUAGE %s #-}\n" extension))))

  (defun haskell-cabal-fmt ()
    "If the cabal-fmt executable is available, use it to format the
current buffer. Otherwise do nothing."
    (when (executable-find "cabal-fmt")
      (haskell-mode-buffer-apply-command "cabal-fmt")))

  (defun haskell-cabal-save-and-format ()
    "If the cabal-fmt executable is available, use it to format the
current buffer. Save the buffer afterwards either way."
    (interactive)
    (save-buffer)
    (haskell-cabal-fmt)
    (save-buffer))

  (defun haskell-process-flycheck-error (err)
    "Process ERR, returning a more compact error message for flycheck
to display."
    (when (eq major-mode #'haskell-mode)
      (setf (flycheck-error-message err)
            (concat (haskell-message-compact (flycheck-error-message err)) "\n"))
      nil))
  (add-hook 'flycheck-process-error-functions #'haskell-process-flycheck-error)

  (defvar haskell-message-ignored-parts
    '("^Relevant bindings include"
      "^In a stmt"
      "^In the \\w+ argument of"
      "^In the expression:")
    "Regexps to determine whether a section of an error
message (delimited by •) should be ignored.")

  (defvar haskell-message-ignored-lines
    '("^ *at /.*$")
    "Regexps to determine whether a line in an error message should
be filtered out.")

  (defun haskell-message-part-is-ignored (part)
    "Returns whether a subset of an error message is ignored.

A part of a message is ignored if it matches any of the patterns
in `haskell-message-ignored-parts'."
    (-any (lambda (expr) (string-match expr part)) haskell-message-ignored-parts))

  (defun haskell-message-line-is-ignored (line)
    "Returns whether the given line is ignored.

A line is ignored if it matches any of the patternsin
`haskell-message-ignored-lines'."
    (-any (lambda (expr) (string-match expr line)) haskell-message-ignored-lines))

  (defun haskell-message-trim-part (part)
    "Given a single part of an error message, trim out unneeded
bits."
    (let ((lines
           (seq-filter (-not 'haskell-message-line-is-ignored) (split-string part "\n"))))
      (string-join lines "\n")))

  (defun haskell-message-compact (error-message)
    "Use some heuristics to trim down Haskell error message strings
to be easier to read at a glance.

If the format of ERROR-MESSAGE is not recognized, it should be returned
unchanged."
    (let* ((parts (mapcar #'string-trim (split-string error-message "•" t)))
           (kept (seq-filter (-not 'haskell-message-part-is-ignored) parts)))
      (string-join (mapcar #'haskell-message-trim-part kept) "\n\n")))

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
  (add-hook 'haskell-mode-hook 'font-lock-mode)

  (defun my-interactive-haskell-mode-hook ()
    (interactive-haskell-mode 1)
    (unbind-key "M-." interactive-haskell-mode-map))
  (add-hook 'haskell-mode-hook 'my-interactive-haskell-mode-hook)

  (defun mwb-settings-haskell-mode-hook ()
    (when (string= (file-name-base (haskell-cabal-find-file)) "mwb")
      (setq-local haskell-process-type 'ghci)
      (setq-local haskell-process-path-ghci "mwb-ghci"))))

(use-package lsp-haskell
  :after lsp-mode
  :ensure t
  :hook (haskell-mode . haskell-lsp-if-available)
  :init
  (defun haskell-lsp-if-available ()
    "Turn on LSP mode if the haskell-lsp-server is available in the buffer's PATH.

I manage lsp-servers on a per-project basis with Nix and direnv,
so I only want to try running LSP if the server executable is
available.

This also ignores files named Setup.hs because HLS seems to
consistently fail on them."
    (unless (or (not (buffer-file-name))
                (equal (file-name-nondirectory (buffer-file-name)) "Setup.hs"))
      (cond ((executable-find "static-ls")
             (setq-local lsp-haskell-server-path "static-ls")
             (lsp t))
            ((executable-find "haskell-language-server-wrapper")
             (lsp t))
            ((executable-find "haskell-language-server")
             (setq-local lsp-haskell-server-path "haskell-language-server")
             (lsp t))))))

                                        ; LEAN
;;; Lean mode is not loading correctly for some reason; will debug
;;; later

;; (use-package dash
;;   :ensure t)

;; (use-package lean4-mode
;;   :after dash lsp-mode
;;   :ensure t)


                                        ; UNISON
(use-package unisonlang-mode
  :ensure t)

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

                                        ; LUA
(use-package lua-mode
  :ensure t)

                                        ; SCALA
(use-package scala-mode
  :ensure t
  :mode "\\.\\(scala\\|sbt\\)$"

  :bind
  (:map scala-mode-map
        ("RET" . scala-insert-asterisk-on-newline))

  :hook
  (scala-mode . yas-minor-mode)

  :init
  (defun scala-insert-asterisk-on-newline ()
    "Insert a newline, indent and add an asterisk if inside a
Scaladoc comment."
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))

  :config
  (defun scala-lsp-save-and-format ()
    "First format the buffer with `lsp-organize-imports' then save
it."
    (interactive)
    (save-buffer)
    (save-buffer))

  (defun scala-lsp-maybe ()
    "Turns on LSP mode if a metals executable is available.

I like to manage tools on a per-project basis with Nix + direnv
rather than installing them globally. If a project isn't set up
with an LSP server available, lsp-mode prompts me to install it,
which I don't want to do. This hook avoids that problem."
    (when (executable-find "metals")
      (lsp)
      (local-set-key (kbd "C-x C-s") #'scala-lsp-save-and-format)))
  (add-hook 'scala-mode-hook #'scala-lsp-maybe)

  (defun scala-auto-format ()
    "Turn on `format-all-mode' if scalafmt is in the path in this
buffer."
    (when (executable-find "scalafmt")
      (setq-local format-all-formatters '(("Scala" scalafmt)))
      (format-all-mode t)))
  (add-hook 'scala-mode-hook #'scala-auto-format)

  ;; Basic scaladoc highlighting (eg @param foo gets highlighted
  ;; specially).
  ;;
  ;; List of keywords is not exhaustive (for now?)
  (let* ((no-args
          (rx (: "@"
                 (| "author"
                    "constructor"
                    "deprecated"
                    "example"
                    "note"
                    "return"
                    "see"
                    "since"
                    "version"))))
         (args
          (rx (: (group (: "@" (| "param" "throws")))
                 (1+ blank)
                 (group (: (| letter "_") (0+ word)))))))
    (font-lock-add-keywords 'scala-mode
                            `((,no-args 0 font-lock-keyword-face t)
                              (,args 1 font-lock-keyword-face t)
                              (,args 2 font-lock-variable-name-face t)))))

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :bind
  (:map scala-mode-map
        ("RET" . scala-insert-asterisk-on-newline)
        ("C-c C-l" . sbt-do-run)))

(use-package lsp-metals
  :ensure t)

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
(use-package visual-fill-column
  :ensure t)

(use-package markdown-mode
  :ensure t
  :after visual-fill-column
  :mode "\\.md\\'"

  :custom
  (markdown-enable-math t)

  :bind
  ("C-c C-;" . markdown-insert-gfm-code-block)
  ("C-M-;" . markdown-insert-code)

  :config
  (defun my-markdown-hook ()
    (prose-mode 1)
    (flyspell-mode)
    (visual-line-mode 1)
    (visual-fill-column-mode 1)
    (flyspell-buffer)
    (local-unset-key (kbd "C-M-b"))
    (local-unset-key (kbd "C-M-f"))
    (setq sentence-end-double-space nil))
  (add-hook 'markdown-mode-hook #'my-markdown-hook))

(defun copy-markdown-formatted ()
  "Copy a region or buffer in Markdown as RTF—convenient way to
get Markdown-formatted text into email/Word/etc."
  (interactive)
  (pcase-let ((`(,start ,end)
               (if (use-region-p)
                   (list (region-beginning) (region-end))
                 '(nil 0))))
      (call-process-region start end "formatted-copy")))

                                        ; LATEX

(use-package tex-mode
  :mode
  ("latex\\.template" . latex-mode)     ; Pandoc templates

  :config
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'autofill-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))

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

(use-package mhtml-mode
  :mode "\\.html")

(defun typescript-mode-formatting-hook ()
  (message "prettier: %s" (executable-find "prettier"))
  (when (executable-find "prettier")
    (setq-local format-all-formatters '(("TypeScript" prettier)))
    (format-all-mode t)))

(use-package typescript-mode
  :ensure t
  :hook ((typescript-mode . typescript-mode-formatting-hook))

  :custom
  (typescript-indent-level 2))

(defun typescript-tide-setup-hook ()
  (direnv-update-environment)
  (sleep-for 0 100)
  (tide-setup))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . typescript-tide-setup-hook)))

                                        ; COMMANDS
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)
