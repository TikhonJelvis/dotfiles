                                        ; POWERLINE
(powerline-default-theme)

                                        ; ICONS
;; Icon settings inspired by
;; https://github.com/domtronn/all-the-icons.el/wiki/Mode-Line

(defun mode-line/modified (face)
  "Display an icon for the modified status of a buffer:

   A chain for unmodified.
   A broken chain for modified.
   A padlock for read-only. "
  (let* ((config-alist
          '(("*" nerd-icons-faicon "nf-fa-chain_broken")
            ("-" nerd-icons-faicon "nf-fa-chain")
            ("%" nerd-icons-octicon "nf-oct-lock")))
         (result (cdr (assoc (format-mode-line "%*") config-alist))))
    (propertize (apply (car result) (cdr result))
                'face `(:inherit ,face :family ,nerd-icons-font-family))))

(defun mode-line/major-mode (face)
  "Display the icon for the major mode of the current buffer."
  (let ((icon (propertize
               (format "%s " (nerd-icons-icon-for-buffer))
               'face `(:inherit ,face :height 1.1)
               'display '(raise -0.1))))
    (powerline-raw icon face 'l)))

(defun mode-line/git (face &optional pad)
  "Display a Git icon for files saved in Git repos."
  (if (and vc-mode (string-match "Git[:-]" vc-mode))
      (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
        (powerline-raw
         (concat
          " "
          (propertize (nerd-icons-octicon "nf-oct-git_branch")
                      'face `(:height 1.0 :family ,nerd-icons-font-family :inherit ,face)
                      'display '(raise 0.1))
          (format " %s" branch))
         face pad))
    (propertize "      " 'face face)))

;; The default setup for powerline, customized with symbols in a few
;; locations.
(setq-default mode-line-format
      '("%e"
        (:eval
         (let*
             ((active
               (powerline-selected-window-active))
              (face0
               (if active 'powerline-active0 'powerline-inactive0))
              (face1
               (if active 'powerline-active1 'powerline-inactive1))
              (face2
               (if active 'powerline-active2 'powerline-inactive2))
              (separator-left
               (intern
                (format "powerline-%s-%s"
                        (powerline-current-separator)
                        (car powerline-default-separator-dir))))
              (separator-right
               (intern
                (format "powerline-%s-%s"
                        (powerline-current-separator)
                        (cdr powerline-default-separator-dir))))
              (lhs
               (list
                (powerline-raw " " face0)
                (mode-line/modified face0)
                (powerline-raw mode-line-mule-info face0 'l)
                (powerline-buffer-id
                 `(mode-line-buffer-id ,face0)
                 'r)
                (funcall separator-left face0 face1)
                (when
                    (and
                     (boundp 'erc-track-minor-mode)
                     erc-track-minor-mode)
                  (powerline-raw erc-modified-channels-object face1 'l))
                (powerline-raw " " face1)
                (powerline-major-mode face1)
                (powerline-process face1)
                (powerline-narrow face1 'l)
                (powerline-raw " " face1)
                (funcall separator-left face1 face2)))
              (rhs
               (list
                (funcall separator-right face2 face1)
                (mode-line/git face1 'r)
                (powerline-raw global-mode-string face1 'r)
                (funcall separator-right face1 face0)
                (powerline-raw "%4l" face0 'l)
                (powerline-raw ":" face0)
                (powerline-raw "%2c" face0 'r)
                (when powerline-display-hud
                  (powerline-hud face0 face1))
                (powerline-fill face0 0))))
           (concat
            (powerline-render lhs)
            (powerline-fill face2
                            (powerline-width rhs))
            (powerline-render rhs))))))
