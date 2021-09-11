                                        ; POWERLINE
(powerline-default-theme)

                                        ; ICONS
(require 'all-the-icons)

;; Icon settings based off
;; https://github.com/domtronn/all-the-icons.el/wiki/Mode-Line

(defun mode-line/modified ()
  "Display an icon for the modified status of a buffer:

   A chain for unmodified.
   A broken chain for modified.
   A padlock for read-only. "
  (let* ((config-alist
          '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.2 :v-adjust -0.0)
            ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.2 :v-adjust -0.0)
            ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)))
         (result (cdr (assoc (format-mode-line "%*") config-alist))))
    (propertize (apply (cadr result) (cddr result))
                'face `(:family ,(funcall (car result))))))

(defun mode-line/major-mode (face)
  "Display the icon for the major mode of the current buffer."
  (let ((icon (propertize
               (format "%s "(all-the-icons-icon-for-buffer))
               'face `(:inherit ,face :height 1.1)
               'display '(raise -0.1))))
    (powerline-raw icon face 'l)))

(defun mode-line/git ()
  "Display a Git icon for files saved in Git repos."
  (when (and vc-mode (string-match "Git[:-]" vc-mode))
    (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
      (concat
       (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:height 1.2) 'display '(raise -0.1))
       " · "
       (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                   'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
                   'display '(raise -0.1))
       (propertize (format " %s" branch) 'face `(:height 0.9))))))

;; The default setup for powerline, customized with symbols in a few
;; locations.
(setq-default mode-line-format
      '("%e"
        (:eval
         (let*
             ((active
               (powerline-selected-window-active))
              (mode-line-buffer-id
               (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
              (mode-line
               (if active 'mode-line 'mode-line-inactive))
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
                " "
                (mode-line/modified)
                (when powerline-display-buffer-size
                  (powerline-buffer-size face0 'l))
                (when powerline-display-mule-info
                  (powerline-raw mode-line-mule-info face0 'l))
                (powerline-buffer-id
                 `(mode-line-buffer-id ,face0)
                 'l)
                (when
                    (and
                     (boundp 'which-func-mode)
                     which-func-mode)
                  (powerline-raw which-func-format face0 'l))
                (powerline-raw " " face0)
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
                (funcall separator-left face1 face2)
                (powerline-vc face2 'r)
                (when
                    (bound-and-true-p nyan-mode)
                  (powerline-raw
                   (list
                    (nyan-create))
                   face2 'l))))
              (rhs
               (list
                (powerline-raw global-mode-string face2 'r)
                (funcall separator-right face2 face1)
                (unless window-system
                  (powerline-raw
                   (char-to-string 57505)
                   face1 'l))
                (powerline-raw "%4l" face1 'l)
                (powerline-raw ":" face1 'l)
                (powerline-raw "%3c" face1 'r)
                (funcall separator-right face1 face0)
                (powerline-raw " " face0)
                (powerline-raw "%6p" face0 'r)
                (when powerline-display-hud
                  (powerline-hud face0 face2))
                (powerline-fill face0 0))))
           (concat
            (powerline-render lhs)
            (powerline-fill face2
                            (powerline-width rhs))
            (powerline-render rhs))))))
