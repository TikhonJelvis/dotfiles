(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(describe-char-unidata-list
   (quote
    (name old-name general-category decomposition uppercase lowercase titlecase)))
 '(magit-commit-ask-to-stage (quote stage))
 '(safe-local-variable-values
   (quote
    ((python-shell-interpreter . "bin/nix-aware-python")
     (python-shell-interpreter . "nix-shell"))))
 '(send-mail-function (quote sendmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 `(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height ,(if (eq system-type 'darwin) 150 122) :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(company-scrollbar-bg ((t (:background "#19b3224446ac"))))
 '(company-scrollbar-fg ((t (:background "#12df192a33e6"))))
 '(company-tooltip ((t (:inherit default :background "#0ec713b428a3"))))
 '(company-tooltip-annotation ((t (:inherit font-lock-builtin-face))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(erc-input-face ((t (:foreground "cornflower blue"))))
 '(erc-my-nick-face ((t (:foreground "CornflowerBlue" :weight bold))))
 '(flycheck-error ((t (:underline "red"))))
 '(flycheck-warning ((t (:underline "darkorange"))))
 '(flymake-error ((t (:background "#00000000" :underline "red"))))
 '(flymake-warning ((t (:background "#00000000" :underline "dark orange"))))
 '(sgml-namespace ((t (:inherit font-lock-builtin-face)))))
