(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(describe-char-unidata-list
   '(name old-name general-category decomposition uppercase lowercase titlecase))
 '(package-selected-packages
   '(nerd-icons bazel cargo company-box consult direnv docker-compose-mode dockerfile-mode el-patch ess exec-path-from-shell flycheck-posframe flycheck-rust forge format-all git-modes htmlize journalctl-mode js2-mode json-mode lean4-mode lsp-haskell lsp-metals lsp-pyright lsp-ui lua-mode marginalia nerd-icons-dired nix-mode org-bullets org-ql ox-reveal paredit pdf-tools powerline projectile python-black python-docstring python-pytest rainbow-mode request restclient rust-mode sbt-mode selectrum-prescient tide typescript-mode unisonlang-mode use-package visual-fill-column xterm-color yasnippet))
 '(safe-local-variable-values
   '((package-lint-main-file . "haskell-mode-pkg.el")
     (lsp-haskell-plugin-hlint-config-flags .
                                            ["--language=QuasiQuotes"])
     (lsp-haskell-plugin-hlint-config-flags .
                                            ["-XQuasiQuotes"])
     (python-shell-interpreter . "bin/nix-aware-python")
     (python-shell-interpreter . "nix-shell")))
 '(send-mail-function 'sendmail-send-it)
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "Input Mono Condensed"))))
 '(company-scrollbar-bg ((t (:background "#19b3224446ac"))) t)
 '(company-scrollbar-fg ((t (:background "#12df192a33e6"))) t)
 '(company-tooltip ((t (:inherit default :background "#0ec713b428a3"))))
 '(company-tooltip-annotation ((t (:inherit font-lock-builtin-face))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-scrollbar-thumb ((t (:background "DarkOrange"))))
 '(company-tooltip-scrollbar-track ((t (:background "#4ccc4cccfffe"))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(erc-input-face ((t (:foreground "cornflower blue"))))
 '(erc-my-nick-face ((t (:foreground "CornflowerBlue" :weight bold))))
 '(flycheck-error ((t (:underline "red"))))
 '(flycheck-warning ((t (:underline "darkorange"))))
 '(flymake-error ((t (:background "#00000000" :underline "red"))))
 '(flymake-warning ((t (:background "#00000000" :underline "dark orange"))))
 '(lsp-lsp-flycheck-info-unnecessary-face ((t (:underline (:color "#3366FF" :style line)))) t)
 '(sgml-namespace ((t (:inherit font-lock-builtin-face))))
 '(variable-pitch ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "Input Sans Condensed")))))
