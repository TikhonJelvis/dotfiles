((haskell-mode . ((haskell-process-type . ghci)
                  (haskell-process-wrapper-function . (lambda (argv)
                                                        (append
                                                         (list "nix" "run" "-f" "run.nix" "-c")
                                                         argv))))))
