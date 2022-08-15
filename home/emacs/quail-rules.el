(let ((quail-current-package (assoc "TeX" quail-package-alist)))
  ;; A few extra symbols I find useful.
  (quail-define-rules ((append . t))
                      ("_i" ?ᵢ)
                      ("\\To" ?⇒)
                      ("\\::" ?∷)
                      ("''" ?`)
                      ("\\x" ?×)
                      ("\\tm" ?™)
                      ("\\grad" ?∇)
                      ("\\inc" ?∆)
                      ("\\sf" ?➲))

  ;; Use ; in place of \
  (quail-defrule ";" (quail-lookup-key "\\")))
