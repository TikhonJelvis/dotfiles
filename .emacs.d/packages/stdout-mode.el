(require 'man)

(define-derived-mode stdout-mode fundamental-mode "STDOUT"
  "Stdout mode is a simple little mode for paging stdout. It
handles ANSI colors and man-style “overstriking” (using ‘’ to
specify bold parts of the text)."
  (ansi-color-apply-on-region (point-min) (point-max))
  (Man-fontify-manpage))

(provide 'stdout-mode)
