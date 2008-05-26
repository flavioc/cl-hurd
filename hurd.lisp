
; load cffi
(asdf:oos 'asdf:load-op 'cffi)

; define used packages
(load "packages.lisp")

; load package's implementation
(load "mach/mach.lisp")
