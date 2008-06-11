
; load cffi
(asdf:oos 'asdf:load-op 'cffi)

; define used packages
(load "packages")

(defvar *helper-libraries-directory*
  (truename "./hurd/helper-libs/")) ; FIXME

(defvar *stub-libraries-directory*
  (truename "./stubs/")) ; FIXME

(setf cffi:*foreign-library-directories*
  (list #p"/lib/"
        *helper-libraries-directory*
		*stub-libraries-directory*))

; load some utilities
(load "common/common")

; load package's implementation
(load "mach/mach")
(load "hurd/hurd")
(load "translator/translator")
