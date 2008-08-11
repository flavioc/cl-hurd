
(in-package :hurd)

(defcfun ("file_get_translator_cntl" %file-get-translator-cntl)
  err
  (file port)
  (control port-pointer))

(defun file-get-translator-cntl (file)
  "Get the control port from the translator."
  (with-foreign-pointer (control (foreign-type-size 'port))
    (select-error (%file-get-translator-cntl file control)
                  (mem-ref control 'port))))

