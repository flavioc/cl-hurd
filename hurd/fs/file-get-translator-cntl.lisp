
(in-package :hurd)

(defcfun ("file_get_translator_cntl" %file-get-translator-cntl)
  err
  (file port)
  (control port-pointer))

(defun file-get-translator-cntl (file)
  (with-foreign-pointer (control (foreign-type-size 'port))
    (let ((err (%file-get-translator-cntl file control)))
      (select-error err
                    (mem-ref control 'port)))))
