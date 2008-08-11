
(in-package :hurd)

(defcfun ("fsys_set_options" %fsys-set-options)
  err
  (fsys port)
  (data :pointer)
  (data-len msg-type-number)
  (do-children :boolean))

(defun fsys-set-options (fsys &key options (do-children t))
  "Set the translator options. 'options' must be a translator options object. Pass T to 'do-children' to set options to children translator."
  (declare (type fixnum fsys)
           (type boolean do-children))
  (let* ((ls (get-translator-options options))
         (len-ls (string-list-len ls))
         (total (sum-list len-ls)))
    (with-foreign-pointer (ptr total)
      (list-to-foreign-string-zero-separated ls
                                             ptr
                                             len-ls)
      (select-error (%fsys-set-options fsys ptr total do-children)))))

