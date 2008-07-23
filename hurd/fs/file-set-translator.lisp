
(in-package :hurd)

(defcfun ("file_set_translator" %file-set-translator)
  err
  (file port)
  (passive-flags fs-trans-flags)
  (active-flags fs-trans-flags)
  (oldtrans-flags fsys-goaway-flags)
  (passive :pointer)
  (passive-cnt msg-type-number)
  (active port)
  (active-type msg-type-name))

(defun file-set-translator (file path
                                 &optional (flags '(:set))
                                 (oldtrans-flags nil))
  "Set the file passive translator to 'path'."
  (declare (type fixnum file)
           (type cons flags oldtrans-flags path))
  (let* ((ls-len (string-list-len path))
         (total (sum-list ls-len)))
    (with-foreign-pointer (ptr total)
      (list-to-foreign-string-zero-separated path ptr ls-len)
      (let ((err (%file-set-translator file
                                       flags
                                       nil
                                       oldtrans-flags
                                       ptr
                                       total
                                       nil
                                       :make-send))) ; Can be anything.
        (select-error err)))))

(defun file-remove-translator (file &optional (flags '(:set)))
  "Remove an active translator from 'file'."
  (declare (type fixnum file)
           (type list flags))
  (select-error (%file-set-translator file
                                      nil
                                      flags
                                      nil
                                      (null-pointer)
                                      0
                                      nil
                                      :make-send)))

