
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

(defun file-set-translator (file &key
                                 (path nil) ; Passive path
                                 (passive-flags '(:set :excl))
                                 (active-flags '(:set :excl))
                                 (oldtrans-flags nil)
                                 (active-port nil)
                                 (active-poly :copy-send))
  "Set the file passive or/and active translator."
  (declare (type fixnum file)
           (type list path)
           (type list passive-flags active-flags oldtrans-flags)
           (type keyword active-poly))
  (let (ls-len (total 0) (ptr (null-pointer)))
    (when path
      (setf ls-len (string-list-len path))
      (setf total (sum-list ls-len))
      (setf ptr (foreign-alloc :char :count total))
      (list-to-foreign-string-zero-separated path ptr ls-len))
    (with-cleanup (when path
                    (foreign-free ptr))
      (select-error (%file-set-translator file
                                          passive-flags
                                          active-flags
                                          oldtrans-flags
                                          ptr
                                          total
                                          active-port
                                          active-poly)))))

