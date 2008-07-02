
(defctype uid-t :int)
(defctype gid-t :int)

(defun valid-id-p (id)
  (not (= -1 id)))
