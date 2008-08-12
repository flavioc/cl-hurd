
(in-package :hurd-translator)

(defun %get-options-callback (port data data-len)
  "Writes translator options to data foreign pointer."
  (when (port-exists-p port)
    (let* ((opts (options *translator*))
           (args0 (when opts (get-translator-options opts)))
           (args (cons
                   (name *translator*) args0))
           (len-args (string-list-len args))
           (total (sum-list len-args)))
      (when (< (mem-ref data-len 'msg-type-number)
               total)
          (setf (mem-ref data :pointer)
                (mmap (null-pointer)
                      total
                      '(:prot-read :prot-write)
                      '(:map-anon)
                      0
                      0)))
      (setf (mem-ref data-len 'msg-type-number) total)
      (list-to-foreign-string-zero-separated args
                                             (mem-ref data :pointer)
                                             len-args)
      t)))

