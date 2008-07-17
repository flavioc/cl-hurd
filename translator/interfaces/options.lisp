
(in-package :hurd-translator)

(defun %get-options-callback (port data data-len)
  (with-lookup protid port
    (let* ((args0 (get-options *translator*))
           (args (cons
                   (name *translator*) args0))
           (len-args (mapcar (lambda (str) (1+ (length str)))
                             args))
           (total (reduce #'+ len-args)))
      (when (< (mem-ref data-len 'msg-type-number)
               total)
        (let ((new-ptr (mmap (make-pointer 0)
                             total
                             '(:prot-read :prot-write)
                             '(:map-anon)
                             0
                             0)))
          (when (null new-ptr)
            (return-from %get-options-callback :no-memory))
          (setf (mem-ref data :pointer) new-ptr)))
      (setf (mem-ref data-len 'msg-type-number) total)
      (let ((ptr (mem-ref data :pointer)))
        (loop for item in args
              for item-len in len-args
              do (progn
                   (lisp-string-to-foreign item
                                           ptr
                                           item-len)
                   (incf-pointer ptr item-len))))
      t)))
