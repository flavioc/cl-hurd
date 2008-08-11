
(in-package :hurd)

(defcfun ("io_read" %io-read)
  err
  (file port)
  (data :pointer)
  (data-len :pointer)
  (offset :unsigned-long-long)
  (amount vm-size))

(defconstant +default-io-read-size+ 1024)

(defun io-read (file &key (offset +minus-one-ll+)
                     (amount +default-io-read-size+))
  (declare (type fixnum file amount)
           (type integer offset))
  (when (<= amount 0)
    (return-from io-read nil))
  (let ((initial-read-size (if (> amount 0)
                             (min amount +default-io-read-size+))))
    (with-foreign-pointer (data initial-read-size)
      (with-foreign-pointer (len (foreign-type-size 'msg-type-number))
        (setf (mem-ref len 'msg-type-number) initial-read-size)
        (with-foreign-pointer (ptr (foreign-type-size :pointer))
          (setf (mem-ref ptr :pointer) data)
          (let ((err (%io-read file ptr len offset amount)))
            (select-error err
                          (let ((new-ptr (mem-ref ptr :pointer))
                                (total (mem-ref len 'msg-type-number)))
                            (with-cleanup (unless (pointer-eq new-ptr data)
                                            (munmap new-ptr total))
                              (cond
                                ((= total 0) nil)
                                (t
                                  (let ((ret (make-array total :fill-pointer 0
                                                         :element-type '(unsigned-byte 8))))
                                    (loop for i from 0 below total
                                          do (vector-push
                                               (mem-aref new-ptr :unsigned-char i)
                                               ret))
                                    ret))))))))))))

