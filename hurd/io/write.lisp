
(in-package :hurd)

(defcfun ("io_write" %io-write)
  err
  (file port)
  (data :pointer)
  (data-cnt msg-type-number)
  (offset :unsigned-long-long)
  (amount :pointer))

(defun %convert-item (item)
  (if (typep item 'character)
    (char-code item)
    item))

(defmethod %write-sequence-to-ptr ((ls cons) total ptr)
  (loop for i from 0 below total
        for item in ls
        do (setf (mem-aref ptr :unsigned-char i)
                 (%convert-item item))))

(defmethod %write-sequence-to-ptr ((arr vector) total ptr)
  (loop for i from 0 below total
        for item across arr
        do (setf (mem-aref ptr :unsigned-char i)
                 (%convert-item item))))

(defun io-write (file data &key (offset +minus-one-ll+))
  "Write 'data' to 'file' starting at 'offset'. Data can be a string or an octet sequence/array."
  (declare (type fixnum file)
           (type integer offset))
  (let ((total (length data)))
    (when (zerop total)
      (return-from io-write 0))
    (with-foreign-pointer (ptr total)
      (with-foreign-pointer (amount (foreign-type-size 'vm-size))
        (%write-sequence-to-ptr data total ptr)
        (let ((err (%io-write file ptr total offset amount)))
          (select-error err
                        (mem-ref amount 'vm-size)))))))

