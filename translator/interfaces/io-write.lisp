
(in-package :hurd-translator)

(defun %foreign-vector-to-array (data len)
  "Transforms a foreign vector to a lisp array."
  (make-array len
              :initial-contents (loop for i from 0 below len
                                collect (mem-aref data :unsigned-char i))))

(def-io-interface :io-write ((port port)
                             (data :pointer)
                             (datalen msg-type-number)
                             (offset loff-t)
                             (amount :pointer))
  (with-lookup protid port
	  (block io-write
           (let ((open (open-node protid))
                 (node (get-node protid))
                 (user (get-user protid)))
             (unless (flag-is-p (flags open) 'write)
               (return-from io-write :invalid-argument))
             (when (%is-minus-one-p offset)
               (when (flag-is-p (flags open) 'append)
                 ;; Move file offset to the end of the file!
                 (setf (file-offset open)
                       (stat-get (stat node) 'size)))
               (setf offset (file-offset open)))
             (let ((data-array (%foreign-vector-to-array data datalen)))
               (with-input-from-sequence (stream data-array)
                 (let* ((ret (file-write *translator* node user offset stream))
                        (total (file-position stream)))
                   (incf (file-offset open) total)
                   (setf (mem-ref amount 'vm-size)
                         (if ret total 0))
                   t)))))))
