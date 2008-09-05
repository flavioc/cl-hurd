
(in-package :hurd-streams)

(defclass hurd-output-stream (hurd-stream fundamental-binary-output-stream)
  ((cache :initform (%create-adjustable-array 0)
          :accessor cache)))

(defmethod stream-write-byte ((stream hurd-output-stream) byte)
  (vector-push-extend byte (cache stream)))

(defmethod stream-write-char ((stream hurd-output-stream) char)
  (stream-write-byte stream (char-code char)))

(defun %hurd-stream-write-warn (stream err)
  (warn "Error writing to hurd-output-stream ~s: ~s"
        stream err))

(defmethod %hurd-stream-inner-write ((stream hurd-output-stream))
  (with-accessors ((cache cache) (port port) (offset offset))
    stream
    (let ((total (fill-pointer cache)))
      (multiple-value-bind (total-written err)
        (io-write port cache :offset offset)
        (when err
          (%hurd-stream-write-warn stream err)
          (return-from %hurd-stream-inner-write nil))
        (incf offset total-written)
        (unless (= total-written total)
          (replace cache cache
                   :start2 total-written)
          (setf (fill-pointer cache) total-written)
          (%hurd-stream-inner-write stream))))))

(defun %hurd-stream-has-data-p (stream)
  (plusp (fill-pointer (cache stream))))

(defmethod %hurd-stream-write ((stream hurd-output-stream))
  (with-accessors ((cache cache)) stream
    (cond
      ((%hurd-stream-has-data-p stream)
       (when (%hurd-stream-inner-write stream)
         (setf (fill-pointer cache) 0)
         t))
      (t t))))

(defmethod stream-finish-output ((stream hurd-output-stream))
  (%hurd-stream-write stream))

(defmethod stream-force-output ((stream hurd-output-stream))
  (%hurd-stream-write stream))

(defmethod %hurd-stream-write-seq ((stream hurd-output-stream) seq)
  (with-accessors ((port port) (offset offset))
    stream
    (let ((total (length seq)))
      (multiple-value-bind (total-written err)
        (io-write port seq :offset offset)
        (when err
          (%hurd-stream-write-warn stream err)
          (return-from %hurd-stream-write-seq nil))
        (incf offset total-written)
        (unless (= total-written total)
          (%hurd-stream-write-seq
            stream
            (subseq seq total-written))))))
  t)

(defmethod stream-write-sequence ((stream hurd-output-stream)
                                  sequence start end &key)
  (when (%hurd-stream-write stream)
    (%hurd-stream-write-seq stream
                            (subseq sequence start end))))

(defmethod stream-start-line-p ((stream hurd-output-stream))
  nil)

(defmethod stream-line-column ((stream hurd-output-stream))
  nil)

(defmethod make-hurd-output-stream ((file string) &optional (flags '(:write)))
  (make-hurd-output-stream
    (file-name-lookup file :flags flags)))

(defmethod make-hurd-output-stream ((port number) &optional flags)
  (declare (ignore flags))
  (make-instance 'hurd-output-stream :port port))

(defmacro with-hurd-output-stream ((stream-name file &optional (flags ''(:write))) &body body)
  `(with-stream (,stream-name (make-hurd-output-stream ,file ,flags))
     ,@body))

