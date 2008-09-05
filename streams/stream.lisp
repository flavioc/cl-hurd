
(in-package :hurd-streams)

(defclass hurd-stream (trivial-gray-stream-mixin)
  ((port :initform nil
         :initarg :port
         :accessor port)
   (offset :initform 0
           :accessor offset)))

(defmethod initialize-instance :after ((stream hurd-stream) &rest initargs)
  (declare (ignore initargs))
  (with-accessors ((port port))
    stream
    (unless (port-valid-p port)
      (error "Port not valid: ~A" port))))

(defmethod print-object ((istream hurd-stream) stream)
  (format stream "#<HURD-STREAM port=~a offset=~a>"
          (port istream)
          (offset istream)))

(defmethod open-stream-p ((stream hurd-stream))
  "Returns a true value if STREAM is open."
  (port-valid-p (port stream)))

(defmethod close ((stream hurd-stream) &key abort)
  "Closes the stream STREAM."
  (declare (ignore abort))
  (when (open-stream-p stream)
    (port-deallocate (port stream))
    (setf (port stream) nil)))

(defmethod stream-element-type ((stream hurd-stream))
  "The element type is always unsigned-byte 8."
  '(unsigned-byte 8))

(defmethod stream-file-position ((stream hurd-stream))
  (offset stream))

(defmethod (setf stream-file-position) (position (stream hurd-stream))
  "Sets the file offfset."
  (case position
    (:end
      (setf (offset stream)
            (io-seek (port stream)
                     :offset 0
                     :whence :seek-end)))
    (otherwise
      (when (eq position :start)
        (setf position 0))
      (let ((new-offset (io-seek (port stream)
                                 :offset position
                                 :whence :seek-set)))
        (setf (offset stream) new-offset)))))

