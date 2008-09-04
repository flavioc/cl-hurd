
(in-package :hurd-streams)

(defconstant +default-read-ahead+ 512)

(defun %create-adjustable-array (&optional (size 0))
  (make-array size
              :fill-pointer size
              :adjustable t
              :element-type '(unsigned-byte 8)))

(defclass hurd-input-stream (trivial-gray-stream-mixin fundamental-binary-input-stream)
  ((port :initform nil
         :initarg :port
         :accessor port)
   (cache :initform (%create-adjustable-array +default-read-ahead+)
          :accessor cache)
   (last-byte :initform nil
              :accessor last-byte)
   (cache-pos :initform 0
              :accessor cache-pos)
   (offset :initform 0
           :accessor offset)))

(defun read-stream-cache (stream)
  (let ((data (io-read (port stream)
                       :offset (offset stream)
                       :amount +default-read-ahead+)))
    (setf (fill-pointer (cache stream)) (length data))
    (setf (cache-pos stream) 0)
    (replace (cache stream) data)
    t))

(defmethod initialize-instance :after ((stream hurd-input-stream) &rest initargs)
  (with-accessors ((port port) (cache cache) (cache-pos cache-pos))
      stream
    (unless (port-valid-p port)
      (error "Port not valid: ~A" port))
    (read-stream-cache stream)))

(defmethod print-object ((istream hurd-input-stream) stream)
  (format stream "#<HURD-INPUT-STREAM port=~a offset=~a>"
          (port istream)
          (offset istream)))

(defmethod open-stream-p ((stream hurd-input-stream))
  "Returns a true value if STREAM is open."
  (port-valid-p (port stream)))

(defmethod close ((stream hurd-input-stream) &key abort)
  "Closes the stream STREAM."
  (declare (ignore abort))
  (when (open-stream-p stream)
    (port-deallocate (port stream))
    (setf (cache stream) nil)
    (setf (port stream) nil)))

(defmethod stream-element-type ((stream hurd-input-stream))
  "The element type is always OCTET."
  '(unsigned-byte 8))

(defmethod stream-file-position ((stream hurd-input-stream))
  (offset stream))

(defmethod (setf stream-file-position) (position (stream hurd-input-stream))
  "Sets the file offfset."
  (with-cleanup (read-stream-cache stream)
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
          (setf (offset stream) new-offset))))))

(defun %hurd-eof-reached-p (cache)
  (not (and cache
            (plusp (length cache)))))

(defmethod stream-read-byte ((stream hurd-input-stream))
  (with-accessors ((port port) (cache cache)
                               (cache-pos cache-pos)
                               (offset offset)
                               (last-byte last-byte))
    stream
    (unless (< cache-pos (length cache))
      (read-stream-cache stream))
    (when (%hurd-eof-reached-p cache)
      (return-from stream-read-byte :eof))
    (let ((byte (elt cache cache-pos)))
      (incf offset)
      (incf cache-pos)
      (setf last-byte byte)
      byte)))

(defmethod stream-read-char ((stream hurd-input-stream))
  (let ((byte (stream-read-byte stream)))
    (case byte
      (:eof :eof)
      (otherwise
        (code-char byte)))))

(defmethod unread-byte (byte (stream hurd-input-stream))
  (with-accessors ((last-byte last-byte) (cache-pos cache-pos))
    stream
    (unless last-byte
      (error "No byte to unread from this stream."))
    (unless (= byte last-byte)
      (error "Last byte read was different from #x~X" byte))
    (setf last-byte nil)
    (decf (offset stream))
    (cond
      ((zerop cache-pos)
       (read-stream-cache stream))
      (t
        (decf cache-pos)))
    nil))

(defmethod stream-unread-char ((stream hurd-input-stream) char)
  (unread-byte (char-code char) stream))

(defmethod peek-byte ((stream hurd-input-stream)
                      &optional peek-type (eof-error-p t)
                      eof-value)
  (loop for octet = (read-byte stream eof-error-p eof-value)
        until (cond ((null peek-type))
                    ((eql octet eof-value))
                    ((eq peek-type t)
                     (plusp octet))
                    (t (= octet peek-type)))
        finally (unless (eql octet eof-value)
                  (unread-byte octet stream))
                (return octet)))

(defmethod stream-peek-char ((stream hurd-input-stream))
  (let ((byte (peek-byte stream)))
    (cond
      ((plusp byte)
       (code-char byte))
      (t byte))))

(defmethod stream-read-sequence ((stream hurd-input-stream)
                                 sequence start end &key)
  (let ((total (- end start)))
    (with-accessors ((cache cache) (cache-pos cache-pos)
                                   (offset offset))
      stream
      (when (or (%hurd-eof-reached-p cache)
                (= start end))
        (return-from stream-read-sequence start))
      (let* ((size-cache (length cache))
             (cache-rest (- size-cache cache-pos))
             (current-pos start)
             (this-size (min cache-rest total))
             (new-cache-pos (+ cache-pos this-size)))
        (replace sequence cache
                 :start1 start :end1 end
                 :start2 cache-pos :end2 new-cache-pos)
        (incf offset this-size)
        (setf cache-pos new-cache-pos)
        (cond
          ((= this-size total) end)
          (t
            (read-stream-cache stream)
            (stream-read-sequence stream sequence
                                  (+ start this-size)
                                  end)))))))
            
(defmethod make-hurd-input-stream ((file string))
  (make-hurd-input-stream
    (file-name-lookup file :flags '(:read))))

(defmethod make-hurd-input-stream ((port number))
  (make-instance 'hurd-input-stream
                 :port port))

(defmacro with-hurd-input-stream ((stream-name file) &body body)
  `(with-stream (,stream-name (make-hurd-input-stream ,file))
     ,@body))

