
(in-package :mach)

(defcfun ("mach_msg" %mach-msg)
  err
  (msg :pointer)
  (option msg-option)
  (send-size msg-size)
  (rcv-size msg-size)
  (rcv-name port)
  (timeout msg-timeout)
  (notify port))

(defgeneric msg-type-exists-p (obj))
(defgeneric msg-type-size (obj))
(defgeneric msg-type-number (type))
(defgeneric msg-type-data-p (obj data))
(defgeneric msg-type-to-msg-type (obj))
(defgeneric msg-type-set-data (type ptr data))
(defgeneric msg-type-get-data (type ptr))

(defmethod msg-type-exists-p ((obj t)) nil)
(defmethod msg-type-size ((obj t)) 0)
(defmethod msg-type-number ((type t)) 0)
(defmethod msg-type-data-p ((obj t) data) nil)
(defmethod msg-type-to-msg-type ((obj t)) nil)
(defmethod msg-type-set-data ((type t) ptr data) nil)
(defmethod msg-type-get-data ((type t) ptr) nil)

(defmacro msg-add-type (type-name &key size number test
                                  msg-type
                                  set
                                  get)
  `(progn
     (defmethod msg-type-exists-p ((obj (eql ,type-name))) t)
     (defmethod msg-type-size ((obj (eql ,type-name))) ,size)
     (defmethod msg-type-number ((obj (eql ,type-name))) ,number)
     (defmethod msg-type-data-p ((obj (eql ,type-name)) data) ,test)
     (defmethod msg-type-to-msg-type ((obj (eql ,type-name))) ,msg-type)
     (defmethod msg-type-set-data ((type (eql ,type-name)) ptr data) ,set)
     (defmethod msg-type-get-data ((type (eql ,type-name)) ptr) ,get)))

(msg-add-type :integer
              :size 4
              :number 1
              :test (numberp data)
              :msg-type :type-integer-32
              :set (setf (mem-ref ptr :int) data)
              :get (mem-ref ptr :int))

(msg-add-type :char
              :size 4
              :number 1
              :test (characterp data)
              :msg-type :type-char
              :set (setf (mem-ref ptr :char) (char-code data))
              :get (code-char (mem-ref ptr :char)))

(msg-add-type :real
              :size (foreign-type-size :double)
              :number 1
              :test (numberp data)
              :msg-type :type-integer-64
              :set (setf (mem-ref ptr :float) data)
              :get (mem-ref ptr :float))

(msg-add-type :string
              :size 1
              :number 1024
              :test (and (stringp data) (<= (length data) 1024))
              :msg-type :type-string
              :set (lisp-string-to-foreign data
                                           ptr
                                           (1+ (length data)))
              :get (foreign-string-to-lisp ptr))

(defclass message-spec ()
  ((fields :initform nil
           :accessor fields
           :initarg :fields)
   (id :initform 0
       :accessor id
       :initarg :id)
   (size :initform 0
         :initarg :size
         :accessor size)))

(defun make-message-spec (&key fields (id 0))
  (declare (type fixnum id))
  (declare (type cons fields))
  (validate-types fields)
  (make-instance 'message-spec
                 :fields fields
                 :size (calculate-size-total fields)
                 :id id))

(defclass message ()
   ((spec :initform nil
         :accessor spec
         :initarg :spec)
    (ptr :initform nil
        :accessor ptr
        :initarg :ptr)))

(defmethod size ((msg message)) (size (spec msg)))
(defmethod fields ((msg message)) (fields (spec msg)))

(defun msg-type-total-size (field)
  (* (msg-type-size field)
     (msg-type-number field)))

(defun calculate-size-total (fields)
  (+ +msg-header-size+
     (loop for field in fields
           sum (+ +msg-type-size+
                  (msg-type-total-size field)))))

(defun validate-types (fields)
  (loop for field in fields
        do (unless (msg-type-exists-p field)
             (error "Type ~s not recognized" field))))

(defun make-message (&key spec (ptr nil))
  (let ((ptr-null-p (null ptr)))
    (when ptr-null-p
      (setf ptr (foreign-alloc :char :count (size spec))))
    (let ((obj (make-instance 'message
                              :spec spec
                              :ptr ptr)))
      (when ptr-null-p
        (tg:finalize obj (lambda () (foreign-free ptr))))
      obj)))

(defun validate-data (fields datas)
  (assert (= (length fields) (length datas)))
  (loop for field in fields
        for data in datas
        do (unless (msg-type-data-p field data)
             (error "Data ~s is not of type ~s"
                    data field))))

(defun msg-type-bits (type)
  (num-bits (msg-type-size type)))

(defun has-timeout-p (timeout)
  (and (not (null timeout))
       (numberp timeout)
       (plusp timeout)))

(defun fill-msg-header (ptr size local remote id)
  (header-set-bits ptr
                   (if local
                     (msgh-bits :make-send :make-send-once)
                     (msgh-bits-remote :make-send)))
  (header-set-size ptr size)
  (header-set-local-port ptr local)
  (header-set-remote-port ptr remote)
  (when id
    (header-set-id ptr id)))

(defun build-msg-type-val (field)
  (let ((val 0))
    (setf val
          (set-type-name val (msg-type-to-msg-type field)))
    (setf val
          (set-type-size val (msg-type-bits field)))
    (setf val
          (set-type-number val (msg-type-number field)))
    (setf val
          (set-type-inline val t))
    (setf val
          (set-type-longform val nil))
    (setf val
          (set-type-deallocate val nil))
    val))

(defmethod send-message ((msg message) &key (local nil) remote data
                                       (timeout nil) (notify nil))
  (validate-data (fields msg) data)
  (let ((ptr (ptr msg))
        (size (size msg))
        (fields (fields msg)))
    (fill-msg-header ptr size local remote (id (spec msg)))
    (incf-pointer ptr +msg-header-size+)
    (loop for field in fields
          for data-field in data
          do (progn
               (setf (mem-ref ptr 'msg-type)
                     (build-msg-type-val field))
               (incf-pointer ptr +msg-type-size+)
               (msg-type-set-data field ptr data-field)
               (incf-pointer ptr (msg-type-total-size field))))
    (let* ((timeout-p (has-timeout-p timeout)))
      (%mach-msg (ptr msg)
                 (if timeout-p
                   '(:send-msg :send-timeout)
                   '(:send-msg))
                 size
                 0
                 nil
                 (if timeout-p timeout 0)
                 notify))))

(defmethod receive-message ((msg message) &key source (timeout nil) (notify nil))
  (let ((timeout-p (has-timeout-p timeout)))
    (when (eq t
              (%mach-msg (ptr msg)
                         (if timeout-p
                           '(:rcv-msg :rcv-timeout)
                           '(:rcv-msg))
                         0
                         (size msg)
                         source
                         (if timeout-p timeout 0)
                         notify))
      (validate-message msg))))

(defmethod validate-message ((msg message))
  (unless (eq (size msg) (header-get-size (ptr msg)))
    (return-from validate-message nil))
  (when (id (spec msg))
    (unless (eq (id (spec msg)) (get-message-id msg))
      (return-from validate-message nil)))
  (let ((ptr (inc-pointer (ptr msg)
                          +msg-header-size+)))
    (loop for field in (fields msg)
          do (let ((type-val (mem-ref ptr 'msg-type)))
               (unless (eq (get-type-name type-val)
                           (msg-type-to-msg-type field))
                 (return-from validate-message nil))
               (unless (eq (get-type-size type-val)
                           (msg-type-bits field))
                 (warn "failed eval type size")
                 (return-from validate-message nil))
               (unless (eq (get-type-number type-val)
                           (msg-type-number field))
                 (warn "failed eval type number")
                 (return-from validate-message nil))
               ; Jump to next message field.
               (incf-pointer ptr +msg-type-size+)
               (incf-pointer ptr (msg-type-total-size field))))
    t))


(defmethod get-message ((msg message))
  (let ((ptr (inc-pointer (ptr msg)
                          +msg-header-size+)))
    (loop for field in (fields msg)
          collect (progn
                    (incf-pointer ptr +msg-type-size+)
                    (with-cleanup (incf-pointer ptr (msg-type-total-size field))
                                  (msg-type-get-data field ptr))))))

(defmethod get-message-id ((msg message))
  (header-get-id (ptr msg)))

(defmethod get-message-local-port ((msg message))
  (header-get-local-port (ptr msg)))

(defmethod get-message-remote-port ((msg message))
  (header-get-remote-port (ptr msg)))

(defvar *p1* (port-allocate :right-receive))
(defparameter *spec-mixed* (make-message-spec
                             :fields '(:string :integer :char :string :integer :real)))
(defparameter *msg-mixed* (make-message :spec *spec-mixed*))
(print (send-message *msg-mixed* :remote *p1* :data (list "ola" 42 #\b "sim" 314 3.14)))
