
(in-package :mach)

;; mach_msg function.
(defcfun ("mach_msg" %mach-msg)
  err
  (msg :pointer)
  (option msg-option)
  (send-size msg-size)
  (rcv-size msg-size)
  (rcv-name port)
  (timeout msg-timeout)
  (notify port))

;; A type must implement this methods.
(defgeneric msg-type-exists-p (obj))
(defgeneric msg-type-size (obj))
(defgeneric msg-type-number (type))
(defgeneric msg-type-data-p (obj data))
(defgeneric msg-type-to-msg-type (obj))
(defgeneric msg-type-set-data (type ptr data))
(defgeneric msg-type-get-data (type ptr))

;; Generic methods for the base type.
(defmethod msg-type-exists-p ((obj t))
  (declare (ignore obj))
  nil)
(defmethod msg-type-size ((obj t))
  (declare (ignore obj))
  0)
(defmethod msg-type-number ((type t))
  (declare (ignore type))
  0)
(defmethod msg-type-data-p ((obj t) data)
  (declare (ignore obj data))
  nil)
(defmethod msg-type-to-msg-type ((obj t))
  (declare (ignore obj))
  nil)
(defmethod msg-type-set-data ((type t) ptr data)
  (declare (ignore type ptr data))
  nil)
(defmethod msg-type-get-data ((type t) ptr)
  (declare (ignore type ptr))
  nil)

(defmacro msg-add-type (type-name &key size number test
                                  msg-type
                                  set
                                  get)
  "Add a new message type with name 'type-name'.
'size' is an expression that calculates the type size.
'number' indicates the number of items in this type.
'test' a test to check if a specific object is inside the type domain.
'msg-type' indicates the message type code.
'set' is an expression that sets the memory area to this type.
'get' returns the object from a memory region.
size * number gives bytes occupied by an object of this type.
"
  `(progn
     (defmethod msg-type-exists-p ((obj (eql ,type-name))) t)
     (defmethod msg-type-size ((obj (eql ,type-name))) ,size)
     (defmethod msg-type-number ((obj (eql ,type-name))) ,number)
     (defmethod msg-type-data-p ((obj (eql ,type-name)) data) ,test)
     (defmethod msg-type-to-msg-type ((obj (eql ,type-name))) ,msg-type)
     (defmethod msg-type-set-data ((type (eql ,type-name)) ptr data) ,set)
     (defmethod msg-type-get-data ((type (eql ,type-name)) ptr) ,get)))

(msg-add-type :integer
              :size (foreign-type-size :integer)
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
           :accessor spec-fields
           :initarg :fields
           :documentation "List of types of this spec.")
   (id :initform 0
       :accessor spec-id
       :initarg :id
       :documentation "Message spec id, if any.")
   (size :initform 0
         :initarg :size
         :accessor spec-size
         :documentation "Total size of messages of this kind."))
  (:documentation "A spec is a combination of types that generates new kind of messages."))

(defun make-message-spec (&key fields (id 0))
  "Create a new message spec."
  (declare (type fixnum id))
  (declare (type cons fields))
  (validate-types fields)
  (make-instance 'message-spec
                 :fields fields
                 :size (calculate-size-total fields)
                 :id id))

(defclass message ()
   ((spec :initform nil
         :accessor msg-spec
         :initarg :spec
         :documentation "Spec of this message.")
    (ptr :initform nil
        :accessor ptr
        :initarg :ptr
        :documentation "Foreign pointer to a message structure."))
   (:documentation "The message object, with a spec and a memory structure."))

(defmethod msg-size ((msg message)) (spec-size (msg-spec msg)))
(defmethod msg-fields ((msg message)) (spec-fields (msg-spec msg)))

(defun msg-type-total-size (field)
  "Returns the size of a message type."
  (* (msg-type-size field)
     (msg-type-number field)))

(defun calculate-size-total (fields)
  "Returns the size of a type field."
  (+ +msg-header-size+
     (loop for field in fields
           sum (+ +msg-type-size+
                  (msg-type-total-size field)))))

(defun validate-types (fields)
  "Validates the existence of the list of types 'fields'."
  (loop for field in fields
        do (unless (msg-type-exists-p field)
             (error "Type ~s not recognized" field))))

(defun make-message (&key spec (ptr nil))
  "Creates a new message with the given spec and possibly a memory pointer."
  (let ((ptr-null-p (null ptr)))
    (when ptr-null-p
      (setf ptr (foreign-alloc :char :count (spec-size spec))))
    (let ((obj (make-instance 'message
                              :spec spec
                              :ptr ptr)))
      (when ptr-null-p
        (tg:finalize obj (lambda () (foreign-free ptr))))
      obj)))

(defun validate-data (fields datas)
  "Checks if the 'data' list only contains types specified in the 'fields' list."
  (assert (= (length fields) (length datas)))
  (loop for field in fields
        for data in datas
        do (unless (msg-type-data-p field data)
             (error "Data ~s is not of type ~s"
                    data field))))

(defun msg-type-bits (type)
  "Returns number of bits in a message type."
  (num-bits (msg-type-size type)))

(defun has-timeout-p (timeout)
  "Check if 'timeout' is really a timeout value."
  (and (not (null timeout))
       (numberp timeout)
       (plusp timeout)))

(defun fill-msg-header (ptr size local remote id)
  "Fill the foreign pointer 'ptr' with size 'size', and ports 'remote' and 'local'. The message id is 'id'."
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
  "Create a message val field, specifying a type."
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
  "Sends the message 'msg' with data 'data' to 'remote' with source 'local'.
A timeout value may be specified. A notification port 'notify' can also be passed."
  (validate-data (msg-fields msg) data)
  (let ((ptr (ptr msg))
        (size (msg-size msg))
        (fields (msg-fields msg)))
    (fill-msg-header ptr size local remote (spec-id (msg-spec msg)))
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
  "Receives a message in port 'source' to 'msg' with a specific timeout (or none with nil), with the notification port 'notify'."
  (let ((timeout-p (has-timeout-p timeout)))
    (when (eq t
              (%mach-msg (ptr msg)
                         (if timeout-p
                           '(:rcv-msg :rcv-timeout)
                           '(:rcv-msg))
                         0
                         (msg-size msg)
                         source
                         (if timeout-p timeout 0)
                         notify))
      (validate-message msg))))

(defmethod validate-message ((msg message))
  "Validates the message present in the foreign pointer."
  (unless (eq (msg-size msg) (header-get-size (ptr msg)))
    (return-from validate-message nil))
  (when (spec-id (msg-spec msg))
    (unless (eq (spec-id (msg-spec msg)) (get-message-id msg))
      (return-from validate-message nil)))
  (let ((ptr (inc-pointer (ptr msg)
                          +msg-header-size+)))
    (loop for field in (msg-fields msg)
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
  "Returns the message data in 'msg' as a list of objects."
  (let ((ptr (inc-pointer (ptr msg)
                          +msg-header-size+)))
    (loop for field in (msg-fields msg)
          collect (progn
                    (incf-pointer ptr +msg-type-size+)
                    (with-cleanup (incf-pointer ptr (msg-type-total-size field))
                                  (msg-type-get-data field ptr))))))

(defmethod get-message-id ((msg message))
  "Returns the message id of this message."
  (header-get-id (ptr msg)))

(defmethod get-message-local-port ((msg message))
  "Returns the local port of this message."
  (header-get-local-port (ptr msg)))

(defmethod get-message-remote-port ((msg message))
  "Returns the remote port of this message."
  (header-get-remote-port (ptr msg)))

; Example code:
; (defvar *p1* (port-allocate :right-receive))
; (defvar *spec-mixed* (make-message-spec
;                             :fields '(:string :integer :char :string :integer :real)))
; (defvar *msg-mixed* (make-message :spec *spec-mixed*))
; (send-message *msg-mixed* :remote *p1* :data (list "abc" 42 #\b "cba" 314 3.14))
; (receive-message *msg-mixed* :source *p1*)
; (get-message *msg-mixed*) -> '("abc" 42 #\b "cba" 314 3.14)
