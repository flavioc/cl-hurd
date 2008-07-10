
(in-package :hurd-translator)

;;
;; This file implements the translator class.
;;

(defun %get-identity-port ()
  "Creates a new port as the translator identity port."
  (let ((port (port-allocate :right-receive)))
    (if (not (port-valid port))
      (error "Invalid fsys identity port")
      port)))

(defclass translator ()
  ((identity-port :initform (%get-identity-port)
                  :accessor identity-port
                  :documentation "The translator's identity port.")
   (port-bucket :initform nil
                :accessor port-bucket
                :documentation "The bucket, where we save all the translator ports.")
   (temporary-bucket :initform (make-hash-table)
                     :accessor temporary-bucket
                     :documentation "Temporary bucket to pass data between foreign calls.")
   (underlying-node :initform nil
                    :reader underlying-node
                    :documentation "The port to the underlying node where the translator is set.")
   (root-node :initform nil
              :accessor root
              :documentation "The root node.")
   (name :initform "cl-translator"
         :accessor name
         :documentation "Translator's name.")
   (version :initform (list 1 0 0)
            :accessor version
            :documentation "Translator version."))
  (:documentation "Translator class."))

(defmethod initialize-instance :after ((translator translator) &key)
  "Sets up the translator bucket with a port cleanup function."
  (setf (port-bucket translator)
        (make-bucket (lambda (protid)
                       (let ((node (get-node protid)))
                         (dec-refs node)
                         (when (no-refs-p node)
                           (pre-drop-node node)
                           (drop-node translator node))
                         t)))))

(defmethod insert-temporary-data ((trans translator) key value)
  "Inserts temporary data on the temporary bucket."
  (setf (gethash (temporary-bucket trans) key) value))

(defmethod get-temporary-data ((trans translator) key)
  "Gets inserted temporary data."
  (with-cleanup (remove-temporary-data trans key)
    (gethash (temporary-bucket trans) key)))

(defmethod remove-temporary-data ((trans translator) key)
  "Remove a temporary entry."
  (remhash (temporary-bucket trans) key))

(defmethod new-protid ((trans translator) user (open-node open-node))
  "Creates a new protid and inserts it into the translator bucket."
  (inc-refs (refers open-node)) ; Increment references to this node
  (add-port (port-bucket trans)
            (make-protid user open-node)))

(defun make-translator (&optional (base-class 'translator))
  "Creates a new translator with class 'base-class'."
  (let ((translator (make-instance base-class)))
    (with-accessors ((id-port identity-port)) translator
      ; Destroy identity port when translator goes away
      (finalize translator (lambda () (port-destroy id-port))))
    translator))

(defmethod setup ((translator translator) &optional (flags nil))
  "Gets the bootstrap port to call fsys-startup and create a new port into the bucket. After that we get the underlying node."
  (with-port-deallocate (bootstrap (task-get-bootstrap-port))
    (let ((port (add-control-port (port-bucket translator))))
      (with-port-deallocate (right (get-send-right port))
        (let ((file (fsys-startup bootstrap flags right :copy-send)))
          (setf (slot-value translator 'underlying-node) file)
          t)))))
