
(in-package :hurd-translator)

;;
;; This file implements the translator class.
;;

(defun %get-identity-port ()
  "Creates a new port as the translator identity port."
  (port-allocate :right-receive))

(defclass translator ()
  ((identity-port :initform (%get-identity-port)
                  :accessor identity-port
                  :documentation "The translator's identity port.")
   (port-bucket :initform (make-bucket)
                :accessor port-bucket
                :documentation "The bucket, where we save all the translator ports.")
   (underlying-node :initform nil
                    :reader underlying-node
                    :documentation "The port to the underlying node where the translator is set.")
   (root-node :initform nil
              :accessor root
              :documentation "The root node.")
   (statfs :initform (make-statfs)
           :accessor get-statfs
           :documentation "File system statistics")
   (name :initform "cl-translator"
         :accessor name
         :documentation "Translator's name.")
   (version :initform (list 1 0 0)
            :accessor version
            :documentation "Translator version.")
   (flags :initform nil
          :reader flags
          :initarg flags
          :documentation "Startup translator flags to be passed to fsys-startup.")
   (options :initform (make-translator-options)
            :accessor options
            :initarg :options
            :documentation "Translator options."))
  (:documentation "Translator class."))

(defmethod new-protid ((trans translator) user (open-node open-node))
  "Creates a new protid and inserts it into the translator bucket."
  (add-port (port-bucket trans)
            (make-protid user open-node)))

(defmethod initialize-instance :after ((translator translator) &key)
  "Gets the bootstrap port to call fsys-startup and installs a new control port into the bucket."
  (with-port-deallocate (bootstrap (task-get-bootstrap-port))
    (let ((port (add-control-port (port-bucket translator))))
      (with-port-deallocate (right (get-send-right port))
        (setf (slot-value translator 'underlying-node)
              (fsys-startup bootstrap (flags translator) right :copy-send)))))
  ;; Destroy identity port when translator goes away.
  (with-accessors ((id identity-port)) translator
    (finalize translator (lambda () (port-destroy id)))))

(defmethod running-p ((translator translator))
  (slot-value translator 'underlying-node))
