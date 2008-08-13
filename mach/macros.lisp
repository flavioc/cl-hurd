
(in-package :mach)

(define-condition port-invalid (error)
  ((expr :initarg :expr :reader expr)
   (ret :initarg :ret :reader ret))
  (:documentation "Signals an error if the returned port is invalid.")
  (:report (lambda (condition stream)
             (format stream "Port returned by the expression ~s (~s) is not valid."
                     (expr condition)
                     (ret condition)))))

(defun %get-nil-task (task)
  "Returns (task-self) if 'task' is nil, 'task' otherwise."
  (if (null task)
    '(task-self)
    task))

(defun with-port-generic (destroy port-name creation task
                                  body)
  "Generates code for port using and releasing."
  `(let ((,port-name ,creation))
     (cond
       ((port-valid-p ,port-name)
        (with-cleanup ,(funcall destroy port-name (%get-nil-task task))
                      ,@body))
       (t (error 'port-invalid :expr ',creation :ret ,port-name)))))

(defmacro with-port-deallocate ((port-name creation &optional (task nil))
				&body body)
  "Uses a port and then port-deallocate's"
  (with-port-generic (lambda (port task)
                       `(port-deallocate ,port ,task))
                     port-name creation task body))

(defmacro with-port-destroy ((port-name creation &optional (task nil))
			     &body body)
  "Uses a port and then port-destroy's"
  (with-port-generic (lambda (port task)
                       `(port-destroy ,port ,task))
                     port-name creation task body))

(defun %generate-release-list (port task ls)
  "Generate code for port and port rights releasing based on the 'ls' list."
  (loop for right in (cadr ls)
        collect (if (eq right :deallocate)
                  `(port-deallocate ,port ,task)
                  `(port-mod-refs ,port ,right -1 ,task))))

(defmacro with-port ((port-name creation &optional (task nil))
                     release-list &body body)
  "Generic with-port, uses a port initialized with 'creation'
and then releases rights.
release-list can have :deallocate or any port right specified to port-mod-rights."
  (with-port-generic (lambda (port task)
                       `(progn ,@(%generate-release-list port task release-list)))
                     port-name creation task body))
