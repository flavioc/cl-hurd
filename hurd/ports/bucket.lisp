
(in-package :hurd)

;;
;; This file implements a port bucket.
;; It groups various ports and a port set.
;;

(defvar *all-ports* (make-hash-table) "All the ports from all buckets.")

(defclass port-bucket ()
  ((port-set :initform (port-allocate :right-port-set)
             :accessor port-set
             :documentation "A port set containing all the ports in the bucket")
   (cleanup :initform nil
            :initarg :cleanup
            :accessor cleanup-function
            :documentation "To be run when a port is not used anymore")
   (hash :initform (make-hash-table)
         :accessor table
         :documentation "Contains the ports in this bucket"))
  (:documentation "The bucket class."))

(defmethod bucket-cleanup ((bucket port-bucket))
  "Destroys a port bucket."
  (with-accessors ((port port-set)) bucket
    (port-destroy port)))

(defmethod add-control-port ((bucket port-bucket))
  "Adds a new control port to bucket."
  (let ((port (make-instance 'port-info :control t)))
    (add-port bucket port)))

(defmethod add-port ((bucket port-bucket) (port port-info))
  "Adds port 'port' to bucket 'bucket'."
  (move-receive-right bucket (port-right port))
  (setf (gethash (port-right port) (table bucket)) port)
  ;; Also add this port to *all-ports* so we can look
  ;; it up when a no-senders notification arrives
  (setf (gethash (port-right port) *all-ports*)
        (list port bucket)) ; We save the port _and_ the bucket
  port)

(defmethod move-receive-right ((bucket port-bucket) right)
  "Moves the receive right to the bucket's port set."
  (port-move-member right (port-set bucket)))

(defmethod has-port ((bucket port-bucket) port)
  "Has 'bucket' the port 'port'?"
  (with-accessors ((table table)) bucket
    (multiple-value-bind (val found) (gethash port table)
      found)))

(defmethod lookup-port ((bucket port-bucket) port)
  "Returns a port-info with port name 'port' from 'bucket'."
  (with-accessors ((table table)) bucket
    (gethash port table)))

(defmethod remove-port ((bucket port-bucket) port)
  "Removes the port 'port' from the bucket 'bucket'."
  (with-accessors ((cleanup-fun cleanup-function)) bucket
    (remhash (port-right port) (table bucket))
    (set-send-rights port nil)
    (when (and cleanup-fun
               (port-is-user-p port)) ; Must be an user port.
      (funcall cleanup-fun port))
    (port-cleanup port)))

(defun make-bucket (cleanup)
  "Create a new bucket with the cleanup function 'cleanup'."
  (make-instance 'port-bucket :cleanup cleanup))

(defun bucket-total-users ((bucket port-bucket))
  "Return number of user ports"
  (loop for key being the hash-keys of (table bucket)
        using (hash-value value)
        sum (if (port-is-user-p value) 1 0)))

(defun bucket-total-control ((bucket port-bucket))
  "Return number of control ports"
  (loop for key being the hash-keys of (table bucket)
        using (hash-value value)
        sum (if (port-is-control-p value) 1 0)))

(defmethod bucket-statistics ((bucket port-bucket))
  "Prints bucket statistics."
  (format *error-output* "Bucket statistics:~%")
  (format *error-output* "User ports: ~a~%Control ports: ~a~%"
          (bucket-total-users bucket)
          (bucket-total-control bucket)))
