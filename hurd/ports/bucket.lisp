
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

(defmethod add-new-port ((bucket port-bucket))
  "Adds a new port to bucket."
  (let ((port (make-instance 'port-info)))
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
    (when cleanup-fun
      (funcall cleanup-fun port))
    (port-cleanup port)))

(defun make-bucket (cleanup)
  "Create a new bucket with the cleanup function 'cleanup'."
  (make-instance 'port-bucket :cleanup cleanup))
