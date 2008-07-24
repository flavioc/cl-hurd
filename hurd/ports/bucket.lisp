
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
  (add-port bucket
            (make-instance 'port-info)))

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
      (declare (ignore val))
      found)))

(defmethod lookup-port ((bucket port-bucket) port)
  "Returns a port-info with port name 'port' from 'bucket'."
  (with-accessors ((table table)) bucket
    (gethash port table)))

(defmethod remove-port ((bucket port-bucket) port &optional (cleanup t))
  "Removes the port 'port' from the bucket 'bucket'."
  (with-accessors ((table table)) bucket
    (remhash (port-right port) table)
    (when cleanup
      (port-cleanup port))))

(defmethod bucket-iterate ((bucket port-bucket) fn)
  "Apply 'fn' for each port-info in 'bucket'."
  (maphash (lambda (key value)
             (declare (ignore key))
             (funcall fn value)) (table bucket)))

(defun make-bucket ()
  "Create a new bucket with the cleanup function 'cleanup'."
  (make-instance 'port-bucket))

(defmethod bucket-count ((bucket port-bucket))
  "Count total number of ports in bucket."
  (hash-table-count (table bucket)))

(defmethod bucket-count-type ((bucket port-bucket) this-type)
  "Count number of ports with a certain type."
  (loop for key being the hash-keys of (table bucket)
        using (hash-value value)
        sum (if (typep value this-type)
              1 0)))

(defmethod bucket-find ((bucket port-bucket) fn)
  "Find a port that satisfies 'fn'."
  (loop for key being the hash-keys of (table bucket)
        using (hash-value value)
        when (funcall fn value)
        return value))

