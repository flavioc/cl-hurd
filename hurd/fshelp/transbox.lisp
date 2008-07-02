
(defcstruct (transbox-struct :size 36))

(define-foreign-type transbox-type ()
		     ()
		     (:actual-type :pointer)
		     (:simple-parser transbox-t))

;(defclass transbox ()
;  ((ptr :documentation "Pointer to a transbox struct"
;	:initarg :ptr
;	:initform nil)))

(defclass transbox ()
  ((active :initarg :active
	   :accessor active
	   :initform nil)
   (starting :initform nil)
   (wanted :initform nil)
   (node :initform nil
		 :initarg :node
		 :accessor node)))

(defun make-transbox (node)
  (let ((obj (make-instance 'transbox :node node)))
    (finalize obj (lambda ()
		    (when (active obj)
		      (port-deallocate (active obj)))))
  obj))

(defmethod box-translated-p ((box transbox))
  (with-accessors ((port active)) box
	(port-valid port)))

(defmethod set-starting ((box transbox) v)
  (setf (slot-value box 'starting) v))

(defmethod set-wanted ((box transbox) v)
  (setf (slot-value box 'wanted) v))

(defmethod box-starting-p ((box transbox))
  (slot-value box 'starting))
