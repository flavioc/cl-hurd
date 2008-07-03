
(defclass open-node ()
  ((node :initarg :refers
		 :accessor refers)
   (file-pos :initform 0
			 :accessor file-offset)
   (lock-status :initform 'unlock) ; /usr/include/sys/file.h
   (openstat :initform nil
			 :initarg :flags
			 :accessor flags)
   (root-parent :initform nil
				:accessor root-parent)
   (shadow-root :initform nil
				:accessor shadow-root)
   (shadow-root-parent :initform nil
					   :accessor shadow-root-parent)))

(defun make-open-node (node flags
							&key (root-parent nil)
								 (shadow-root nil)
								 (shadow-root-parent nil))
  (let ((obj (make-instance 'open-node :refers node :flags flags)))
	(setf (root-parent obj) root-parent)
	(if (port-valid (root-parent obj))
	  (port-mod-refs (root-parent obj) :right-send 1))
	(setf (shadow-root obj) shadow-root)
	(setf (shadow-root-parent obj) shadow-root-parent)
	(if (port-valid shadow-root-parent)
	  (port-mod-refs shadow-root-parent :right-send 1))
	obj))
