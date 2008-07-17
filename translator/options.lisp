
(in-package :hurd-translator)

(defclass translator-options ()
  ((table :initform (make-hash-table :test 'equal)
            :accessor table
            :documentation "Hash table mapping option names and values."))
  (:documentation "Class to manage translator options."))

(defun %get-symbol-name (symbol)
  (string-downcase (symbol-name symbol)))

(defun make-translator-options (&optional option-list)
  (let ((obj (make-instance 'translator-options)))
    (when option-list
      (set-translator-options obj option-list nil))
    obj))

(defun %obj-to-string (obj)
  (cond
    ((null obj)
     "")
    ((eq t obj)
     "yes")
    (t
      (prin1-to-string obj))))

(defmethod get-translator-options ((options translator-options))
  (loop for key being the hash-keys of (table options)
        using (hash-value value)
        collect (let ((base (concatenate 'string "--" key)))
                  (if (null value)
                    base
                    (concatenate 'string base "="
                                 (%obj-to-string value))))))

(defmethod set-translator-options ((options translator-options) option-list &optional (clear-old t))
  (with-accessors ((table table)) options
    ; Clear old options.
    (when clear-old
      (clrhash table))
    (loop for option in option-list
          do (cond
               ((listp option)
                (setf (gethash (first option) table)
                      (second option)))
               (t
                 (setf (gethash option table) nil))))
    t))

(defmethod has-translator-option-p ((options translator-options) (option string))
  (multiple-value-bind (val found-p)
    (gethash option (table options))
    found-p))

(defmethod get-translator-option ((options translator-options) (option string))
  (gethash option (table options)))

(defmethod print-object ((options translator-options) stream)
  (format stream "#<translator-options ~s>"
          (get-translator-options options)))
