
(in-package :hurd-translator)

(defclass translator-options ()
  ((table :initform nil
          :accessor table
          :documentation "Alist mapping option names and values."))
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
    ((null obj) "")
    ((eq t obj) "yes")
    (t (prin1-to-string obj))))

(defmethod get-translator-options ((options translator-options))
  (loop for (key . value) in (table options)
        collect (let ((base (concatenate 'string "--" key)))
                  (if (null value)
                    base
                    (concatenate 'string base "="
                                 (%obj-to-string value))))))

(defmethod set-translator-options ((options translator-options)
                                   option-list &optional (clear-old t))
  ; Clear old options.
  (when clear-old
    (setf (table options) nil))
  (loop for option in option-list
        do (cond
             ((listp option)
              (push (cons (first option) (second option))
                    (table options)))
             (t
               (push (cons option nil) (table options)))))
  t)

(defmethod has-translator-option-p ((options translator-options) (option string))
  (if (assoc option (table options) :test #'equal)
    t
    nil))

(defmethod get-translator-option ((options translator-options) (option string))
  (let ((found (assoc option (table options) :test #'equal)))
    (when found
      (if (cdr found)
        (cdr found)
        t))))

(defmethod print-object ((options translator-options) stream)
  (format stream "#<translator-options ~s>"
          (get-translator-options options)))

