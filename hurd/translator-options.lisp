
(in-package :hurd)

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

(defun %get-string-keyword (keyword)
  (concatenate-string "--"
                      (string-downcase (symbol-name keyword))))

(defun %build-option-string (base value)
  (if (null value)
    base
    (concatenate-string base "="
                        (%obj-to-string value))))

(defmethod get-translator-options ((options translator-options))
  "Build a list of string option assignments nearly ready to be passed down a foreign pointer, also good for printing."
  (iterate-options
    options
    (lambda (key value)
      (%build-option-string (%get-string-keyword key) value))))

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

(defmethod has-translator-option-p ((options translator-options) option)
  "Check if 'option' is enabled in this option set."
  (if (assoc option (table options) :test #'equal)
    t
    nil))

(defmethod get-translator-option ((options translator-options) option)
  "Returns the value assigned to 'option' if it exists."
  (let ((found (assoc option (table options) :test #'equal)))
    (when found
      (if (cdr found)
        (cdr found)
        t))))

(defmethod add-option ((options translator-options) option &optional value)
  "Add a new option to a set of translator options."
  (push (cons option value)
        (table options))
  options)

(defmethod iterate-options ((options translator-options) (fn function))
  "For each option/value in options call 'fn'."
  (loop for (key . value) in (table options)
        collect (funcall fn key value)))

(defun %get-keyword (str)
  (intern (string-upcase str) "KEYWORD"))

(defun %split-options (item)
  "Split options, examples:
'readonly' -> :readonly
'max-files=5' -> (:max-files 5)"
  (let ((pos (position #\= item)))
    (cond
      ((null pos) (%get-keyword item))
      (t
        (list (%get-keyword (subseq item 0 pos))
              (let* ((value (subseq item (1+ pos)))
                     (converted (read-from-string value)))
                (if (symbolp converted)
                  value
                  converted)))))))

(defun get-foreign-options (ptr len)
  (let* ((options-list (foreign-string-zero-separated-to-list
                         ptr len))
         (filtered-list ; Remove options without "--"
           (remove-if-not (lambda (item)
                            (and (> (length item) 2)
                                 (string= "--"
                                          (subseq item 0 2))))
                          options-list))
         (final-list ; Remove initial --
           (mapcar (lambda (item)
                     (%split-options (subseq item 2)))
                   filtered-list)))
    (make-translator-options final-list)))

(defmethod print-object ((options translator-options) stream)
  (format stream "#<translator-options ~a>"
          (get-translator-options options)))

