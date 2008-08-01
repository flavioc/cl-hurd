
(defpackage :test-translator
  (:use :cl :hurd-common :mach
        :hurd :hurd-translator
        :hurd-tree-translator))

(in-package :test-translator)

(assert (= (length ext:*args*) 1))
(defconstant +file+ (first ext:*args*))

(defclass test-translator (tree-translator)
  ((file-stat :initarg :file-stat
              :initform nil
              :accessor file-stat)
   (dir-stat :initarg :dir-stat
             :initform nil
             :accessor dir-stat)))

(defclass mod-entry (entry)
  ((contents :initarg :data
             :initform nil
             :accessor data)))

(define-callback allow-open-p test-translator
                 (node user flags is-new-p)
  (declare (ignore is-new-p))
  (when (flag-is-p flags :write)
    (return-from allow-open-p nil))
  (when (flag-is-p flags :read)
    (unless (has-access-p node user :read)
      (return-from allow-open-p nil)))
  t)

(define-callback read-file test-translator
                 (node user start amount stream)
  (when (has-access-p node user :read)
    (let* ((size (stat-get (stat node) 'st-size))
           (size-res (- size start)))
      (unless (plusp size-res)
        (return-from read-file t))
      (let* ((total (min size-res amount))
             (end (+ start total)))
        (write-sequence (subseq (data node) start end)
                        stream)
        ; Also write newline.
        (write-byte #x0A stream)
        t))))

(define-callback shutdown test-translator ()
  (warn "Mod translator going down!")
  t)

(defun %create-data-array (size contents)
  (make-array size
              :initial-contents contents
              :adjustable nil
              :element-type '(unsigned-byte 8)))

(defun %read-file-data (str)
  (%create-data-array (length str)
                      (loop for char across str
                            collect (char-code char))))

(defun %fill-node (translator ls node)
  (let ((type (first ls))
        (name (second ls))
        (args (rest (rest ls))))
    (case type
      (:dir
        (let ((dir (make-instance 'dir-entry
                                  :stat (make-stat (dir-stat translator))
                                  :parent node)))
          (add-entry node dir name)
          (loop for item in args
                do (%fill-node translator item dir))))
      (:file
        (let* ((data (first args))
               (file (make-instance 'mod-entry
                                    :stat (make-stat
                                            (file-stat translator)
                                            :size (length data))
                                    :parent node
                                    :data (%read-file-data data))))
          (add-entry node file name))))))

(define-callback fill-root-node test-translator
                 ((node dir-entry))
  (let ((data (%read-lisp-file)))
    (setf (file-stat translator)
          (make-stat (stat node)
                     :mode (make-mode :perms '((:owner :read)
                                               (:group :read)))
                     :type :reg))
    (setf (dir-stat translator)
          (make-stat (stat node)
                     :mode (make-mode :perms '((:owner :read :exec)
                                               (:group :read :exec)))
                     :type :dir))
    (%fill-node translator data node)))


(defun %read-lisp-file ()
  (with-open-file (stream +file+)
    (read stream)))

(defun main ()
  (with-port-deallocate (port (file-name-lookup +file+ :flags '(:read)))
    (let ((translator
            (make-instance 'test-translator
                           :name "test-translator"
                           :version (list 1 2 3))))
      (run-translator translator))))

(main)

