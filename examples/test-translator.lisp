
(defpackage :test-translator
  (:use :cl :hurd-common :mach
        :hurd :hurd-translator
        :hurd-tree-translator))

(in-package :test-translator)

(defconstant +file+ (first ext:*args*))

(defun %create-data-array (size contents)
  (make-array size
              :initial-contents contents
              :adjustable t
              :fill-pointer t
              :element-type '(unsigned-byte 8)))

(defclass test-translator (tree-translator)
  ((file-stat :initarg :file-stat
              :initform nil
              :accessor file-stat)
   (dir-stat :initarg :dir-stat
             :initform nil
             :accessor dir-stat)))

(defclass test-entry (entry)
  ((contents :initarg :data
             :initform (%create-data-array 0 nil)
             :accessor data)))

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
        t))))

(defun %read-sequence (stream amount)
  (let ((arr (make-array amount
                         :element-type '(unsigned-byte 8))))
    (read-sequence arr stream)
    arr))

(define-callback write-file test-translator
                 (node user offset stream amount)
  (unless (has-access-p node user :write)
    (return-from write-file nil))
  (when (is-dir-p (stat node))
    (return-from write-file :is-a-directory))
  (let* ((size (stat-get (stat node) 'st-size))
         (arr (%read-sequence stream amount))
         (final-size (max (+ amount offset) size)))
    (unless (= final-size size)
      (adjust-array (data node)
                    final-size
                    :fill-pointer t))
    (replace (data node) arr :start1 offset)
    ; Update stat size.
    (setf (stat-get (stat node) 'st-size) final-size)
    t))

(define-callback file-change-size test-translator
                 (node user new-size)
  (when (is-dir-p (stat node))
    (return-from file-change-size :is-a-directory))
  (when (has-access-p node user :write)
    (adjust-array (data node) new-size :fill-pointer t)
    (setf (stat-get (stat node) 'st-size) new-size)
    t))

(define-callback create-file test-translator
                 (node user filename mode)
  (unless (has-access-p node user :write)
    (return-from create-file nil))
  (let ((entry (make-instance 'test-entry
                              :stat (make-stat (stat node)
                                               :mode mode
                                               :size 0)
                              :parent node)))
    (add-entry node entry filename)
    entry))

(define-callback create-anonymous-file test-translator
                 (node user mode)
  (when (can-modify-dir-p node user)
    (make-instance 'test-entry
                   :stat (make-stat (stat node)
                                    :mode mode)
                   :parent node)))

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
               (file (make-instance 'test-entry
                                    :stat (make-stat
                                            (file-stat translator)
                                            :size (length data))
                                    :parent node
                                    :data (%read-file-data data))))
          (add-entry node file name)))
      (:link
        (let ((target (first args))
               (new (make-instance 'entry
                                    :stat (make-stat
                                            (file-stat translator)
                                            :type :lnk)
                                    :parent node)))
          (setf (link new) target)
          (add-entry node new name))))))

(define-callback fill-root-node test-translator
                 ((node dir-entry))
  (setf (file-stat translator)
        (make-stat (stat node)
                   :mode (make-mode :perms '((:owner :read)
                                             (:group :read)))
                   :type :reg)
        (dir-stat translator)
        (make-stat (stat node)
                   :mode (make-mode :perms '((:owner :read :exec)
                                             (:group :read :exec)))
                   :type :dir))
  (%fill-node translator (with-open-file (s +file+) (read s)) node))

(defun main ()
  (let ((translator
          (make-instance 'test-translator
                         :name "test-translator"
                         :version (list 1 2 3))))
    (run-translator translator)))

(main)

