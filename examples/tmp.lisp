
(defpackage :tmp-translator
  (:use :cl :hurd-common :mach
        :hurd :hurd-translator
        :hurd-tree-translator))

(in-package :tmp-translator)

;;
;; This is a simple tmpfs translator.
;;

(defclass tmp-translator (tree-translator) ())

(defun %create-data-array ()
  (make-array 0
              :fill-pointer 0
              :adjustable t
              :element-type '(unsigned-byte 8)))

(defclass tmp-entry (entry)
  ((contents :initform (%create-data-array)
             :accessor data))
  (:documentation "A temporary node."))

(defmethod print-object ((entry tmp-entry) stream)
  (format stream "#<tmp-entry data=~s>" (data entry)))

(define-callback create-file tmp-translator
                 (node user filename mode)
  (unless (has-access-p node user :write)
    (return-from create-file nil))
  (let ((entry (make-instance 'tmp-entry
                              :stat (make-stat (stat node)
                                               :mode mode
                                               :size 0)
                              :parent node)))
    (add-entry node entry filename)
    entry))

(define-callback read-file tmp-translator
                 (node user start amount stream)
  (when (has-access-p node user :read)
    (let* ((size (stat-get (stat node) 'st-size))
           (size-res (- size start)))
      (unless (plusp size-res)
        (return-from read-file t))
      (let* ((total (min size-res amount))
             (end (+ start total)))
        (write-sequence (subseq (data node) start end)
                        stream)))))

(defun %read-sequence (stream amount)
  (let ((arr (make-array amount
                         :element-type '(unsigned-byte 8))))
    (read-sequence arr stream)
    arr))

(define-callback write-file tmp-translator
                 (node user offset stream amount)
  (unless (has-access-p node user :write)
    (return-from write-file nil))
  (when (is-dir-p (stat node))
    (return-from write-file :is-a-directory))
  (let* ((size (stat-get (stat node) 'st-size))
         (arr (%read-sequence stream amount))
         (amount (length arr))
         (final-size (max (+ amount offset) size)))
    (unless (= final-size size)
      (adjust-array (data node)
                    final-size
                    :fill-pointer t))
    (loop for octet across arr
          for i from offset
          do (setf (aref (data node) i) octet))
    ; Update stat size.
    (setf (stat-get (stat node) 'st-size) final-size)
    t))

(define-callback file-change-size tmp-translator
                 (node user new-size)
  (when (is-dir-p (stat node))
    (return-from file-change-size :is-a-directory))
  (when (is-owner-p node user)
    (adjust-array (data node) new-size :fill-pointer t)
    (setf (stat-get (stat node) 'st-size) new-size)
    t))

(define-callback create-anonymous-file tmp-translator
                 (node user mode)
  (unless (has-access-p node user :write)
    (return-from create-anonymous-file nil))
  (make-instance 'tmp-entry
                 :stat (make-stat (stat node) :mode mode)
                 :parent node))

(define-callback fill-root-node tmp-translator
                 ((node dir-entry))
  t)

(defun main ()
  (run-translator (make-instance 'tmp-translator
                                 :name "tmp-translator")))

(main)

