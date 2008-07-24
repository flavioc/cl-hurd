
(in-package :hurd-example-translators)

;;
;; This is a simple tmpfs translator.
;;

(defclass tmp-translator (tree-translator)
  ((name :initform "tmp-translator")))

(defun %create-data-array ()
  (make-array 0
              :fill-pointer 0
              :adjustable t
              :element-type '(unsigned-byte 8)))

(defclass tmp-entry (entry)
  ((contents :initform (%create-data-array)
             :accessor data))
  (:documentation "A temporary node."))

(define-callback create-file tmp-translator
                 (node user filename mode)
  (unless (has-access-p node user 'write)
    (return-from create-file nil))
  (let ((entry (make-instance 'tmp-entry
                              :stat (make-stat (stat node)
                                               :mode mode
                                               :size 0)
                              :parent node)))
    (add-entry node entry filename)
    entry))

(define-callback file-read tmp-translator
                 (node user start amount stream)
  (when (has-access-p node user 'read)
    (let* ((size (stat-get (stat node) 'size))
           (size-res (- size start)))
      (unless (plusp size-res)
        (return-from file-read t))
      (let* ((total (min size-res amount))
             (end (+ start total)))
        (write-sequence (subseq (data node) start end)
                        stream)))))

(defun %read-sequence (stream)
  (let ((arr (%create-data-array)))
    (loop for c = (read-byte stream nil)
          while c
          do (vector-push-extend c arr))
    arr))

(define-callback file-write tmp-translator
                 (node user offset stream)
  (unless (has-access-p node user 'write)
    (return-from file-write nil))
  (let* ((size (stat-get (stat node) 'size))
         (arr (%read-sequence stream))
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
    (setf (stat-get (stat node) 'size) final-size)
    t))

(define-callback file-change-size tmp-translator
                 (node user new-size)
  (when (is-owner-p node user)
    (adjust-array (data node) new-size :fill-pointer t)
    t))

(define-callback shutdown tmp-translator ()
  (warn "Temporary translator going down!")
  t)

(define-callback create-anonymous-file tmp-translator
                 (node user mode)
  (unless (has-access-p node user 'write)
    (return-from create-anonymous-file nil))
  (make-instance 'tmp-entry
                 :stat (make-stat (stat node) :mode mode)
                 :parent node))

(define-callback fill-root-node tmp-translator
                 ((node dir-entry))
  t)

(defun main ()
  (run-translator (make-instance 'tmp-translator)))

(main)
