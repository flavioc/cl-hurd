
(defpackage :mod-translator
  (:use :cl :hurd-common :mach
        :hurd :hurd-translator
        :hurd-tree-translator))

(in-package :mod-translator)

(defconstant +file+ (first ext:*args*))

(defclass mod-translator (tree-translator)
  ((file-stat :initarg :file-stat
              :initform nil
              :accessor file-stat)
   (dir-stat :initarg :dir-stat
             :initform nil
             :accessor dir-stat)
   (timestamp :initform nil
              :accessor timestamp
              :initarg :timestamp)))

(defclass dirty-entry ()
  ((dirty :initform nil
          :accessor dirty)))

(defclass mod-entry (dirty-entry entry)
  ((contents :initarg :data
             :initform nil
             :accessor data)))

(defclass mod-dir-entry (dirty-entry dir-entry) ())

(defun %create-data-array (size contents)
  (make-array size
              :initial-contents contents
              :adjustable nil
              :element-type '(unsigned-byte 8)))

(define-callback allow-open-p mod-translator
                 (node user flags is-new-p)
  (declare (ignore is-new-p))
  (when (flag-is-p flags :write)
    (return-from allow-open-p nil))
  (when (flag-is-p flags :read)
    (unless (has-access-p node user :read)
      (return-from allow-open-p nil)))
  t)

(define-callback read-file mod-translator
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

(define-callback refresh-node mod-translator
                 (node user)
  (declare (ignore node user))
  (with-port-deallocate (port (file-name-lookup +file+ :flags '(:read :notrans)))
    (let* ((stat (io-stat port))
           (new-timestamp (stat-get stat 'st-mtime)))
      (when (time-value-newer-p new-timestamp (timestamp translator))
        ; Mark every node as un-visited.
        (iterate-entries-deep (root translator)
                              (lambda (name node)
                                (declare (ignore name))
                                (setf (dirty node) nil)
                                t))
        (%update-data translator
                      (with-open-file (s +file+) (read s))
                      (root translator))
        ; Now remove the nodes we have not visited during the update.
        (iterate-entries-deep (root translator)
                              (lambda (name node)
                                (cond
                                  ((dirty node) t) ; Keep going down there
                                  (t
                                    (remove-dir-entry (parent node)
                                                      name)
                                    nil))))
        (setf (timestamp translator) new-timestamp)))))

(defun %update-data (translator ls node)
  (let* ((type (first ls))
         (name (second ls))
         (args (rest (rest ls)))
         (found (get-entry node name)))
    (case type
      (:dir
        (when (or (and found
                       (typep found 'mod-entry))
                  (not found))
          (when found
            (remove-dir-entry node name))
          (setf found
                (make-instance 'mod-dir-entry
                               :stat (make-stat (dir-stat translator))
                               :parent node))
          (add-entry node found name))
        (loop for item in args
              do (%update-data translator item found)))
      (:file
        (let ((data (first args)))
          (when (or (and found
                         (typep found 'mod-dir-entry))
                    (not found))
            (when found
              (remove-dir-entry node name))
            (setf found
                  (make-instance 'mod-entry
                                 :stat (make-stat
                                         (file-stat translator))
                                 :parent node))
            (add-entry node found name))
          ; Update file size.
          (setf (stat-get (stat found) 'st-size) (length data))
          ; Update byte array.
          (setf (data found) (%read-file-data data)))))
    ; Flag this node as visited.
    (setf (dirty found) t)))

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
        (let ((dir (make-instance 'mod-dir-entry
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

(define-callback fill-root-node mod-translator
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
  (%fill-node translator
              (with-open-file (s +file+) (read s))
              node))

(defun main ()
  (with-port-deallocate (port (file-name-lookup +file+ :flags '(:read)))
    (let ((translator
            (make-instance 'mod-translator
                           :timestamp (stat-get (io-stat port) 'st-mtime)
                           :name "mod-translator")))
      (run-translator translator))))

(main)

