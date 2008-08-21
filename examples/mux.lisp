
(defpackage :mux-translator
  (:use :cl :hurd-common :mach
        :hurd :hurd-translator
        :hurd-tree-translator
        :split-sequence))

(in-package :mux-translator)

(unless (= (length ext:*args*) 2)
  (error "Argument syntax: <file list> <classification command>"))

(defconstant +file-list+ (first ext:*args*))
(defconstant +class-command+ (second ext:*args*))

(defclass mux-translator (tree-translator)
  ((timestamp :initform nil
              :initarg :timestamp
              :accessor timestamp)))

(defclass mux-entry (entry)
  ((port :initarg :port
         :initform nil
         :accessor port)))

(define-callback read-file mux-translator
                 (node user start amount stream)
  (when (has-access-p node user :read)
    (let ((data (io-read (port node)
                         :amount amount
                         :offset start)))
      (when data
        (write-sequence data stream))
      t)))

(define-callback write-file mux-translator
                 (node user offset stream amount)
  (unless (has-access-p node user :write)
    (return-from write-file nil))
  (when (is-dir-p (stat node))
    (return-from write-file :is-a-directory))
  (let ((arr (make-array amount :element-type '(unsigned-byte 8))))
    (read-sequence arr stream)
    (let ((ret (io-write (port node)
                         arr
                         :offset offset)))
      (cond
        ((numberp ret)
         (setf (stat node) (io-stat (port node)))
         t)
        (t ret)))))

(define-callback file-change-size mux-translator
                 (node user new-size)
  (when (is-dir-p (stat node))
    (return-from file-change-size :is-a-directory))
  (unless (has-access-p node user :write)
    (return-from file-change-size :operation-denied))
  (let ((err (file-set-size (port node) new-size)))
    (when (eq err t)
      (setf (stat node) (io-stat (port node))))
    err))

(define-callback create-directory mux-translator
                 (node user name mode)
  (declare (ignore node user name mode))
  nil)

(define-callback remove-directory-entry mux-translator
				 (node user name)
  (declare (ignore node user name))
  nil)

(define-callback create-hard-link mux-translator
                 (dir user file name)
  (declare (ignore dir user file name))
  nil)

(define-callback file-rename mux-translator
                 (user old-dir old-name new-dir new-name)
  (declare (ignore user old-dir old-name new-dir new-name))
  nil)

(define-callback allow-link-p mux-translator
                 (node user)
  (declare (ignore node user))
  nil)

(define-callback create-symlink mux-translator
                 (node user target)
  (declare (ignore node user target))
  nil)

(define-callback refresh-node mux-translator
                 (node user)
  (declare (ignore node user))
  (let ((new-timestamp (get-timestamp)))
    (when (time-value-newer-p new-timestamp (timestamp translator))
      (let ((root (root *translator*)))
        (clear-dir root)
        (mirror-file-list root)
        (setf (timestamp *translator*) new-timestamp)
        t))))

(defun read-file-lines (file)
  "Return a list of lines in file 'file'."
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun classify-node (file)
  (let ((cmd (concatenate-string +class-command+ " " file)))
    (with-stream (stream
                   (ext:make-pipe-input-stream cmd))
      (split-sequence #\/ (read-line stream nil)))))

(defun file-basename (file)
  (first (last
           (split-sequence #\/ file))))

(defun mirror-file (node dir-list full-name name port)
  (let ((last-p (null dir-list)))
    (cond
      (last-p
        (let ((file-stat (io-stat port)))
          (let ((entry (make-instance 'mux-entry
                                      :stat file-stat
                                      :parent node
                                      :port port)))
            (tg:finalize entry (lambda () (port-deallocate port)))
            (add-entry node entry name))))
      (t
        (let ((dir-name (first dir-list))
              (rest-dir-name (rest dir-list)))
          (let ((found (get-entry node dir-name)))
            (cond
              ((and found
                    (not (is-dir-p (stat found))))
               (warn "~s could not be classified" full-name)
               (port-deallocate port)
               (return-from mirror-file nil))
              ((not found)
               (setf found (make-instance 'dir-entry
                                          :parent node
                                          :stat (make-stat (stat node))))
               (add-entry node found dir-name)))
            (mirror-file found rest-dir-name full-name name port)))))))

(defun mirror-file-list (node)
  (loop for file in (read-file-lines +file-list+)
        do (let ((port (file-name-lookup file
                                         :flags '(:read :write :exec))))
             (when (port-valid-p port)
               (mirror-file node
                            (classify-node file)
                            file
                            (file-basename file)
                            port)))))

(define-callback fill-root-node mux-translator
                 ((node dir-entry))
  (mirror-file-list node))

(defun get-timestamp ()
  (with-port-deallocate (port (file-name-lookup +file-list+ :flags '(:read :notrans)))
    (stat-get (io-stat port) 'st-mtime)))

(defun main ()
  (let ((translator (make-instance 'mux-translator
                                   :name "mux-translator"
                                   :timestamp (get-timestamp))))
    (run-translator translator)))

(main)

