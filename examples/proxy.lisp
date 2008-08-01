
(defpackage :proxy-translator
  (:use :cl :hurd-common :mach
        :hurd :hurd-translator
        :hurd-tree-translator))

(in-package :proxy-translator)

(defclass proxy-translator (tree-translator)
  ())

(defclass underlying-entry ()
  ((port :initarg :port
         :initform nil
         :accessor port)))

(defclass proxy-entry (underlying-entry entry) ())

(defclass proxy-dir-entry (underlying-entry dir-entry) ())

(define-callback read-file proxy-translator
                 (node user start amount stream)
  (when (has-access-p node user :read)
    (let ((data (io-read (port node)
                         :amount amount
                         :offset start)))
      (when data
        (loop for item across data
              do (cond
                   ((and (>= item 97)
                         (<= item 122))
                    (write-byte (- item 32) stream))
                   (t (write-byte item stream)))))
      t)))

(define-callback shutdown proxy-translator ()
  (warn "Proxy translator going down!"))

(defun %get-file-type (stat)
  (cond
    ((is-dir-p stat) 'proxy-dir-entry)
    (t 'proxy-entry)))

(defconstant +max-dir-entries+ 2048)

(defun %fetch-nodes (node)
  (when (is-dir-p (stat node))
    (let ((entries (dir-readdir (port node)
                                :nentries +max-dir-entries+)))
      (loop for dirent in entries
            do (let ((name (dirent-name dirent)))
                 (unless (or (string= name ".")
                             (string= name ".."))
                   (let* ((port (file-name-lookup name
                                                  :under (port node)
                                                  :flags '(:read :nolink :notrans)))
                          (stat (io-stat port))
                          (entry (make-instance (%get-file-type stat)
                                                :stat stat
                                                :port port
                                                :parent node)))
                     (add-entry node entry name)
                     (when (is-dir-p stat)
                       (%fetch-nodes entry)))))))))

(define-callback make-root-node proxy-translator
                 (underlying-node underlying-stat)
  (let ((node (make-instance (%get-file-type underlying-stat)
                              :stat underlying-stat
                              :port underlying-node)))
    (%fetch-nodes node)
    node))

(defun main ()
  (let ((translator (make-instance 'proxy-translator
                                   :name "proxy-translator")))
    (run-translator translator :flags '(:notrans :read))))

(main)

