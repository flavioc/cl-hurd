
(defpackage :zip-translator
  (:use :cl :hurd-common :mach
        :hurd :hurd-translator
        :zip))

(in-package :zip-translator)

(defclass zip-translator (translator)
  ()
  (:documentation "Zip translators zips the target directory and exposes the translated node as a zip file."))

(defun temporary-file-name ()
  (format nil "zip-translator-~A.zip" (random 50000)))

(defconstant +target-dir+ (first ext:*args*))
(defconstant +zip-file+ (temporary-file-name))
(defconstant +zip-file-path+ (concatenate-string "/tmp/" +zip-file+))

;; Zip target target directory.
(warn "Zipping directory ~A to ~A" +target-dir+ +zip-file+)
(zip +zip-file-path+ +target-dir+)
(warn "Zip of directory ~A done." +target-dir+)

(defvar *zip-port* (file-name-lookup +zip-file-path+ :flags '(:read)))

;; Remove file when clisp exits.
(push (lambda ()
        (port-deallocate *zip-port*)
        (with-port-deallocate (port (file-name-lookup "/tmp" :flags '(:read)))
          (dir-unlink port +zip-file+)))
      custom:*fini-hooks*)

(define-callback allow-open-p zip-translator
                 (node user flags is-new-p)
  (declare (ignore is-new-p))
  (when (flag-is-p flags :write)
    (return-from allow-open-p nil))
  (when (flag-is-p flags :read)
    (unless (has-access-p node user :read)
      (return-from allow-open-p nil)))
  t)

(define-callback report-access zip-translator
                 (node user)
  (let ((ret))
    (when (has-access-p node user :read)
      (push :read ret))
    ret))

(define-callback read-file zip-translator
                 (node user start amount stream)
  (when (has-access-p node user :read)
    (let ((data (io-read *zip-port*
                         :amount amount
                         :offset start)))
      (when data
        (write-sequence data stream)
        t))))

(define-callback make-root-node zip-translator
                 (underlying-node underlying-stat)
  (declare (ignore underlying-node))
  (let ((mode (make-mode :perms '((:owner :read)
                                  (:group :read)
                                  (:others :read))
                         :type :reg))
        (stat (io-stat *zip-port*)))
    (make-instance 'node
                   :stat (make-stat underlying-stat
                                    :mode mode
                                    :size (stat-get stat 'st-size)))))

(defun main ()
  (run-translator (make-instance 'zip-translator
                                 :name "zip-translator"
                                 :version (list 0 0 1))))

(main)

