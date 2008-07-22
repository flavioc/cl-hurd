
(in-package :hurd-example-translators)

;;
;; This is a simple zip translator.
;; Right now it supports file and directory listing.
;;

(defvar *zip* (open-zipfile "tmp.zip") "The zip handle.")

(defclass zip-translator (tree-translator)
  ((name :initform "zip-translator"
         :documentation "Translator name"))
  (:documentation "Zip translator."))

(defclass zip-entry (entry)
  ((data-sequence :initarg :data
                  :accessor data
                  :documentation "The zip data associated with this file."))
  (:documentation "Extends entry with a zip-entry."))

(define-callback allow-open-p zip-translator (node user flags is-new-p) t)

(define-callback file-read zip-translator
                 (node user start amount stream)
  (let* ((size (stat-get (stat node) 'size))
         (size-res (- size start)))
    (cond
      ((not (plusp size-res))
       nil)
      (t
        (let* ((total (if (null amount)
                        size-res
                        (min size-res amount)))
               (end (+ start total)))
        (write-sequence (subseq (data node) start end)
                        stream)
        ;(if (eq end size)
        ;  (write-byte #xA0 stream))
        t)))))

(define-callback report-access zip-translator
                 (node user)
  '(:read))

(define-callback refresh-statfs zip-translator
                 (user)
  (setf (statfs-get (get-statfs *translator*) 'bfree) 1)
  t)

(define-callback file-change-size zip-translator
                 (node user new-size)
  (warn "user ~s wants to change ~s to size ~s"
        user node new-size)
  nil)

(define-callback shutdown zip-translator
                 ()
  (warn "Going down...~%")
  t)

(define-callback file-rename zip-translator
                 (user old-dir old-name new-dir new-name)
  (rename-dir-entry old-dir old-name new-dir new-name)
  t)

(define-callback create-file zip-translator
                 (node user filename mode)
  (warn "Create file ~s in ~s" filename node)
  (let ((entry (make-instance 'zip-entry
                              :stat (make-stat (stat node)
                                               :mode mode)
                              :parent node
                              :data (make-array 0))))
    (setup-entry entry)
    (add-entry node entry filename)
    entry))

(define-callback create-anonymous-file zip-translator
                 (node user mode)
  (make-instance 'zip-entry
                 :stat (make-stat (stat node) :mode mode)
                 :parent node
                 :data (make-array 0)))

(define-callback create-hard-link zip-translator
                 (dir user file name)
  (add-entry dir file name)
  t)

(define-callback options-changed zip-translator
                 ()
  (warn "Options changed:")
  (if (has-translator-option-p (options *translator*) "readonly")
    (warn "READONLY activated"))
  (if (has-translator-option-p (options *translator*) "coolness-level")
    (warn "COOLNESS LEVEL ~s"
          (get-translator-option (options *translator*)
                                 "coolness-level")))
  t)

(define-callback create-symlink zip-translator
                 (node user target)
  (set-type (stat node) :lnk)
  (setf (link node) target)
  t)

(define-callback create-block zip-translator
                 (node user device)
  (warn "create-block")
  (set-type (stat node) :blk)
  (setf (stat-get (stat node) 'rdev) device)
  t)

(define-callback create-character zip-translator
                 (node user device)
  (warn "create character.")
  (set-type (stat node) :chr)
  (setf (stat-get (stat node) 'rdev) device)
  t)

(define-callback create-fifo zip-translator
                 (node user)
  (warn "create fifo.")
  (set-type (stat node) :fifo)
  t)

(define-callback create-socket zip-translator
                 (node user)
  (warn "create socket.")
  (set-type (stat node) :sock)
  t)

(define-callback set-translator zip-translator
                 (node user arg-list)
  (warn "setting passive translator ~s" arg-list)
  (setf (translator node) arg-list)
  t)

;; XXX
(define-callback file-write zip-translator
				 (node user offset stream)
  t)

(defun %create-zip-file (parent entry)
  "Create a new zip entry."
  (let ((data-stream
          (make-in-memory-output-stream
            :element-type '(unsigned-byte 8)))
        seq
        stat)
    (zipfile-entry-contents entry data-stream)
    (setf seq (get-output-stream-sequence data-stream)
          stat (make-stat (stat parent)
                          :size (length seq)
                          :type :reg))
    (clear-perms stat 'exec) ; Clear exec permissions.
    (let ((obj (make-instance 'zip-entry
                              :stat stat
                              :parent parent
                              :data seq)))
      (setup-entry obj)
      obj)))

(defun %create-zip-dir (parent)
  "Create a new zip directory."
  (make-instance 'dir-entry
                 :stat (make-stat (stat parent))
                 :parent parent))

(defun add-zip-file (node name zip-entry)
  "Recursively using name as a path list add into 'node' a new 'zip-entry'."
  (let* ((name-rest (rest name))
         (this-name (first name))
         (final-p (null name-rest)))
    (if (string= this-name "")
      ;; Last node was a directory and it is already created.
      (return-from add-zip-file nil))
    (multiple-value-bind (entry found-p)
      (get-entry node this-name)
      (cond
        (found-p
          (unless final-p
            (add-zip-file entry name-rest zip-entry)))
        (t
          (let ((new-dir (add-entry node
                                    (if final-p
                                      (%create-zip-file node zip-entry)
                                      (%create-zip-dir node))
                                    this-name)))
            ;(warn "new-dir ~s" new-dir)
            (unless final-p
              (add-zip-file new-dir name-rest zip-entry))))))))

(defmethod fill-root-node ((translator zip-translator) (node dir-entry))
  "Add all entries found on the zip file."
  (do-zipfile-entries (name entry *zip*)
                      (add-zip-file node (split-path name) entry)))


;; to be removed
;;
(define-callback file-utimes zip-translator
				 (node user atime mtime)
  (when atime
    (setf (stat-get (stat node) 'atime) atime))
  (when mtime
    (setf (stat-get (stat node) 'mtime) mtime))
  t)

(define-callback file-chmod zip-translator
				 (node user mode)
  (set-perms-if (stat node)
                (has-perms-p mode 'exec 'owner)
                'exec 'owner)
  (set-perms-if (stat node)
                (has-perms-p mode 'read 'others)
                'read 'others))

(define-callback file-chown zip-translator
				 (node user uid gid)
  (when (valid-id-p uid)
    (setf (stat-get (stat node) 'uid) uid))
  (when (valid-id-p gid)
    (setf (stat-get (stat node) 'gid) gid))
  t)

(define-callback allow-author-change zip-translator
				 (node user author)
  t)

(defun main ()
  (let ((trans (make-instance 'zip-translator
                              :options (make-translator-options
                                         '(("coolness-level" 20)
                                           "fast")))))
    (run-translator trans)))

(main)
