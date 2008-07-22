
(in-package :hurd-translator)

(defun %readdir-count-size (entries limit)
  "Counts number of size in bytes and items needed to write 'entries'."
  (let ((current 0)
        (nitens 0))
    (loop for dirent in entries
          do (let* ((this-size (size dirent))
                    (newval (+ this-size current)))
               (cond
                 ((and limit (> newval limit)) (return))
                 (t
                   (incf nitens)
                   (setf current newval)))))
    (list current nitens)))

(defun %write-dir-data (ptr items)
  "Writes to initial-ptr all dirent items."
  (loop for item in items
        do (incf-pointer ptr (write-dirent item ptr))))

(defun %calculate-final-entry (entry nentries existing-entries)
  (let ((unlimited-p (= nentries -1)))
    (1- (if unlimited-p
          existing-entries
          (min (+ entry nentries) existing-entries)))))

(defun %dir-readdir (protid limit entry nentries existing-entries
                            old-dataptr current-size)
  "Returns pointer to entries, size and total of entries, otherwise nil."
  (let* ((node (get-node protid))
         (user (get-user protid))
         (final-entry (%calculate-final-entry entry nentries existing-entries))
         (entries (get-entries *translator* node user entry final-entry)))
    (unless entries
      (return-from %dir-readdir nil))
    (let* ((sizes (%readdir-count-size entries limit))
           (size-bytes (first sizes))
           (size-items (second sizes))
           (real-entries (subseq entries 0 size-items)))
      (unless real-entries
        (return-from %dir-readdir nil))
      (let* ((needs-more-p (< current-size size-bytes))
             (dataptr (if needs-more-p
                        (mmap (null-pointer)
                              size-bytes
                              '(:prot-read :prot-write)
                              '(:map-anon)
                              0
                              0)
                        old-dataptr))) ; Old value is large enough.
        (%write-dir-data dataptr real-entries)
        (values dataptr 
                size-bytes
                size-items)))))

(def-fs-interface :dir-readdir ((port port)
                                (data :pointer)
                                (data-count :pointer)
                                (data-dealloc :pointer)
                                (entry :int)
                                (nentries :int)
                                (bufsiz :unsigned-int)
                                (amount :pointer))
  (with-lookup dir-protid port
    (block dir-readdir
           (unless (flag-is-p (get-open-flags dir-protid) :read)
             (return-from dir-readdir :bad-fd))
           (unless (is-dir-p (get-stat dir-protid))
             (return-from dir-readdir :not-directory))
           (let ((existing-entries (number-of-entries *translator*
                                                      (get-node dir-protid)
                                                      (get-user dir-protid))))
             (when (> (1+ entry) existing-entries)
               (setf (mem-ref amount :int) 0)
               (return-from dir-readdir t))
             (let ((old-ptr (mem-ref data :pointer)))
               (multiple-value-bind (data-ptr len cnt)
                 (%dir-readdir dir-protid (if (zerop bufsiz) nil bufsiz)
                               entry
                               nentries
                               existing-entries
                               old-ptr
                               (mem-ref data-count 'msg-type-number))
                 (when (null data-ptr)
                   (setf (mem-ref amount :int) 0)
                   (return-from dir-readdir t))
                 (setf (mem-ref amount :int) cnt
                       (mem-ref data :pointer) data-ptr
                       (mem-ref data-count 'msg-type-number) len
                       (mem-ref data-dealloc :boolean)
                       (not (pointer-eq old-ptr data-ptr)))
                 t))))))

