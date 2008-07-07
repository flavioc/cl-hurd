
(in-package :hurd-translator)

(defconstant +chunk-size+ (* 8 1024))

(defun %get-dirent-type (stat)
  "Converts the stat type into a valid dirent type."
  (cond
    ((is-reg-p stat) :reg)
    ((is-dir-p stat) :dir)
    ((is-blk-p stat) :blk)
    ((is-lnk-p stat) :lnk)
    ((is-sock-p stat) :sock)
    ((is-fifo-p stat) :fifo)
    (t
      :unknown)))

(defcstruct dirent-struct
  (ino ino-t)
  (reclen :unsigned-short)
  (type :unsigned-char)
  (namlen :unsigned-char))

(defconstant +name-offset+ (foreign-type-size 'dirent-struct))

(defun %dir-readdir-count (dir-protid entry final-entry data initial-size limited-p)
  "Returns number of entries, pointer to them and size in bytes, otherwise nil."
  (let ((size initial-size)
        (ptr data)
        (total (- final-entry entry))
        (entries (get-entries *translator* (get-node dir-protid) (get-user dir-protid) entry final-entry)))
    (loop for dirent in entries
          for cnt = 0 then (1+ cnt)
          do (let* ((dirent-name (name dirent))
                    (namelen (1+ (length dirent-name)))
                    (this-size (+ +name-offset+ namelen))
                    (node (node dirent)))
               (when (> (+ this-size
                           (- (pointer-address ptr)
                              (pointer-address data)))
                        size)
                 (warn "exceeded")
                 (when limited-p
                   (warn "limited! returning..")
                   (return (values cnt ptr size)))
                 (with-foreign-pointer (extension (foreign-type-size 'vm-address))
                   (warn "extending..")
                   (let ((result (vm-allocate extension +chunk-size+ 0)))
                     (if (null result)
                       (return (values nil nil size))
                       (incf size +chunk-size+)))))
               (with-foreign-pointer (dirent-ptr this-size)
                 ; (warn "name-offset: ~s node is ~s size: ~s ; this-size ~s~%"
                 ;	   +name-offset+ dirent-name (+ +name-offset+ namelen) this-size)
                 (setf (foreign-slot-value dirent-ptr 'dirent-struct 'ino) (stat-get (stat node) 'ino))
                 (setf (foreign-slot-value dirent-ptr 'dirent-struct 'reclen) this-size)
                 (setf (foreign-slot-value dirent-ptr 'dirent-struct 'type)
                       (foreign-enum-value 'dirent-type (%get-dirent-type (stat node))))
                 (setf (foreign-slot-value dirent-ptr 'dirent-struct 'namlen) namelen)
                 (lisp-string-to-foreign dirent-name
                                         (inc-pointer dirent-ptr +name-offset+)
                                         namelen)
                 (memcpy ptr dirent-ptr this-size)
                 (incf-pointer ptr this-size)))
          when (= cnt total)
          return (values (1+ cnt) ptr size))))

(defun %dir-readdir (dir-protid bufsiz entry nentries existing-entries)
  "Returns pointer to entries, size and total of entries, otherwise nil."
  (let* ((size (if (or (zerop bufsiz) (> bufsiz +chunk-size+))
                 +chunk-size+
                 bufsiz))
         (data (mmap (make-pointer 0) size '(:prot-read :prot-write) '(:map-anon) 0 0)))
    (unless data
      (return-from %dir-readdir nil))
    (let* ((limited-p (> bufsiz 0))
           (final-entry (1- (if (= nentries -1)
                              existing-entries
                              (min (+ entry nentries) existing-entries)))))
      (multiple-value-bind (cnt ptr new-size)
        (%dir-readdir-count dir-protid entry final-entry data size limited-p)
        (cond
          ((null cnt)
           (unless (null new-size)
             (munmap data new-size))
           nil)
          (t
            ;(warn "start ptr ~s final ptr ~s total size ~s" data ptr new-size)
            (let ((alloc-end (pointer-address (inc-pointer data new-size)))
                  (real-end (round-page (pointer-address ptr))))
              (when (> alloc-end real-end)
                ;(warn "munmap unused from ~s to ~s" real-end alloc-end)
                (munmap (make-pointer real-end) (- alloc-end real-end)))
              (values data
                      (- (pointer-address ptr) (pointer-address data))
                      cnt))))))))

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
           (unless (flag-is-p (get-open-flags dir-protid) 'read)
             (return-from dir-readdir :bad-fd))
           (unless (is-dir-p (get-stat dir-protid))
             (return-from dir-readdir :not-directory))
           (let ((existing-entries (number-of-entries *translator*
                                                      (get-node dir-protid)
                                                      (get-user dir-protid))))
             (when (> (1+ entry) existing-entries)
               (setf (mem-ref amount :int) 0)
               (return-from dir-readdir t))
             (multiple-value-bind (data-ptr len cnt)
               (%dir-readdir dir-protid bufsiz entry nentries existing-entries)
               (when (null data-ptr)
                 (setf (mem-ref amount :int) 0)
                 (return-from dir-readdir t))
               (setf (mem-ref amount :int) cnt)
               (setf (mem-ref data :pointer) data-ptr)
               (setf (mem-ref data-count 'msg-type-number) len)
               (setf (mem-ref data-dealloc :int) 1)
               t)))))
