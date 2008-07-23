
(in-package :hurd-translator)

(def-fs-interface :file-get-storage-info ((file port)
                                          (ports :pointer)
                                          (ports-type :pointer)
                                          (num-ports :pointer)
                                          (ints :pointer)
                                          (num-ints :pointer)
                                          (offsets :pointer)
                                          (num-offsets :pointer)
                                          (data :pointer)
                                          (data-len :pointer))
  (declare (ignore ports ports-type offsets data))
  (when (port-exists-p file)
    (setf (mem-ref data-len 'msg-type-number) 0
          (mem-ref num-offsets 'msg-type-number) 0
          (mem-ref num-ports 'msg-type-number) 0)
    (unless (plusp (mem-ref num-ints 'msg-type-number))
      (setf (mem-ref ints :pointer)
            (mmap (null-pointer)
                  (foreign-type-size 'msg-type-number)
                  '(:prot-read :prot-write)
                  '(:map-anon)
                  0
                  0)))
    (setf (mem-ref num-ints 'msg-type-number) 1
          (mem-ref (mem-ref ints :pointer) 'file-storage-class)
          (storage *translator*))
    t))
