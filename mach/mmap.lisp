
(defbitfield mmap-prot-flags
			 (:prot-none #x00)
			 (:prot-read #x04)
			 (:prot-write #x02)
			 (:prot-exec #x01))

(defbitfield mmap-map-flags
			 (:map-shared #x0010)
			 (:map-private #x0000)
			 (:map-anon #x0002))

(defcfun ("mmap" %mmap)
		 :pointer
		 (addr :pointer)
		 (len :unsigned-int)
		 (prot mmap-prot-flags)
		 (flags mmap-map-flags)
		 (filedes :int)
		 (off off-t))

(defun mmap (addr len prot flags filedes off)
  (let ((ptr (%mmap addr len prot flags filedes off)))
	(if (= (mem-ref ptr :int) -1)
	  nil
	  ptr)))

(defcfun ("munmap" %munmap)
		 :int
		 (addr :pointer)
		 (len :unsigned-int))

(defun munmap (addr len)
  (let ((result (%munmap addr len)))
	(= result 0)))
