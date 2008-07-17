
(in-package :mach)

;; 'prot' protection flags for mmap.
(defbitfield mmap-prot-flags
  (:prot-none #x00)
  (:prot-read #x04)
  (:prot-write #x02)
  (:prot-exec #x01))

;; Specifies the type of the object in mmap 'flags'.
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
  "Map files or devices into memory."
  (let ((ptr (%mmap addr len prot flags filedes off)))
    ; Mmap returns -1 in case of error
    (if (= (pointer-address ptr) #xffffffff)
      nil
      ptr)))

(defcfun ("munmap" %munmap) :int
  (addr :pointer)
  (len :unsigned-int))

(defun munmap (addr len)
  "Remove a mapping."
  (cond
    ((zerop len)
     t)
    ((null addr)
     nil)
    (t
      (let ((result (%munmap addr len)))
        ; In case of success, munmap returns 0.
        (= result 0)))))
