
(in-package :hurd-common)

;;
;; This file implements the dirent class.
;; It serves as a way to translators tell
;; what entries they have in a directory.
;;

;;
;; File types for the 'd_type' field
;; from struct dirent (bits/dirent.h).
;; This enum is found at dirent.h.
;;
(defcenum dirent-type
  (:unknown 0)
  (:fifo 1)
  (:chr 2)
  (:dir 4)
  (:blk 6)
  (:reg 8)
  (:lnk 10)
  (:sock 12)
  (:wht 14))

(defcstruct dirent-struct
  (ino ino-t)
  (reclen :unsigned-short)
  (type :unsigned-char)
  (namlen :unsigned-char))

(defconstant +dirent-size+ (foreign-type-size 'dirent-struct))

(defclass dirent ()
  ((name :initarg :name
         :accessor dirent-name
         :documentation "Entry name")
   (ino :initarg :ino
        :accessor dirent-ino
        :documentation "Ino value of this entry.")
   (filetype :initarg :file-type
             :accessor dirent-file-type
             :documentation "Mode type of this entry.")
   (size :initform nil
         :initarg :size
         :accessor dirent-size
         :documentation "Total foreign size.")
   (name-size :initform nil
              :initarg :name-size
              :accessor dirent-name-size
              :documentation "Name lenght."))
  (:documentation "Dirent class with name + node."))

(defun make-dirent (name ino file-type)
  "Creates a new dirent object."
  (make-instance 'dirent :name name :ino ino :file-type file-type))

(defmethod initialize-instance :after ((dirent dirent) &key)
  "Automagically fill size fields."
  (unless (dirent-name-size dirent)
    (setf (dirent-name-size dirent)
          (1+ (length (dirent-name dirent)))))
  (unless (dirent-size dirent)
    (setf (dirent-size dirent)
          (+ (dirent-name-size dirent) +dirent-size+)))
  dirent)

(defmethod write-dirent ((dirent dirent) ptr)
  "Writes dirent object to ptr."
  (setf (foreign-slot-value ptr 'dirent-struct 'ino) (dirent-ino dirent)
        (foreign-slot-value ptr 'dirent-struct 'reclen) (dirent-size dirent)
        (foreign-slot-value ptr 'dirent-struct 'type)
        (foreign-enum-value 'dirent-type (dirent-file-type dirent))
        (foreign-slot-value ptr 'dirent-struct 'namlen) (dirent-name-size dirent))
  (lisp-string-to-foreign (dirent-name dirent)
                          (inc-pointer ptr +dirent-size+)
                          (dirent-name-size dirent))
  (dirent-size dirent))

(defun read-dirent (ptr)
  "Read dirent object from foreign pointer 'ptr'."
  (let* ((ino (foreign-slot-value ptr 'dirent-struct 'ino))
         (size (foreign-slot-value ptr 'dirent-struct 'reclen))
         (type (foreign-enum-keyword
                 'dirent-type
                 (foreign-slot-value ptr 'dirent-struct 'type)))
         (namlen (foreign-slot-value ptr 'dirent-struct 'namlen))
         (name (foreign-string-to-lisp (inc-pointer ptr +dirent-size+) namlen)))
    (make-instance 'dirent
                   :name name
                   :ino ino
                   :file-type type
                   :size size
                   :name-size namlen)))

(defmethod print-object ((dirent dirent) stream)
  (format stream "#<dirent name=~s ino=~s filetype=~s>"
          (dirent-name dirent)
          (dirent-ino dirent)
          (dirent-file-type dirent)))

