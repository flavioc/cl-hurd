
(in-package :hurd-common)

;;
;; This file implements the 'open-flags' CFFI type.
;; It is associated with the various flags used in file
;; opening operations, like dir_lookup (and, of course, file_name_lookup), etc.
;;

;; 
;; These are the possible flags and associated bits
;; that are declared at bits/fcntl.h.
;; See that file for more information about each one.
;;

(defbitfield open-flags
  (:norw #x0000)
  (:read #x0001)
  (:write #x0002)
  (:exec #x0004)
  (:largefile 0)
  (:creat #x0010)
  (:excl #x0020)
  (:nolink #x0040)
  (:notrans #x0080)
  (:nofollow #x00100000)
  (:directory #x00200000)
  (:append #x0100)
  (:async #x0200)
  (:fsync #x0400)
  (:sync #x0400)
  (:noatime #x0800)
  (:shlock #x00020000)
  (:exlock #x00040000)
  (:dsync #x0400)
  (:rsync #x0400)
  (:nonblock #x0008)
  (:trunc #x00010000)
  (:cloexec #x00400000))
