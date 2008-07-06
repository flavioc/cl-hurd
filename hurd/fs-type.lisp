
(in-package :hurd)

;;
;; File system types for the stat's fsid field.
;; Can be found at hurd/hurd_types.h.
;;

(defcenum fs-type
  (:ufs #x00000000)
  (:nfs #x00000001)
  (:gfs #x00000002)
  (:lfs #x00000003)
  (:sysv #x00000004)
  (:ftp #x00000005)
  (:tar #x00000006)
  (:ar #x00000007)
  (:cpio #x00000008)
  (:msloss #x00000009)
  (:cpm #x0000000a)
  (:hfs #x0000000b)
  (:dtfs #x0000000c)
  (:grfs #x0000000d)
  (:term #x0000000e)
  (:dev #x0000000f)
  (:proc #x00000010)
  (:ifsock #x00000011)
  (:afs #x00000012)
  (:dfs #x00000013)
  (:proc9 #x00000014)
  (:socket #x00000015)
  (:misc #x00000016)
  (:ext2fs #x00000017)
  (:http #x00000018)
  (:memfs #x00000019)
  (:iso9660 #x0000001a))

