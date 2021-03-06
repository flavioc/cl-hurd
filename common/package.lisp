
(in-package #:cl-user)

(defpackage :cl-hurd.common
  (:nicknames :hurd-common)
  (:use :cl :cffi :tg)
  (:export :+minus-one-ll+
           :+minus-one+
           :largest-representable-number
           :num-bits
           :error->string
           :err
           :unrecognized-error-code
           :fs-type
           :translate-foreign-list
           :with-gensyms
           :select-error
           :flag-is-p
           :enable-flags
           :disable-flags
           :only-flags
           :free-memory-list
           :mode-t
           :mode
           :stat
           :make-stat
           :stat-t
           :st-type
           :st-fstype
           :st-fsid
           :st-ino
           :st-gen
           :st-rdev
           :st-mode
           :st-nlink
           :st-uid
           :st-gid
           :st-size
           :st-atime
           :st-mtime
           :st-ctime
           :st-blksize
           :st-blocks
           :st-author
           :st-flags
           :set-trans
           :set-type
           :set-vtx
           :stat-get
           :open-flags
           :with-cleanup
           :getuid
           :getgid
           :geteuid
           :getegid
           :has-perms-p
           :set-perms
           :set-perms-if
           :clear-perms
           :copy-perms
           :is-dir-p
           :is-reg-p
           :is-lnk-p
           :is-chr-p
           :is-blk-p
           :is-sock-p
           :is-fifo-p
           :is-useunk-p
           :is-vtx-p
           :has-passive-trans-p
           :has-active-trans-p
           :is-fs-root-p
           :is-mmap-p
           :is-nocache-p
           :is-useunk-p
           :set-uid
           :set-gid
           :set-mmap
           :set-nocache
           :set-useunk
           :set-active-trans
           :set-passive-trans
           :set-root
           :set-types
           :memcpy
           :stat-copy
           :open-modes
           :set-spare
           :set-owner
           :set-group
           :set-others
           :set-unknown
           :gid-t
           :+gid-t-size+
           :uid-t
           :+uid-t-size+
           :valid-id-p
           :split-path
           :join-path
           :pathconf-type
           :dirent-type
           :dirent-name
           :dirent-size
           :off-t
           :loff-t
           :ino-t
           :pid-t
           :get-type
           :statfs-t
           :make-statfs
           :statfs-get
           :statfs-copy
           :statfs-struct
           :statfs
           :f-type
           :f-bsize
           :f-bfree
           :f-bavail
           :f-files
           :f-ffree
           :f-namelen
           :f-favail
           :f-frsize
           :f-flags
           :seek-type
           :select-type
           :foreign-string-zero-separated-to-list
           :is-uid-p
           :is-gid-p
           :device
           :device-major
           :device-minor
           :device-id
           :with-stream
           :concatenate-string
           :exit
           :list-to-foreign-string-zero-separated
           :string-list-len
           :sum-list
           :lock-flags
           :make-mode
           :make-mode-clone
           :time-value-t
           :time-value
           :make-time-value
           :+now-time-value+
           :make-dirent
           :write-dirent
           :read-dirent
           :time-value-eq
           :time-value-newer-p
           :time-value-seconds
           :time-value-microseconds
           :remove-declare
           :getpid
           :getppid
           :stat-eq
           :load-hurd-common-libraries))

