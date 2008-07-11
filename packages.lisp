
(in-package #:cl-user)

(defpackage :cl-hurd.common
  (:nicknames :hurd-common)
  (:use :cl :cffi :tg)
  (:export :largest-representable-number
           :num-bits
           :define-helper-library
           :define-stub-library
           :error->string
           :err
           :unless-return
           :translate-foreign-list
           :with-gensyms
           :select-error
           :open-flags-t
           :disable
           :disabled
           :enable
           :disabled
           :flag-is-p
           :read
           :write
           :exec
           :norw
           :largefile
           :excl
           :creat
           :nolink
           :notrans
           :nofollow
           :directory
           :append
           :async
           :fsync
           :sync
           :noatime
           :shlock
           :exlock
           :dsync
           :rsync
           :nonblock
           :hurd
           :trunc
           :cloexec
           :mode-t
           :mode-bits
           :stat
           :stat-struct
           :make-stat
           :stat-t
           :stat-clean
           :ptr
           :fstype
           :fsid
           :ino
           :gen
           :rdev
           :mode
           :nlink
           :uid
           :gid
           :size
           :atime
           :mtime
           :ctime
           :blksize
           :blocks
           :author
           :flags
           :set-trans
           :set-type
           :set-vtx
           :stat-get
           :stat-set
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
           :read
           :write
           :exec
           :owner
           :group
           :others
           :unknown
           :reg
           :lnk
           :dir
           :chr
           :blk
           :sock
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
           :memcpy
           :set-active-trans
           :set-passive-trans
           :set-root
           :stat-copy
           :only
           :open-modes
           :set-types
           :set-spare
           :gid-t
           :uid-t
           :valid-id-p
           :time-value-t
           :time-value
           :split-path
           :join-path
           :link-max
           :max-canon
           :max-input
           :name-max
           :path-max
           :pipe-buf
           :chown-restricted
           :no-trunc
           :vdisable
           :sync-io
           :prio-io
           :sock-maxbuf
           :filesizebits
           :rec-incr-xfer-size
           :rec-max-xfer-size
           :rec-min-xfer-size
           :rec-xfer-align
           :alloc-size-min
           :symlink-max
           :2-symlinks
           :pathconf-type
           :dirent-type
           :unknown
           :fifo
           :chr
           :dir
           :blk
           :reg
           :lnk
           :sock
           :wht
           :off-t
           :loff-t
           :ino-t
           :pid-t
           :get-type
           :type
           :from
           :to
           :statfs-t
           :make-statfs
           :statfs-get
           :statfs-set
           :statfs-clean
           :statfs-copy
           :bsize
           :bfree
           :bavail
           :files
           :ffree
           :namelen
           :favail
           :frsize
           :seek-type))

(defpackage :cl-mach
  (:nicknames :mach)
  (:use :cl :cffi :hurd-common)
  (:export :task-self
           :with-port-deallocate
           :with-port-destroy
           :with-port
           :with-send-right
           :with-receive-right
           :port
           :port-type-is-p
           :port-pointer
           :port-valid
           :port-allocate
           :port-deallocate
           :port-destroy
           :port-mod-refs
           :port-move-member
           :port-insert-right
           :port-type
           :port-request-notification
           :port-mscount
           :msg-seqno
           :task-get-special-port
           :task-get-bootstrap-port
           :msg-type-name
           :msg-type-number
           :msg-server-timeout
           :msg-server
           :maptime-map
           :maptime-seconds
           :maptime-microseconds
           :maptime-check-seconds
           :vm-size
           :mmap-prot-flags
           :mmap-map-flags
           :prot-none
           :prot-read
           :prot-write
           :prot-exec
           :map-shared
           :map-private
           :map-anon
           :mmap
           :munmap
           :round-page
           :vm-allocate
           :vm-deallocate
           :vm-task
           :vm-address
           :right-send
           :right-receive
           :right-send-once
           :right-port-set
           :right-dead-name
           :right-number
           :move-receive
           :move-send
           :move-send-once
           :copy-send
           :make-send
           :make-send-once
           :type-send
           :type-receive
           :type-send-once
           :type-port-set
           :type-send-receive
           :type-dead-name
           :type-send-rights
           :type-port-rights
           :type-port-or-dead
           :type-all-rights
           :type-dnrequest
           :type-compat
           :task-kernel-port
           :task-bootstrap-port
           :task-exception-port
           :msg-option-none
           :send-msg
           :rcv-msg
           :send-timeout
           :send-notify
           :send-interrupt
           :send-cancel
           :rcv-timeout
           :rcv-notify
           :rcv-interrupt
           :rcv-large
           :msg-option
           :reply-port
           :port-allocate-name
           :port-names
           :port-rename
           :port-get-refs
           :port-extract-right
           :port-status-get
           :port-status-has-send-rights-p
           :port-status-has-port-deleted-notification-p
           :port-status-has-no-senders-notification-p
           :port-set-mscount
           :port-set-qlimit
           :+qlimit-default+
           :+qlimit-min+
           :+qlimit-max+
           :port-set-seqno
           :port-get-set-status
           ))

(defpackage :cl-hurd
  (:nicknames :hurd)
  (:use :cl :cffi :mach :hurd-common :tg)
  (:export :getauth
           :get-send-right
           :get-right
           :port-right
           :fsys-startup
           :fsys-getroot
           :make-bucket
           :run-server
           :add-port
           :add-control-port
           :has-port
           :lookup-port
           :+servers+
           :+servers-exec+
           :+servers-crash+
           :+servers-proc+
           :+servers-password+
           :+servers-socket+
           :+hurd+
           :+hurd-init+
           :+hurd-proc+
           :+hurd-auth+
           :+hurd-symlink+
           :+hurd-chrdev+
           :+hurd-blkdev+
           :+hurd-fifo+
           :+hurd-ifsock+
           :file-name-lookup
           :touch
           :io-stat
           :io-server-version
           :make-iouser
           :make-iouser-mem
           :make-iouser-root
           :contains-uid
           :contains-gid
           :empty-uids-p
           :empty-gids-p
           :restrict-iouser
           :iouser
           :retry-type
           :box-translated-p
           :make-transbox
           :port-info
           :define-hurd-interface
           :notify-server
           :fetch-root
           :auth-t
           :fsys-t
           :io-t
           :process-t
           :socket-t
           :pf-t
           :addr-port-t
           :startup-t
           :fs-notify-t
           :proccoll-t
           :is-controller-p
           :has-access-p
           :can-modify-dir-p
           :can-modify-file-in-dir-p
           :is-owner-p
           :transbox-drop
           :nowait
           :nosync
           :force
           :unlink
           :recurse
           :fsys-goaway-flags
           :fsys-goaway-flag-is-p
           :fsys-goaway-reply
           :bucket-statistics
           :port-is-control-p
           :port-is-user-p
           :bucket-total-users
           :bucket-total-control
           ))

(defpackage :cl-hurd.translator
  (:nicknames :hurd-translator)
  (:use :cl :cffi :mach :hurd-common :hurd :tg :flexi-streams)
  (:export :translator
           :make-root-node
           :pathconf
           :allow-open
           :node
           :get-translator
           :file-chmod
           :file-chown
           :file-utimes
           :dir-lookup
           :create-file
           :number-of-entries
           :number-of-entries
           :get-entries
           :allow-author-change
           :create-directory
           :remove-entry
           :file-read
           :file-write
           :run-translator
           :define-callback
           :make-translator
           :make-dirent
           :propagate-read-to-execute
           :file-sync
           :file-syncfs
           :name
           :inc-refs
           :dec-refs
           :drop-node
           :make-node-dirent
           :report-access
           :refresh-statfs
           :*translator*
           :get-statfs))

(defpackage :cl-hurd.translator.tree
  (:nicknames :hurd-tree-translator)
  (:use :cl :hurd-common :mach :hurd :hurd-translator) ;:cl-containers)
  (:export :fill-root-node
           :tree-translator
           :dir-entry
           :entry
           :add-entry
           :make-dir
           :make-entry
           :get-entry
           :setup-entry))

(defpackage :cl-hurd.translator.examples
  (:nicknames :hurd-example-translators)
  (:use :cl :hurd-common :mach
        :hurd :hurd-translator
        :hurd-tree-translator
        :zip
        :flexi-streams))

