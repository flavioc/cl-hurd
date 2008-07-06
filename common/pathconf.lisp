
(in-package :hurd-common)

;;
;; Values for the name argument to pathconf.
;; These can be found at bits/confname.h
;;
(defcenum pathconf-type
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
  :2-symlinks)

