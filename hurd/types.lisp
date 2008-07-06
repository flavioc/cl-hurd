
(in-package :hurd)

;;
;; Hurd types that don't deserve a file.
;;

(defctype auth-t port "auth_t type")
(defctype fsys-t port "fsys_t type")
(defctype io-t port "io_t type")
(defctype process-t port "process_t type")
(defctype socket-t port "socket_t type")
(defctype pf-t port "pf_t type")
(defctype addr-port-t port "addr_port_t type")
(defctype startup-t port "startup_t type")
(defctype fs-notify-t port "fs_notify_t type")
(defctype proccoll-t port "proccoll_t type")

