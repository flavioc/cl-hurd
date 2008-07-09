
(in-package :mach)

(defconstant +msgh-bits-zero+ #x00000000)
(defconstant +msgh-bits-remote-mask+ #x000000ff)
(defconstant +msgh-bits-local-mask+ #x0000ff00)
(defconstant +msgh-bits-complex+ #x80000000)
(defconstant +msgh-bits-unused+ #x07ff0000)
(defconstant +msgh-bits-ports-mask+
             (boole boole-ior
                    +msgh-bits-remote-mask+
                    +msgh-bits-local-mask+))

(defcstruct msg-header
  (bits msg-bits)
  (size msg-size)
  (remote-port port)
  (local-port port)
  (seqno port-seqno)
  (id msg-id))

(defconstant +msg-header-size+ (foreign-type-size 'msg-header))

(assert (= 24 +msg-header-size+))

(defun %set-header-value (header what val)
  (setf (foreign-slot-value header 'msg-header what) val))

(defun header-set-bits (header val)
  (%set-header-value header 'bits val))

(defun header-set-size (header val)
  (%set-header-value header 'size val))

(defun header-set-local-port (header val)
  (%set-header-value header 'local-port val))

(defun header-set-remote-port (header val)
  (%set-header-value header 'remote-port val))

(defun header-set-id (header val)
  (%set-header-value header 'id val))

(defun %header-get-value (header what)
  (foreign-slot-value header 'msg-header what))

(defun header-get-id (header)
  (%header-get-value header 'id))

(defun header-get-local-port (header)
  (%header-get-value header 'local-port))

(defun header-get-remote-port (header)
  (%header-get-value header 'remote-port))

(defun header-get-size (header)
  (%header-get-value header 'size))

(defun msgh-bits-remote (type-name)
  (boole boole-and
         +msgh-bits-remote-mask+
         (translate-msg-type-name-symbol type-name)))

(defun msgh-bits-local (type-name)
  (ash (boole boole-and +msgh-bits-local-mask+
              (translate-msg-type-name-symbol type-name))
       -8))

(defun msgh-bits (remote local)
  (boole boole-ior
         (msgh-bits-remote remote)
         (ash (msgh-bits-local local) 8)))

