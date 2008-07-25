
(in-package :hurd)

(defcfun ("get_privileged_ports" %get-privileged-ports)
  err
  (host-priv :pointer)
  (device-master :pointer))

(defun get-privileged-ports ()
  (with-foreign-pointer (host-priv-ptr (foreign-type-size 'host-priv-t))
    (with-foreign-pointer (device-master-ptr (foreign-type-size 'device-t))
      (select-error (%get-privileged-ports host-priv-ptr device-master-ptr)
                    (values
                      (mem-ref host-priv-ptr 'host-priv-t)
                      (mem-ref device-master-ptr 'device-t))))))
