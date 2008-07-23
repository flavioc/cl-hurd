
(in-package :hurd)

(defconstant +honored-open-modes+ '(:append :async :fsync :nonblock))

(defconstant +honored-get-modes+ (append +honored-open-modes+
                                         '(:write :exec :read)))

(defcfun ("io_set_all_openmodes" %io-set-all-openmodes)
  err
  (file port)
  (newbits open-flags))

(defun io-set-all-openmodes (file modes)
  (declare (type fixnum file)
           (type list modes))
  (select-error
    (%io-set-all-openmodes file
                           (only-flags modes
                                       +honored-open-modes+))))

(defcfun ("io_get_openmodes" %io-get-openmodes)
  err
  (file port)
  (bits :pointer))

(defun io-get-openmodes (file)
  (declare (type fixnum file))
  (with-foreign-pointer (bits (foreign-type-size 'open-flags))
    (select-error (%io-get-openmodes file bits)
                  (mem-ref bits 'open-flags))))

(defcfun ("io_set_some_openmodes" %io-set-some-openmodes)
  err
  (file port)
  (bits open-flags))

(defun io-set-some-openmodes (file bits)
  (declare (type fixnum file)
           (type list bits))
  (select-error (%io-set-some-openmodes file bits)))

(defcfun ("io_clear_some_openmodes" %io-clear-some-openmodes)
  err
  (file port)
  (bits open-flags))

(defun io-clear-some-openmodes (file bits)
  (declare (type fixnum file)
           (type list bits))
  (select-error
    (%io-clear-some-openmodes file bits)))
