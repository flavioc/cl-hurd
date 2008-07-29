
(in-package :hurd)

(defclass identity-spec (port-info)
  ((ino :initform nil
        :initarg :ino
        :accessor ino
        :documentation "Ino stat's field."))
  (:documentation "IO identification port."))

(defmethod get-io-identity ((bucket port-bucket) ino)
  "Return an identity port  for the node numbered 'ino'."
  (let ((found (bucket-find bucket
                            (lambda (port)
                              (and (typep port 'identity-spec)
                                   (eq (ino port) ino))))))
    (unless found
      (let ((new-port (make-instance 'identity-spec :ino ino)))
        (bucket-add-port bucket new-port)
        (setf found new-port)))
    (get-right found)))

