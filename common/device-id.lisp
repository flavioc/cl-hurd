
(in-package :hurd-common)

(defclass device-id ()
  ((major :initform nil
          :initarg :major
          :accessor major
          :documentation "Major device number.")
   (minor :initform nil
          :initarg :minor
          :accessor minor
          :documentation "Minor device number."))
  (:documentation "Pair of major/minor device numbers representing some resources."))

(defmethod get-device-integer ((device device-id))
  (boole boole-ior
         (ash (major device) 8)
         (minor device)))

(defmethod print-object ((device device-id) stream)
  (format stream "#<device-id major=~s minor=~s>"
          (major device)
          (minor device)))

(defun get-major-dev (int)
  "Get the major device number from an integer, as major(dev) from sys/sysmacros.h"
  (boole boole-and
         (ash int -8)
         #xff))

(defun get-minor-dev (int)
  "Get the minor device number from an integer, as minor(dev) from sys/sysmacros.h"
  (boole boole-and int #xff))

