
(in-package :hurd-common)

(defconstant +minus-one-ll+ (largest-representable-number
                              (num-bits (foreign-type-size :unsigned-long-long))))

(defconstant +minus-one+ (largest-representable-number
                           (num-bits (foreign-type-size :unsigned-int))))
