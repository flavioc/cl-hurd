
(in-package :mach)

;; Different kinds of message field types.
(defconstant +msg-type-unstructured+ 0)
(defconstant +msg-type-bit+ 0)
(defconstant +msg-type-boolean+ 0)
(defconstant +msg-type-integer-16+ 1)
(defconstant +msg-type-integer-32+ 2)
(defconstant +msg-type-char+ 8)
(defconstant +msg-type-byte+ 9)
(defconstant +msg-type-integer-8+ 9)
(defconstant +msg-type-real+ 10)
(defconstant +msg-type-integer-64+ 11)
(defconstant +msg-type-string+ 12)

(defconstant +msg-type-codes+
  `((,+msg-type-unstructured+ :type-unstructured)
    (,+msg-type-bit+ :type-bit)
    (,+msg-type-boolean+ :type-boolean)
    (,+msg-type-integer-16+ :type-integer-16)
    (,+msg-type-integer-32+ :type-integer-32)
    (,+msg-type-char+ :type-char)
    (,+msg-type-byte+ :type-byte)
    (,+msg-type-integer-8+ :type-integer-8)
    (,+msg-type-real+ :type-real)
    (,+msg-type-integer-64+ :type-integer-64)
    (,+msg-type-string+ :type-string)))

(defun translate-msg-type-bits (value)
  "Return the code from a type symbol."
  (translate-foreign-list value +msg-type-codes+ :to))

(defun translate-msg-type-symbol (value)
  "Return the symbol from a type code."
  (translate-foreign-list value +msg-type-codes+ :from))

(defconstant +msg-type-name-bits+ #xff "Type name mask.")
(defconstant +max-msg-type-name+ #xff "Max type name mask.")

(defun clear-type-name (val)
  "Clear the type name field."
  (boole boole-andc2 val +msg-type-name-bits+))

(defun set-type-name (val type)
  "Set the type name field."
  (boole boole-ior
         (clear-type-name val)
         (boole boole-and (translate-msg-type-bits type)
                +max-msg-type-name+)))

(defun get-type-name (val)
  "Return the type name field in a symbol."
  (translate-msg-type-symbol
    (boole boole-and val +msg-type-name-bits+)))

(defconstant +msg-type-size-bits+ #xff00 "The size field mask.")
(defconstant +max-msg-type-size+ #xff "Max size field.")
(defconstant +msg-type-size-shift+ 8 "Shift in the unsigned int.")

(defun clear-type-size (val)
  "Clear the type size field."
  (boole boole-andc2 val +msg-type-size-bits+))

(defun set-type-size (val size)
  "Set the type size field."
  (boole boole-ior
         (clear-type-size val)
         (ash (boole boole-and +max-msg-type-size+ size)
              +msg-type-size-shift+)))

(defun get-type-size (val)
  "Return the size field."
  (ash (boole boole-and +msg-type-size-bits+ val)
       (- +msg-type-size-shift+)))

(defconstant +msg-type-number-bits+ #xfff0000 "Type number mask.")
(defconstant +max-msg-type-number+ #xfff "Max type number.")
(defconstant +msg-type-number-shift+ 16 "Type number shift in the unsigned int.")

(defun clear-type-number (val)
  "Clear the type number field."
  (boole boole-andc2 val
         +msg-type-number-bits+))

(defun set-type-number (val number)
  "Set the type number field."
  (boole boole-ior
         (clear-type-number val)
         (ash (boole boole-and
                     number +max-msg-type-number+)
              +msg-type-number-shift+)))

(defun get-type-number (val)
  "Return the type number."
  (ash (boole boole-and val +msg-type-number-bits+)
       (- +msg-type-number-shift+)))

(defun set-type-boolean (val yes bit-pos)
  "Set a specific bit in the unsigned int."
  (let ((val2 (ash #x1 bit-pos)))
    (if yes
      (boole boole-ior val val2)
      (boole boole-andc2 val val2))))

(defun set-type-inline (val yes)
  "Set inline field."
  (set-type-boolean val yes 28))

(defun set-type-longform (val yes)
  "Set longform field."
  (set-type-boolean val yes 29))

(defun set-type-deallocate (val yes)
  "Set deallocate field."
  (set-type-boolean val yes 30))

(defun set-type-unused (val yes)
  "Set unused field."
  (set-type-boolean val yes 31))

(defctype msg-type :unsigned-int)

(defconstant +msg-type-size+ (foreign-type-size 'msg-type))

