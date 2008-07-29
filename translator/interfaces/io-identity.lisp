
(in-package :hurd-translator)

(def-io-interface :io-identity ((file port)
                                (id port-pointer)
                                (id-type :pointer)
                                (fsys port-pointer)
                                (fsys-type :pointer)
                                (fileno :pointer))
  (with-lookup protid file
    (let* ((ino (stat-get (get-stat protid) 'st-ino))
           (io-identity (get-io-identity (port-bucket *translator*) ino)))
      (setf (mem-ref id 'port) io-identity
            (mem-ref id-type 'msg-type-name) :make-send
            (mem-ref fsys 'port) (identity-port *translator*)
            (mem-ref fsys-type 'msg-type-name) :make-send
            (mem-ref fileno 'ino-t) ino)
      t)))

