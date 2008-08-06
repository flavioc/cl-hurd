
(in-package :mach)

;;
;; In this file we define the foreign type mach_port_type_t.
;;

(defmacro %create-port-type-type ()
  (flet ((%mach-port-type-get (code)
                              (ash 1 (+ (foreign-enum-value 'port-right code) 16))))
    (let* ((+type-none+ 0)
           (+type-send+ (%mach-port-type-get :right-send))
           (+type-receive+ (%mach-port-type-get :right-receive))
           (+type-send-once+ (%mach-port-type-get :right-send-once))
           (+type-port-set+ (%mach-port-type-get :right-port-set))
           (+type-dead-name+ (%mach-port-type-get :right-dead-name))
           (+type-send-receive+ (boole boole-ior
                                       +type-send+
                                       +type-receive+))
           (+type-send-rights+ (boole boole-ior
                                      +type-send+
                                      +type-send-once+))
           (+type-port-rights+ (boole boole-ior
                                      +type-send-rights+
                                      +type-receive+))
           (+type-port-or-dead+ (boole boole-ior
                                       +type-port-rights+
                                       +type-dead-name+))
           (+type-all-rights+ (boole boole-ior
                                     +type-port-or-dead+
                                     +type-port-set+))
           (+type-dnrequest+ #x80000000)
           (+type-marequest+ #x40000000)
           (+type-compat+ #x20000000))
      `(defbitfield port-type-t
        (:type-none ,+type-none+)
        (:type-send ,+type-send+)
        (:type-receive ,+type-receive+)
        (:type-send-once ,+type-send-once+)
        (:type-port-set ,+type-port-set+)
        (:type-dead-name ,+type-dead-name+)
        (:type-send-receive ,+type-send-receive+)
        (:type-send-rights ,+type-send-rights+)
        (:type-port-rights ,+type-port-rights+)
        (:type-port-or-dead ,+type-port-or-dead+)
        (:type-all-rights ,+type-all-rights+)
        (:type-dnrequest ,+type-dnrequest+)
        (:type-compat ,+type-compat+)))))

(%create-port-type-type)

