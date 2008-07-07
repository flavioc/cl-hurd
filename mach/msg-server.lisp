
(in-package :mach)

(defcfun ("mach_msg_server_timeout" %mach-msg-server-timeout)
  err
  (demuxer :pointer)
  (max-size msg-size)
  (port-set port)
  (options msg-option)
  (timeout msg-timeout))

(defmacro msg-server-timeout (demuxer port-set &optional (timeout 0) (max-size 0))
  "Receive RPC request messages on port-set and pass them to function demuxer with a timeout."
  (with-gensyms (callback-name)
    `(progn
       (defcallback ,callback-name :boolean ((in :pointer) (out :pointer))
         (funcall ,demuxer in out))
       (%mach-msg-server-timeout (callback ,callback-name)
                                 ,max-size
                                 ,port-set
                                 ,(if (> timeout 0)
                                    '(:rcv-timeout)
                                    '())
                                 ,timeout))))

(defcfun ("mach_msg_server" %mach-msg-server)
  err
  (demuxer :pointer)
  (max-size msg-size)
  (rcv-name port))

(defmacro msg-server (demuxer port-set &optional (max-size 0))
  "Receive RPC request messages on port-set and pass them to function demuxer."
  (with-gensyms (callback-name)
    `(progn
       (defcallback ,callback-name :boolean ((in :pointer) (out :pointer))
         (funcall ,demuxer in out))
       (%mach-msg-server (callback ,callback-name) ,max-size ,port-set))))

