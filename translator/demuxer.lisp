
(in-package :hurd-translator)

(defun translator-demuxer (in out)
  "The translator demuxer."
  (stack-servers in out
                 io-server
                 notify-server
                 fs-server
                 fsys-server))

