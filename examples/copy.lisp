
(asdf:operate 'asdf:load-op 'hurd)
(use-package 'hurd)

;; 
;; This file tries to mimick in Lisp the copy.c program
;; from The Hurd Hacking Guide (http://www.gnu.org/software/hurd/hacking-guide/hhg.html)
;;

(defun write-block (dst data)
  (let ((total (length data))
        (total-written 0))
    (loop until (= total total-written)
          do (let ((err (io-write dst
                                  (subseq data total-written))))
               (if (or (null err)
                       (= err 0))
                 (return))
               (incf total-written err)))))

(defun copy-file (src dst)
  (declare (type string src dst))
  "Returns total of bytes copied."
  (with-port-deallocate (src-port
                          (file-name-lookup src '(:read)))
    (with-port-deallocate (dst-port
                            (file-name-lookup dst '(:write :creat :trunc)))
      (loop for data = (io-read src-port)
            while data
            sum (length data)
            while (write-block dst-port data)))))

;; To run:
;; (copy-file "myfile-a" "myfile-b")
