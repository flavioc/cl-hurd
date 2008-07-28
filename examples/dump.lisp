
(asdf:operate 'asdf:load-op 'hurd)
(use-package 'hurd)

;; 
;; This file tries to mimick in Lisp the dump.c program
;; from The Hurd Hacking Guide (http://www.gnu.org/software/hurd/hacking-guide/hhg.html)
;;

(defun array->string (arr)
  (let ((ret (make-array 0
                         :element-type 'character :fill-pointer 0
                         :adjustable t)))
    (loop for item across arr
          unless (eq item 0)
          do (vector-push-extend (code-char item) ret))
    ret))


(defun dump-file (path)
  (with-port-deallocate (file (file-name-lookup path '(:read)))
    (let ((data (io-read file)))
      (when data 
        (array->string data)))))

;; to print results just like the C version, do:
;; (format t "~a" (dump-file "myfile"))

