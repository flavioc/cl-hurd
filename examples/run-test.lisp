#!/usr/bin/run-lisp-trans

; settrans -ac foo ./run-test.lisp data/test.lisp
;

(unless (= (length ext:*args*) 1)
  (error "You must provide one argument with a file."))

(asdf:operate 'asdf:load-op 'test-translator)

