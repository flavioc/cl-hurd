#!/usr/bin/run-lisp-trans

; settrans -a foo ./run-test.lisp data/test.lisp
;
(asdf:operate 'asdf:load-op 'test-translator)

