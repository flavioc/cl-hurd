#!/usr/bin/run-lisp-trans

; settrans -a foo /usr/bin/run-lisp-trans ./run-zero.lisp
;
(asdf:operate 'asdf:load-op 'zero-translator)

