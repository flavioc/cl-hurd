#!/usr/bin/run-lisp-trans

; settrans -ac foo ./tmp.lisp
;
(asdf:operate 'asdf:load-op 'tmp-translator)

