#!/usr/bin/run-lisp-trans

; settrans -ac foo ./run-mod.lisp spec-file.lisp
;
(asdf:operate 'asdf:load-op 'mod-translator)

