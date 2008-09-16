#!/usr/bin/run-lisp-trans

; settrans -ac foo ./unzip.lisp zip-file.zip
;

(asdf:operate 'asdf:load-op 'unzip-translator)

