#!/usr/bin/run-lisp-trans

; settrans -ac foo ./zip.lisp zip-file.zip
;

(asdf:operate 'asdf:load-op 'zip-translator)

