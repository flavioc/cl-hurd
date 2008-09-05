#!/usr/bin/run-lisp-trans

; settrans -ac foo ./run-zip.lisp zip-file.zip
;

(unless (= (length ext:*args*) 1)
  (error "You must pass a zip file as an argument."))

(asdf:operate 'asdf:load-op 'zip-translator)

