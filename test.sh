#!/bin/sh

cd examples
settrans --timeout=0 -a ../foo /usr/bin/run-lisp-trans -E utf-8 ../test.lisp $*
