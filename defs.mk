
CFLAGS = -Wall -Wextra -D_GNU_SOURCE=1
LDFLAGS = -lmachuser -lc -lhurduser

HURD_DIRECTORY = /usr/include/hurd
MACH_DIRECTORY = /usr/include/mach
MIG = mig -v -prefix lisp_ -user /dev/null -header /dev/null -server
