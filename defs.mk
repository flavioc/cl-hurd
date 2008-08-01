
CFLAGS = -Wall -Wextra -D_GNU_SOURCE=1
LDFLAGS = -lmachuser -lc -lhurduser

HURD_DIRECTORY = /usr/include/hurd
MACH_DIRECTORY = /usr/include/mach
MIG_SERVER = mig -prefix lisp_ -user /dev/null -header /dev/null -server
MIG_USER = mig -server /dev/null -header /dev/null -user

