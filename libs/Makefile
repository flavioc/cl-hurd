
include ../defs.mk

# fs stubs
FS_DEFS = $(HURD_DIRECTORY)/fs.defs
GENERATED_FS_SERVER = fs-server
FS_OBJECTS = $(GENERATED_FS_SERVER).o fs-wrapper.o

# io stubs
IO_DEFS = $(HURD_DIRECTORY)/io.defs
GENERATED_IO_SERVER = io-server
IO_OBJECTS = $(GENERATED_IO_SERVER).o io-wrapper.o

# fsys stubs
FSYS_DEFS = $(HURD_DIRECTORY)/fsys.defs
GENERATED_FSYS_SERVER = fsys-server
FSYS_OBJECTS = $(GENERATED_FSYS_SERVER).o fsys-wrapper.o

# notify stubs
NOTIFY_DEFS = $(MACH_DIRECTORY)/notify.defs
GENERATED_NOTIFY_SERVER = notify-server
NOTIFY_OBJECTS = $(GENERATED_NOTIFY_SERVER).o notify-wrapper.o

# fsys reply stubs

FSYS_REPLY_DEFS = $(HURD_DIRECTORY)/fsys_reply.defs
GENERATED_FSYS_REPLY_USER = fsys-reply-user
FSYS_REPLY_OBJECTS = $(GENERATED_FSYS_REPLY_USER).o

TARGETS = liblisptrans.so
OBJS = fetch-root.o file-exec.o \
			 file-utimes.o portset-demuxer.o \
			 $(IO_OBJECTS) $(FS_OBJECTS) \
			 $(FSYS_OBJECTS) $(NOTIFY_OBJECTS) \
			 $(FSYS_REPLY_OBJECTS)

LDFLAGS = -lfshelp

all: $(TARGETS)

liblisptrans.so: $(OBJS)
	$(CC) -shared -Wl \
		-o liblisptrans.so $(OBJS) $(LDFLAGS) -lfshelp

$(GENERATED_IO_SERVER).c: $(IO_DEFS)
	$(MIG_SERVER) $(GENERATED_IO_SERVER).c $(IO_DEFS)
	sed -e 's:lisp_S_io_server:lisp_io_server:g' \
		-i $(GENERATED_IO_SERVER).c

io_wrapper.o: io_wrapper.c io_wrapper.h common.c

$(GENERATED_FS_SERVER).c: $(FS_DEFS)
	$(MIG_SERVER) $(GENERATED_FS_SERVER).c $(FS_DEFS)
	sed -e 's:lisp_S_fs_server:lisp_fs_server:g' -i $(GENERATED_FS_SERVER).c

fs_wrapper.o: fs_wrapper.c fs_wrapper.h common.c

$(GENERATED_FSYS_SERVER).c: $(FSYS_DEFS)
	cpp $(FSYS_DEFS) -D REPLY_PORTS > tmp.defs
	$(MIG_SERVER) $(GENERATED_FSYS_SERVER).c tmp.defs
	sed -e 's:lisp_S_fsys_server:lisp_fsys_server:g' \
		-i $(GENERATED_FSYS_SERVER).c
	rm -f tmp.defs

fsys_wrapper.o: fsys_wrapper.c fsys_wrapper.h common.c

$(GENERATED_NOTIFY_SERVER).c: $(NOTIFY_DEFS)
	$(MIG_SERVER) $(GENERATED_NOTIFY_SERVER).c $(NOTIFY_DEFS)
	sed -e 's:lisp_S_notify_server:lisp_notify_server:g' \
		-i $(GENERATED_NOTIFY_SERVER).c

notify_wrapper.o: notify_wrapper.c notify_wrapper.h common.c

$(GENERATED_FSYS_REPLY_USER).c: $(FSYS_REPLY_DEFS)
	$(MIG_USER) $(GENERATED_FSYS_REPLY_USER).c $(FSYS_REPLY_DEFS)

clean:
	rm -f *.so *.o \
		$(GENERATED_IO_SERVER).c \
		$(GENERATED_FS_SERVER).c \
		$(GENERATED_FSYS_SERVER).c \
		$(GENERATED_NOTIFY_SERVER).c \
		$(GENERATED_FSYS_REPLY_USER).c
