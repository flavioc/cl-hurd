
include ../defs.mk

# fs stubs
FS_DEFS = $(HURD_DIRECTORY)/fs.defs
GENERATED_FS_SERVER = fs_server
FS_OBJECTS = $(GENERATED_FS_SERVER).o fs_wrapper.o

# io stubs
IO_DEFS = $(HURD_DIRECTORY)/io.defs
GENERATED_IO_SERVER = io_server
IO_OBJECTS = $(GENERATED_IO_SERVER).o io_wrapper.o

# fsys stubs
FSYS_DEFS = $(HURD_DIRECTORY)/fsys.defs
GENERATED_FSYS_SERVER = fsys_server
FSYS_OBJECTS = $(GENERATED_FSYS_SERVER).o fsys_wrapper.o

# notify stubs
NOTIFY_DEFS = $(MACH_DIRECTORY)/notify.defs
GENERATED_NOTIFY_SERVER = notify_server
NOTIFY_OBJECTS = $(GENERATED_NOTIFY_SERVER).o notify_wrapper.o

# fsys reply stubs

FSYS_REPLY_DEFS = $(HURD_DIRECTORY)/fsys_reply.defs
GENERATED_FSYS_REPLY_USER = fsys_reply_user
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

io_wrapper.o: io_wrapper.c io_wrapper.h common.c

$(GENERATED_FS_SERVER).c: $(FS_DEFS)
	$(MIG_SERVER) $(GENERATED_FS_SERVER).c $(FS_DEFS)

fs_wrapper.o: fs_wrapper.c fs_wrapper.h common.c

$(GENERATED_FSYS_SERVER).c: $(FSYS_DEFS)
	cpp $(FSYS_DEFS) -D REPLY_PORTS > tmp.defs
	$(MIG_SERVER) $(GENERATED_FSYS_SERVER).c tmp.defs
	rm -f tmp.defs

fsys_wrapper.o: fsys_wrapper.c fsys_wrapper.h common.c

$(GENERATED_NOTIFY_SERVER).c: $(NOTIFY_DEFS)
	$(MIG_SERVER) $(GENERATED_NOTIFY_SERVER).c $(NOTIFY_DEFS)

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
