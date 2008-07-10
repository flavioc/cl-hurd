
include ../defs.mk

TARGETS = fs_stubs.so io_stubs.so \
					fsys_stubs.so notify_stubs.so \
					fsys_reply_stubs.so

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

all: $(TARGETS)

#
# io stubs go here
#

io_stubs.so: $(IO_OBJECTS)
	$(CC) -shared -Wl \
		-o io_stubs.so $(IO_OBJECTS) $(LDFLAGS)

$(GENERATED_IO_SERVER).o: $(GENERATED_IO_SERVER).c

$(GENERATED_IO_SERVER).c: $(IO_DEFS)
	$(MIG_SERVER) $(GENERATED_IO_SERVER).c $(IO_DEFS)

io_wrapper.o: io_wrapper.c io_wrapper.h common.c

#
# fs stubs start here
#

fs_stubs.so: $(FS_OBJECTS)
	$(CC) -shared -Wl \
		-o fs_stubs.so $(FS_OBJECTS) $(LDFLAGS)

$(GENERATED_FS_SERVER).o: $(GENERATED_FS_SERVER).c

$(GENERATED_FS_SERVER).c: $(FS_DEFS)
	$(MIG_SERVER) $(GENERATED_FS_SERVER).c $(FS_DEFS)

fs_wrapper.o: fs_wrapper.c fs_wrapper.h common.c

#
# and fsys right here
#

fsys_stubs.so: $(FSYS_OBJECTS)
	$(CC) -shared -Wl \
		-o fsys_stubs.so $(FSYS_OBJECTS) $(LDFLAGS)

$(GENERATED_FSYS_SERVER).o: $(GENERATED_FSYS_SERVER).c

$(GENERATED_FSYS_SERVER).c: $(FSYS_DEFS)
	cpp $(FSYS_DEFS) -D REPLY_PORTS > tmp.defs
	$(MIG_SERVER) $(GENERATED_FSYS_SERVER).c tmp.defs
	rm -f tmp.defs

fsys_wrapper.o: fsys_wrapper.c fsys_wrapper.h common.c

#
# notifications stubs from GNU Mach
#

notify_stubs.so: $(NOTIFY_OBJECTS)
	$(CC) -shared -Wl \
		-o notify_stubs.so $(NOTIFY_OBJECTS) $(LDFLAGS)

$(GENERATED_NOTIFY_SERVER).o: $(GENERATED_NOTIFY_SERVER).c

$(GENERATED_NOTIFY_SERVER).c: $(NOTIFY_DEFS)
	$(MIG_SERVER) $(GENERATED_NOTIFY_SERVER).c $(NOTIFY_DEFS)

#
# fsys reply stubs
#

fsys_reply_stubs.so: $(FSYS_REPLY_OBJECTS)
	$(CC) -shared -Wl \
		-o fsys_reply_stubs.so $(FSYS_REPLY_OBJECTS) $(LDFLAGS)

$(GENERATED_FSYS_REPLY_USER).o: $(GENERATED_FSYS_REPLY_USER).c

$(GENERATED_FSYS_REPLY_USER).c: $(FSYS_REPLY_DEFS)
	$(MIG_USER) $(GENERATED_FSYS_REPLY_USER).c $(FSYS_REPLY_DEFS)

#
# misc stuff
#

clean.c:

clean:
	rm -f *.so *.o \
		$(GENERATED_FS_SERVER).c \
		$(GENERATED_IO_SERVER).c \
		$(GENERATED_FSYS_SERVER).c \
		$(GENERATED_FSYS_REPLY_USER).c
