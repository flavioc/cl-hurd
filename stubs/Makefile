
include defs.mk

TARGETS = fs_stubs.so io_stubs.so fsys_stubs.so

# fs stubs
FS_DEFS = $(DEFS_DIRECTORY)/fs.defs
GENERATED_FS_SERVER = fs_server
FS_OBJECTS = $(GENERATED_FS_SERVER).o fs_wrapper.o

# io stubs
IO_DEFS = $(DEFS_DIRECTORY)/io.defs
GENERATED_IO_SERVER = io_server
IO_OBJECTS = $(GENERATED_IO_SERVER).o io_wrapper.o

# fsys stubs
FSYS_DEFS = $(DEFS_DIRECTORY)/fsys.defs
GENERATED_FSYS_SERVER = fsys_server
FSYS_OBJECTS = $(GENERATED_FSYS_SERVER).o fsys_wrapper.o

all: $(TARGETS)

#
# io stubs go here
#

io_stubs.so: $(IO_OBJECTS)
	$(CC) -shared -Wl \
		-o io_stubs.so $(IO_OBJECTS) $(LDFLAGS)

$(GENERATED_IO_SERVER).o: $(GENERATED_IO_SERVER).c

$(GENERATED_IO_SERVER).c: $(IO_DEFS)
	$(MIG) $(GENERATED_IO_SERVER).c $(IO_DEFS)

io_wrapper.o: io_wrapper.c io_wrapper.h debug.c

#
# fs stubs start here
#

fs_stubs.so: $(FS_OBJECTS)
	$(CC) -shared -Wl \
		-o fs_stubs.so $(FS_OBJECTS) $(LDFLAGS)

$(GENERATED_FS_SERVER).o: $(GENERATED_FS_SERVER).c

$(GENERATED_FS_SERVER).c: $(FS_DEFS)
	$(MIG) $(GENERATED_FS_SERVER).c $(FS_DEFS)

fs_wrapper.o: fs_wrapper.c fs_wrapper.h debug.c

#
# and fsys right here
#

fsys_stubs.so: $(FSYS_OBJECTS)
	$(CC) -shared -Wl \
		-o fsys_stubs.so $(FSYS_OBJECTS) $(LDFLAGS)

$(GENERATED_FSYS_SERVER).o: $(GENERATED_FSYS_SERVER).c

$(GENERATED_FSYS_SERVER).c: $(FSYS_DEFS)
	$(MIG) $(GENERATED_FSYS_SERVER).c $(FSYS_DEFS)

fsys_wrapper.o: fsys_wrapper.c fsys_wrapper.h debug.c


#
# misc stuff
#

clean.c:

clean:
	rm -f *.so *.o $(GENERATED_FS_SERVER).c
