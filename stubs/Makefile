
include defs.mk

TARGETS = fs_stubs.so io_stubs.so

# fs stubs
FS_DEFS = $(DEFS_DIRECTORY)/fs.defs
GENERATED_FS_SERVER = fs_server
FS_OBJECTS = $(GENERATED_FS_SERVER).o fs_wrapper.o

# io stubs
IO_DEFS = $(DEFS_DIRECTORY)/io.defs
GENERATED_IO_SERVER = io_server
IO_OBJECTS = $(GENERATED_IO_SERVER).o io_wrapper.o

all: $(TARGETS)

#
# io stubs go here
#

io_stubs.so: $(IO_OBJECTS)
	$(CC) -shared -Wl \
		-o io_stubs.o $(IO_OBJECTS) $(LDFLAGS)

$(GENERATED_IO_SERVER).o: $(GENERATED_IO_SERVER).c

$(GENERATED_IO_SERVER).c: $(IO_DEFS)
	$(MIG) $(GENERATED_IO_SERVER).c $(IO_DEFS)

#
# fs stubs start here
#

fs_stubs.so: $(FS_OBJECTS)
	$(CC) -shared -Wl \
		-o fs_stubs.o $(FS_OBJECTS) $(LDFLAGS)

$(GENERATED_FS_SERVER).o: $(GENERATED_FS_SERVER).c

$(GENERATED_FS_SERVER).c: $(FS_DEFS)
	$(MIG) $(GENERATED_FS_SERVER).c $(FS_DEFS)

clean:
	rm -f *.so *.o $(GENERATED_FS_SERVER).c
