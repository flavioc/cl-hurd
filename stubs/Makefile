
include defs.mk

TARGETS = fs_stubs.so

# fs stubs
FS_DEFS = $(DEFS_DIRECTORY)/fs.defs
GENERATED_FS_SERVER = fs_server
FS_OBJECTS = $(GENERATED_FS_SERVER).o fs_wrapper.o

all: $(TARGETS)

fs_stubs.so: $(FS_OBJECTS)
	$(CC) -shared -Wl \
		-o fs_stubs.o $(FS_OBJECTS) $(LDFLAGS)

$(GENERATED_FS_SERVER).o: $(GENERATED_FS_SERVER).c

$(GENERATED_FS_SERVER).c: $(FS_DEFS)
	$(MIG) $(GENERATED_FS_SERVER).c $(FS_DEFS)

clean:
	rm -f *.so *.o $(GENERATED_FS_SERVER).c
