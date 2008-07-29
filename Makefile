
TARGET_DIRS = stubs libs launcher

all:
	@for dir in $(TARGET_DIRS); do \
		$(MAKE) -C $$dir; \
	done

clean:
	@for dir in $(TARGET_DIRS); do \
		$(MAKE) -C $$dir clean; \
	done
