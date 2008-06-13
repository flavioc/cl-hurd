
static void
_get_module_info(void)
{
	int i;
	for(i = 0; i != _NUMBER_OF_ROUTINES; ++i) {
		printf("Routine #%d: ", i);
		if(routines[i] == NULL) {
			printf("not set");
		} else {
			printf("set to address %x", (vm_size_t)routines[i]);
		}
		printf("\n");
	}
}

#define DEBUG_INFO(module) \
	void get_ ## module ## _info(void) { \
		_get_module_info(); \
	}
