
inline static void
_set_routine(const unsigned what, void *fun)
{
	assert(what < _NUMBER_OF_ROUTINES);

	if(routines[what] != NULL) {
		fprintf(stderr, "Warning: redefining routine %s\n",
				routine_to_str(what));
	} else {
		fprintf(stdout, "Information: defining routine %s\n",
				routine_to_str(what));
	}

	routines[what] = fun;
}

inline static void
_get_module_info(void)
{
	int i;
	for(i = 0; i != _NUMBER_OF_ROUTINES; ++i) {
		if(routines[i] != NULL) {
			printf("Routine #%d (%s): set to address %x\n", i,
				routine_to_str(i), (vm_size_t)routines[i]);
		}
	}
}

#define COMMON_FUNCTIONS(module) \
	void get_ ## module ## _info(void) { \
		_get_module_info(); \
	} \
	void set_ ## module ## _routine(const unsigned what, void *fun) { \
		_set_routine(what, fun); \
	}
