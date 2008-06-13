
#define FUNCTION_NAME get_ ## MODULE_NAME ##_info

void
FUNCTION_NAME(void)
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
