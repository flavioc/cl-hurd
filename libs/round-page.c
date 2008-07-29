
#include <mach/vm_param.h>

vm_offset_t
helper_round_page(vm_offset_t address)
{
	return round_page(address);
}
