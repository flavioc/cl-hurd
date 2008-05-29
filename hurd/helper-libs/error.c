
#include <errno.h>

int
get_hurd_error_code(const int id)
{
	/* wrap macro in the function */
	return _HURD_ERRNO(id);
}
