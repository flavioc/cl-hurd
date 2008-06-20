
#include <stdio.h>
#include <hurd/ports.h>

struct port_info*
create_port(struct port_bucket* bucket,
		struct port_class* class,
		size_t size,
		error_t *error)
{
	struct port_info *pi = NULL;

	*error = ports_create_port(class, bucket, size, &pi);

	if(*error) {
		return NULL;
	} else {
		fprintf(stderr, "right: %d\n", ports_get_send_right(pi));
		return pi;
	}
}
