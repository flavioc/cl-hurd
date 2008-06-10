
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <mach/boolean.h>
#include <mach/kern_return.h>
#include <mach/message.h>
#include <mach/mig_errors.h>
#include <mach/mig_support.h>

#include <mach/std_types.h>
#include <mach/mach_types.h>
#include <device/device_types.h>
#include <device/net_status.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/statfs.h>
#include <sys/resource.h>
#include <sys/utsname.h>
#include <hurd/hurd_types.h>

#include "io_wrapper.h"

/* this is NULL initialized */
static void* routines[_NUMBER_OF_ROUTINES];

/* function wrappers follows... */

/* io write */

typedef kern_return_t (*io_write_type)(io_t,
		data_t, mach_msg_type_number_t,
		loff_t, vm_size_t *);

kern_return_t
lisp_io_write(io_t io_object,
		data_t data,
		mach_msg_type_number_t dataCnt,
		loff_t offset,
		vm_size_t *amount)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_read(io_t io_object,
		data_t *data,
		mach_msg_type_number_t *dataCnt,
		loff_t offset,
		vm_size_t amount)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_seek(io_t io_object,
		loff_t offset,
		int whence,
		loff_t *newp)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_readable(io_t io_object,
		vm_size_t *amount)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_set_all_openmodes(io_t io_object,
		int newbits)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_get_openmodes(io_t io_object,
		int *bits)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_set_some_openmodes(io_t io_object,
		int bits_to_set)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_clear_some_openmodes(io_t io_object,
		int bits_to_clear)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_async(io_t io_object,
		mach_port_t notify_port,
		mach_port_t *async_id_port,
		mach_msg_type_name_t *async_id_portPoly)
{
	return EOPNOTSUPP;
}


kern_return_t
lisp_io_mod_owner(io_t io_object,
		pid_t owner)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_get_owner(io_t io_object,
		pid_t *owner)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_get_icky_async_id(io_t io_object,
		mach_port_t *icky_async_id_port,
		mach_msg_type_name_t *icky_async_id_portPoly)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_select(io_t io_object,
		int *select_type)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_stat(io_t stat_object,
		io_statbuf_t *stat_info)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_reauthenticate(io_t auth_object,
		mach_port_t rendezvous2)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_restrict_auth(io_t io_object,
		mach_port_t *new_object,
		mach_msg_type_name_t *new_objectPoly,
		idarray_t uids,
		mach_msg_type_number_t uidsCnt,
		idarray_t gids,
		mach_msg_type_number_t gidsCnt)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_duplicate(io_t io_object,
		mach_port_t *newport,
		mach_msg_type_name_t *newportPoly)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_server_version(io_t vers_object,
		string_t server_name,
		int *server_major_version,
		int *server_minor_version,
		int *server_edit_level)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_map(io_t io_object,
		mach_port_t *memobjrd,
		mach_msg_type_name_t *memobjrdPoly,
		mach_port_t *memobjwt,
		mach_msg_type_name_t *memobjwtPoly)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_map_cntl(io_t io_object,
		mach_port_t *memobj,
		mach_msg_type_name_t *memobjPoly)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_get_conch(io_t io_object)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_release_conch(io_t io_object)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_eofnotify(io_t io_object)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_prenotify(io_t io_object,
		vm_offset_t write_start,
		vm_offset_t write_end)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_postnotify(io_t io_object,
		vm_offset_t write_start,
		vm_offset_t write_end)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_readnotify(io_t io_object)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_readsleep(io_t io_object)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_sigio(io_t io_object)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_pathconf(io_t io_object,
		int name,
		int *value)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_identity(io_t io_object,
		mach_port_t *idport,
		mach_msg_type_name_t *idportPoly,
		mach_port_t *fsidport,
		mach_msg_type_name_t *fsidportPoly,
		ino64_t *fileno)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_io_revoke(io_t io_object)
{
	return EOPNOTSUPP;
}

void
set_io_routine(const IoRoutine what, void *fun)
{
	routines[what] = fun;
}
