
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

/* routines that can be set by the middle level */
typedef enum {
	FSYS_STARTUP,
	FSYS_GOAWAY,
	FSYS_GETROOT,
	FSYS_GETFILE,
	FSYS_SYNCFS,
	FSYS_SET_OPTIONS,
	FSYS_GETPRIV,
	FSYS_INIT,
	FSYS_FORWARD,
	FSYS_GET_OPTIONS
} FsysRoutine;

/* function wrappers follows... */

kern_return_t
lisp_fsys_startup(mach_port_t bootstrap,
		int openflags,
		mach_port_t control_port,
		mach_port_t *realnode,
		mach_msg_type_name_t *realnodePoly)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_fsys_goaway(fsys_t fsys, int flags)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_fsys_getroot(fsys_t fsys,
		mach_port_t dotdot_node,
		idarray_t gen_uids,
		mach_msg_type_number_t gen_uidsCnt,
		idarray_t gen_gids,
		mach_msg_type_number_t gen_gidsCnt,
		int flags,
		retry_type *do_retry,
		string_t retry_name,
		mach_port_t *file,
		mach_msg_type_name_t *filePoly)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_fsys_getfile(fsys_t fsys,
		idarray_t gen_uids,
		mach_msg_type_number_t gen_uidsCnt,
		idarray_t gen_gids,
		mach_msg_type_number_t gen_gidsCnt,
		data_t filehandle,
		mach_msg_type_number_t filehandleCnt,
		mach_port_t *file,
		mach_msg_type_name_t *filePoly)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_fsys_syncfs(fsys_t fsys,
		int wait,
		int do_children)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_fsys_set_options(fsys_t fsys,
		data_t options,
		mach_msg_type_number_t optionsCnt,
		int do_children)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_fsys_getpriv(fsys_t fsys,
		mach_port_t *host_priv,
		mach_msg_type_name_t *host_privPoly,
		mach_port_t *device_master,
		mach_msg_type_name_t *device_masterPoly,
		mach_port_t *fstask,
		mach_msg_type_name_t *fstaskPoly)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_fsys_init(fsys_t fsys,
		mach_port_t reply_port,
		mach_msg_type_name_t reply_portPoly,
		mach_port_t proc_server,
		auth_t auth_handle)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_fsys_forward(mach_port_t server,
		mach_port_t requestor,
		data_t argv,
		mach_msg_type_number_t argvCnt)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_fsys_get_options(fsys_t server,
		data_t *options,
		mach_msg_type_number_t *optionsCnt)
{
	return EOPNOTSUPP;
}
