
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
	FILE_EXEC,
	FILE_CHOWN,
	FILE_CHAUTHOR,
	FILE_CHMOD,
	FILE_CHFLAGS,
	FILE_UTIMES,
	FILE_SET_SIZE,
	FILE_LOCK,
	FILE_LOCK_STAT,
	FILE_CHECK_ACCESS,
	FILE_NOTICE_CHANGES,
	FILE_GETCONTROL,
	FILE_STATFS,
	FILE_SYNC,
	FILE_SYNCFS,
	FILE_GET_STORAGE_INFO,
	FILE_GETLINKNODE,
	FILE_GETFH,
	DIR_LOOKUP,
	DIR_READDIR,
	DIR_MKDIR,
	DIR_RMDIR,
	DIR_UNLINK,
	DIR_LINK,
	DIR_RENAME,
	DIR_MKFILE,
	DIR_NOTICE_CHANGES,
	FILE_SET_TRANSLATOR,
	FILE_GET_TRANSLATOR,
	FILE_GET_TRANSLATOR_CNTL,
	FILE_GET_FS_OPTIONS,
	FILE_REPARENT
} FsRoutine;

/* function wrappers follows... */

kern_return_t
lisp_file_exec(file_t exec_file,
		mach_port_t exec_task,
		int flags,
		data_t argv,
		mach_msg_type_number_t argvCnt,
		data_t envp,
		mach_msg_type_number_t envpCnt,
		portarray_t fdarray,
		mach_msg_type_number_t fdarrayCnt,
		portarray_t portarray,
		mach_msg_type_number_t portarrayCnt,
		intarray_t intarray,
		mach_msg_type_number_t intarrayCnt,
		mach_port_array_t deadllocnames,
		mach_msg_type_number_t deallocnamesCnt,
		mach_port_array_t destroynames,
		mach_msg_type_number_t destroynamesCnt)
{
	/* check routine existence and then run it */
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_chown(file_t chown_file,
		uid_t new_owner,
		gid_t new_group)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_chauthor(file_t chauth_file,
		uid_t new_author)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_chmod(file_t chmod_file,
		mode_t new_mode)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_chflags(file_t chflags_file,
		int new_flags)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_utimes(file_t utimes_file,
		time_value_t new_atime,
		time_value_t new_mtime)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_set_size(file_t trunc_file,
		loff_t new_size)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_lock(file_t lock_file,
		int flags)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_lock_stat(file_t lock_file,
		int *mystatus,
		int *otherstatus)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_check_access(file_t file,
		int* allowed)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_notice_changes(file_t file,
		mach_port_t port)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_getcontrol(file_t file,
		mach_port_t *control,
		mach_msg_type_name_t *controlPoly)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_statfs(file_t file,
		fsys_statfsbuf_t *info)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_sync(file_t file,
		int wait,
		int omit_metadata)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_syncfs(file_t file,
		int wait,
		int do_children)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_get_storage_info(file_t file,
		portarray_t* ports,
		mach_msg_type_name_t *portsPoly,
		mach_msg_type_number_t *portsCnt,
		intarray_t *ints,
		mach_msg_type_number_t *intsCnt,
		off_array_t *offsets,
		mach_msg_type_number_t *offsetsCnt,
		data_t *data,
		mach_msg_type_number_t *dataCnt)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_getlinknode(file_t file,
		mach_port_t *linknode,
		mach_msg_type_name_t *linknodePoly)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_getfh(file_t file,
		data_t *filehandle,
		mach_msg_type_number_t *filehandleCnt)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_dir_lookup(file_t startdir,
		string_t filename,
		int flags,
		mode_t mode,
		retry_type *do_retry,
		string_t retry_name,
		mach_port_t *result,
		mach_msg_type_name_t *resultPoly)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_dir_readdir(file_t dir,
		data_t *data,
		mach_msg_type_number_t *dataCnt,
		boolean_t *dataDealloc,
		int entry,
		int nentries,
		vm_size_t bufsiz,
		int *amount)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_dir_mkdir(file_t directory,
		string_t name,
		mode_t mode)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_dir_rmdir(file_t directory,
		string_t name)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_dir_unlink(file_t directory,
		string_t name)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_dir_link(file_t dir,
		file_t file,
		string_t name,
		int excl)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_dir_rename(file_t olddirectory,
		string_t oldname,
		file_t newdirectory,
		string_t newname,
		int excl)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_dir_mkfile(file_t directory,
		int flags,
		mode_t mode,
		mach_port_t *newnode,
		mach_msg_type_name_t *newnodePoly)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_dir_notice_changes(file_t directory,
		mach_port_t port)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_set_translator(file_t file,
		int passive_flags,
		int active_flags,
		int oldtrans_flags,
		data_t passive,
		mach_msg_type_number_t passiveCnt,
		mach_port_t active)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_get_translator(file_t file,
		data_t *translator,
		mach_msg_type_number_t *translatorCnt)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_get_translator_cntl(file_t file,
		mach_port_t *translator_cntl,
		mach_msg_type_name_t *translator_cntlPoly)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_get_fs_options(file_t file,
		data_t *options,
		mach_msg_type_number_t *optionsCnt)
{
	return EOPNOTSUPP;
}

kern_return_t
lisp_file_reparent(file_t file,
		mach_port_t parent,
		mach_port_t *newfile,
		mach_msg_type_name_t *new_filePoly)
{
	return EOPNOTSUPP;
}
