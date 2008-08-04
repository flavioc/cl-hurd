
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

#include <stdio.h>
#include <assert.h>

#include "fs_wrapper.h"

/* this is NULL initialized */
static void *routines[_NUMBER_OF_ROUTINES];

/* function wrappers follows... */

/* file exec */

typedef kern_return_t (*file_exec_type) (file_t,
					 mach_port_t, int,
					 data_t, mach_msg_type_number_t,
					 data_t, mach_msg_type_number_t,
					 portarray_t, mach_msg_type_number_t,
					 portarray_t, mach_msg_type_number_t,
					 intarray_t, mach_msg_type_number_t,
					 mach_port_array_t,
					 mach_msg_type_number_t,
					 mach_port_array_t,
					 mach_msg_type_number_t);

kern_return_t
lisp_file_exec (file_t exec_file,
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
		mach_port_array_t deallocnames,
		mach_msg_type_number_t deallocnamesCnt,
		mach_port_array_t destroynames,
		mach_msg_type_number_t destroynamesCnt)
{
  if (routines[FILE_EXEC] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_exec_type exec_routine = routines[FILE_EXEC];

  return exec_routine (exec_file, exec_task, flags,
		       argv, argvCnt, envp,
		       envpCnt, fdarray, fdarrayCnt,
		       portarray, portarrayCnt,
		       intarray, intarrayCnt,
		       deallocnames, deallocnamesCnt,
		       destroynames, destroynamesCnt);
}

/* file chown */

typedef kern_return_t (*file_chown_type) (file_t chown_file,
					  uid_t new_owner, gid_t new_group);

kern_return_t
lisp_file_chown (file_t chown_file, uid_t new_owner, gid_t new_group)
{
  if (routines[FILE_CHOWN] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_chown_type chown_routine = routines[FILE_CHOWN];

  return chown_routine (chown_file, new_owner, new_group);
}

/* file chauthor */

typedef kern_return_t (*file_chauthor_type) (file_t, uid_t);

kern_return_t
lisp_file_chauthor (file_t chauth_file, uid_t new_author)
{
  if (routines[FILE_CHAUTHOR] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_chauthor_type chauthor_routine = routines[FILE_CHAUTHOR];

  return chauthor_routine (chauth_file, new_author);
}

/* file chmod */

typedef kern_return_t (*file_chmod_type) (file_t, mode_t);

kern_return_t
lisp_file_chmod (file_t chmod_file, mode_t new_mode)
{
  if (routines[FILE_CHMOD] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_chmod_type chmod_routine = routines[FILE_CHMOD];

  return chmod_routine (chmod_file, new_mode);
}

/* file chflags */

typedef kern_return_t (*file_chflags_type) (file_t, int);

kern_return_t
lisp_file_chflags (file_t chflags_file, int new_flags)
{
  if (routines[FILE_CHFLAGS] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_chflags_type chflags_routine = routines[FILE_CHFLAGS];

  return chflags_routine (chflags_file, new_flags);
}

/* file utimes */

typedef kern_return_t (*file_utimes_type) (file_t,
					   time_value_t *, time_value_t *);

kern_return_t
lisp_file_utimes (file_t utimes_file,
		  time_value_t new_atime, time_value_t new_mtime)
{
  if (routines[FILE_UTIMES] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_utimes_type utimes_routine = routines[FILE_UTIMES];

  // easier in lisp to just pass pointers
  return utimes_routine (utimes_file, &new_atime, &new_mtime);
}

/* file set size */
typedef kern_return_t (*file_set_size_type) (file_t, loff_t);

kern_return_t
lisp_file_set_size (file_t trunc_file, loff_t new_size)
{
  if (routines[FILE_SET_SIZE] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_set_size_type set_size_routine = routines[FILE_SET_SIZE];

  return set_size_routine (trunc_file, new_size);
}

/* file lock */

typedef kern_return_t (*file_lock_type) (file_t, int);

kern_return_t
lisp_file_lock (file_t lock_file, int flags)
{
  if (routines[FILE_LOCK] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_lock_type lock_routine = routines[FILE_LOCK];

  return lock_routine (lock_file, flags);
}

/* file lock stat */

typedef kern_return_t (*file_lock_stat_type) (file_t, int *, int *);

kern_return_t
lisp_file_lock_stat (file_t lock_file, int *mystatus, int *otherstatus)
{
  if (routines[FILE_LOCK_STAT] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_lock_stat_type lock_stat_routine = routines[FILE_LOCK_STAT];

  return lock_stat_routine (lock_file, mystatus, otherstatus);
}

/* file check access */

typedef kern_return_t (*file_check_access_type) (file_t, int *);

kern_return_t
lisp_file_check_access (file_t file, int *allowed)
{
  if (routines[FILE_CHECK_ACCESS] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_check_access_type check_access_routine = routines[FILE_CHECK_ACCESS];

  return check_access_routine (file, allowed);
}

/* file notice changes */

typedef kern_return_t (*file_notice_changes_type) (file_t, mach_port_t);

kern_return_t
lisp_file_notice_changes (file_t file, mach_port_t port)
{
  if (routines[FILE_NOTICE_CHANGES] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_notice_changes_type notice_changes_routine =
    routines[FILE_NOTICE_CHANGES];

  return notice_changes_routine (file, port);
}

/* file getcontrol */

typedef kern_return_t (*file_getcontrol_type) (file_t,
					       mach_port_t *,
					       mach_msg_type_name_t *);

kern_return_t
lisp_file_getcontrol (file_t file,
		      mach_port_t * control,
		      mach_msg_type_name_t * controlPoly)
{
  if (routines[FILE_GETCONTROL] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_getcontrol_type getcontrol_routine = routines[FILE_GETCONTROL];

  return getcontrol_routine (file, control, controlPoly);
}

/* file statfs */

typedef kern_return_t (*file_statfs_type) (file_t, fsys_statfsbuf_t *);

kern_return_t
lisp_file_statfs (file_t file, fsys_statfsbuf_t * info)
{
  printf ("STATING\n");
  if (routines[FILE_STATFS] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_statfs_type statfs_routine = routines[FILE_STATFS];

  return statfs_routine (file, info);
}

/* file sync */

typedef kern_return_t (*file_sync_type) (file_t, int, int);

kern_return_t
lisp_file_sync (file_t file, int wait, int omit_metadata)
{
  if (routines[FILE_SYNC] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_sync_type sync_routine = routines[FILE_SYNC];

  return sync_routine (file, wait, omit_metadata);
}

/* file syncfs */

typedef kern_return_t (*file_syncfs_type) (file_t, int, int);

kern_return_t
lisp_file_syncfs (file_t file, int wait, int do_children)
{
  if (routines[FILE_SYNCFS] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_syncfs_type syncfs_routine = routines[FILE_SYNCFS];

  return syncfs_routine (file, wait, do_children);
}

/* file get storage info */

typedef kern_return_t (*file_get_storage_info_type) (file_t,
						     portarray_t *,
						     mach_msg_type_name_t *,
						     mach_msg_type_number_t *,
						     intarray_t *,
						     mach_msg_type_number_t *,
						     off_array_t *,
						     mach_msg_type_number_t *,
						     data_t *,
						     mach_msg_type_number_t
						     *);

kern_return_t
lisp_file_get_storage_info (file_t file,
			    portarray_t * ports,
			    mach_msg_type_name_t * portsPoly,
			    mach_msg_type_number_t * portsCnt,
			    intarray_t * ints,
			    mach_msg_type_number_t * intsCnt,
			    off_array_t * offsets,
			    mach_msg_type_number_t * offsetsCnt,
			    data_t * data, mach_msg_type_number_t * dataCnt)
{
  if (routines[FILE_GET_STORAGE_INFO] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_get_storage_info_type get_storage_info_routine =
    routines[FILE_GET_STORAGE_INFO];

  return get_storage_info_routine (file, ports,
				   portsPoly, portsCnt, ints, intsCnt,
				   offsets, offsetsCnt, data, dataCnt);
}

/* file getlinknode */

typedef kern_return_t (*file_getlinknode_type) (file_t,
						mach_port_t *,
						mach_msg_type_name_t *);

kern_return_t
lisp_file_getlinknode (file_t file,
		       mach_port_t * linknode,
		       mach_msg_type_name_t * linknodePoly)
{
  if (routines[FILE_GETLINKNODE] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_getlinknode_type getlinknode_routine = routines[FILE_GETLINKNODE];

  return getlinknode_routine (file, linknode, linknodePoly);
}

/* file getfh */

typedef kern_return_t (*file_getfh_type) (file_t,
					  data_t *, mach_msg_type_number_t *);

kern_return_t
lisp_file_getfh (file_t file,
		 data_t * filehandle, mach_msg_type_number_t * filehandleCnt)
{
  if (routines[FILE_GETFH] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_getfh_type getfh_routine = routines[FILE_GETFH];

  return getfh_routine (file, filehandle, filehandleCnt);
}

/* dir lookup */

typedef kern_return_t (*dir_lookup_type) (file_t,
					  string_t, int,
					  mode_t, retry_type *,
					  string_t, mach_port_t *,
					  mach_msg_type_name_t *);

kern_return_t
lisp_dir_lookup (file_t startdir,
		 string_t filename,
		 int flags,
		 mode_t mode,
		 retry_type * do_retry,
		 string_t retry_name,
		 mach_port_t * result, mach_msg_type_name_t * resultPoly)
{
  printf ("DIR LOOKUP %s\n", filename);
  if (routines[DIR_LOOKUP] == NULL)
    {
      return EOPNOTSUPP;
    }

  dir_lookup_type lookup_routine = routines[DIR_LOOKUP];

  return lookup_routine (startdir, filename,
			 flags, mode, do_retry, retry_name,
			 result, resultPoly);
}

/* dir readdir */

typedef kern_return_t (*dir_readdir_type) (file_t,
					   data_t *, mach_msg_type_number_t *,
					   boolean_t *, int,
					   int, vm_size_t, int *);

kern_return_t
lisp_dir_readdir (file_t dir,
		  data_t * data,
		  mach_msg_type_number_t * dataCnt,
		  boolean_t * dataDealloc,
		  int entry, int nentries, vm_size_t bufsiz, int *amount)
{
  if (routines[DIR_READDIR] == NULL)
    {
      return EOPNOTSUPP;
    }

  dir_readdir_type readdir_routine = routines[DIR_READDIR];

  return readdir_routine (dir, data, dataCnt,
			  dataDealloc, entry, nentries, bufsiz, amount);
}

/* dir mkdir */

typedef kern_return_t (*dir_mkdir_type) (file_t, string_t, mode_t);

kern_return_t
lisp_dir_mkdir (file_t directory, string_t name, mode_t mode)
{
  if (routines[DIR_MKDIR] == NULL)
    {
      return EOPNOTSUPP;
    }

  dir_mkdir_type mkdir_routine = routines[DIR_MKDIR];

  return mkdir_routine (directory, name, mode);
}

/* dir rmdir */

typedef kern_return_t (*dir_rmdir_type) (file_t, string_t);

kern_return_t
lisp_dir_rmdir (file_t directory, string_t name)
{
  if (routines[DIR_RMDIR] == NULL)
    {
      return EOPNOTSUPP;
    }

  dir_rmdir_type rmdir_routine = routines[DIR_RMDIR];

  return rmdir_routine (directory, name);
}

/* dir unlink */

typedef kern_return_t (*dir_unlink_type) (file_t, string_t);

kern_return_t
lisp_dir_unlink (file_t directory, string_t name)
{
  if (routines[DIR_UNLINK] == NULL)
    {
      return EOPNOTSUPP;
    }

  dir_unlink_type unlink_routine = routines[DIR_UNLINK];

  return unlink_routine (directory, name);
}

/* dir link */

typedef kern_return_t (*dir_link_type) (file_t, file_t, string_t, int);

kern_return_t
lisp_dir_link (file_t dir, file_t file, string_t name, int excl)
{
  if (routines[DIR_LINK] == NULL)
    {
      return EOPNOTSUPP;
    }

  dir_link_type link_routine = routines[DIR_LINK];

  return link_routine (dir, file, name, excl);
}

/* dir rename */

typedef kern_return_t (*dir_rename_type) (file_t,
					  string_t, file_t, string_t, int);

kern_return_t
lisp_dir_rename (file_t olddirectory,
		 string_t oldname,
		 file_t newdirectory, string_t newname, int excl)
{
  if (routines[DIR_RENAME] == NULL)
    {
      return EOPNOTSUPP;
    }

  dir_rename_type rename_routine = routines[DIR_RENAME];

  return rename_routine (olddirectory, oldname, newdirectory, newname, excl);
}

/* dir mkfile */

typedef kern_return_t (*dir_mkfile_type) (file_t,
					  int, mode_t, mach_port_t *,
					  mach_msg_type_name_t *);

kern_return_t
lisp_dir_mkfile (file_t directory,
		 int flags,
		 mode_t mode,
		 mach_port_t * newnode, mach_msg_type_name_t * newnodePoly)
{
  if (routines[DIR_MKFILE] == NULL)
    {
      return EOPNOTSUPP;
    }

  dir_mkfile_type mkfile_routine = routines[DIR_MKFILE];

  return mkfile_routine (directory, flags, mode, newnode, newnodePoly);
}

/* dir notice changes */

typedef kern_return_t (*dir_notice_changes_type) (file_t, mach_port_t);

kern_return_t
lisp_dir_notice_changes (file_t directory, mach_port_t port)
{
  if (routines[DIR_NOTICE_CHANGES] == NULL)
    {
      return EOPNOTSUPP;
    }

  dir_notice_changes_type notice_changes_routine =
    routines[DIR_NOTICE_CHANGES];

  return notice_changes_routine (directory, port);
}

/* file set translator */

typedef kern_return_t (*file_set_translator_type) (file_t,
						   int, int, int,
						   data_t,
						   mach_msg_type_number_t,
						   mach_port_t);

kern_return_t
lisp_file_set_translator (file_t file,
			  int passive_flags,
			  int active_flags,
			  int oldtrans_flags,
			  data_t passive,
			  mach_msg_type_number_t passiveCnt,
			  mach_port_t active)
{
  if (routines[FILE_SET_TRANSLATOR] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_set_translator_type set_translator_routine =
    routines[FILE_SET_TRANSLATOR];

  return set_translator_routine (file, passive_flags,
				 active_flags, oldtrans_flags,
				 passive, passiveCnt, active);
}

/* file get translator */

typedef kern_return_t (*file_get_translator_type) (file_t,
						   data_t *,
						   mach_msg_type_number_t *);

kern_return_t
lisp_file_get_translator (file_t file,
			  data_t * translator,
			  mach_msg_type_number_t * translatorCnt)
{
  if (routines[FILE_GET_TRANSLATOR] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_get_translator_type get_translator_routine =
    routines[FILE_GET_TRANSLATOR];

  return get_translator_routine (file, translator, translatorCnt);
}

/* file get translator cntl */

typedef kern_return_t (*file_get_translator_cntl_type) (file_t,
							mach_port_t *,
							mach_msg_type_name_t
							*);

kern_return_t
lisp_file_get_translator_cntl (file_t file,
			       mach_port_t * translator_cntl,
			       mach_msg_type_name_t * translator_cntlPoly)
{
  if (routines[FILE_GET_TRANSLATOR_CNTL] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_get_translator_cntl_type get_translator_cntl_routine =
    routines[FILE_GET_TRANSLATOR_CNTL];

  return get_translator_cntl_routine (file, translator_cntl,
				      translator_cntlPoly);
}

/* file get fs options */

typedef kern_return_t (*file_get_fs_options_type) (file_t,
						   data_t *,
						   mach_msg_type_number_t *);

kern_return_t
lisp_file_get_fs_options (file_t file,
			  data_t * options,
			  mach_msg_type_number_t * optionsCnt)
{
  if (routines[FILE_GET_FS_OPTIONS] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_get_fs_options_type get_fs_options_routine =
    routines[FILE_GET_FS_OPTIONS];

  return get_fs_options_routine (file, options, optionsCnt);
}

/* file reparent */

typedef kern_return_t (*file_reparent_type) (file_t,
					     mach_port_t, mach_port_t *,
					     mach_msg_type_name_t *);

kern_return_t
lisp_file_reparent (file_t file,
		    mach_port_t parent,
		    mach_port_t * newfile,
		    mach_msg_type_name_t * new_filePoly)
{
  if (routines[FILE_REPARENT] == NULL)
    {
      return EOPNOTSUPP;
    }

  file_reparent_type reparent_routine = routines[FILE_REPARENT];

  return reparent_routine (file, parent, newfile, new_filePoly);
}

static const char *
routine_to_str (const FsRoutine rot)
{
#define RET(val) case val: return #val ;
  switch (rot)
    {
    RET (FILE_EXEC) RET (FILE_CHOWN) RET (FILE_CHAUTHOR) RET (FILE_CHMOD) RET (FILE_CHFLAGS) RET (FILE_UTIMES) RET (FILE_SET_SIZE) RET (FILE_LOCK) RET (FILE_LOCK_STAT) RET (FILE_CHECK_ACCESS) RET (FILE_NOTICE_CHANGES) RET (FILE_GETCONTROL) RET (FILE_STATFS) RET (FILE_SYNC) RET (FILE_SYNCFS) RET (FILE_GET_STORAGE_INFO) RET (FILE_GETLINKNODE) RET (FILE_GETFH) RET (DIR_LOOKUP) RET (DIR_READDIR) RET (DIR_MKDIR) RET (DIR_RMDIR) RET (DIR_UNLINK) RET (DIR_LINK) RET (DIR_RENAME) RET (DIR_MKFILE) RET (DIR_NOTICE_CHANGES) RET (FILE_SET_TRANSLATOR) RET (FILE_GET_TRANSLATOR) RET (FILE_GET_TRANSLATOR_CNTL) RET (FILE_GET_FS_OPTIONS) RET (FILE_REPARENT) case _NUMBER_OF_ROUTINES:
    default:
      return "";
    }

#undef RET
}

#include "common.c"

COMMON_FUNCTIONS (fs);
