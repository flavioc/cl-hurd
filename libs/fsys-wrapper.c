
/* fsys.defs wrapper code.

   Copyright (C) 2008 Free Software Foundation, Inc.

   Written by Fl�vio Cruz <flaviocruz@gmail.com>

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

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

#include "fsys-wrapper.h"

/* this is NULL initialized */
static void *routines[_NUMBER_OF_ROUTINES];

/* function wrappers follows... */

/* fsys startup */

typedef kern_return_t (*fsys_startup_type) (mach_port_t,
					    mach_port_t, mach_msg_type_name_t,
					    int, mach_port_t,
					    mach_port_t *,
					    mach_msg_type_name_t *);

kern_return_t
lisp_S_fsys_startup (mach_port_t bootstrap,
		   mach_port_t reply,
		   mach_msg_type_name_t replyPoly,
		   int openflags,
		   mach_port_t control_port,
		   mach_port_t * realnode,
		   mach_msg_type_name_t * realnodePoly)
{
  if (routines[FSYS_STARTUP] == NULL)
    {
      return EOPNOTSUPP;
    }

  fsys_startup_type startup_routine = routines[FSYS_STARTUP];

  return startup_routine (bootstrap, reply, replyPoly, openflags,
			  control_port, realnode, realnodePoly);
}

/* fsys goaway */

typedef kern_return_t (*fsys_goaway_type) (fsys_t, mach_port_t,
					   mach_msg_type_name_t, int);

kern_return_t
lisp_S_fsys_goaway (fsys_t fsys, mach_port_t reply,
		  mach_msg_type_name_t replyPoly, int flags)
{
  if (routines[FSYS_GOAWAY] == NULL)
    {
      return EOPNOTSUPP;
    }

  fsys_goaway_type goaway_routine = routines[FSYS_GOAWAY];

  return goaway_routine (fsys, reply, replyPoly, flags);
}

/* fsys getroot */

typedef kern_return_t (*fsys_getroot_type) (fsys_t,
					    mach_port_t,
					    mach_msg_type_name_t,
					    mach_port_t,
					    idarray_t,
					    mach_msg_type_number_t,
					    idarray_t,
					    mach_msg_type_number_t,
					    int, retry_type *,
					    string_t,
					    mach_port_t *,
					    mach_msg_type_name_t *);

kern_return_t
lisp_S_fsys_getroot (fsys_t fsys,
		   mach_port_t reply,
		   mach_msg_type_name_t replyPoly,
		   mach_port_t dotdot_node,
		   idarray_t gen_uids,
		   mach_msg_type_number_t gen_uidsCnt,
		   idarray_t gen_gids,
		   mach_msg_type_number_t gen_gidsCnt,
		   int flags,
		   retry_type * do_retry,
		   string_t retry_name,
		   mach_port_t * file, mach_msg_type_name_t * filePoly)
{
  if (routines[FSYS_GETROOT] == NULL)
    {
      return EOPNOTSUPP;
    }

  fsys_getroot_type getroot_routine = routines[FSYS_GETROOT];

  return getroot_routine (fsys, reply, replyPoly,
			  dotdot_node,
			  gen_uids, gen_uidsCnt,
			  gen_gids, gen_gidsCnt,
			  flags, do_retry, retry_name, file, filePoly);
}

/* fsys getfile */

typedef kern_return_t (*fsys_getfile_type) (fsys_t,
					    mach_port_t,
					    mach_msg_type_name_t,
					    idarray_t,
					    mach_msg_type_number_t,
					    idarray_t,
					    mach_msg_type_number_t,
					    data_t,
					    mach_msg_type_number_t,
					    mach_port_t *,
					    mach_msg_type_name_t *);

kern_return_t
lisp_S_fsys_getfile (fsys_t fsys,
		   mach_port_t reply,
		   mach_msg_type_name_t replyPoly,
		   idarray_t gen_uids,
		   mach_msg_type_number_t gen_uidsCnt,
		   idarray_t gen_gids,
		   mach_msg_type_number_t gen_gidsCnt,
		   data_t filehandle,
		   mach_msg_type_number_t filehandleCnt,
		   mach_port_t * file, mach_msg_type_name_t * filePoly)
{
  if (routines[FSYS_GETFILE] == NULL)
    {
      return EOPNOTSUPP;
    }

  fsys_getfile_type getfile_routine = routines[FSYS_GETFILE];

  return getfile_routine (fsys, reply, replyPoly,
			  gen_uids, gen_uidsCnt,
			  gen_gids, gen_gidsCnt,
			  filehandle, filehandleCnt, file, filePoly);
}

/* fsys syncfs */

typedef kern_return_t (*fsys_syncfs_type) (fsys_t,
					   mach_port_t, mach_msg_type_name_t,
					   int, int);

kern_return_t
lisp_S_fsys_syncfs (fsys_t fsys,
		  mach_port_t reply,
		  mach_msg_type_name_t replyPoly, int wait, int do_children)
{
  if (routines[FSYS_SYNCFS] == NULL)
    {
      return EOPNOTSUPP;
    }

  fsys_syncfs_type syncfs_routine = routines[FSYS_SYNCFS];

  return syncfs_routine (fsys, reply, replyPoly, wait, do_children);
}

/* fsys set options */

typedef kern_return_t (*fsys_set_options_type) (fsys_t,
						mach_port_t,
						mach_msg_type_name_t, data_t,
						mach_msg_type_number_t, int);

kern_return_t
lisp_S_fsys_set_options (fsys_t fsys,
		       mach_port_t reply,
		       mach_msg_type_name_t replyPoly,
		       data_t options,
		       mach_msg_type_number_t optionsCnt, int do_children)
{
  if (routines[FSYS_SET_OPTIONS] == NULL)
    {
      return EOPNOTSUPP;
    }

  fsys_set_options_type set_options_routine = routines[FSYS_SET_OPTIONS];

  return set_options_routine (fsys, reply, replyPoly,
			      options, optionsCnt, do_children);
}

/* fsys getpriv */

typedef kern_return_t (*fsys_getpriv_type) (fsys_t,
					    mach_port_t,
					    mach_msg_type_name_t,
					    mach_port_t *,
					    mach_msg_type_name_t *,
					    mach_port_t *,
					    mach_msg_type_name_t *,
					    mach_port_t *,
					    mach_msg_type_name_t *);

kern_return_t
lisp_S_fsys_getpriv (fsys_t fsys,
		   mach_port_t reply,
		   mach_msg_type_name_t replyPoly,
		   mach_port_t * host_priv,
		   mach_msg_type_name_t * host_privPoly,
		   mach_port_t * device_master,
		   mach_msg_type_name_t * device_masterPoly,
		   mach_port_t * fstask, mach_msg_type_name_t * fstaskPoly)
{
  if (routines[FSYS_GETPRIV] == NULL)
    {
      return EOPNOTSUPP;
    }

  fsys_getpriv_type getpriv_routine = routines[FSYS_GETPRIV];

  return getpriv_routine (fsys, reply, replyPoly,
			  host_priv,
			  host_privPoly, device_master,
			  device_masterPoly, fstask, fstaskPoly);
}

/* fsys init */

typedef kern_return_t (*fsys_init_type) (fsys_t,
					 mach_port_t, mach_msg_type_name_t,
					 mach_port_t, auth_t);

kern_return_t
lisp_S_fsys_init (fsys_t fsys,
		mach_port_t reply_port,
		mach_msg_type_name_t reply_portPoly,
		mach_port_t proc_server, auth_t auth_handle)
{
  if (routines[FSYS_INIT] == NULL)
    {
      return EOPNOTSUPP;
    }

  fsys_init_type init_routine = routines[FSYS_INIT];

  return init_routine (fsys, reply_port, reply_portPoly,
		       proc_server, auth_handle);
}

/* fsys forward */

typedef kern_return_t (*fsys_forward_type) (mach_port_t,
					    mach_port_t, mach_msg_type_name_t,
					    mach_port_t, data_t,
					    mach_msg_type_number_t);

kern_return_t
lisp_S_fsys_forward (mach_port_t server,
		   mach_port_t reply,
		   mach_msg_type_name_t replyPoly,
		   mach_port_t requestor,
		   data_t argv, mach_msg_type_number_t argvCnt)
{
  if (routines[FSYS_FORWARD] == NULL)
    {
      return EOPNOTSUPP;
    }

  fsys_forward_type forward_routine = routines[FSYS_FORWARD];

  return forward_routine (server, reply, replyPoly, requestor, argv, argvCnt);
}

/* fsys get options */

typedef kern_return_t (*fsys_get_options_type) (fsys_t,
						mach_port_t,
						mach_msg_type_name_t,
						data_t *,
						mach_msg_type_number_t *);

kern_return_t
lisp_S_fsys_get_options (fsys_t server,
		       mach_port_t reply,
		       mach_msg_type_name_t replyPoly,
		       data_t * options, mach_msg_type_number_t * optionsCnt)
{
  if (routines[FSYS_GET_OPTIONS] == NULL)
    {
      return EOPNOTSUPP;
    }

  fsys_get_options_type get_options_routine = routines[FSYS_GET_OPTIONS];

  return get_options_routine (server, reply, replyPoly, options, optionsCnt);
}


static const char *
routine_to_str (const FsysRoutine rot)
{
#define RET(val) case val: return #val ;
  switch (rot)
    {
    RET (FSYS_STARTUP)
    RET (FSYS_GOAWAY)
    RET (FSYS_GETROOT)
    RET (FSYS_GETFILE)
    RET (FSYS_SYNCFS)
    RET (FSYS_SET_OPTIONS)
    RET (FSYS_GETPRIV)
    RET (FSYS_INIT)
    RET (FSYS_FORWARD)
    RET (FSYS_GET_OPTIONS)
    case _NUMBER_OF_ROUTINES:
    default:
      return "";
    }

#undef RET
}

#include "common.c"

COMMON_FUNCTIONS (fsys);
