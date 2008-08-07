
/* notify.defs wrapper code.

   Copyright (C) 2008 Free Software Foundation, Inc.

   Written by Fl√vio Cruz <flaviocruz@gmail.com>

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

#include "notify-wrapper.h"

/* this is NULL initialized */
static void* routines[_NUMBER_OF_ROUTINES];

/* function wrappers follows... */

/* do mach notify port deleted */

typedef kern_return_t (*do_mach_notify_port_deleted_type)(mach_port_t,
		mach_port_t name);

kern_return_t
lisp_do_mach_notify_port_deleted(mach_port_t notify,
		mach_port_t name)
{
	//fprintf(stderr, "notify: port deleted\n");
	if(routines[DO_MACH_NOTIFY_PORT_DELETED] == NULL) {
		return 0;
	}

	do_mach_notify_port_deleted_type do_mach_notify_port_deleted_routine =
		routines[DO_MACH_NOTIFY_PORT_DELETED];

	return do_mach_notify_port_deleted_routine(notify, name);
}

/* do mach notify msg accepted */

typedef kern_return_t (*do_mach_notify_msg_accepted_type)(mach_port_t,
		mach_port_t);

kern_return_t
lisp_do_mach_notify_msg_accepted(mach_port_t notify,
		mach_port_t name)
{
	//fprintf(stderr, "notify: msg accepted\n");
	if(routines[DO_MACH_NOTIFY_MSG_ACCEPTED] == NULL) {
		return 0;
	}

	do_mach_notify_msg_accepted_type do_mach_notify_msg_accepted_routine =
		routines[DO_MACH_NOTIFY_MSG_ACCEPTED];

	return do_mach_notify_msg_accepted_routine(notify, name);
}

/* do mach notify port destroyed */

typedef kern_return_t (*do_mach_notify_port_destroyed_type)(mach_port_t,
		mach_port_t);

kern_return_t
lisp_do_mach_notify_port_destroyed(mach_port_t notify,
		mach_port_t rights)
{
	//fprintf(stderr, "notify: port-destroyed\n");

	if(routines[DO_MACH_NOTIFY_PORT_DESTROYED] == NULL) {
		return 0;
	}

	do_mach_notify_port_destroyed_type
		do_mach_port_notify_port_destroyed_routine =
			routines[DO_MACH_NOTIFY_PORT_DESTROYED];

	return do_mach_port_notify_port_destroyed_routine(notify, rights);
}

/* do mach notify no senders */

typedef kern_return_t (*do_mach_notify_no_senders_type)(mach_port_t,
		mach_port_mscount_t);

kern_return_t
lisp_do_mach_notify_no_senders(mach_port_t notify,
		mach_port_mscount_t mscount)
{
	//fprintf(stderr, "notify: no senders\n");
	if(routines[DO_MACH_NOTIFY_NO_SENDERS] == NULL) {
		return EOPNOTSUPP;
	}

	do_mach_notify_no_senders_type do_mach_notify_no_senders_routine =
		routines[DO_MACH_NOTIFY_NO_SENDERS];

	return do_mach_notify_no_senders_routine(notify, mscount);
}

/* do mach notify send once */

typedef kern_return_t (*do_mach_notify_send_once_type)(mach_port_t);

kern_return_t
lisp_do_mach_notify_send_once(mach_port_t notify)
{
	//fprintf(stderr, "notify: send once\n");
	if(routines[DO_MACH_NOTIFY_SEND_ONCE] == NULL) {
		return 0;
	}

	do_mach_notify_send_once_type do_mach_notify_send_once_routine =
		routines[DO_MACH_NOTIFY_SEND_ONCE];

	return do_mach_notify_send_once_routine(notify);
}

/* do mach notify dead name */

typedef kern_return_t (*do_mach_notify_dead_name_type)(mach_port_t,
		mach_port_t);

kern_return_t
lisp_do_mach_notify_dead_name(mach_port_t notify,
		mach_port_t name)
{
	//fprintf(stderr, "notify: dead name\n");
	if(routines[DO_MACH_NOTIFY_DEAD_NAME] == NULL) {
		return EOPNOTSUPP;
	}

	do_mach_notify_dead_name_type do_mach_notify_dead_name_routine =
		routines[DO_MACH_NOTIFY_DEAD_NAME];

	return do_mach_notify_dead_name_routine(notify, name);
}

static const char*
routine_to_str(const NotifyRoutine rot)
{
#define RET(val) case val: return #val ;
	switch(rot) {
		RET(DO_MACH_NOTIFY_PORT_DELETED)
		RET(DO_MACH_NOTIFY_MSG_ACCEPTED)
		RET(DO_MACH_NOTIFY_PORT_DESTROYED)
		RET(DO_MACH_NOTIFY_NO_SENDERS)
		RET(DO_MACH_NOTIFY_SEND_ONCE)
		RET(DO_MACH_NOTIFY_DEAD_NAME)
		case _NUMBER_OF_ROUTINES:
		default:
			return "";
	}

#undef RET
}

#include "common.c"

COMMON_FUNCTIONS(notify);
