/*
   Copyright (C) 1995,96,99,2000,02 Free Software Foundation, Inc.
   Written by Michael I. Bushnell.

   This file is part of the GNU Hurd.

   The GNU Hurd is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   The GNU Hurd is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */


#include <unistd.h>
#include <assert.h>
#include <string.h>
#include <hurd.h>
#include <hurd/fsys.h>
#include <hurd/fshelp.h>
#include <hurd/auth.h>
#include <hurd/io.h>

typedef error_t (*mycallback2_t) (int, mach_port_t *, mach_msg_type_name_t *);

error_t
helper_fetch_root (file_t dotdot,
		   mycallback2_t callback2,
		   uid_t uid, gid_t gid,
		   char *argz, size_t argz_len, mach_port_t * control_port)
{
  error_t err;
  mach_port_t control;
  int i;
  mach_port_t ports[INIT_PORT_MAX];
  int ints[INIT_INT_MAX];
  mach_port_t fds[STDERR_FILENO + 1];
  auth_t ourauth, newauth;

  mach_port_t reauth (mach_port_t port)	/* Consumes PORT.  */
  {
    mach_port_t rend, ret;
    error_t err;

    if (port == MACH_PORT_NULL)
      return port;

    if (ourauth == MACH_PORT_NULL)
      /* We have no auth server, so we aren't doing reauthentications.
         Just pass on our own ports directly.  */
      return port;

    rend = mach_reply_port ();

    /* MAKE_SEND is safe here because we destroy REND ourselves. */
    err = io_reauthenticate (port, rend, MACH_MSG_TYPE_MAKE_SEND);
    mach_port_deallocate (mach_task_self (), port);
    if (!err)
      err = auth_user_authenticate (newauth, rend,
				    MACH_MSG_TYPE_MAKE_SEND, &ret);
    if (err)
      ret = MACH_PORT_NULL;

    mach_port_destroy (mach_task_self (), rend);

    return ret;
  }

  error_t fetch_underlying (int flags, mach_port_t * underlying,
			    mach_msg_type_name_t * underlying_type,
			    task_t task, void *cookie)
  {
    return (*callback2) (flags, underlying, underlying_type);
  }

  ourauth = getauth ();
  if (ourauth == MACH_PORT_NULL)
    newauth = ourauth;
  else
    {
      uid_t uidarray[2] = { uid, uid };
      gid_t gidarray[2] = { gid, gid };
      err = auth_makeauth (ourauth, 0, MACH_MSG_TYPE_COPY_SEND, 0,
			   uidarray, 1, uidarray, 2,
			   gidarray, 1, gidarray, 2, &newauth);
      if (err)
	return err;
    }

  bzero (ports, INIT_PORT_MAX * sizeof (mach_port_t));
  bzero (fds, (STDERR_FILENO + 1) * sizeof (mach_port_t));
  bzero (ints, INIT_INT_MAX * sizeof (int));

  ports[INIT_PORT_CWDIR] = dotdot;
  ports[INIT_PORT_CRDIR] = reauth (getcrdir ());
  ports[INIT_PORT_AUTH] = newauth;

  fds[STDERR_FILENO] = reauth (getdport (STDERR_FILENO));

  err = fshelp_start_translator_long (fetch_underlying, NULL,
				      argz, argz, argz_len,
				      fds, MACH_MSG_TYPE_COPY_SEND,
				      STDERR_FILENO + 1,
				      ports, MACH_MSG_TYPE_COPY_SEND,
				      INIT_PORT_MAX,
				      ints, INIT_INT_MAX, uid, 0, &control);
  for (i = 0; i <= STDERR_FILENO; i++)
    mach_port_deallocate (mach_task_self (), fds[i]);

  for (i = 0; i < INIT_PORT_MAX; i++)
    if (i != INIT_PORT_CWDIR)
      mach_port_deallocate (mach_task_self (), ports[i]);

  *control_port = control;

  return err;
}
