
/* Demuxer wrapper.

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

#include <mach.h>
#include <mach/notify.h>
#include <mach/mig_errors.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <stdio.h>

typedef int (*demuxer_fun_type) (mach_port_t,
				 mach_msg_header_t *, mach_msg_header_t *);

static demuxer_fun_type demuxer = NULL;

int
portset_demuxer (mach_msg_header_t * inp, mach_msg_header_t * outheadp)
{
  assert (demuxer != NULL);

  register mig_reply_header_t *outp = (mig_reply_header_t *) outheadp;
  static const mach_msg_type_t RetCodeType = {
    /* msgt_name = */ MACH_MSG_TYPE_INTEGER_32,
    /* msgt_size = */ 32,
    /* msgt_number = */ 1,
    /* msgt_inline = */ TRUE,
    /* msgt_longform = */ FALSE,
    /* msgt_deallocate = */ FALSE,
    /* msgt_unused = */ 0
  };

  /* Fill in default response */
  outp->Head.msgh_bits
    = MACH_MSGH_BITS (MACH_MSGH_BITS_REMOTE (inp->msgh_bits), 0);
  outp->Head.msgh_size = sizeof (*outp);
  outp->Head.msgh_remote_port = inp->msgh_remote_port;
  outp->Head.msgh_local_port = MACH_PORT_NULL;
  outp->Head.msgh_seqno = 0;
  outp->Head.msgh_id = inp->msgh_id + 100;
  outp->RetCodeType = RetCodeType;
  outp->RetCode = MIG_BAD_ID;

  int ret = demuxer (inp->msgh_local_port, inp, outheadp);

  if (ret == EOPNOTSUPP)
    {
      outp->RetCode = EOPNOTSUPP;
      return (1);
    }
  else
    {
      return (ret);
    }
}

void
set_demuxer (demuxer_fun_type fun)
{
  assert (fun != NULL);

  demuxer = fun;
}
