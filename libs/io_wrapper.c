
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

#include "io_wrapper.h"

/* this is NULL initialized */
static void *routines[_NUMBER_OF_ROUTINES];

/* function wrappers follows... */

/* io write */

typedef kern_return_t (*io_write_type) (io_t,
					data_t, mach_msg_type_number_t,
					off_t, vm_size_t *);

kern_return_t
lisp_io_write (io_t io_object,
	       data_t data,
	       mach_msg_type_number_t dataCnt,
	       loff_t offset, vm_size_t * amount)
{
  if (routines[IO_WRITE] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_write_type write_routine = routines[IO_WRITE];

  return write_routine (io_object, data, dataCnt, offset, amount);
}

/* io read */

typedef kern_return_t (*io_read_type) (io_t,
				       data_t *, mach_msg_type_number_t *,
				       off_t, vm_size_t);

kern_return_t
lisp_io_read (io_t io_object,
	      data_t * data,
	      mach_msg_type_number_t * dataCnt,
	      loff_t offset, vm_size_t amount)
{
  if (routines[IO_READ] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_read_type read_routine = routines[IO_READ];

  return read_routine (io_object, data, dataCnt, offset, amount);
}

/* io seek */

typedef kern_return_t (*io_seek_type) (io_t, off_t, int, off_t *);

kern_return_t
lisp_io_seek (io_t io_object, loff_t offset, int whence, loff_t * newp)
{
  if (routines[IO_SEEK] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_seek_type seek_routine = routines[IO_SEEK];

  return seek_routine (io_object, offset, whence, (off_t *)newp);
}

/* io readable */

typedef kern_return_t (*io_readable_type) (io_t, vm_size_t *);

kern_return_t
lisp_io_readable (io_t io_object, vm_size_t * amount)
{
  if (routines[IO_READABLE] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_readable_type readable_routine = routines[IO_READABLE];

  return readable_routine (io_object, amount);
}

/* io set all openmodes */

typedef kern_return_t (*io_set_all_openmodes_type) (io_t, int);

kern_return_t
lisp_io_set_all_openmodes (io_t io_object, int newbits)
{
  if (routines[IO_SET_ALL_OPENMODES] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_set_all_openmodes_type set_all_openmodes_routine =
    routines[IO_SET_ALL_OPENMODES];

  return set_all_openmodes_routine (io_object, newbits);
}

/* io get openmodes */

typedef kern_return_t (*io_get_openmodes_type) (io_t, int *);

kern_return_t
lisp_io_get_openmodes (io_t io_object, int *bits)
{
  if (routines[IO_GET_OPENMODES] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_get_openmodes_type get_openmodes_routine = routines[IO_GET_OPENMODES];

  return get_openmodes_routine (io_object, bits);
}

/* io set some openmodes */

typedef kern_return_t (*io_set_some_openmodes_type) (io_t, int);

kern_return_t
lisp_io_set_some_openmodes (io_t io_object, int bits_to_set)
{
  if (routines[IO_SET_SOME_OPENMODES] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_set_some_openmodes_type set_some_openmodes_routine =
    routines[IO_SET_SOME_OPENMODES];

  return set_some_openmodes_routine (io_object, bits_to_set);
}

/* io clear some openmodes */

typedef kern_return_t (*io_clear_some_openmodes_type) (io_t, int);

kern_return_t
lisp_io_clear_some_openmodes (io_t io_object, int bits_to_clear)
{
  if (routines[IO_CLEAR_SOME_OPENMODES] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_clear_some_openmodes_type clear_some_openmodes_routine =
    routines[IO_CLEAR_SOME_OPENMODES];

  return clear_some_openmodes_routine (io_object, bits_to_clear);
}

/* io async */

typedef kern_return_t (*io_async_type) (io_t, mach_port_t,
					mach_port_t *,
					mach_msg_type_name_t *);

kern_return_t
lisp_io_async (io_t io_object,
	       mach_port_t notify_port,
	       mach_port_t * async_id_port,
	       mach_msg_type_name_t * async_id_portPoly)
{
  if (routines[IO_ASYNC] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_async_type async_routine = routines[IO_ASYNC];

  return async_routine (io_object, notify_port,
			async_id_port, async_id_portPoly);
}

/* io mod owner */

typedef kern_return_t (*io_mod_owner_type) (io_t, pid_t);

kern_return_t
lisp_io_mod_owner (io_t io_object, pid_t owner)
{
  if (routines[IO_MOD_OWNER] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_mod_owner_type mod_owner_routine = routines[IO_MOD_OWNER];

  return mod_owner_routine (io_object, owner);
}

/* io get owner */

typedef kern_return_t (*io_get_owner_type) (io_t, pid_t *);

kern_return_t
lisp_io_get_owner (io_t io_object, pid_t * owner)
{
  if (routines[IO_GET_OWNER] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_get_owner_type get_owner_routine = routines[IO_GET_OWNER];

  return get_owner_routine (io_object, owner);
}

/* io get icky async id */

typedef kern_return_t (*io_get_icky_async_id) (io_t,
					       mach_port_t *,
					       mach_msg_type_name_t *);

kern_return_t
lisp_io_get_icky_async_id (io_t io_object,
			   mach_port_t * icky_async_id_port,
			   mach_msg_type_name_t * icky_async_id_portPoly)
{
  if (routines[IO_GET_ICKY_ASYNC_ID] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_get_icky_async_id get_icky_async_id_routine =
    routines[IO_GET_ICKY_ASYNC_ID];

  return get_icky_async_id_routine (io_object,
				    icky_async_id_port,
				    icky_async_id_portPoly);
}

/* io select */

typedef kern_return_t (*io_select_type) (io_t, int *);

kern_return_t
lisp_io_select (io_t io_object, int *select_type)
{
  if (routines[IO_SELECT] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_select_type select_routine = routines[IO_SELECT];

  return select_routine (io_object, select_type);
}

/* io stat */

typedef kern_return_t (*io_stat_type) (io_t, io_statbuf_t *);

kern_return_t
lisp_io_stat (io_t stat_object, io_statbuf_t * stat_info)
{
  if (routines[IO_STAT] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_stat_type stat_routine = routines[IO_STAT];

  return stat_routine (stat_object, stat_info);
}

/* io reauthenticate */

typedef kern_return_t (*io_reauthenticate_type) (io_t, mach_port_t);

kern_return_t
lisp_io_reauthenticate (io_t auth_object, mach_port_t rendezvous2)
{
  if (routines[IO_REAUTHENTICATE] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_reauthenticate_type reauthenticate_routine = routines[IO_REAUTHENTICATE];

  return reauthenticate_routine (auth_object, rendezvous2);
}

/* io restrict auth */

typedef kern_return_t (*io_restrict_auth_type) (io_t,
						mach_port_t *,
						mach_msg_type_name_t *,
						idarray_t,
						mach_msg_type_number_t,
						idarray_t,
						mach_msg_type_number_t);

kern_return_t
lisp_io_restrict_auth (io_t io_object,
		       mach_port_t * new_object,
		       mach_msg_type_name_t * new_objectPoly,
		       idarray_t uids,
		       mach_msg_type_number_t uidsCnt,
		       idarray_t gids, mach_msg_type_number_t gidsCnt)
{
  if (routines[IO_RESTRICT_AUTH] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_restrict_auth_type restrict_auth_routine = routines[IO_RESTRICT_AUTH];

  return restrict_auth_routine (io_object,
				new_object, new_objectPoly,
				uids, uidsCnt, gids, gidsCnt);
}

/* io duplicate */

typedef kern_return_t (*io_duplicate_type) (io_t,
					    mach_port_t *,
					    mach_msg_type_name_t *);

kern_return_t
lisp_io_duplicate (io_t io_object,
		   mach_port_t * newport, mach_msg_type_name_t * newportPoly)
{
  if (routines[IO_DUPLICATE] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_duplicate_type duplicate_routine = routines[IO_DUPLICATE];

  return duplicate_routine (io_object, newport, newportPoly);
}

/* io server version */

typedef kern_return_t (*io_server_version_type) (io_t,
						 string_t, int *, int *,
						 int *);

kern_return_t
lisp_io_server_version (io_t vers_object,
			string_t server_name,
			int *server_major_version,
			int *server_minor_version, int *server_edit_level)
{
  if (routines[IO_SERVER_VERSION] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_server_version_type server_version_routine = routines[IO_SERVER_VERSION];

  return server_version_routine (vers_object, server_name,
				 server_major_version, server_minor_version,
				 server_edit_level);
}

/* io map */

typedef kern_return_t (*io_map_type) (io_t,
				      mach_port_t *,
				      mach_msg_type_name_t *,
				      mach_port_t *, mach_msg_type_name_t *);

kern_return_t
lisp_io_map (io_t io_object,
	     mach_port_t * memobjrd,
	     mach_msg_type_name_t * memobjrdPoly,
	     mach_port_t * memobjwt, mach_msg_type_name_t * memobjwtPoly)
{
  if (routines[IO_MAP] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_map_type map_routine = routines[IO_MAP];

  return map_routine (io_object, memobjrd,
		      memobjrdPoly, memobjwt, memobjwtPoly);
}

/* io map cntl */

typedef kern_return_t (*io_map_cntl_type) (io_t,
					   mach_port_t *,
					   mach_msg_type_name_t *);

kern_return_t
lisp_io_map_cntl (io_t io_object,
		  mach_port_t * memobj, mach_msg_type_name_t * memobjPoly)
{
  if (routines[IO_MAP_CNTL] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_map_cntl_type map_cntl_routine = routines[IO_MAP_CNTL];

  return map_cntl_routine (io_object, memobj, memobjPoly);
}

/* io get conch */

typedef kern_return_t (*io_get_conch_type) (io_t);

kern_return_t
lisp_io_get_conch (io_t io_object)
{
  if (routines[IO_GET_CONCH] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_get_conch_type get_conch_routine = routines[IO_GET_CONCH];

  return get_conch_routine (io_object);
}

/* io release conch */

typedef kern_return_t (*io_release_conch_type) (io_t);

kern_return_t
lisp_io_release_conch (io_t io_object)
{
  if (routines[IO_RELEASE_CONCH] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_release_conch_type release_conch_routine = routines[IO_RELEASE_CONCH];

  return release_conch_routine (io_object);
}

/* io eofnotify */

typedef kern_return_t (*io_eofnotify_type) (io_t);

kern_return_t
lisp_io_eofnotify (io_t io_object)
{
  if (routines[IO_EOFNOTIFY] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_eofnotify_type eofnotify_routine = routines[IO_EOFNOTIFY];

  return eofnotify_routine (io_object);
}

/* io prenotify */

typedef kern_return_t (*io_prenotify_type) (io_t, vm_offset_t, vm_offset_t);

kern_return_t
lisp_io_prenotify (io_t io_object,
		   vm_offset_t write_start, vm_offset_t write_end)
{
  if (routines[IO_PRENOTIFY] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_prenotify_type prenotify_routine = routines[IO_PRENOTIFY];

  return prenotify_routine (io_object, write_start, write_end);
}

/* io postnotify */

typedef kern_return_t (*io_postnotify_type) (io_t, vm_offset_t, vm_offset_t);

kern_return_t
lisp_io_postnotify (io_t io_object,
		    vm_offset_t write_start, vm_offset_t write_end)
{
  if (routines[IO_POSTNOTIFY] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_postnotify_type postnotify_routine = routines[IO_POSTNOTIFY];

  return postnotify_routine (io_object, write_start, write_end);
}

/* io readnotify */

typedef kern_return_t (*io_readnotify_type) (io_t);

kern_return_t
lisp_io_readnotify (io_t io_object)
{
  if (routines[IO_READNOTIFY] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_readnotify_type readnotify_routine = routines[IO_READNOTIFY];

  return readnotify_routine (io_object);
}

/* io readsleep */

typedef kern_return_t (*io_readsleep_type) (io_t);

kern_return_t
lisp_io_readsleep (io_t io_object)
{
  if (routines[IO_READSLEEP] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_readsleep_type readsleep_routine = routines[IO_READSLEEP];

  return readsleep_routine (io_object);
}

/* io sigio */

typedef kern_return_t (*io_sigio_type) (io_t);

kern_return_t
lisp_io_sigio (io_t io_object)
{
  if (routines[IO_SIGIO] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_sigio_type sigio_routine = routines[IO_SIGIO];

  return sigio_routine (io_object);
}

/* io pathconf */

typedef kern_return_t (*io_pathconf_type) (io_t, int, int *);

kern_return_t
lisp_io_pathconf (io_t io_object, int name, int *value)
{
  if (routines[IO_PATHCONF] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_pathconf_type pathconf_routine = routines[IO_PATHCONF];

  return pathconf_routine (io_object, name, value);
}

/* io identity */

typedef kern_return_t (*io_identity_type) (io_t,
					   mach_port_t *,
					   mach_msg_type_name_t *,
					   mach_port_t *,
					   mach_msg_type_name_t *, ino64_t *);

kern_return_t
lisp_io_identity (io_t io_object,
		  mach_port_t * idport,
		  mach_msg_type_name_t * idportPoly,
		  mach_port_t * fsidport,
		  mach_msg_type_name_t * fsidportPoly, ino64_t * fileno)
{
  if (routines[IO_IDENTITY] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_identity_type identity_routine = routines[IO_IDENTITY];

  return identity_routine (io_object, idport,
			   idportPoly, fsidport, fsidportPoly, fileno);
}

/* io revoke */

typedef kern_return_t (*io_revoke_type) (io_t);

kern_return_t
lisp_io_revoke (io_t io_object)
{
  if (routines[IO_REVOKE] == NULL)
    {
      return EOPNOTSUPP;
    }

  io_revoke_type revoke_routine = routines[IO_REVOKE];

  return revoke_routine (io_object);
}

static const char *
routine_to_str (const IoRoutine rot)
{
#define RET(val) case val: return #val ;
  switch (rot)
    {
    RET (IO_WRITE) RET (IO_READ) RET (IO_SEEK) RET (IO_READABLE) RET (IO_SET_SOME_OPENMODES) RET (IO_SET_ALL_OPENMODES) RET (IO_GET_OPENMODES) RET (IO_CLEAR_SOME_OPENMODES) RET (IO_ASYNC) RET (IO_MOD_OWNER) RET (IO_GET_OWNER) RET (IO_GET_ICKY_ASYNC_ID) RET (IO_SELECT) RET (IO_STAT) RET (IO_REAUTHENTICATE) RET (IO_RESTRICT_AUTH) RET (IO_DUPLICATE) RET (IO_SERVER_VERSION) RET (IO_MAP) RET (IO_MAP_CNTL) RET (IO_GET_CONCH) RET (IO_RELEASE_CONCH) RET (IO_EOFNOTIFY) RET (IO_PRENOTIFY) RET (IO_POSTNOTIFY) RET (IO_READNOTIFY) RET (IO_READSLEEP) RET (IO_SIGIO) RET (IO_PATHCONF) RET (IO_IDENTITY) RET (IO_REVOKE) case _NUMBER_OF_ROUTINES:
    default:
      return "";
    }

#undef RET
}

#include "common.c"

COMMON_FUNCTIONS (io);
