
/* io.defs wrapper code.

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

#ifndef IO_WRAPPER_H
#define IO_WRAPPER_H

/* module that enables lisp to change what
 * routines will be run on certain input/output
 * events. this header is specially made
 * to be used with swig
 */

/* routines that can be set by the middle level */
typedef enum
{
  IO_WRITE,
  IO_READ,
  IO_SEEK,
  IO_READABLE,
  IO_SET_ALL_OPENMODES,
  IO_GET_OPENMODES,
  IO_SET_SOME_OPENMODES,
  IO_CLEAR_SOME_OPENMODES,
  IO_ASYNC,
  IO_MOD_OWNER,
  IO_GET_OWNER,
  IO_GET_ICKY_ASYNC_ID,
  IO_SELECT,
  IO_STAT,
  IO_REAUTHENTICATE,
  IO_RESTRICT_AUTH,
  IO_DUPLICATE,
  IO_SERVER_VERSION,
  IO_MAP,
  IO_MAP_CNTL,
  IO_GET_CONCH,
  IO_RELEASE_CONCH,
  IO_EOFNOTIFY,
  IO_PRENOTIFY,
  IO_POSTNOTIFY,
  IO_READNOTIFY,
  IO_READSLEEP,
  IO_SIGIO,
  IO_PATHCONF,
  IO_IDENTITY,
  IO_REVOKE,
  _NUMBER_OF_ROUTINES
} IoRoutine;

/* we could make a function for every handler
 * but that would get kinda tedious
 * just use a generic pointer
 * that will be cast when needed
 */
void set_io_routine (const IoRoutine what, void *fun);

#endif
