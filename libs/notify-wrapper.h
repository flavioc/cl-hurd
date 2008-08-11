
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

#ifndef NOTIFY_WRAPPER_H
#define NOTIFY_WRAPPER_H

/* module that enables lisp to change what
 * routines will be run on certain mach port
 * events. this header is specially made
 * to be used with swig
 */

/* routines that can be set by the middle level */
typedef enum
{
  DO_MACH_NOTIFY_PORT_DELETED,
  DO_MACH_NOTIFY_MSG_ACCEPTED,
  DO_MACH_NOTIFY_PORT_DESTROYED,
  DO_MACH_NOTIFY_NO_SENDERS,
  DO_MACH_NOTIFY_SEND_ONCE,
  DO_MACH_NOTIFY_DEAD_NAME,
  _NUMBER_OF_ROUTINES
} NotifyRoutine;

/* we could make a function for every handler
 * but that would get kinda tedious
 * just use a generic pointer
 * that will be cast when needed
 */
void set_notify_routine (const NotifyRoutine what, void *fun);

#endif
