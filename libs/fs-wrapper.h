
/* fs.defs wrapper code.

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

#ifndef FS_WRAPPER_H
#define FS_WRAPPER_H

/* module that enables lisp to change what
 * routines will be run on certain filesystem
 * events. this header is specially made
 * to be used with swig
 */

/* routines that can be set by the middle level */
typedef enum
{
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
  FILE_REPARENT,
  _NUMBER_OF_ROUTINES
} FsRoutine;

/* we could make a function for every handler
 * but that would get kinda tedious
 * just use a generic pointer
 * that will be cast when needed
 */
void set_fs_routine (const FsRoutine what, void *fun);

#endif
