
/* Pointer-wrap file-utimes.

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

#include <hurd/fs.h>
#include <mach/time_value.h>

/* Wrap file_utimes to pass pointers as arguments. */
kern_return_t
helper_file_utimes (file_t utimes_file,
		    time_value_t * new_atime, time_value_t * new_mtime)
{
  return (file_utimes (utimes_file, *new_atime, *new_mtime));
}
