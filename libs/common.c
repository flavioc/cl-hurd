
/* Common code for wrappers.

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

inline static void
_set_routine (const unsigned what, void *fun)
{
  assert (what < _NUMBER_OF_ROUTINES);

  if (routines[what] != NULL)
    {
      fprintf (stderr, "Warning: redefining routine %s\n",
	       routine_to_str (what));
    }
  else
    {
      fprintf (stderr, "Information: defining routine %s\n",
	       routine_to_str (what));
    }

  routines[what] = fun;
}

inline static void
_get_module_info (void)
{
  int i;
  for (i = 0; i != _NUMBER_OF_ROUTINES; ++i)
    {
      if (routines[i] != NULL)
	{
	  printf ("Routine #%d (%s): set to address %x\n", i,
		  routine_to_str (i), (vm_size_t) routines[i]);
	}
    }
}

#define COMMON_FUNCTIONS(module) \
	void get_ ## module ## _info(void) { \
		_get_module_info(); \
	} \
	void set_ ## module ## _routine(const unsigned what, void *fun) { \
		_set_routine(what, fun); \
	}

