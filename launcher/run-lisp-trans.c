
/* Run a clisp process with stdin and stdout open.

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

#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <paths.h>
#include <errno.h>
#include <stdio.h>
#include <error.h>

/* lisp interpreter absolute location */
static char *LISP_PATH = "/usr/bin/clisp";

static int
fd_is_open (const int fd)
{
  static struct stat buf;

  return (fstat (fd, &buf) == 0);
}

static void
run_lisp (int argc, char **argv)
{
  int stdin_handle, stdout_handle;
  char *new_argv[argc + 1];
  int i;

  if (!fd_is_open (STDIN_FILENO))
    {
      stdin_handle = open (_PATH_DEVNULL, O_RDONLY);

      if (stdin_handle == -1)
	{
	  error (EXIT_FAILURE, errno, "Could not open file %s",
		 _PATH_DEVNULL);
	}
    }

  if (!fd_is_open (STDOUT_FILENO))
    {
      stdout_handle = open (_PATH_DEVNULL, O_WRONLY);

      if (stdout_handle == -1)
	{
	  error (EXIT_FAILURE, errno, "Could not open file %s",
		 _PATH_DEVNULL);
	}
    }

  new_argv[0] = LISP_PATH;

  for (i = 1; i <= argc; ++i)
    {
      new_argv[i] = argv[i];
    }

  execvp (new_argv[0], new_argv);

  /* we should not get here ... */
  error (EXIT_FAILURE, errno, "Could not launch lisp %s", LISP_PATH);
}

int
main (int argc, char **argv)
{
  run_lisp (argc, argv);

  return EXIT_SUCCESS;
}
