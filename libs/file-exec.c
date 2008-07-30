
#include <sys/types.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <hurd.h>
#include <hurd/exec.h>
#include <unistd.h>

pid_t
do_exec_exec (file_t execserver,
	      mach_port_t file,
	      mach_msg_type_name_t filePoly,
	      mach_port_t oldtask,
	      int flags,
	      data_t argv,
	      mach_msg_type_number_t argvCnt,
	      data_t envp,
	      mach_msg_type_number_t envpCnt,
	      portarray_t dtable,
	      mach_msg_type_name_t dtablePoly,
	      mach_msg_type_number_t dtableCnt,
	      portarray_t portarray,
	      mach_msg_type_name_t portarrayPoly,
	      mach_msg_type_number_t portarrayCnt,
	      intarray_t intarray,
	      mach_msg_type_number_t intarrayCnt,
	      mach_port_array_t deallocnames,
	      mach_msg_type_number_t deallocnamesCnt,
	      mach_port_array_t destroynames,
	      mach_msg_type_number_t destroynamesCnt)
{
  pid_t pid = fork ();
  unsigned i;

  if (pid == 0)
    {				/* child */
      error_t err = 0;
      pid_t parent = getppid ();
      task_t parent_task = pid2task (parent);
      mach_msg_type_name_t foo;
      mach_port_t new_execserver;
      mach_port_t file_copy;

      err = mach_port_extract_right (parent_task, execserver,
				     MACH_MSG_TYPE_COPY_SEND, &new_execserver,
				     &foo);

      if (!err)
	{
	  err = mach_port_extract_right (parent_task, file,
					 MACH_MSG_TYPE_MOVE_SEND, &file_copy,
					 &foo);
	}

      if (!err)
	{
	  err = exec_exec (new_execserver, file_copy, filePoly,
			   oldtask, flags, argv, argvCnt,
			   envp, envpCnt, dtable, dtablePoly,
			   dtableCnt, portarray, portarrayPoly,
			   portarrayCnt, intarray, intarrayCnt,
			   deallocnames, deallocnamesCnt,
			   destroynames, destroynamesCnt);
	}

      mach_port_deallocate (mach_task_self (), parent_task);
      mach_port_deallocate (mach_task_self (), file_copy);
      mach_port_deallocate (mach_task_self (), oldtask);

      for (i = 0; i < dtableCnt; ++i)
	{
	  mach_port_deallocate (mach_task_self (), dtable[i]);
	}

      for (i = 0; i < portarrayCnt; ++i)
	{
	  mach_port_deallocate (mach_task_self (), portarray[i]);
	}

      exit (err);
    }

  mach_port_deallocate (mach_task_self (), oldtask);

  for (i = 0; i < dtableCnt; ++i)
    {
      mach_port_deallocate (mach_task_self (), dtable[i]);
    }

  for (i = 0; i < portarrayCnt; ++i)
    {
      mach_port_deallocate (mach_task_self (), portarray[i]);
    }

  return (pid);
}

int
exec_finished (pid_t pid, int *status)
{
  int ret = waitpid (pid, status, WNOHANG);

  if (ret > 0)
    {
      *status = WEXITSTATUS (*status);
    }

  return (ret > 0);
}
