
#include <hurd.h>
#include <mach.h>
#include <hurd/auth.h>
#include <hurd/io.h>
#include <hurd/process.h>
#include <stdio.h>

pid_t
helper_exec_reauth (auth_t newauth, int secure,
		    mach_port_t * ports, unsigned num_ports,
		    mach_port_t * fds, unsigned num_fds,
		    int is_empty, uid_t uid)
{
  pid_t pid = fork ();

  if (pid == 0)
    {
      pid_t parent = getppid ();
      task_t parent_task = pid2task (parent);
      unsigned int i;
      mach_msg_type_name_t foo;
      mach_port_t new_ports[num_ports];
      mach_port_t new_fds[num_fds];
      mach_port_t newauth_new;

      for (i = 0; i < num_ports; ++i)
	{
	  if (ports[i] == MACH_PORT_NULL)
	    continue;
	  mach_port_extract_right (parent_task, ports[i],
				   MACH_MSG_TYPE_MOVE_SEND, &new_ports[i],
				   &foo);
	}

      for (i = 0; i < num_fds; ++i)
	{
	  if (fds[i] == MACH_PORT_NULL)
	    continue;
	  mach_port_extract_right (parent_task, fds[i],
				   MACH_MSG_TYPE_MOVE_SEND, &new_fds[i],
				   &foo);
	}

      mach_port_extract_right (parent_task, newauth, MACH_MSG_TYPE_MOVE_SEND,
			       &newauth_new, &foo);

      error_t err =
	exec_reauth (newauth_new, secure, 0, new_ports, num_ports, new_fds,
		     num_fds);

      fprintf (stderr, "exec_reauth: %s\n", strerror (err));

      proc_setowner (new_ports[INIT_PORT_PROC], uid, is_empty);

      mach_port_insert_right (parent_task, newauth, newauth_new,
			      MACH_MSG_TYPE_MOVE_SEND);

      for (i = 0; i < num_ports; ++i)
	{
	  if (ports[i] == MACH_PORT_NULL)
	    continue;
	  fprintf (stderr, "%s\n",
		   strerror (mach_port_insert_right
			     (parent_task, new_ports[i], ports[i],
			      MACH_MSG_TYPE_MOVE_SEND)));
	}

      for (i = 0; i < num_fds; ++i)
	{
	  if (fds[i] == MACH_PORT_NULL)
	    continue;
	  fprintf (stderr, "%s\n",
		   strerror (mach_port_insert_right
			     (parent_task, new_fds[i], fds[i],
			      MACH_MSG_TYPE_MOVE_SEND)));
	}

      fprintf (stderr, "EXITING...\n");
      exit (err);
    }

  return (pid);
}

int
exec_reauth_finished (pid_t pid)
{
  int status;
  int ret = waitpid (pid, &status, WNOHANG);

  if (ret > 0)
    {
      status = WEXITSTATUS (status);
    }

  return (ret > 0);
}
