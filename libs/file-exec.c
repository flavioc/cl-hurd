
#include <stdlib.h>
#include <stdio.h>
#include <hurd.h>
#include <hurd/exec.h>

pid_t
do_exec_exec(file_t execserver0,
    mach_port_t file0,
    mach_msg_type_name_t filePoly0,
    mach_port_t oldtask0,
    int flags0,
    data_t argv0,
    mach_msg_type_number_t argvCnt0,
    data_t envp0,
    mach_msg_type_number_t envpCnt0,
    portarray_t dtable0,
    mach_msg_type_name_t dtablePoly0,
    mach_msg_type_number_t dtableCnt0,
    portarray_t portarray0,
    mach_msg_type_name_t portarrayPoly0,
    mach_msg_type_number_t portarrayCnt0,
    intarray_t intarray0,
    mach_msg_type_number_t intarrayCnt0,
    mach_port_array_t deallocnames0,
    mach_msg_type_number_t deallocnamesCnt0,
    mach_port_array_t destroynames0,
    mach_msg_type_number_t destroynamesCnt0)
{
  pid_t pid = fork();
  unsigned i;

  if(pid == 0) { // child
    error_t err;
    pid_t parent = getppid();
    task_t parent_task = pid2task(parent);
    mach_msg_type_name_t foo;
    mach_port_t new_execserver;
    mach_port_t file_copy;

    err = mach_port_extract_right(parent_task, execserver0, MACH_MSG_TYPE_COPY_SEND, &new_execserver, &foo);

    if(!err) {
      err = mach_port_extract_right(parent_task, file0, MACH_MSG_TYPE_COPY_SEND, &file_copy, &foo);
    }

    if(!err) {
      err = exec_exec(new_execserver, file_copy, filePoly0,
          oldtask0, flags0, argv0, argvCnt0,
          envp0, envpCnt0, dtable0, dtablePoly0,
          dtableCnt0, portarray0, portarrayPoly0,
          portarrayCnt0, intarray0, intarrayCnt0,
          deallocnames0, deallocnamesCnt0,
          destroynames0, destroynamesCnt0);
    }

    mach_port_deallocate(mach_task_self(), parent_task);
    mach_port_deallocate(mach_task_self(), new_execserver);
    mach_port_deallocate(mach_task_self(), file_copy);
    mach_port_deallocate(mach_task_self(), oldtask0);

    for(i = 0; i < dtableCnt0; ++i) {
      mach_port_deallocate(mach_task_self(), dtable0[i]);
    }

    for(i = 0; i < portarrayCnt0; ++i) {
      mach_port_deallocate(mach_task_self(), portarray0[i]);
    }

    exit(err);
  }
  mach_port_deallocate(mach_task_self(), oldtask0);
  for(i = 0; i < dtableCnt0; ++i) {
    mach_port_deallocate(mach_task_self(), dtable0[i]);
  }

  for(i = 0; i < portarrayCnt0; ++i) {
    mach_port_deallocate(mach_task_self(), portarray0[i]);
  }

  return(pid);
}

int
exec_finished(pid_t pid, int* status)
{
  int ret = waitpid(pid, &status, WNOHANG);

  return(ret > 0);
}
