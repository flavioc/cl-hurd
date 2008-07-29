
#include <stdio.h>
#include <hurd.h>
#include <hurd/exec.h>
#include <cthreads.h>

typedef struct {
  int ended;
  kern_return_t err;
  file_t execserver;
  mach_port_t file;
  mach_msg_type_name_t filePoly;
  mach_port_t oldtask;
  int flags;
  data_t argv;
  mach_msg_type_number_t argvCnt;
  data_t envp;
  mach_msg_type_number_t envpCnt;
  portarray_t dtable;
  mach_msg_type_name_t dtablePoly;
  mach_msg_type_number_t dtableCnt;
  portarray_t portarray;
  mach_msg_type_name_t portarrayPoly;
  mach_msg_type_number_t portarrayCnt;
  intarray_t intarray;
  mach_msg_type_number_t intarrayCnt;
  mach_port_array_t deallocnames;
  mach_msg_type_number_t deallocnamesCnt;
  mach_port_array_t destroynames;
  mach_msg_type_number_t destroynamesCnt;
} exec_data;

any_t
thread_exec(any_t arg)
{
  //exec_data *data = (exec_data*)arg;
  exec_data *data;

fprintf(stderr, "starting exec_exec\n");
  return(NULL);
  /*

  data->err =
    exec_exec(data->execserver, data->file, data->filePoly,
        data->oldtask, data->flags, data->argv, data->argvCnt,
        data->envp, data->envpCnt, data->dtable, data->dtablePoly,
        data->dtableCnt, data->portarray, data->portarrayPoly,
        data->portarrayCnt, data->intarray, data->intarrayCnt,
        data->deallocnames, data->deallocnamesCnt,
        data->destroynames, data->destroynamesCnt);

  fprintf(stderr, "exec_exec done\n");

  data->ended = 1;

  if(!data->err) {

    unsigned int i;

    mach_port_deallocate(mach_task_self(), data->oldtask);

    for(i = 0; i < data->dtableCnt; ++i) {
      mach_port_deallocate(mach_task_self(), data->dtable[i]);
    }

    for(i = 0; i < data->portarrayCnt; ++i) {
      mach_port_deallocate(mach_task_self(), data->portarray[i]);
    }

  }

  fprintf(stderr, "ending thread_exec\n");
  return(NULL);
  */
}

exec_data*
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
  exec_data *data = (exec_data*)malloc(sizeof(exec_data));

  fprintf(stderr, "Data created\n");

  data->ended = 0;
  data->err = 0;
  data->execserver = execserver0;
  data->file = file0;
  data->filePoly = filePoly0;
  data->oldtask = oldtask0;
  data->flags = flags0;
  data->argv = argv0;
  data->argvCnt = argvCnt0;
  data->envp = envp0;
  data->envpCnt = envpCnt0;
  data->dtable = dtable0;
  data->dtablePoly = dtablePoly0;
  data->dtableCnt = dtableCnt0;
  data->portarray = portarray0;
  data->portarrayPoly = portarrayPoly0;
  data->portarrayCnt = portarrayCnt0;
  data->intarray = intarray0;
  data->intarrayCnt = intarrayCnt0;
  data->deallocnames = deallocnames0;
  data->deallocnamesCnt = deallocnamesCnt0;
  data->destroynames = destroynames0;
  data->destroynamesCnt = destroynamesCnt0;

  fprintf(stderr, "Running cthread_fork\n");
  cthread_fork(thread_exec, NULL);
  return(NULL);

  return(data);
}

