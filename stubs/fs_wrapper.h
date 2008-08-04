
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
