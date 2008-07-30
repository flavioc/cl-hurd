
#include <hurd/fs.h>
#include <mach/time_value.h>

/* Wrap file_utimes to pass pointers as arguments. */
kern_return_t
helper_file_utimes (file_t utimes_file,
		    time_value_t * new_atime, time_value_t * new_mtime)
{
  return (file_utimes (utimes_file, *new_atime, *new_mtime));
}
