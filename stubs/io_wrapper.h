
#ifndef IO_WRAPPER_H
#define IO_WRAPPER_H

/* module that enables lisp to change what
 * routines will be run on certain input/output
 * events. this header is specially made
 * to be used with swig
 */

/* routines that can be set by the middle level */
typedef enum {
	IO_WRITE,
	IO_READ,
	IO_SEEK,
	IO_READABLE,
	IO_SET_ALL_OPENMODES,
	IO_GET_OPENMODES,
	IO_SET_SOME_OPENMODES,
	IO_CLEAR_SOME_OPENMODES,
	IO_ASYNC,
	IO_MOD_OWNER,
	IO_GET_OWNER,
	IO_GET_ICKY_ASYNC_ID,
	IO_SELECT,
	IO_STAT,
	IO_REAUTHENTICATE,
	IO_RESTRICT_AUTH,
	IO_DUPLICATE,
	IO_SERVER_VERSION,
	IO_MAP,
	IO_MAP_CNTL,
	IO_RELEASE_CONCH,
	IO_EOFNOTIFY,
	IO_PRENOTIFY,
	IO_POSTNOTIFY,
	IO_READSLEEP,
	IO_SIGIO,
	IO_PATHCONF,
	IO_IDENTITY,
	IO_REVOKE,
	_NUMBER_OF_ROUTINES
} IoRoutine;

/* we could make a function for every handler
 * but that would get kinda tedious
 * just use a generic pointer
 * that will be cast when needed
 */
void set_io_routine(const IoRoutine what, void *fun);

#endif
