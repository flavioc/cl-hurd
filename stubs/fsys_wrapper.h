
#ifndef FSYS_WRAPPER_H
#define FSYS_WRAPPER_H

/* module that enables lisp to change what
 * routines will be run on certain input/output
 * events. this header is specially made
 * to be used with swig
 */

/* routines that can be set by the middle level */
typedef enum {
	FSYS_STARTUP,
	FSYS_GOAWAY,
	FSYS_GETROOT,
	FSYS_GETFILE,
	FSYS_SYNCFS,
	FSYS_SET_OPTIONS,
	FSYS_GETPRIV,
	FSYS_INIT,
	FSYS_FORWARD,
	FSYS_GET_OPTIONS,
	_NUMBER_OF_ROUTINES
} FsysRoutine;

/* we could make a function for every handler
 * but that would get kinda tedious
 * just use a generic pointer
 * that will be cast when needed
 */
void set_fsys_routine(const FsysRoutine what, void *fun);

#endif
