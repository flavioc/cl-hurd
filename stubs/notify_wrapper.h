
#ifndef NOTIFY_WRAPPER_H
#define NOTIFY_WRAPPER_H

/* module that enables lisp to change what
 * routines will be run on certain mach port
 * events. this header is specially made
 * to be used with swig
 */

/* routines that can be set by the middle level */
typedef enum {
	DO_MACH_NOTIFY_PORT_DELETED,
	DO_MACH_NOTIFY_MSG_ACCEPTED,
	DO_MACH_NOTIFY_PORT_DESTROYED,
	DO_MACH_NOTIFY_NO_SENDERS,
	DO_MACH_NOTIFY_SEND_ONCE,
	DO_MACH_NOTIFY_DEAD_NAME,
	_NUMBER_OF_ROUTINES
} NotifyRoutine;

/* we could make a function for every handler
 * but that would get kinda tedious
 * just use a generic pointer
 * that will be cast when needed
 */
void set_notify_routine(const NotifyRoutine what, void *fun);

#endif
