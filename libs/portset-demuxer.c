
#include <mach.h>
#include <mach/notify.h>
#include <mach/mig_errors.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <stdio.h>

typedef int (*demuxer_fun_type)(mach_port_t,
		mach_msg_header_t *,
		mach_msg_header_t *);

static demuxer_fun_type demuxer = NULL;

int
portset_demuxer(mach_msg_header_t *inp,
		mach_msg_header_t *outheadp)
{
	assert(demuxer != NULL);

	register mig_reply_header_t *outp = (mig_reply_header_t *) outheadp;
	static const mach_msg_type_t RetCodeType = {
		/* msgt_name = */	MACH_MSG_TYPE_INTEGER_32,
		/* msgt_size = */	32,
		/* msgt_number = */	1,
		/* msgt_inline = */	TRUE,
		/* msgt_longform = */	FALSE,
		/* msgt_deallocate = */	FALSE,
		/* msgt_unused = */	0
	};

	/* Fill in default response */
	outp->Head.msgh_bits
		= MACH_MSGH_BITS(MACH_MSGH_BITS_REMOTE(inp->msgh_bits), 0);
	outp->Head.msgh_size = sizeof(*outp);
	outp->Head.msgh_remote_port = inp->msgh_remote_port;
	outp->Head.msgh_local_port = MACH_PORT_NULL;
	outp->Head.msgh_seqno = 0;
	outp->Head.msgh_id = inp->msgh_id + 100;
	outp->RetCodeType = RetCodeType;
	outp->RetCode = MIG_BAD_ID;

	//fprintf(stderr, "=====Got message=====!\n");

	int ret = demuxer(inp->msgh_local_port, inp, outheadp);

	//fprintf(stderr, "=====End Message!=====\n");

	if(ret == EOPNOTSUPP) {
		outp->RetCode = EOPNOTSUPP;
		return(1);
	} else {
		//fprintf(stderr, "ret demuxer: %d\n", ret);
		return(ret);
	}
}

void
set_demuxer(demuxer_fun_type fun)
{
	assert(fun != NULL);

	demuxer = fun;
}
