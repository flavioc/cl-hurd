
#include <unistd.h>
#include <stdlib.h>
#include <argp.h>
#include <fcntl.h>
#include <paths.h>

#include <mach/mach.h>
#include <error.h>

/* general program information */
const char *argp_program_version =
"run-lisp-trans 1.0";
const char *argp_program_bug_address =
"<FIXME@gnu.org>";

/* program documentation */
static char doc[] =
"run-lisp-trans -- startups an Hurd translator written in Common Lisp";

/* a description of the arguments we accept. */
static char args_doc[] = "LISP-FILE";

static struct argp_option options[] = {
	{"stdout", 'o', "FILE", 0, "File attached to standard output", 0},
	{"stdin", 'i', "FILE", 0, "File attached to standard input", 0},
	{ NULL, 0, NULL, 0, NULL, 0 }
};

/* struct where we save args configuration */
struct arguments
{
	char *file;
	char *new_stdout;
	char *new_stdin;
};

/* lisp interpreter absolute location */
static const char* LISP_PATH = "/usr/bin/clisp";

/* arg parser actions */
static error_t
parse_opt(int key, char *arg, struct argp_state *state)
{
	struct arguments *arguments = state->input;

	switch(key) {
		case 'o':
			arguments->new_stdout = arg;
			break;
		case 'i':
			arguments->new_stdin = arg;
			break;
		case ARGP_KEY_ARG:
			arguments->file = arg;
			break;
		case ARGP_KEY_END:
			if(state->arg_num == 0) {
				argp_usage(state);
			}
			break;
		default:
			return ARGP_ERR_UNKNOWN;
	}

	return(0);
}

/* argp parser */
static struct argp argp = { options, parse_opt, args_doc, doc, NULL, NULL, NULL };

/* test if we are running as a translator */
static int
running_as_translator(void)
{
	mach_port_t bootstrap;

	task_get_bootstrap_port(mach_task_self(), &bootstrap);

	const int is_translator = bootstrap != MACH_PORT_NULL;

#ifndef NDEBUG
	fprintf(stderr, "Bootstrap port is %d, so %s\n", bootstrap,
			is_translator ? "we are running as a translator" :
							"we are not running as a translator!");
#endif
	
	return(is_translator);
}

static inline const char*
get_file_path(const char *path)
{
	return(path == NULL ? _PATH_DEVNULL : path);
}

static void
run_translator(struct arguments *arguments)
{
	int stdin_handle, stdout_handle;

	/* because fshelp closes stdin + stdout (only stderr available)
	 * when starting translators and clisp
	 * has deep dependencies on using these file descriptors
	 * we give clisp the illusion that he is writing on the real ones,
	 * opening files for each
	 * (fd's 0 and 1 are free, so 'open' will assign them the right order)
	 */

	/* now, we open the first file for stdin (fd = 0).
	 * when the caller doesn't want to use a file, we open /dev/null
	 */
	stdin_handle = open(get_file_path(arguments->new_stdin), O_RDONLY);

	if(stdin_handle == -1) {
		error(EXIT_FAILURE, errno, "Could not open stdin file %s",
				get_file_path(arguments->new_stdin));
	}

#ifndef NDEBUG
	fprintf(stderr, "Stdin handle attached to fd %d: %s\n", stdin_handle,
			get_file_path(arguments->new_stdin));
#endif

	/* time for stdout! */
	stdout_handle = open(get_file_path(arguments->new_stdout), O_WRONLY | O_APPEND | O_CREAT);

	if(stdout_handle == -1) {
		error(EXIT_FAILURE, errno, "Could not open stdout file %s",
				get_file_path(arguments->new_stdout));
	}

#ifndef NDEBUG
	fprintf(stderr, "Stdout handle attached to fd %d: %s\n", stdout_handle,
			get_file_path(arguments->new_stdout));
#endif

	/* by this moment clisp is ready to run */
	execl(LISP_PATH, LISP_PATH, arguments->file, NULL);

	/* we should not get here ... */
	error(EXIT_FAILURE, errno, "Could not launch lisp %s", LISP_PATH);
}

int
main(int argc, char **argv)
{
	struct arguments arguments;

	arguments.file = NULL;
	arguments.new_stdin = NULL;
	arguments.new_stdout = NULL;

	argp_parse(&argp, argc, argv, 0, 0, &arguments);

	/* first we check that we are really running as a translator.
	 * we don't wanna be opening files and executing
	 * clisp when it's not really needed.
	 */
	if(!running_as_translator()) {
		error(EXIT_FAILURE, 0, "Must be started as a translator");
	}

	run_translator(&arguments);

	return EXIT_SUCCESS;
}
