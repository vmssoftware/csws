#pragma module APACHE$DCL_RUN "X-13"
#ifdef _USE_STD_STAT
#undef _USE_STD_STAT
#endif

/*
**
** Copyright (c) 2000 Compaq Computer Corporation
** COMPAQ Registered in U.S. Patent and Trademark Office.
**
** Confidential computer software. Valid license from Compaq or
** authorized sublicensor required for possession, use or copying.
** Consistent with FAR 12.211 and 12.212, Commercial Computer Software,
** Computer Software Documentation, and Technical Data for Commercial
** Items are licensed to the U.S. Government under vendor's standard
** commercial license.
**
*/

/*
**++
**
**  FACILITY:  Apache Web Server
**
**  ABSTRACT:
**
**	This program facilitates the execution of a command procedure or
**	image within the current process by using LIB$DO_COMMAND.
**
**	The command line syntax is:
**
**	    APACHE$DCL_RUN [-a arg-file] [-D] [-d dbg-file] [-r run-file] [-s shell-cmd]
**
**	where:
**
**	    -a arg-file	    specifies an optional alternate argument filename.
**
**	    -D 		    specifies an optional debug indicator.
**
**	    -d dbg-file	    specifies an optional alternate debug filename.
**
**	    -r run-file	    specifies an optional alternate run filename.
**
**	    -s shell-cmd    specifies an optional alternate shell command.
**
**  AUTHOR:  Matthew Doremus			CREATION DATE:  27-Jun-2000
**
**  Modification History:
**
**	X-13	Matthew Doremus				27-Nov-2002
**		Added support for Apache 2.0 and allowed only global section
**		data support.
**
**	X-12	Matthew Doremus				04-Sep-2002
**		Added extended input global section and filenam support.
**
**	X-11	Matthew Doremus				19-Dec-2001
**		Fixed use of ParseFile in determining whether the first argument
**		passed is the image/procedure name being executed.
**
**	X-10	Matthew Doremus				20-Nov-2001
**		Fixed use of decc$to_vms and to handle invalid filec.
**
**	X-9	Matthew Doremus				07-Nov-2001
**		Added optional global section processing options and support.
**
**	X-8	Matthew Doremus				15-Feb-2001
**		Added a listing of the environment file using "APACHE$DCL_ENV -l"
**		when APACHE$SHOW_CGI_SYMBOL was present.
**
**	X-7	Kevin O'Kelley				06-Feb-2001
**		SYS$PARSE is leaving a semicolon in the expanded file
**		specification.  This causes problems for images installed
**		with privileges.  Remove the version string.
**
**	X-6	Matthew Doremus				20-Aug-2000
**		Added shell command support.  The shell command is specified in
**		the APACHE$DCL_CMD environment variable or on the command line
**		using the "-s shell-cmd" option.
**
**	X-5	Matthew Doremus				19-Aug-2000
**		Changed the debug processing of the APACHE$VERIFY_DCL_CGI and
**		APACHE$SHOW_CGI_SYMBOL environment variables so that they are
**		dependant on the APACHE$DEBUG_DCL_CGI environment variable.
**
**	X-4	Matthew Doremus				19-Jul-2000
**		Changed the ParseFile routine to check for another file type when
**		we are parsing a given file name.  This will allow a null type to
**		be found before a type of .COM or .EXE.
**
**	X-3	Matthew Doremus				18-Jul-2000
**		Added FileParse routine to determine whether a script filename
**		has been provided without a file type and if the file type was
**		not present then a check is made to determine whether a file
**		with the file type of .COM or .EXE is available.  If so, then
**		that filename is used in lue of the given script filename.
**
**	X-2	Matthew Doremus				17-Jul-2000
**		Removed the environment variable processing to APACHE$DCL_ENV.C
**		so as preserve the predefined environment variable processing
**		options.
**
**	X-1	Matthew Doremus				27-Jun-2000
**		Initial development
**
**--
**
**  Compile/Link instructions:
**
**	OpenVMS Alpha/VAX:
**	    $ CC APACHE$DCL_RUN+SYS$LIBRARY:SYS$LIB_C/LIBRARY
**	    $ LINK APACHE$DCL_RUN
**
*/


/*
** Define __NEW_STARLET if it's not already defined
*/
#ifndef __NEW_STARLET
#define __NEW_STARLET
#define __NEW_STARLET_SET
#endif

#include <lib$routines.h>
#include <libclidef.h>
#include <processes.h>
#if defined(__ALPHA)
#include <eihddef.h>
#elif defined(__ia64) || defined(__x86_64)
#include <elfdef.h>
#else
#include <ihddef.h>
#endif
#include <starlet.h>
#include <descrip.h>
#include <unixlib.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <ssdef.h>
#include <stat.h>
#include <rms.h>

/*
** Undefine __NEW_STARLET if we had defined it
*/
#ifndef __NEW_STARLET_SET
#undef  __NEW_STARLET_SET
#undef  __NEW_STARLET
#endif

/*
** Option Data Structure
*/
typedef struct _opt_data {
    char *prg_fn;
    char *arg_fn;
    char arg_sw;
    char *run_fn;
    char *run_sc;
    int argc;
    char **argv;
    FILE *dbg_fd;
} OPT_DATA;


static char *vms_fn;

static void ParseCmdLine(int, char *[], OPT_DATA *);
static void ParseArgFile(OPT_DATA *);
static void ExecuteCmd(OPT_DATA *);
static int IsImageFile(OPT_DATA *, char *);
static int ConvertToVms(char *, int);
static int ParseFile(OPT_DATA *, char *);
static void Usage(char *);

extern int apr$dcl_shm_enabled();
extern void *apr$dcl_shm_open();
extern int apr$dcl_shm_read();
extern int apr$dcl_shm_close();

/*
**
**  main - Main processing routine for the APACHE$DCL_RUN utility
**
**  Functional Description:
**
**	This routine controls overall program execution.
**
**  Usage:
**
**      main argc, argv, envp
**
**  Formal parameters:
**
**      argc 		- (IN) argument count
**      argv         	- (IN) address of an argument array
**      envp         	- (IN) address of an environment string
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      None
**
**  Side Effects:
**
**      None
**
*/
int main(int argc, char *argv[], char *envp[])
{
    OPT_DATA OptData;

/*
** Parse the command line
*/
    ParseCmdLine(argc, argv, &OptData);

/*
** Parse the argument file
*/
    ParseArgFile(&OptData);

/*
** Execute the command
*/
    ExecuteCmd(&OptData);

    exit(1);
}


/*
**
**  ParseCmdLine - Parse the command line options
**
**  Functional Description:
**
**      This routine parses the command line options.
**
**  Usage:
**
**      ParseCmdLine argc, argv, OptData
**
**  Formal parameters:
**
**      argc 		- (IN) argument count
**      argv         	- (IN) address of an argument array
**      OptData		- (OUT) address of command option data structure
**			  which will contain the parsed input.
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      None
**
**  Side Effects:
**
**      None
**
*/
static void ParseCmdLine(int argc, char *argv[], OPT_DATA * OptData) {
    char *dbg_fn = NULL;
    int dbg_sw = FALSE;
    int option, i;

/*
** Initialize the option data
*/
    OptData->prg_fn = NULL;
    OptData->arg_fn = NULL;
    OptData->arg_sw = FALSE;
    OptData->run_fn = NULL;
    OptData->run_sc = NULL;
    OptData->argc = 0;
    OptData->argv = NULL;
    OptData->dbg_fd = NULL;

/*
** Get the program name
*/
    if (argc)
	OptData->prg_fn = strdup(argv[0]);

/*
** Process the command line options
*/
    while ((option = getopt(argc, argv, "a:Dd:r:s:?")) != EOF) {
	switch (option) {
	    /*
	     ** Alternate argument file ?
	     */
	case 'a':
	    OptData->arg_fn = strdup(optarg);
	    for (i = 0; i < strlen(OptData->arg_fn); i++)
		OptData->arg_fn[i] = toupper(OptData->arg_fn[i]);
	    OptData->arg_sw = TRUE;
	    break;

	    /*
	     ** Optional debug indicator ?
	     */
	case 'D':
	    dbg_sw = TRUE;
	    break;

	    /*
	     ** Alternate debug file ?
	     */
	case 'd':
	    dbg_fn = strdup(optarg);
	    break;

	    /*
	     ** Alternate run file ?
	     */
	case 'r':
	    OptData->run_fn = strdup(optarg);
	    break;

	    /*
	     ** Alternate run command line ?
	     */
	case 's':
	    OptData->run_sc = strdup(optarg);
	    break;

	    /*
	     ** Invalid argument ?
	     */
	case '?':
	default:
	    Usage(OptData->prg_fn);
	    exit(1);
	    break;
	}
    }

/*
** Are the number of parameters correct ?
*/
    if (argc - optind != 0) {
	Usage(OptData->prg_fn);
	exit(1);
    }

/*
** Determine whether we should create a debug file
*/
    if (getenv("APACHE$DCL_DEBUG") || dbg_sw || dbg_fn) {
	if (!dbg_fn) {
	    dbg_fn =
		malloc(strlen("SYS$SCRATCH:APACHE$DCL_RUN_%08X.DBG") + 8);
	    if (!dbg_fn)
		fprintf(stderr,
			"Error allocating memory for debug file name\n");
	    else
		sprintf(dbg_fn, "SYS$SCRATCH:APACHE$DCL_RUN_%08X.DBG",
			getpid());
	}

	if (dbg_fn) {
	    OptData->dbg_fd = fopen(dbg_fn, "wb");
	    if (!OptData->dbg_fd)
		fprintf(stderr,
			"Error (%d, %08X) Opening debug file (%s)\n",
			errno, vaxc$errno, dbg_fn);
	    free(dbg_fn);
	}
    }

/*
** Allocate the argument filename if we didn't get one from the command line
*/
    if (!OptData->arg_fn) {
	char *dcl_proc_ctx = getenv("APACHE$DCL_PROC_CTX");

	OptData->arg_fn = malloc(strlen("APR$SHM_DCL_ARG_%08X%s") + 8 +
				 (dcl_proc_ctx ? strlen(dcl_proc_ctx) :
				  0));
	if (!OptData->arg_fn) {
	    fprintf(stderr,
		    "Error allocating memory for argument section name\n");
	    exit(1);
	}
	sprintf(OptData->arg_fn, "APR$SHM_DCL_ARG_%08X%s",
		(getppid()? getppid() : getpid()),
		(dcl_proc_ctx ? dcl_proc_ctx : ""));
    }

/*
** Check if we have a run file to provide (if necessary).
*/
    if (!OptData->run_fn)
	OptData->run_fn = getenv("SCRIPT_FILENAME");
    if (!OptData->run_fn)
	OptData->run_fn = getenv("WWW_SCRIPT_FILENAME");

/*
** Check if we have a shell command to provide (if necessary).
*/
    if (!OptData->run_sc)
	OptData->run_sc = getenv("APACHE$DCL_CMD");
    if (!OptData->run_sc)
	OptData->run_sc = getenv("WWW_APACHE$DCL_CMD");

}


/*
**
**  ParseArgFile - Parse the argument file
**
**  Functional Description:
**
**      This routine parses the argument file.
**
**  Usage:
**
**      ParseArgFile OptData
**
**  Formal parameters:
**
**      OptData		- (IN/OUT) address of command option data structure
**			  which will contain the parsed input.
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      None
**
**  Side Effects:
**
**      None
**
*/
static void ParseArgFile(OPT_DATA * OptData)
{
    struct _arg_node {
	void *flink;
	char *data;
    } *ArgList, *ArgRoot = NULL;
    int arg_buf_max = 1024;
    char *arg_buf = NULL;
    int arg_len = 0;
    FILE *arg_fd;
    void *arg_gs = NULL;
    int i;

/*
** Open the argument global section
*/
    arg_gs = apr$dcl_shm_open(OptData->arg_fn);
    if (!arg_gs) {
	if (OptData->arg_sw || vaxc$errno != SS$_NOSUCHSEC)
	    fprintf(stderr,
		    "Error (%08X) Opening argument global section (%s)\n",
		    vaxc$errno, OptData->arg_fn);
	if (OptData->dbg_fd)
	    fprintf(OptData->dbg_fd,
		    "Error (%d, %08X) Opening argument global section (%s)\n",
		    errno, vaxc$errno, OptData->arg_fn);
	free(OptData->arg_fn);
	return;
    }

/*
** Free the argument filename buffer
*/
    free(OptData->arg_fn);
    OptData->arg_fn = NULL;

/*
** Allocate the argument file read buffer
*/
    arg_buf = malloc(arg_buf_max + 1);
    if (!arg_buf) {
	fprintf(stderr,
		"Error allocating argument file read buffer (%d bytes)\n",
		arg_buf_max + 1);
	if (OptData->dbg_fd) {
	    fprintf(OptData->dbg_fd,
		    "Error allocating argument file read buffer (%d bytes)\n",
		    arg_buf_max + 1);
	    fclose(OptData->dbg_fd);
	}
	apr$dcl_shm_close(arg_gs);
	exit(1);
    }

/*
** Read the argument data
*/
    while (1) {
	/*
	 ** Read the environment global section
	 */
	if (apr$dcl_shm_read(&arg_len, sizeof(arg_len), 1, arg_gs) == 0)
	    break;
	if (arg_len < 0)
	    break;

	/*
	 ** Determine whether the argument read buffer is large enough
	 */
	if (arg_len > arg_buf_max) {
	    free(arg_buf);
	    arg_buf = malloc(arg_len + 1);
	    if (!arg_buf) {
		fprintf(stderr,
			"Error allocating argument file read buffer (%d bytes)\n",
			arg_len + 1);
		if (OptData->dbg_fd) {
		    fprintf(OptData->dbg_fd,
			    "Error allocating argument file read buffer (%d bytes)\n",
			    arg_len + 1);
		    fclose(OptData->dbg_fd);
		}
		apr$dcl_shm_close(arg_gs);
		exit(1);
	    }
	    arg_buf_max = arg_len;
	};

	/*
	 ** Read the argument global section into our buffer
	 */
	if (apr$dcl_shm_read(arg_buf, sizeof(char), arg_len, arg_gs) !=
	    arg_len) {
	    fprintf(stderr,
		    "Error: end of argument global section reached\n");
	    if (OptData->dbg_fd) {
		fprintf(OptData->dbg_fd,
			"Error: end of argument global section reached\n");
		fclose(OptData->dbg_fd);
	    }
	    apr$dcl_shm_close(arg_gs);
	    free(arg_buf);
	    exit(1);
	}

	/*
	 ** Null terminate the argument record
	 */
	arg_buf[arg_len] = '\0';

	/*
	 ** If the environment variable is commented out, then just continue
	 */
	if (strncmp(arg_buf, "#", 1) == 0)
	    continue;

	/*
	 ** Load the argument link list
	 */
	if (!ArgRoot) {
	    ArgRoot = ArgList = malloc(sizeof(*ArgList));
	    if (!ArgRoot) {
		fprintf(stderr,
			"Error allocating argument list node (%d bytes)\n",
			sizeof(*ArgList));
		if (OptData->dbg_fd) {
		    fprintf(OptData->dbg_fd,
			    "Error allocating argument list node (%d bytes)\n",
			    sizeof(*ArgList));
		    fclose(OptData->dbg_fd);
		}
		apr$dcl_shm_close(arg_gs);
		free(arg_buf);
		exit(1);
	    }
	} else {
	    ArgList->flink = malloc(sizeof(*ArgList));
	    if (!ArgList->flink) {
		fprintf(stderr,
			"Error allocating argument list node (%d bytes)\n",
			sizeof(*ArgList));
		if (OptData->dbg_fd) {
		    fprintf(OptData->dbg_fd,
			    "Error allocating argument list node (%d bytes)\n",
			    sizeof(*ArgList));
		    fclose(OptData->dbg_fd);
		}
		apr$dcl_shm_close(arg_gs);
		free(arg_buf);
		exit(1);
	    }
	    ArgList = ArgList->flink;
	}
	ArgList->flink = NULL;
	ArgList->data = malloc(arg_len + 1);
	if (!ArgList->data) {
	    fprintf(stderr,
		    "Error allocating argument list data (%d bytes)\n",
		    arg_len + 1);
	    if (OptData->dbg_fd) {
		fprintf(OptData->dbg_fd,
			"Error allocating argument list data (%d bytes)\n",
			arg_len + 1);
		fclose(OptData->dbg_fd);
	    }
	    apr$dcl_shm_close(arg_gs);
	    free(arg_buf);
	    exit(1);
	}
	strcpy(ArgList->data, arg_buf);

	/*
	 ** Increment the argument count
	 */
	OptData->argc++;
    }

/*
** Close the argument global section
*/
    apr$dcl_shm_close(arg_gs);

/*
** Free the argument file read buffer
*/
    free(arg_buf);

/*
** Load the argument array
*/
    OptData->argv = malloc((OptData->argc) * sizeof(char *));
    if (!OptData->argv) {
	fprintf(stderr, "Error allocating argument array (%d bytes)\n",
		(OptData->argc) * sizeof(char *));
	if (OptData->dbg_fd) {
	    fprintf(OptData->dbg_fd,
		    "Error allocating argument list array (%d bytes)\n",
		    (OptData->argc) * sizeof(char *));
	    fclose(OptData->dbg_fd);
	}
	exit(1);
    }
    for (i = 0; ArgRoot; i++) {
	OptData->argv[i] = ArgRoot->data;
	ArgList = ArgRoot;
	ArgRoot = ArgRoot->flink;
	free(ArgList);
    }

/*
** Check if we have a run file to provide (if necessary).
*/
    if (!OptData->run_fn) {
	if (OptData->argc)
	    OptData->run_fn = strdup(OptData->argv[0]);
    }

}


/*
**
**  Usage - Display the acceptable unix style command usage
**
**  Functional Description:
**
**      This routine displays to standard output the appropriate unix style
**	command usage.
**
**  Usage:
**
**      Usage prog_name
**
**  Formal parameters:
**
**      prog_name	- (IN) address of the program name string
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      None
**
**  Side Effects:
**
**      None
**
*/
static void Usage(char *prog_name)
{
    fprintf(stdout,
	    "Usage: %s [-a arg-file] [-D] [-d dbg-file] [-r run-file] [-s shell-cmd]\n",
	    prog_name);
}


/*
**
**  ExecuteCmd - Execute the request image or procedure
**
**  Functional Description:
**
**      This routine executes the requested image or procedure file.
**
**  Usage:
**
**      ExecuteCmd OptData
**
**  Formal parameters:
**
**      OptData		- (IN) address of command option data structure
**			  which will contain the parsed input.
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      None
**
**  Side Effects:
**
**      None
**
*/
static void ExecuteCmd(OPT_DATA * OptData)
{
    struct dsc$descriptor_s cmd_run_desc =
	{ 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    struct dsc$descriptor_s sym_nam_desc =
	{ 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    struct dsc$descriptor_s sym_val_desc =
	{ 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    char *cmd_nam_buf = "APACHE$DCL_CMD";
    char cmd_val_buf[255 + 1];
    char cmd_run_buf[255 + 1];
    char *tmp_fn, *ptr;
    int arg_idx = 0, status, i;

/*
** Determine whether we have anything to run
*/
    if (!OptData->run_fn && !OptData->run_sc) {
	fprintf(stderr, "Error: No shell command or filename to run !\n");
	if (OptData->dbg_fd) {
	    fprintf(OptData->dbg_fd,
		    "Error: No shell command or filename to run !\n");
	    fclose(OptData->dbg_fd);
	}
	exit(1);
    }

/*
** Setup the run of the shell command or the image/procedure file
*/
    if (OptData->run_sc) {
	/*
	 ** Trim the shell command
	 */
	for (ptr = OptData->run_sc; *ptr == ' '; ptr++);
	strcpy(OptData->run_sc, ptr);
	for (ptr = OptData->run_sc + strlen(OptData->run_sc) - 1;
	     *ptr == ' '; ptr--);
	ptr++;
	*ptr = '\0';

	/*
	 ** Is the trimmed shell command greater than 255 bytes long ?
	 */
	if (strlen(OptData->run_sc) > 255) {
	    fprintf(stderr, "Error: Shell command to long !\n");
	    if (OptData->dbg_fd) {
		fprintf(OptData->dbg_fd,
			"Error: Shell command to long !\n");
		fclose(OptData->dbg_fd);
	    }
	    exit(1);
	}

	/*
	 ** Create the do command descriptor
	 */
	cmd_run_desc.dsc$w_length = strlen(OptData->run_sc);
	cmd_run_desc.dsc$a_pointer = OptData->run_sc;

	/*
	 ** If we're debugging, then lets show the shell command
	 */
	if (OptData->dbg_fd) {
	    fprintf(OptData->dbg_fd,
		    "\nRunning the following shell command:\n");
	    fprintf(OptData->dbg_fd, "%s\n", cmd_run_desc.dsc$a_pointer);
	}
    } else {
	/*
	 ** Convert the filename to VMS
	 */
	if (decc$to_vms(OptData->run_fn, ConvertToVms, 0, 0) && vms_fn) {
	    free(OptData->run_fn);
	    OptData->run_fn = strdup(vms_fn);
	}

	/*
	 ** Parse the filename to determine whether the file type was provided
	 ** or one has to be determined.
	 */
	if (ParseFile(OptData, OptData->run_fn)) {
	    free(OptData->run_fn);
	    OptData->run_fn = strdup(vms_fn);
	}

	/*
	 ** Create the foreign command as an image or procedure
	 */
	if (IsImageFile(OptData, OptData->run_fn))
	    sprintf(cmd_val_buf, "$%s", OptData->run_fn);
	else
	    sprintf(cmd_val_buf, "@%s", OptData->run_fn);

	/*
	 ** Create the foreign command symbol descriptors
	 */
	sym_nam_desc.dsc$w_length = strlen(cmd_nam_buf);
	sym_nam_desc.dsc$a_pointer = cmd_nam_buf;
	sym_val_desc.dsc$w_length = strlen(cmd_val_buf);
	sym_val_desc.dsc$a_pointer = cmd_val_buf;

	/*
	 ** Create the foreign command symbol
	 */
	status =
	    lib$set_symbol(&sym_nam_desc, &sym_val_desc,
			   &LIB$K_CLI_GLOBAL_SYM);
	if (!(status & 1)) {
	    fprintf(stderr, "Error creating symbol: '%s', status: %08X\n",
		    cmd_nam_buf, status);
	    if (OptData->dbg_fd) {
		fprintf(OptData->dbg_fd,
			"Error creating symbol: '%s', status: %08X\n",
			cmd_nam_buf, status);
		fclose(OptData->dbg_fd);
	    }
	    exit(1);
	}

	/*
	 ** If the running filename is the same as the first argument then let's skip
	 ** the argument.
	 */
	if (OptData->argc) {
	    struct stat StatBuf;
	    char *StatDev, *StatIno;

	    if (stat(OptData->run_fn, &StatBuf) == 0) {
		StatDev = strdup(StatBuf.st_dev);
		StatIno = malloc(sizeof(StatBuf.st_ino));
		memcpy(StatIno, StatBuf.st_ino, sizeof(StatBuf.st_ino));

		if (stat(OptData->argv[0], &StatBuf) == 0) {
		    if (strcasecmp(StatDev, StatBuf.st_dev) == 0 &&
			memcmp(StatIno, StatBuf.st_ino,
			       sizeof(StatBuf.st_ino)) == 0)
			arg_idx = 1;
		}

		free(StatDev);
		free(StatIno);
	    }
	}

	/*
	 ** If this is a procedure (i.e. starts with a "@"), then let's make sure the
	 ** number of parameters does not exceed 8, since that's all the DCL allows.
	 */
	if (strncmp(cmd_val_buf, "@", 1) == 0
	    && OptData->argc - arg_idx > 8) {
	    fprintf(stderr,
		    "Error passing too many parameters for command procedure\n");
	    if (OptData->dbg_fd)
		fprintf(OptData->dbg_fd,
			"Error passing too many parameters for command procedure\n");
	    OptData->argc = (8 + arg_idx);
	}

	/*
	 ** Create the do command descriptor
	 */
	strcpy(cmd_run_buf, cmd_nam_buf);
	for (i = arg_idx; i < OptData->argc; i++) {
	    if (strlen(cmd_run_buf) + strlen(OptData->argv[i]) + 3 <
		sizeof(cmd_run_buf))
		sprintf(cmd_run_buf + strlen(cmd_run_buf), " \"%s\"",
			OptData->argv[i]);
	    else {
		fprintf(stderr,
			"Error passing arguments which are too large for command line\n");
		if (OptData->dbg_fd)
		    fprintf(OptData->dbg_fd,
			    "Error passing arguments which are too large for command line\n");
		OptData->argc = i;
	    }
	}
	cmd_run_desc.dsc$w_length = strlen(cmd_run_buf);
	cmd_run_desc.dsc$a_pointer = cmd_run_buf;

	/*
	 ** If we're debugging, then lets show the foriegn command
	 */
	if (OptData->dbg_fd) {
	    fprintf(OptData->dbg_fd,
		    "\nRunning the following commands:\n");
	    fprintf(OptData->dbg_fd, "   $ %s :== %s\n", cmd_nam_buf,
		    cmd_val_buf);
	}

	/*
	 ** Determine whether we need to jacket the executing command in order to supply
	 ** CGI debug information that may have been requested.
	 */
	if (getenv("APACHE$DEBUG_DCL_CGI")
	    && (getenv("APACHE$VERIFY_DCL_CGI")
		|| getenv("APACHE$SHOW_CGI_SYMBOL"))) {
	    char *cmd_fn;
	    FILE *cmd_fd;

	    cmd_fn =
		malloc(strlen("SYS$SCRATCH:APACHE$DCL_DBG_%08X.COM") + 8);
	    if (!cmd_fn) {
		fprintf(stderr,
			"Error allocating memory for debug command file\n");
		if (OptData->dbg_fd)
		    fprintf(OptData->dbg_fd,
			    "Error allocating memory for debug command file\n");
	    } else
		sprintf(cmd_fn, "SYS$SCRATCH:APACHE$DCL_DBG_%08X.COM",
			getpid());

	    if (cmd_fn) {
		cmd_fd = fopen(cmd_fn, "wb");
		if (!cmd_fd) {
		    fprintf(stderr,
			    "Error (%d, %08X) Opening debug command file (%s)\n",
			    errno, vaxc$errno, cmd_fn);
		    if (OptData->dbg_fd)
			fprintf(OptData->dbg_fd,
				"Error (%d, %08X) Opening debug command file (%s)\n",
				errno, vaxc$errno, cmd_fn);
		} else {
		    fprintf(cmd_fd,
			    "$ write sys$output \"Content-type: text/plain\"\n");
		    fprintf(cmd_fd, "$ write sys$output \"\"\n");
		    if (OptData->dbg_fd) {
			fprintf(OptData->dbg_fd,
				"   $ write sys$output \"Content-type: text/plain\"\n");
			fprintf(OptData->dbg_fd,
				"   $ write sys$output \"\"\n");
		    }

		    if (getenv("APACHE$VERIFY_DCL_CGI")) {
			fprintf(cmd_fd, "$ set verify\n");
			if (OptData->dbg_fd)
			    fprintf(OptData->dbg_fd, "   $ set verify\n");
		    }

		    if (getenv("APACHE$SHOW_CGI_SYMBOL")) {
			fprintf(cmd_fd, "$ show symbol *\n");
			fprintf(cmd_fd, "$ apache$dcl_env -l\n");
			fprintf(cmd_fd, "$ show logical /process /job\n");
			fprintf(cmd_fd, "$ write sys$output \"\"\n");
			if (OptData->dbg_fd) {
			    fprintf(OptData->dbg_fd,
				    "   $ show symbol *\n");
			    fprintf(OptData->dbg_fd,
				    "   $ apache$dcl_env -l\n");
			    fprintf(OptData->dbg_fd,
				    "   $ show logical /process /job\n");
			    fprintf(OptData->dbg_fd,
				    "   $ write sys$output \"\"\n");
			}
		    }

		    fprintf(cmd_fd, "$ %s\n", cmd_run_buf);
		    if (OptData->dbg_fd)
			fprintf(OptData->dbg_fd, "   $ %s\n", cmd_run_buf);

		    cmd_run_buf[0] = '@';
		    fgetname(cmd_fd, &cmd_run_buf[1], 1);
		    fclose(cmd_fd);
		    cmd_run_desc.dsc$w_length = strlen(cmd_run_buf);
		    cmd_run_desc.dsc$a_pointer = cmd_run_buf;
		}
		free(cmd_fn);
	    }
	}

	/*
	 ** If we're debugging and we haven't shelled the command with any of our own
	 ** debugging (indicated by the '@') then let's put out the foreign command.
	 */
	if (OptData->dbg_fd && cmd_run_buf[0] != '@') {
	    fprintf(OptData->dbg_fd, "   $ %-*.*s\n",
		    cmd_run_desc.dsc$w_length, cmd_run_desc.dsc$w_length,
		    cmd_run_desc.dsc$a_pointer);
	}
    }

/*
** Close the debug file if it was opened
*/
    if (OptData->dbg_fd)
	fclose(OptData->dbg_fd);

/*
** Execute the do command
*/
    status = lib$do_command(&cmd_run_desc);
    if (!(status & 1))
	fprintf(stderr, "Error: lib$do_command status = %08X\n", status);

}


/*
**
**  ConvertToVms - Convert a file spec to VMS format
**
**  Functional Description:
**
**      This routine is the decc$to_vms callback used to convert
**	Unix style filenames to VMS format.
**
**  Usage:
**
**      ConvertToVms FileName, FileType
**
**  Formal parameters:
**
**      FileName	- (IN) address of the file name
**      FileType	- (IN) integer of the file type
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      0		- Indicates no more file to process
**
**  Side Effects:
**
**      None
**
*/
static int ConvertToVms(char *FileName, int FileType)
{

/*
** Free any previously converted filename
*/
    if (vms_fn)
	free(vms_fn);

/*
** Let's use the converted file spec if it's a file or directory
*/
    if (FileType == DECC$K_FILE || FileType == DECC$K_DIRECTORY)
	vms_fn = strdup(FileName);
    else
	vms_fn = NULL;

/*
** Return zero to indicate no more files
*/
    return (0);
}


/*
**
**  ParseFile - Parse a given file to determine whether there is any
**	implied file type.
**
**  Functional Description:
**
**      This routine determines whether the given filename exists.  If
**	it does not exist and the file type is NULL, then let's check
**	to see whether it has a corresponding file with either if the
**	the following file types: ".EXE" or ".COM".
**
**  Usage:
**
**      ParseFile OptData, FileName
**
**  Formal parameters:
**
**      OptData		- (IN) address of command option data structure
**			  which will contain the parsed input.
**      FileName	- (IN) address of the file name
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      0		- Indicates the file was NOT found
**      1		- Indicates the file was found
**
**  Side Effects:
**
**      None
**
*/
static int ParseFile(OPT_DATA * OptData, char *FileName)
{
    char *FileType[3] = { ".", ".COM", ".EXE" };
    char esa[NAM$C_MAXRSS];
    struct FAB fab;
    struct NAM nam;
    FILE *esa_fd;
    int status, i;

/*
** Free any previously converted filename
*/
    if (vms_fn)
	free(vms_fn);
    vms_fn = NULL;

/*
** Initialize FAB & NAM structures
*/
    fab = cc$rms_fab;
    nam = cc$rms_nam;
    fab.fab$l_fna = FileName;
    fab.fab$b_fns = strlen(FileName);
    fab.fab$l_nam = &nam;
    nam.nam$l_esa = esa;
    nam.nam$b_ess = sizeof(esa);

/*
** Parse the file name
*/
    for (i = 0; i < sizeof(FileType) / sizeof(char *); i++) {
	/*
	 ** Establish the default file types
	 */
	fab.fab$l_dna = FileType[i];
	fab.fab$b_dns = strlen(FileType[i]);

	/*
	 ** Parse the filename
	 */
	status = sys$parse(&fab);
	if ((status & 1)) {
	    /*
	     ** If the file parsed correctly, then if the file is found then return
	     ** establish the filename and return success.
	     */
	    *(nam.nam$l_ver) = '\0';
	    if ((esa_fd = fopen(esa, "rb")) != NULL) {
		fclose(esa_fd);
		vms_fn = malloc(nam.nam$b_esl + 1);
		if (!vms_fn) {
		    fprintf(stderr,
			    "Error allocating file name (%d bytes)\n",
			    nam.nam$b_esl + 1);
		    if (OptData->dbg_fd) {
			fprintf(OptData->dbg_fd,
				"Error allocating file name (%d bytes)\n",
				nam.nam$b_esl + 1);
			fclose(OptData->dbg_fd);
		    }
		    exit(1);
		}
		strncpy(vms_fn, nam.nam$l_esa, nam.nam$b_esl);
		*(vms_fn + nam.nam$b_esl) = '\0';
		return (1);
	    }
	}
    }

/*
** The file was not found, return failure
*/
    return (0);
}


/*
**
**  IsImageFile - Determine whether a file is an image
**
**  Functional Description:
**
**      This routine determines whether the given file is an image.
**
**  Usage:
**
**      IsImageFile OptData, FileName
**
**  Formal parameters:
**
**      OptData		- (IN) address of command option data structure
**			  which will contain the parsed input.
**      FileName	- (IN) address of the filename to process.
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      1		- Indicates that the filename is an Image
**      0		- Indicates that the filename is NOT an Image
**
**  Side Effects:
**
**      None
**
*/
static int IsImageFile(OPT_DATA * OptData, char *FileName)
{
#if defined(__ALPHA)
    EIHD img_hdr;
#elif defined (__ia64) || defined(__x86_64)
    ELF64_EHDR img_hdr;
#else
    IHD img_hdr;
#endif
    FILE *img_fd;

/*
** Open the file
*/
    if ((img_fd = fopen(FileName, "rb")) == NULL) {
	if (OptData->dbg_fd)
	    fprintf(OptData->dbg_fd,
		    "Error (%d, %08X) Opening potential image file %s\n",
		    errno, vaxc$errno, FileName);
	return (0);
    }

/*
** Read the file's header
*/
    if (fread(&img_hdr, sizeof(img_hdr), 1, img_fd) == 0) {
	if (OptData->dbg_fd)
	    fprintf(OptData->dbg_fd,
		    "Error (%d, %08X) Reading potential image file %s\n",
		    errno, vaxc$errno, FileName);
	fclose(img_fd);
	return (0);
    }

/*
** Close the file
*/
    fclose(img_fd);

/*
** Determine whether this file header indicates an Executable
*/
#ifdef __ALPHA
    if ((img_hdr.eihd$l_majorid <= EIHD$K_MAJORID) &&
	((signed int) img_hdr.eihd$l_minorid <= EIHD$K_MINORID) &&
	(img_hdr.eihd$l_imgtype == EIHD$K_EXE))
#elif defined (__ia64) || defined(__x86_64)
/*
** The following check recognizes version 1 ELF images with
** VMS ABI version 1 or 2.
*/
    if ((img_hdr.ehdr$b_ei_mag0 == EHDR$K_ELFMAG0) &&
	(img_hdr.ehdr$b_ei_mag1 == EHDR$K_ELFMAG1) &&
	(img_hdr.ehdr$b_ei_mag2 == EHDR$K_ELFMAG2) &&
	(img_hdr.ehdr$b_ei_mag3 == EHDR$K_ELFMAG3) &&
	(img_hdr.ehdr$b_ei_class == EHDR$K_ELFCLASS64) &&
	(img_hdr.ehdr$b_ei_data == EHDR$K_ELFDATA2LSB) &&
	(img_hdr.ehdr$b_ei_version == 1) &&
	(img_hdr.ehdr$b_ei_osabi == EHDR$K_ELFOSABI_OPENVMS) &&
	(img_hdr.ehdr$b_ei_abiversion >= 1) &&
	(img_hdr.ehdr$b_ei_abiversion <= 2) &&
	(img_hdr.ehdr$w_e_type == EHDR$K_ET_EXEC) &&
	(img_hdr.ehdr$w_e_machine == EHDR$K_EM_IA_64) &&
	(img_hdr.ehdr$l_e_version == img_hdr.ehdr$b_ei_version))
#else
    if ((img_hdr.ihd$w_majorid <= IHD$K_MAJORID) &&
	(img_hdr.ihd$w_minorid <= IHD$K_MINORID) &&
	(img_hdr.ihd$b_imgtype == IHD$K_EXE) &&
	((img_hdr.ihd$w_alias == 0xffff) ||
	 (img_hdr.ihd$w_alias == 0x0002) ||
	 (img_hdr.ihd$w_alias == 0x0003)))
#endif
	return (1);

    return (0);
}
