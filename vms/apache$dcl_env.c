#pragma module APACHE$DCL_ENV "X-6"

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
**	This program creates or deletes environment variables a defined in
**	a given or default environment file.  The variables are processed
**	according the definition of the following directives: APACHE$CGI_MODE,
**	APACHE$PREFIX_DCL_CGI_SYMBOLS_WWW, and APACHE$CREATE_SYMBOLS_GLOBAL.
**
**	The command line syntax is:
**
**	    APACHE$DCL_ENV [-c] [-d] [-D] [-e env-file] [-l]
**
**	where:
**
**	    -c 		    specifies an optional create environment indicator.
**
**	    -d 		    specifies an optional delete environment indicator.
**
**	    -D 		    specifies an optional debug indicator.
**
**	    -e env-file	    specifies an optional alternate environment filename.
**
**	    -l 		    specifies an optional list environment indicator.
**
**  AUTHOR:  Matthew Doremus			CREATION DATE:  17-Jul-2000
**
**  Modification History:
**
**	X-6	Matthew Doremus				27-Nov-2002
**		Added support for Apache 2.0 and allowed only global section
**		data support.
**
**	X-5	Matthew Doremus				04-Sep-2002
**		Added extended input global section and filename support along
**		with extended debugging.
**
**	X-4	Matthew Doremus				07-Nov-2001
**		Added optional global section processing options and support.
**
**	X-3	Matthew Doremus				15-Feb-2001
**		Added optional listing directive to the command line.
**
**	X-2	Matthew Doremus				14-Sep-2000
**		Added initialization for log_val_desc in routine ProcessEnvLogical.
**
**	X-1	Matthew Doremus				17-Jul-2000
**		Initial development
**
**--
**
**  Compile/Link instructions:
**
**	OpenVMS Alpha/VAX:
**	    $ CC APACHE$DCL_ENV
**	    $ LINK APACHE$DCL_ENV
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
#include <unixlib.h>
#include <descrip.h>
#include <iledef.h>
#include <lnmdef.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdio.h>
#include <errno.h>
#include <ssdef.h>

#include "apr_arch_shm.h"

/*
** Undefine __NEW_STARLET if we had defined it
*/
#ifndef __NEW_STARLET_SET
#undef  __NEW_STARLET_SET
#undef  __NEW_STARLET
#endif

/*
** Define Logical & Symbol Maximums
*/
#define LOG_NAM_MAX	255
#define LOG_VAL_MAX	255
#define SYM_NAM_MAX	255
#define SYM_VAL_MAX	970

/*
** Option Data Structure
*/
typedef struct _opt_data {
    char *prg_fn;
    FILE *dbg_fd;
    char *env_fn;
    char env_sw;
    char list_sw;
    char create_sw;
    char delete_sw;
} OPT_DATA;



static void ParseCmdLine(int, char *[], OPT_DATA *);
static void ParseEnvFile(OPT_DATA *);
static int ProcessEnvSymbol(OPT_DATA *, char *, char *, char *, int);
static int ProcessEnvLogical(OPT_DATA *, char *, char *);
static void Usage(char *);


extern void *apr$dcl_shm_open();
extern int apr$dcl_shm_read();
extern int apr$dcl_shm_close();

/*
**
**  main - Main processing routine for the APACHE$DCL_ENV utility
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
** Parse the environment file
*/
    ParseEnvFile(&OptData);

/*
** Close the debug file if it was opened
*/
    if (OptData.dbg_fd)
	fclose(OptData.dbg_fd);

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
    OptData->env_fn = NULL;
    OptData->env_sw = FALSE;
    OptData->list_sw = FALSE;
    OptData->create_sw = FALSE;
    OptData->delete_sw = FALSE;

/*
** Get the program name
*/
    if (argc)
	OptData->prg_fn = strdup(argv[0]);

/*
** Process the command line options
*/
    while ((option = getopt(argc, argv, "lcdDe:?")) != EOF) {
	switch (option) {
	    /*
	     ** Optional create indicator ?
	     */
	case 'c':
	    OptData->create_sw = TRUE;
	    break;

	    /*
	     ** Optional delete indicator ?
	     */
	case 'd':
	    OptData->delete_sw = TRUE;
	    break;

	    /*
	     ** Optional debug indicator ?
	     */
	case 'D':
	    dbg_sw = TRUE;
	    break;

	    /*
	     ** Alternate environment file ?
	     */
	case 'e':
	    OptData->env_fn = strdup(optarg);
	    for (i = 0; i < strlen(OptData->env_fn); i++)
		OptData->env_fn[i] = toupper(OptData->env_fn[i]);
	    OptData->env_sw = TRUE;
	    break;

	    /*
	     ** Optional list indicator ?
	     */
	case 'l':
	    OptData->list_sw = TRUE;
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
** Determine whether we're creating or deleting symbols.  The default if none is specified
** is create.
*/
    if ((OptData->create_sw && OptData->delete_sw) ||
	(OptData->list_sw && (OptData->create_sw || OptData->delete_sw))) {
	Usage(OptData->prg_fn);
	exit(1);
    }
    if (!(OptData->list_sw || OptData->create_sw || OptData->delete_sw))
	OptData->create_sw = TRUE;

/*
** Determine whether we should create a debug file
*/
    if (getenv("APACHE$DCL_DEBUG") || dbg_sw) {
	dbg_fn = malloc(strlen("SYS$SCRATCH:APACHE$DCL_ENV_%08X.DBG") + 8);
	if (!dbg_fn)
	    fprintf(stderr,
		    "Error allocating memory for debug file name\n");
	else
	    sprintf(dbg_fn, "SYS$SCRATCH:APACHE$DCL_ENV_%08X.DBG",
		    getpid());

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
** Allocate the environment filename if we didn't get one from the command line
*/
    if (!OptData->env_fn) {
	char *dcl_proc_ctx = getenv("APACHE$DCL_PROC_CTX");

	OptData->env_fn = malloc(strlen("APR$SHM_DCL_ENV_%08X%s") + 8 +
				 (dcl_proc_ctx ? strlen(dcl_proc_ctx) :
				  0));
	if (!OptData->env_fn) {
	    fprintf(stderr,
		    "Error allocating memory for environment section name\n");
	    if (OptData->dbg_fd) {
		fprintf(OptData->dbg_fd,
			"Error allocating memory for environment section name\n");
		fclose(OptData->dbg_fd);
	    }
	    exit(1);
	}
	sprintf(OptData->env_fn, "APR$SHM_DCL_ENV_%08X%s",
		(getppid()? getppid() : getpid()),
		(dcl_proc_ctx ? dcl_proc_ctx : ""));
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
    fprintf(stdout, "Usage: %s [-c] [-d] [-D] [-e env-name] [-l]\n",
	    prog_name);
}


/*
**
**  ParseEnvFile - Parse the environment file
**
**  Functional Description:
**
**      This routine parses the environment file.
**
**  Usage:
**
**      ParseEnvFile OptData
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
static void ParseEnvFile(OPT_DATA * OptData)
{
    char *env_buf = NULL;
    int env_buf_max = 0;
    int env_len = 0;
    char *eql_sign;
    FILE *env_fd;
    void *env_gs = NULL;
    char *env_pfx;
    char *env_nam, *env_val;
    int cgi_mode;
    int env_atr;
    int status, i;

/*
** Open the environment global section
*/
    env_gs = apr$dcl_shm_open(OptData->env_fn);
    if (!env_gs) {
	if (OptData->env_sw || vaxc$errno != SS$_NOSUCHSEC)
	    fprintf(stderr,
		    "Error (%d, %08X) Opening environment global section (%s)\n",
		    errno, vaxc$errno, OptData->env_fn);
	if (OptData->dbg_fd)
	    fprintf(OptData->dbg_fd,
		    "Error (%d, %08X) Opening environment global section (%s)\n",
		    errno, vaxc$errno, OptData->env_fn);
	free(OptData->env_fn);
	return;
    }

/*
** Free the environment filename buffer
*/
    free(OptData->env_fn);
    OptData->env_fn = NULL;

/*
** Determine CGI mode
*/
    if (env_buf = getenv("APACHE$CGI_MODE")) {
	cgi_mode = atoi(env_buf);
	if (cgi_mode < 0 || cgi_mode > 2 ||
	    strspn(env_buf, "0123456789") != strlen(env_buf)) {
	    fprintf(stderr,
		    "Invalid APACHE$CGI_MODE value %s, defaulting to 0\n",
		    env_buf);
	    if (OptData->dbg_fd)
		fprintf(OptData->dbg_fd,
			"Invalid APACHE$CGI_MODE value %s, defaulting to 0\n",
			env_buf);
	    cgi_mode = 0;
	}
    } else
	cgi_mode = 0;

/*
** Determine CGI symbol prefix
*/
    if (getenv("APACHE$PREFIX_DCL_CGI_SYMBOLS_WWW"))
	env_pfx = "WWW_";
    else
	env_pfx = NULL;

/*
** Determine CGI symbol scope
*/
    if (getenv("APACHE$CREATE_SYMBOLS_GLOBAL"))
	env_atr = LIB$K_CLI_GLOBAL_SYM;
    else
	env_atr = LIB$K_CLI_LOCAL_SYM;

/*
** If we're listing the environment file, then let's put out a "header"
*/
    if (OptData->list_sw)
	fprintf(stdout, "\n(CGI Environment Variables)\n\n");

/*
** Read the environment data
*/
    while (1) {
	/*
	 ** Read the environment global section
	 */
	if (apr$dcl_shm_read(&env_len, sizeof(env_len), 1, env_gs) == 0)
	    break;
	if (env_len < 0)
	    break;

	/*
	 ** Determine whether the environment read buffer is large enough
	 */
	if (env_len > env_buf_max) {
	    /*
	     ** Free the previous environment buffer (if necessary)
	     */
	    if (env_buf)
		free(env_buf);

	    /*
	     ** If this is our first time through then let's allocate a buffer
	     ** large enough for most environment variables.
	     */
	    if (env_buf_max == 0 && env_len <= SYM_VAL_MAX)
		env_buf_max = SYM_VAL_MAX;
	    else
		env_buf_max = env_len;

	    /*
	     ** Allocate the enviroment file read buffer
	     */
	    env_buf = malloc(env_buf_max + 1);
	    if (!env_buf) {
		fprintf(stderr,
			"Error allocating environment file read buffer (%d bytes)\n",
			env_buf_max);
		if (OptData->dbg_fd) {
		    fprintf(OptData->dbg_fd,
			    "Error allocating environment file read buffer (%d bytes)\n",
			    env_buf_max);
		    fclose(OptData->dbg_fd);
		}
		apr$dcl_shm_close(env_gs);
		exit(1);
	    }
	}

	/*
	 ** Read the environment global section into our buffer
	 */
	if (apr$dcl_shm_read(env_buf, sizeof(char), env_len, env_gs) !=
	    env_len) {
	    fprintf(stderr,
		    "Error: end of environment global section reached\n");
	    if (OptData->dbg_fd) {
		fprintf(OptData->dbg_fd,
			"Error: end of environment global section reached\n");
		fclose(OptData->dbg_fd);
	    }
	    apr$dcl_shm_close(env_gs);
	    free(env_buf);
	    exit(1);
	}

	/*
	 ** Null terminate the environment record
	 */
	env_buf[env_len] = '\0';

	/*
	 ** If the environment variable is commented out, then just continue
	 */
	if (strncmp(env_buf, "#", 1) == 0)
	    continue;

	/*
	 ** Separate the environment name and value
	 */
	env_nam = env_buf;
	env_val = "";
	eql_sign = strchr(env_buf, '=');
	if (eql_sign) {
	    env_val = eql_sign + 1;
	    *eql_sign = '\0';
	}

	/*
	 ** Define the environment variable
	 */
	switch (cgi_mode) {
	case 0:
	    ProcessEnvSymbol(OptData, env_nam, env_val, env_pfx, env_atr);
	    break;

	case 1:
	    if (strlen(env_val) > SYM_VAL_MAX)
		ProcessEnvLogical(OptData, env_nam, env_val);
	    else
		ProcessEnvSymbol(OptData, env_nam, env_val, env_pfx,
				 env_atr);
	    break;

	case 2:
	    ProcessEnvLogical(OptData, env_nam, env_val);
	    break;

	default:
	    break;
	}
    }

/*
** Close the environment global section
*/
    apr$dcl_shm_close(env_gs);

/*
** Free the environment file read buffer
*/
    free(env_buf);
}


/*
**
**  ProcessEnvSymbol - Process an environment variable as a symbol
**
**  Functional Description:
**
**      This routine creates or deletes environment variables as a symbol
**
**  Usage:
**
**      ProcessEnvSymbol OptData, sym_nam, sym_val, sym_pfx, sym_atr
**
**  Formal parameters:
**
**      OptData		- (IN) address of command option data structure
**      sym_nam		- (IN) address of the environment symbol name
**      sym_val		- (IN) address of the environment symbol value
**      sym_pfx		- (IN) address of the environment symbol prefix
**      sym_atr		- (IN) address of the environment symbol attributes
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      1		- Processing of the environment symbol succeeded
**      <error_status>	- Error status from LIB$SET_SYMBOL
**      <error_status>	- Error status from LIB$DELETE_SYMBOL
**
**  Side Effects:
**
**      None
**
*/
static int
ProcessEnvSymbol(OPT_DATA * OptData,
		 char *sym_nam, char *sym_val, char *sym_pfx, int sym_atr)
{
    struct dsc$descriptor_s sym_nam_desc =
	{ 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    struct dsc$descriptor_s sym_val_desc =
	{ 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    char *ext_sym_nam = NULL;
    int status = TRUE;

/*
** Setup the symbol name descriptor.  If a symbol prefix was provided, then let's
** allocate an extended symbol name to be created in lue of the one passed in.
*/
    if (sym_pfx) {
	ext_sym_nam = malloc(strlen(sym_nam) + strlen(sym_pfx) + 1);
	sprintf(ext_sym_nam, "%s%s", sym_pfx, sym_nam);
	sym_nam_desc.dsc$w_length = strlen(ext_sym_nam);
	sym_nam_desc.dsc$a_pointer = ext_sym_nam;
    } else {
	sym_nam_desc.dsc$w_length = strlen(sym_nam);
	sym_nam_desc.dsc$a_pointer = sym_nam;
    }

/*
** If the symbol name is larger than than the symbol name maximum then we'll have
** to truncate it.
*/
    if (sym_nam_desc.dsc$w_length > SYM_NAM_MAX) {
	sym_nam_desc.dsc$w_length = SYM_NAM_MAX;
	fprintf(stderr, "Truncating symbol name: %*.*s|%s\n",
		sym_nam_desc.dsc$w_length, sym_nam_desc.dsc$w_length,
		sym_nam_desc.dsc$a_pointer,
		sym_nam_desc.dsc$a_pointer + sym_nam_desc.dsc$w_length);
	if (OptData->dbg_fd)
	    fprintf(OptData->dbg_fd, "Truncating symbol name: %*.*s|%s\n",
		    sym_nam_desc.dsc$w_length, sym_nam_desc.dsc$w_length,
		    sym_nam_desc.dsc$a_pointer,
		    sym_nam_desc.dsc$a_pointer +
		    sym_nam_desc.dsc$w_length);
    }

/*
** If we're listing or creating the symbol then let's setup the symbol value descriptor.
*/
    if (OptData->list_sw || OptData->create_sw) {
	/*
	 ** Setup the symbol value descriptor.
	 */
	sym_val_desc.dsc$w_length = strlen(sym_val);
	sym_val_desc.dsc$a_pointer = sym_val;

	/*
	 ** If the symbol value is larger than than the symbol value maximum then we'll have
	 ** to truncate it.
	 */
	if (sym_val_desc.dsc$w_length > SYM_VAL_MAX) {
	    sym_val_desc.dsc$w_length = SYM_VAL_MAX;
	    fprintf(stderr, "Truncating symbol %*.*s value: %*.*s|%s\n",
		    sym_nam_desc.dsc$w_length, sym_nam_desc.dsc$w_length,
		    sym_nam_desc.dsc$a_pointer, sym_val_desc.dsc$w_length,
		    sym_val_desc.dsc$w_length, sym_val_desc.dsc$a_pointer,
		    sym_val_desc.dsc$a_pointer +
		    sym_val_desc.dsc$w_length);
	    if (OptData->dbg_fd)
		fprintf(OptData->dbg_fd,
			"Truncating symbol %*.*s value: %*.*s|%s\n",
			sym_nam_desc.dsc$w_length,
			sym_nam_desc.dsc$w_length,
			sym_nam_desc.dsc$a_pointer,
			sym_val_desc.dsc$w_length,
			sym_val_desc.dsc$w_length,
			sym_val_desc.dsc$a_pointer,
			sym_val_desc.dsc$a_pointer +
			sym_val_desc.dsc$w_length);
	}
    }

/*
** List the symbol
*/
    if (OptData->list_sw) {
	fprintf(stdout, "  \"%*.*s\" = \"%*.*s\"\n",
		sym_nam_desc.dsc$w_length, sym_nam_desc.dsc$w_length,
		sym_nam_desc.dsc$a_pointer, sym_val_desc.dsc$w_length,
		sym_val_desc.dsc$w_length, sym_val_desc.dsc$a_pointer);
    }

/*
** Create the symbol
*/
    if (OptData->create_sw) {
	/*
	 ** Create the symbol
	 */
	status = lib$set_symbol(&sym_nam_desc, &sym_val_desc, &sym_atr);
	if (!(status & 1)) {
	    fprintf(stderr,
		    "Error creating symbol: '%*.*s', status: %08X\n",
		    sym_nam_desc.dsc$w_length, sym_nam_desc.dsc$w_length,
		    sym_nam_desc.dsc$a_pointer, status);
	    if (OptData->dbg_fd)
		fprintf(OptData->dbg_fd,
			"Error creating symbol: '%*.*s', status: %08X\n",
			sym_nam_desc.dsc$w_length,
			sym_nam_desc.dsc$w_length,
			sym_nam_desc.dsc$a_pointer, status);
	}
    }

/*
** Delete the symbol
*/
    if (OptData->delete_sw) {
	status = lib$delete_symbol(&sym_nam_desc, &sym_atr);
	if (!(status & 1)) {
	    fprintf(stderr,
		    "Error deleting symbol: '%*.*s', status: %08X\n",
		    sym_nam_desc.dsc$w_length, sym_nam_desc.dsc$w_length,
		    sym_nam_desc.dsc$a_pointer, status);
	    if (OptData->dbg_fd)
		fprintf(OptData->dbg_fd,
			"Error deleting symbol: '%*.*s', status: %08X\n",
			sym_nam_desc.dsc$w_length,
			sym_nam_desc.dsc$w_length,
			sym_nam_desc.dsc$a_pointer, status);
	}
    }

/*
** Free the extended symbol name
*/
    if (ext_sym_nam)
	free(ext_sym_nam);

    return (status);

}


/*
**
**  ProcessEnvLogical - Process an environment variable as a logical
**
**  Functional Description:
**
**      This routine creates /deletes environment variables as a logical
**
**  Usage:
**
**      SetEnvLogical OptData, log_nam, log_val
**
**  Formal parameters:
**
**      OptData		- (IN) address of command option data structure
**      log_nam		- (IN) address of the environment logical name
**      log_val		- (IN) address of the environment logical value
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      1		- Setting of the environment logical succeeded
**      <error_status>	- Error status from LIB$SET_LOGICAL
**      <error_status>	- Error status from LIB$DELETE_LOGICAL
**
**  Side Effects:
**
**      None
**
*/
static int
ProcessEnvLogical(OPT_DATA * OptData, char *log_nam, char *log_val)
{
    struct dsc$descriptor_s log_nam_desc =
	{ 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    ILE3 *log_val_desc = NULL;
    char *log_val_ptr;
    int status = TRUE, log_val_cnt, i;

/*
** Setup the logical name descriptor.
*/
    log_nam_desc.dsc$w_length = strlen(log_nam);
    log_nam_desc.dsc$a_pointer = log_nam;

/*
** If the logical name is larger than than the logical name maximum then we'll have
** to truncate it.
*/
    if (log_nam_desc.dsc$w_length > LOG_NAM_MAX) {
	log_nam_desc.dsc$w_length = LOG_NAM_MAX;
	fprintf(stderr, "Truncating logical name: %*.*s|%s\n",
		log_nam_desc.dsc$w_length, log_nam_desc.dsc$w_length,
		log_nam_desc.dsc$a_pointer,
		log_nam_desc.dsc$a_pointer + log_nam_desc.dsc$w_length);
	if (OptData->dbg_fd)
	    fprintf(OptData->dbg_fd, "Truncating logical name: %*.*s|%s\n",
		    log_nam_desc.dsc$w_length, log_nam_desc.dsc$w_length,
		    log_nam_desc.dsc$a_pointer,
		    log_nam_desc.dsc$a_pointer +
		    log_nam_desc.dsc$w_length);
    }

/*
** If we're listing or creating the logical then let's setup the logical value descriptor.
*/
    if (OptData->list_sw || OptData->create_sw) {
	/*
	 ** Setup the logical value descriptor.  We'll use an item list just in case the logical value
	 ** is larger than the logical value maximum and we have to establish an equivilence list for
	 ** this logical name.
	 */
	log_val_ptr = log_val;
	log_val_cnt = (strlen(log_val_ptr) / LOG_VAL_MAX) + 1;
	log_val_desc = (ILE3 *) malloc(sizeof(ILE3) * (log_val_cnt + 1));
	memset((char *) log_val_desc, 0,
	       (sizeof(ILE3) * (log_val_cnt + 1)));
	for (i = 0; i < log_val_cnt; i++) {
	    log_val_desc[i].ile3$w_length =
		(strlen(log_val_ptr) >
		 LOG_VAL_MAX) ? LOG_VAL_MAX : strlen(log_val_ptr);
	    log_val_desc[i].ile3$w_code = LNM$_STRING;
	    log_val_desc[i].ile3$ps_bufaddr = log_val_ptr;
	    log_val_ptr += LOG_VAL_MAX;
	};
    }

/*
** List the logical
*/
    if (OptData->list_sw) {
	fprintf(stdout, "  \"%*.*s\" = \"%*.*s\"\n",
		log_nam_desc.dsc$w_length, log_nam_desc.dsc$w_length,
		log_nam_desc.dsc$a_pointer, log_val_desc[0].ile3$w_length,
		log_val_desc[0].ile3$w_length,
		(char *) log_val_desc[0].ile3$ps_bufaddr);
	for (i = 1; i < log_val_cnt; i++)
	    fprintf(stdout, "	= \"%*.*s\"\n",
		    log_val_desc[i].ile3$w_length,
		    log_val_desc[i].ile3$w_length,
		    (char *) log_val_desc[i].ile3$ps_bufaddr);
    }

/*
** Create the logical
*/
    if (OptData->create_sw) {
	status =
	    lib$set_logical(&log_nam_desc, NULL, NULL, NULL, log_val_desc);
	if (!(status & 1)) {
	    fprintf(stderr,
		    "Error creating logical: '%*.*s', status: %08X\n",
		    log_nam_desc.dsc$w_length, log_nam_desc.dsc$w_length,
		    log_nam_desc.dsc$a_pointer, status);
	    if (OptData->dbg_fd)
		fprintf(OptData->dbg_fd,
			"Error creating logical: '%*.*s', status: %08X\n",
			log_nam_desc.dsc$w_length,
			log_nam_desc.dsc$w_length,
			log_nam_desc.dsc$a_pointer, status);
	}
    }

/*
** Delete the logical
*/
    if (OptData->delete_sw) {
	status = lib$delete_logical(&log_nam_desc, NULL);
	if (!(status & 1)) {
	    fprintf(stderr,
		    "Error deleting logical: '%*.*s', status: %08X\n",
		    log_nam_desc.dsc$w_length, log_nam_desc.dsc$w_length,
		    log_nam_desc.dsc$a_pointer, status);
	    if (OptData->dbg_fd)
		fprintf(OptData->dbg_fd,
			"Error deleting logical: '%*.*s', status: %08X\n",
			log_nam_desc.dsc$w_length,
			log_nam_desc.dsc$w_length,
			log_nam_desc.dsc$a_pointer, status);
	}
    }

/*
** Free the logical value descriptor
*/
    if (log_val_desc)
	free(log_val_desc);

    return (status);
}
