#pragma module APACHE$SET_CCL "X-2"

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
**	This program facilitates the setting of a sockets CCL attribute.
**
**	The command line syntax is:
**
**	    APACHE$SET_CCL [-s ccl-value] [dev-name]
**
**	where:
**
**	    -s ccl-value    specifies an optional ccl value
**
**	    dev-name	    specifies an optional socket device name
**
**  AUTHOR:  Powell "Hap" Hazzard			CREATION DATE:  Oct-2000
**
**  Modification History:
**
**	X-2	Matthew Doremus				27-Nov-2002
**		Added support for Apache 2.0
**
**	X-1	Powell "Hap" Hazzard			Oct-2000
**		Initial development
**
**--
**
**  Compile/Link instructions:
**
**	OpenVMS Alpha/VAX:
**	    $ CC APACHE$SET_CCL
**	    $ LINK APACHE$SET_CCL
**
*/


/*
** Define __NEW_STARLET if it's not already defined
*/
#ifndef __NEW_STARLET
#define __NEW_STARLET
#define __NEW_STARLET_SET
#endif

#include <starlet.h>
#include <descrip.h>
#include <stdlib.h>
#include <string.h>
#include <socket.h>
#include <stdio.h>
#include <errno.h>

#include "protshr.h"

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
    char *dev_nam;
    int ccl_val;
} OPT_DATA;


static void ParseCmdLine(int, char *[], OPT_DATA *);
static void SetSocketCcl(OPT_DATA *);
static void Usage(char *);

/*
**
**  main - Main processing routine for the APACHE$SET_CCL utility
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
** Set the socket CCL value
*/
    SetSocketCcl(&OptData);

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
    int option, i;

/*
** Initialize the option data
*/
    OptData->prg_fn = NULL;
    OptData->dev_nam = "SYS$OUTPUT";
    OptData->ccl_val = -1;

/*
** Get the program name
*/
    if (argc)
	OptData->prg_fn = strdup(argv[0]);

/*
** Process the command line options
*/
    while ((option = getopt(argc, argv, "s:?")) != EOF) {
	switch (option) {
	    /*
	     ** Optional CCL value ?
	     */
	case 's':
	    OptData->ccl_val = atoi(optarg);
	    if (OptData->ccl_val < -1 || OptData->ccl_val > 1) {
		Usage(OptData->prg_fn);
		exit(1);
	    }
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
    if (argc - optind > 1) {
	Usage(OptData->prg_fn);
	exit(1);
    }

/*
** Process the socket device name
*/
    if (argc - optind == 1) {
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
    fprintf(stdout, "Usage: %s [-s ccl-value] [dev-name]\n", prog_name);
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
**      SetSocketCcl OptData
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
static void SetSocketCcl(OPT_DATA * OptData)
{
    struct dsc$descriptor SockDesc =
	{ 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    unsigned short int SockChan;
    int status;

/*
** Setup the socket device descriptor
*/
    SockDesc.dsc$a_pointer = OptData->dev_nam;
    SockDesc.dsc$w_length = strlen(OptData->dev_nam);

/*
** Assign a channel to the socket device
*/
    status = SYS$ASSIGN(&SockDesc, &SockChan, 0, 0);
    if (!(status & 1))
	exit(status);

/*
** Set the socket device CCL
*/
    status = apr$$setsockopt(SockChan, SET_SOCK_DEV_CCL,
			     &OptData->ccl_val, sizeof(&OptData->ccl_val));
    if (!(status & 1))
	exit(status);
}
