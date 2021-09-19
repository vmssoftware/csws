#pragma module APACHE$DCL_BIN "X-5"

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
**	This program displays a given binary file via the output socket
**	described by SYS$OUTPUT.
**
**	The command line syntax is:
**
**	    APACHE$DCL_BIN [-s bin-size] bin-file
**
**	where:
**
**	    -s bin-size	    specifies an optional binary buffer size.
**
**	    bin-file	    specifies a binary file to be displayed.
**
**  AUTHOR:  Matthew Doremus			CREATION DATE:  08-Jul-2000
**
**  Modification History:
**
**	X-5	Matthew Doremus				19-Jul-2002
**		Revived the socket device processing and changed the
**		fwrite to a send.  The use of multiple fwrite calls
**		did not work correctly over the SYS$OUTPUT socket.
**
**	X-4	Matthew Doremus				25-Apr-2002
**		Removed unnecessary logic which predated the use of
**		apache$flip_ccl.  Also, simplified writting of binary
**		file such that the whole file is not read into memory
**		but rather read and written in 'bin-size' segments.
**
**	X-3	Powell Hazzard				01-Nov-2000
**		Change open to binary mode for output
**		Support the apache$flip_ccl mode
**
**	X-2	Matthew Doremus				08-Sep-2000
**		Fixed looping problem for writes which return an error (-1) or
**		zero bytes written.
**
**	X-1	Matthew Doremus				08-Jul-2000
**		Initial development
**
**
**--
**
**  Compile/Link instructions:
**
**	OpenVMS Alpha/VAX:
**	    $ CC APACHE$DCL_BIN
**	    $ LINK APACHE$DCL_BIN
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
#include <unixio.h>
#include <socket.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

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
    char *bin_file;
    int bin_size;
} OPT_DATA;


static void ParseCmdLine(int, char *[], OPT_DATA *);
static void DisplayBinFile(OPT_DATA *);
static void Usage();


/*
**
**  main - Main processing routine for the APACHE$DCL_BIN utility
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
** Display the binary file
*/
    DisplayBinFile(&OptData);
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
static void ParseCmdLine(int argc, char *argv[], OPT_DATA * OptData)
{
    int option, i;

/*
** Initialize the option data
*/
    OptData->bin_file = NULL;
    OptData->bin_size = 32768;

/*
** Process the command line options
*/
    while ((option = getopt(argc, argv, "s:?")) != EOF) {
	switch (option) {
	    /*
	     ** Binary Buffer Size ?
	     */
	case 's':
	    if (strspn(optarg, "0123456789") != strlen(optarg)) {
		Usage();
		exit(1);
	    } else {
		OptData->bin_size = atoi(optarg);
		if (!OptData->bin_size) {
		    Usage();
		    exit(1);
		}
	    }
	    break;

	    /*
	     ** Invalid argument ?
	     */
	case '?':
	default:
	    Usage();
	    exit(1);
	    break;
	}
    }

/*
** Are the number of parameters correct ?
*/
    if (argc - optind != 1) {
	Usage();
	exit(1);
    } else
	OptData->bin_file = strdup(argv[optind]);

}


/*
**
**  DisplayBinFile - Display the binary file.
**
**  Functional Description:
**
**      This routine displays a given binary file via the output socket
**	described by SYS$OUTPUT.
**
**  Usage:
**
**      DisplayBinFile OptData
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
static void DisplayBinFile(OPT_DATA * OptData)
{
    $DESCRIPTOR(out_desc, "SYS$OUTPUT");
    unsigned short out_chan;
    char *bin_buf;
    FILE *bin_fd;
    int bin_len;
    int out_sd;
    int out_len;
    int status;

/*
** Assign a channel to the output device
*/
    status = SYS$ASSIGN(&out_desc, &out_chan, 0, 0);
    if (!(status & 1)) {
	fprintf(stderr, "Error (%08X) Assigning a channel to %-*.*s\n",
		status, out_desc.dsc$w_length, out_desc.dsc$w_length,
		out_desc.dsc$a_pointer);
	exit(1);
    }

/*
** Convert the channel to a descriptor
*/
    out_sd = socket_fd(out_chan);
    if (!out_sd) {
	fprintf(stderr, "Error (%d) Converting channel to descriptor\n",
		errno);
	exit(1);
    }

/*
** Allocate the read buffer
*/
    bin_buf = malloc(OptData->bin_size);
    if (!bin_buf) {
	fprintf(stderr,
		"Error (%d, 0x%08X) Allocating %d bytes for read buffer\n",
		errno, vaxc$errno, OptData->bin_size);
	exit(1);
    }

/*
** Open the binary file
*/
    bin_fd = fopen(OptData->bin_file, "rb");
    if (!bin_fd) {
	fprintf(stderr, "Error (%d, 0x%08X) Opening input file %s\n",
		errno, vaxc$errno, OptData->bin_file);
	exit(1);
    }

/*
** Write the binary file to the output device
*/
    while (TRUE) {
	bin_len = fread(bin_buf, 1, OptData->bin_size, bin_fd);
	if (bin_len > 0) {
	    out_len = send(out_sd, bin_buf, bin_len, 0);
	    if (out_len <= 0) {
		fprintf(stderr,
			"Error (%d, 0x%08X) Writing %d bytes from the write buffer\n",
			errno, vaxc$errno, bin_len);
		break;
	    }
	} else {
	    if (bin_len < 0)
		fprintf(stderr,
			"Error (%d, 0x%08X) Reading %d bytes into the read buffer\n",
			errno, vaxc$errno, OptData->bin_size);
	    break;
	}
    }

/*
** Close the binary file
*/
    fclose(bin_fd);

/*
** Free the read buffer
*/
    free(bin_buf);

/*
** Deassign a channel to the output device
*/
    status = SYS$DASSGN(out_chan);
    if (!(status & 1)) {
	fprintf(stderr, "Error (%08X) Deassigning the channel to %-*.*s\n",
		status, out_desc.dsc$w_length, out_desc.dsc$w_length,
		out_desc.dsc$a_pointer);
	exit(1);
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
**      Usage
**
**  Formal parameters:
**
**      None
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
static void Usage()
{
    fprintf(stdout, "Usage: APACHE$DCL_BIN [-s bin-size] bin-file\n");
}
