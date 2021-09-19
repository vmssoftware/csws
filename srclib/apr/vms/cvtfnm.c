#pragma module CVTFNM "V1.02"

/*
**
** © Copyright 2002 Hewlett-Packard Development Company, L.P.
**
** Hewlett-Packard and the Hewlett-Packard logo are trademarks
** of Hewlett-Packard Development Company L.P. in the U.S. and/or
** other countries.
**
** Confidential computer software.
** Valid license from Hewlett-Packard required for possession, use
** or copying.  Consistent with FAR 12.211 and 12.212, Commercial
** Computer Software, Computer Software Documentation, and Technical
** Data for Commercial.  Items are licensed to the U.S. Government
** under vendor's standard commercial license.
**
** Hewlett-Packard shall not be liable for technical or editorial
** errors or omissions contained herein.  The information is provided
** "as is" without warranty of any kind and is subject to change
** without notice.  The warranties for Hewlett-Packard products are
** set forth in the express warranty statements accompanying such
** products.  Nothing herein should be construed as constituting an
** additional warranty.
**
*/

/*
**
**  FACILITY:
**
**      Secure Web Server
**
**  ABSTRACT:
**
**      This module contains routines to convert filenames to and from
**      VMS & Unix.
**
**  AUTHOR:
**
**      Matthew Doremus
**
**
**  CREATION DATE:   August 29, 2001
**
**  MODIFICATION HISTORY:
**
**  V1.00 	        Matthew Doremus                 29-Aug-2001
**        Initial development.
**
**  V1.01 	        Matthew Doremus                 20-Nov-2001
**        Fixed use of DECC$TO_VMS & DECC$FROM_VMS
**
**  V1.02 	        Scott LePage                    15-Apr-2004
**        Changed prototypes for DECC$TO_VMS and DECC$FROM_VMS so they
**        are only defined for earlier version of the compiler.  V7.x
**        compiler has prototypes defined.
*/

#ifndef __NEW_STARLET
#define __NEW_STARLET
#define __NEW_STARLET_SET
#endif

#include <unixlib.h>
#include <stdlib.h>
#include <string.h>
#ifdef TEST_CVTFNM
#include <unistd.h>
#include <stdio.h>
#endif

#include "cvtfnm.h"

#ifdef  __NEW_STARLET_SET
#undef  __NEW_STARLET_SET
#undef  __NEW_STARLET
#endif

/*
** Define TRUE & FALSE for these routines if necessary
*/
#ifdef  TRUE
#undef  TRUE
#endif
#define TRUE 1
#ifdef  FALSE
#undef  FALSE
#endif
#define FALSE 0

/*
** Define the path names for Unix to VMS conversion
*/
static char *_VmsPath = NULL;
static char *_UnixPath = NULL;

/*
** Define prototypes for external functions
*/
/*#if __DECC_VER < 70000000 */
/* extern int decc$to_vms (); */
/* extern int decc$from_vms (); */
/*#endif */

/*
** Define prototypes for local functions
*/
static int _ConvertToVms (char *, int);
static int _ConvertToUnix (char *);

#ifdef TEST_CVTFNM
/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
main (int argc, char *argv[])
{
char *CvtFile = NULL,
     *path = NULL;
int CvtToUnix = 1,
    CvtToVms = 1,
    option;

/*
** Process the command line options
*/
while ((option = getopt (argc, argv, "uv?")) != EOF)
    {
    switch (option)
	{
	/*
	** To Unix conversion ?
	*/
	case 'u':
	    CvtToVms = 0;
	    break;

	/*
	** To Vms conversion ?
	*/
	case 'v':
	    CvtToUnix = 0;
	    break;

	/*
	** Invalid argument ?
	*/
	case '?':
	default:
	    fprintf (stdout, "Usage: CVTFNM [-v | -u] [cvt-file]\n");
	    exit (1);
	    break;
	}
    }

/*
** Make sure they specified a conversion
*/
if (! (CvtToVms || CvtToUnix))
    {
    fprintf (stdout, "Usage: CVTFNM [-v | -u] [cvt-file]\n");
    exit (1);
    }

/*
** Are the number of parameters correct ?
*/
if (argc - optind)
    path = argv[optind];

/*
** Convert the filename to Unix
*/
if (CvtToUnix)
    {
    if (! path)
        {
        path = argv[0];
        if (apr$cvt_fnm (CVT_FNM_UNIX_TO_VMS, path, &CvtFile))
            path = CvtFile;
        }
    printf ("apr$cvt_fnm (\"%s\") = ", path);
    if (apr$cvt_fnm (CVT_FNM_VMS_TO_UNIX, path, &CvtFile))
	path = CvtFile;
    printf ("\"%s\"\n", path);
    }

/*
** Convert the filename to VMS
*/
if (CvtToVms)
    {
    if (! path)
        {
        path = argv[0];
        if (apr$cvt_fnm (CVT_FNM_VMS_TO_UNIX, path, &CvtFile))
            path = CvtFile;
        }
    printf ("apr$cvt_fnm (\"%s\") = ", path);
    if (apr$cvt_fnm (CVT_FNM_UNIX_TO_VMS, path, &CvtFile))
	path = CvtFile;
    printf ("\"%s\"\n", path);
    }

}
#endif

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int apr$cvt_fnm (int CvtType, char *FromFileName, char **ToFileName)
{
int status;

/*
** Determine what type of conversion was requested
*/
switch (CvtType)
    {
    /*
    ** Convert the VMS file name to Unix
    */
    case CVT_FNM_VMS_TO_UNIX:
	if (decc$from_vms (FromFileName, _ConvertToUnix, 0) && _UnixPath)
	    {
	    *ToFileName = strdup (_UnixPath);
	    status = TRUE;
	    }
	else
	    {
	    *ToFileName = NULL;
	    status = FALSE;
	    }
	if (_UnixPath)
	    free (_UnixPath);
	_UnixPath = NULL;
	break;

    /*
    ** Convert the Unix file name to VMS
    */
    case CVT_FNM_UNIX_TO_VMS:
	if (decc$to_vms (FromFileName, _ConvertToVms, 0, 0) && _VmsPath)
	    {
	    *ToFileName = strdup (_VmsPath);
	    status = TRUE;
	    }
	else
	    {
	    *ToFileName = NULL;
	    status = FALSE;
	    }
	if (_VmsPath)
	    free (_VmsPath);
	_VmsPath = NULL;
	break;

    /*
    ** Invalid conversion type entered
    */
    default:
	status = FALSE;
	break;
    }

/*
** Return the conversion status
*/
return (status);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static int _ConvertToVms (char *FileName, int FileType)
{

/*
** Let's use the converted file spec if it's a file or directory
*/
if (FileType == DECC$K_FILE || FileType == DECC$K_DIRECTORY)
    _VmsPath = strdup (FileName);
else
    _VmsPath = NULL;

/*
** Return zero to indicate no more files
*/
return (0);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static int _ConvertToUnix (char *FileName)
{

/*
** Let's save the converted file spec
*/
_UnixPath = strdup (FileName);

/*
** Return zero to indicate no more files
*/
return (0);

}
