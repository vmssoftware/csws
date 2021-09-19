#pragma module MAILBOX "V1.00"

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
**	These routines provide a simplified API for mailbox maintenance and use.
**
**  AUTHOR:
**
**      Matthew Doremus
**
**
**  CREATION DATE:   November 8, 2002
**
**  MODIFICATION HISTORY:
**
**  V1.00 	        Matthew Doremus                 08-Nov-2002
**        Initial development.
**
**  V1.01 	        Matthew Doremus                 11-Jun-2002
**        Added mixed case support
**
*/

#ifndef __NEW_STARLET
#define __NEW_STARLET
#define __NEW_STARLET_SET
#endif

#include <descrip.h>
#include <starlet.h>
#include <iledef.h>
#include <lnmdef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <psldef.h>
#include <stdio.h>
#include <errno.h>
#include <iodef.h>
#include <ssdef.h>

#include "protshr.h"
#include "ilemac.h"

#ifdef  __NEW_STARLET_SET
#undef  __NEW_STARLET_SET
#undef  __NEW_STARLET
#endif

/*
** Define the maximum mailbox message & buffer size
*/
#define MBX_MAXMSG_SIZE 65535
#define MBX_BUFQUO_SIZE 60000

#ifndef MIN
#define MIN(a,b) a < b ? a : b
#endif

#ifdef TEST_MAILBOX
/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
main(int argc, char *argv[])
{
    unsigned short int MbxChan = 0;
    int status;

/*
** Create the temporary mailbox
*/
    status = apr$mbx_create(&MbxChan, 256, 1024, "TEST_MBX_LNM");
    if (!(status & 1))
	return (status);

/*
** Delete the temporary mailbox
*/
    status = apr$mbx_delete(MbxChan);
    if (!(status & 1))
	return (status);

/*
** Exit
*/
    exit(1);

}
#endif

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int apr$mbx_create(unsigned short int *MbxChan, ...)
{
    struct dsc$descriptor MbxLogNamDesc =
	{ 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    $DESCRIPTOR(TblNamDesc, "LNM$PROCESS_DIRECTORY");
    $DESCRIPTOR(LogNamDesc, "LNM$TEMPORARY_MAILBOX");
    unsigned int MbxProMsk = 0x0000FF00;
    unsigned int MbxMaxMsg = 0;
    unsigned int MbxBufQuo = 0;
    char *MbxLogTbl = NULL;
    char *MbxLogNam = NULL;
    ILE3 LnmItems[2], *Ile3Ptr;
    va_list argp;
    int status, argc;

/*
** Process the optional parameters
*/
    va_start(argp, MbxChan);
    va_count(argc);
    if (argc > 1)
	MbxMaxMsg = va_arg(argp, unsigned int);
    if (argc > 2)
	MbxBufQuo = va_arg(argp, unsigned int);
    if (argc > 3)
	MbxLogNam = va_arg(argp, char *);
    if (argc > 4)
	MbxLogTbl = va_arg(argp, char *);
    va_end(argp);

/*
** Minimize the message & buffer size
*/
    MbxMaxMsg = MIN(MbxMaxMsg, MBX_MAXMSG_SIZE),
	MbxBufQuo = MIN(MbxBufQuo, MBX_BUFQUO_SIZE);

/*
** Define the temporary mailbox logical name
*/
    if (MbxLogNam) {
	/*
	 ** Setup the mailbox logical name descriptor
	 */
	MbxLogNamDesc.dsc$w_length = strlen(MbxLogNam);
	MbxLogNamDesc.dsc$a_pointer = MbxLogNam;
    }

/*
** Define the temporary mailbox logical name table
*/
    if (MbxLogTbl) {
	/*
	 ** Setup the logical name translation items
	 */
	ILE3_INIT(LnmItems);
	ILE3_ADD(LNM$_STRING, strlen(MbxLogTbl), MbxLogTbl, 0);
	ILE3_TERM;

	/*
	 ** Define the mailbox logical name in the Process Directory as system wide
	 */
	status = SYS$CRELNM(0, &TblNamDesc, &LogNamDesc, 0, &LnmItems);
	if (!(status & 1))
	    return (status);

	/*
	 ** Create the privileged mailbox
	 */
	status = apr$$crembx(MbxChan,
				MbxMaxMsg,
				MbxBufQuo,
				MbxLogNam ? &MbxLogNamDesc : NULL);
    } else {
	/*
	 ** Create the mailbox
	 */
	status = SYS$CREMBX(0,
			    MbxChan,
			    MbxMaxMsg,
			    MbxBufQuo,
			    MbxProMsk,
			    PSL$C_USER,
			    MbxLogNam ? &MbxLogNamDesc : NULL, 0);
    }

/*
** Return status
*/
    return (status);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int apr$mbx_delete(unsigned short int MbxChan)
{
    int status;

/*
** Delete the mailbox
*/
    status = SYS$DELMBX(MbxChan);

/*
** Return the last successful status
*/
    return (status);

}
