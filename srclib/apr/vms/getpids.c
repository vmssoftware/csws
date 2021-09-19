#pragma module GETPIDS "V1.00"

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
**	This routine provides a mechanism to retrieve the parent PID for
**	the current process.
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
*/

#ifndef __NEW_STARLET
#define __NEW_STARLET
#define __NEW_STARLET_SET
#endif

#include <starlet.h>
#include <iosbdef.h>
#include <jpidef.h>
#include <efndef.h>
#include <iledef.h>
#include <unistd.h>
#include <stdlib.h>

#include "ilemac.h"

#ifdef  __NEW_STARLET_SET
#undef  __NEW_STARLET_SET
#undef  __NEW_STARLET
#endif

/*
** Define prototypes for external functions
*/
extern int decc$$translate();

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int apr$master_pid(void)
{
    static int MasterPid = 0;
    ILE3 JpiItems[2];
    ILE3 *Ile3Ptr;
    int status;
    IOSB iosb;

/*
** If we've already retrieved the Master PID, then simply return it
*/
    if (MasterPid)
	return (MasterPid);

/*
** Setup the JPI item list
*/
    ILE3_INIT(JpiItems);
    ILE3_ADD(JPI$_MASTER_PID, sizeof(MasterPid), &MasterPid, 0);
    ILE3_TERM;

/*
** Get the JPI information
*/
    status = SYS$GETJPIW(EFN$C_ENF,	/* No event flag              */
			 0,	/* No CSID address            */
			 0,	/* No node name               */
			 &JpiItems,	/* Item list                  */
			 &iosb,	/* IOSB                       */
			 0,	/* No AST routine             */
			 0);	/* No AST parameter           */
    if (status & 1)
	status = iosb.iosb$w_status;
    if (!(status & 1)) {
	decc$$translate(status);
	MasterPid = 0;
    }

/*
** Return the master pid
*/
    return (MasterPid);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int apr$owner_pid(void)
{
    static int OwnerPid = 0;
    ILE3 JpiItems[2];
    ILE3 *Ile3Ptr;
    int status;
    IOSB iosb;

/*
** If we've already retrieved the Owner PID, then simply return it
*/
    if (OwnerPid)
	return (OwnerPid);

/*
** Setup the JPI item list
*/
    ILE3_INIT(JpiItems);
    ILE3_ADD(JPI$_OWNER, sizeof(OwnerPid), &OwnerPid, 0);
    ILE3_TERM;

/*
** Get the JPI information
*/
    status = SYS$GETJPIW(EFN$C_ENF,	/* No event flag              */
			 0,	/* No CSID address            */
			 0,	/* No node name               */
			 &JpiItems,	/* Item list                  */
			 &iosb,	/* IOSB                       */
			 0,	/* No AST routine             */
			 0);	/* No AST parameter           */
    if (status & 1)
	status = iosb.iosb$w_status;
    if (!(status & 1)) {
	decc$$translate(status);
	OwnerPid = 0;
    }

/*
** Return the Owner pid
*/
    return (OwnerPid);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int apr$parent_pid(void)
{
    static int ParentPid = 0;
    char *PidStr = NULL;

/*
** If we've already retrieved the Parent PID, then simply return it
*/
    if (ParentPid)
	return (ParentPid);

/*
** Return the detached processes 'Parent' PID if it exists
*/
    if ((PidStr = getenv("APR$PARENT_PID")) == NULL) {
	PidStr = getenv("APACHE$PARENT_PID");
    }

    if (PidStr) {
	ParentPid = atoi(PidStr);
	return (ParentPid);
    }

/*
** Return the spawned processes 'Parent' PID if it exists
*/
    ParentPid = getppid();
    if (ParentPid)
	return (ParentPid);

/*
** Return our PID
*/
    ParentPid = getpid();
    return (ParentPid);

}
