#pragma module IPV6 "V1.01"

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
**	This routine determines whether IPV6 is installed on the running
**	system.
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
**  V1.01 	        Matthew Doremus                 20-Oct-2003
**        Add in IPV6 workaround for IA64 platform
**
**  V1.02		Scott LePage			11-Feb-2005
**        Remove routines getaddrinfo(), freeaddrinfo(), and getnameinfo()
**        on the IA64 platform (former workaround).  They now appear in
**        the DECC RTL.
*/

/*
** Define the _SOCKADDR_LEN to enable IPV6 data structure changes
*/
#ifndef _SOCKADDR_LEN
#define _SOCKADDR_LEN
#endif

#ifndef __NEW_STARLET
#define __NEW_STARLET
#define __NEW_STARLET_SET
#endif

#include <descrip.h>
#include <starlet.h>
#include <iledef.h>
#include <lnmdef.h>
#include <types.h>
#if defined(__ia64) || defined(__x86_64)
#include <netdb.h>
#include <dlfcn.h>
#include <errno.h>
#endif
#ifdef TEST_IPV6
#include <stdio.h>
#endif
#include  <stdlib.h>

#include "ilemac.h"
extern int apr$ipv6_started(void);

#ifdef  __NEW_STARLET_SET
#undef  __NEW_STARLET_SET
#undef  __NEW_STARLET
#endif

#if 0				/* V1.02 - removed */
/*
** Define the shareable image & entry points
*/
#ifndef IPC_SHR_IMAGE
#define IPC_SHR_IMAGE		"TCPIP$IPC_SHR"
#endif
#ifndef IPC_FREEADDRINFO
#define IPC_FREEADDRINFO	"TCPIP$FREEADDRINFO"
#endif
#ifndef IPC_GETADDRINFO
#define IPC_GETADDRINFO		"TCPIP$GETADDRINFO"
#endif
#ifndef IPC_GETNAMEINFO
#define IPC_GETNAMEINFO		"TCPIP$GETNAMEINFO"
#endif
#endif

/*
** Define TRUE & FALSE
*/
#define TRUE	1
#define FALSE	0

/*
** Define Success & Failure for these functions
*/
#ifndef	SUCCESS
#define	SUCCESS	0
#endif
#ifndef	FAILURE
#define	FAILURE -1
#endif

#if 0				/* V1.02 - removed */
/*
** Define the IPC shareable handle
*/
static void *Ipc_Shr_Handle = (void *) -1;
#endif

#ifdef TEST_IPV6
/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
main(int argc, char *argv[])
{

/*
** Display whether IPV6 is started on this machine
*/
    printf("IPV6 is %sstarted on this machine.\n",
	   apr$ipv6_started()? "" : "not ");

}
#endif

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static int ipv6_ok()
{
	/* TBD: Replace this code with something that tries to validate wether we can really
	   use IPv6 (create a socket, bind to it, and so on) */

	if (getenv("apache$can_use_ipv6")) {
	   return (1);
	} else {
	   return (0);
	}
}

int apr$ipv6_started(void)
{
    $DESCRIPTOR(LnmDesc, "TCPIP$IPV6_STARTED");
    $DESCRIPTOR(TblDesc, "LNM$SYSTEM_TABLE");
    unsigned int LnmAttr = LNM$M_CASE_BLIND;
    static int IPV6_Started = -1;
    char LnmString[255 + 1];
    ILE3 LnmItems[2], *Ile3Ptr;
    int status;

/*
** If we've already established if IPV6 is started, then return it
*/
    if (IPV6_Started >= 0)
	return (IPV6_Started);

/*
** Setup the logical name translation items
*/
    ILE3_INIT(LnmItems);
    ILE3_ADD(LNM$_STRING, sizeof(LnmString) - 1, LnmString, 0);
    ILE3_TERM;

/*
** Translate the IPV6 logical name
*/
    status = SYS$TRNLNM(&LnmAttr, &TblDesc, &LnmDesc, 0, &LnmItems);
    if ((status & 1) && ipv6_ok())
	IPV6_Started = TRUE;
    else
	IPV6_Started = FALSE;

/*
** Return the IPV6 started indicator
*/
    return (IPV6_Started);

}

#if 0				/* V1.02 - removed */

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
void freeaddrinfo(struct addrinfo *AddrInfo)
{
    static int (*Ipc_FreeAddrInfo) () = (int (*)()) -1;

    if ((int) Ipc_Shr_Handle < 0)
	Ipc_Shr_Handle = dlopen(IPC_SHR_IMAGE, 0);
    if (!Ipc_Shr_Handle) {
	errno = ENOTSUP;
	return;
    }

    if ((int) Ipc_FreeAddrInfo < 0)
	Ipc_FreeAddrInfo =
	    (int (*)()) dlsym(Ipc_Shr_Handle, IPC_FREEADDRINFO);
    if (!Ipc_FreeAddrInfo) {
	errno = ENOTSUP;
	return;
    }

    Ipc_FreeAddrInfo(AddrInfo);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int getaddrinfo(const char *NodeName, const char *ServName,
		const struct addrinfo *AddrHint,
		struct addrinfo **AddrRslt)
{
    static int (*Ipc_GetAddrInfo) () = (int (*)()) -1;

    if ((int) Ipc_Shr_Handle < 0)
	Ipc_Shr_Handle = dlopen(IPC_SHR_IMAGE, 0);
    if (!Ipc_Shr_Handle) {
	errno = ENOTSUP;
	return (FAILURE);
    }

    if ((int) Ipc_GetAddrInfo < 0)
	Ipc_GetAddrInfo =
	    (int (*)()) dlsym(Ipc_Shr_Handle, IPC_GETADDRINFO);
    if (!Ipc_GetAddrInfo) {
	errno = ENOTSUP;
	return (FAILURE);
    }

    return (Ipc_GetAddrInfo(NodeName, ServName, AddrHint, AddrRslt));

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int getnameinfo(const struct sockaddr *SockAddr, size_t SockLen,
		char *NodeName, size_t NodeLen,
		char *ServName, size_t ServLen, int Flags)
{
    static int (*Ipc_GetNameInfo) () = (int (*)()) -1;

    if ((int) Ipc_Shr_Handle < 0)
	Ipc_Shr_Handle = dlopen(IPC_SHR_IMAGE, 0);
    if (!Ipc_Shr_Handle) {
	errno = ENOTSUP;
	return (FAILURE);
    }

    if ((int) Ipc_GetNameInfo < 0)
	Ipc_GetNameInfo =
	    (int (*)()) dlsym(Ipc_Shr_Handle, IPC_GETNAMEINFO);
    if (!Ipc_GetNameInfo) {
	errno = ENOTSUP;
	return (FAILURE);
    }

    return (Ipc_GetNameInfo
	    (SockAddr, SockLen, NodeName, NodeLen, ServName, ServLen,
	     Flags));

}
#endif
