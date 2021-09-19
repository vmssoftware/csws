#pragma module SPS "V1.02"

/*
**
** © Copyright 2003 Hewlett-Packard Development Company, L.P.
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
**	These routines provide a simplified API for shared process sockets.
**
**  AUTHOR:
**
**      Matthew Doremus
**
**
**  CREATION DATE:   February 13, 2003
**
**  MODIFICATION HISTORY:
**
**  V1.00 	        Matthew Doremus                 13-Feb-2003
**        Initial development.
**
**  V1.01 	        Matthew Doremus                 10-Mar-2003
**        Changed to interlocked queues.
**
**  V1.02		Scott LePage			10-Mar-2006
**        Changed port field to 'host:port' to make it unique.  Handles case
**        where multiple IP addresses resolve to the same machine, which
**        had multiple Listen directives on the same port.
**        e.g.
**		Listen <hostname>:80
**		Listen locoalhost:80
**
*/

#ifndef __NEW_STARLET
#define __NEW_STARLET
#define __NEW_STARLET_SET
#endif

#include <lib$routines.h>
#include <builtins.h>
#include <descrip.h>
#include <starlet.h>
#include <iosbdef.h>
#include <iledef.h>
#include <efndef.h>
#include <lnmdef.h>
#include <dvidef.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <ssdef.h>

#include "apr.h"
#include "apr_arch_networkio.h"
#include "apr_arch_proc_mutex.h"
#include "apr_errno.h"
#include "httpd.h"
#include "http_core.h"
#include "ap_mpm.h"

extern int apr$parent_pid(void);

#include "ilemac.h"
#include "sps.h"

#ifdef  __NEW_STARLET_SET
#undef  __NEW_STARLET_SET
#undef  __NEW_STARLET
#endif

#define SPS_MAX_QUEUE_RETRY 	10

#define SPS_RES_LCK_MAX		31
#define SPS_RES_LNM_MAX		255

/* Queue header */

typedef struct _que_hdr {
    int Flink;
    int Blink;
} QUE_HDR;


/* Resource structure */

typedef struct _sps_res {
    QUE_HDR SpsResHdr;
    struct timeval SpsResTime;
    char *SpsResLnm;
    char *SpsResDev;
    char *SpsIps;
    apr_proc_mutex_t *SpsLckMutex;
    int SpsIdx;
} SPS_RES;


static QUE_HDR _align(QUADWORD) SpsResQueue =
{
0, 0};

static apr_pool_t *SpsResPool = NULL;
static int SpsLckQueIdx = 0;

static SPS_RES *apache$$sps_resource_find(char *);
static SPS_RES *apache$$sps_alloc_res(char *);
static int apache$$sps_control_process(void);
static int apache$$sps_parent_process(void);
static int apache$$sps_one_process(void);
static char *apache$$sps_server_tag(void);

extern int decc$$translate();

#ifdef TEST_SPS
static void GetInput(char *, char *, int, int *);

main(int argc, char *argv[])
{
    char *CmdFileName = "sps.com";
    apr_sockaddr_t *SockAddr;
    char *Addr = "127.0.0.1";
    apr_port_t Port = 4444;
    char Ips[256];
    apr_socket_t *Socket;
    char ContBuf[255 + 1];
    apr_pool_t *Pool;
    FILE *CmdFile;
    int status;

    GetInput("Press return to continue:", ContBuf, sizeof(ContBuf) - 1, 0);

    /* Initialize APR */
    apr_initialize();

    GetInput("Press return to continue:", ContBuf, sizeof(ContBuf) - 1, 0);

    /* Create the APR pool */
    status = apr_pool_create(&Pool, NULL);
    if (status != APR_SUCCESS) {
	printf("apr_pool_create: unable to create pool\n");
	exit(1);
    }

    GetInput("Press return to continue:", ContBuf, sizeof(ContBuf) - 1, 0);

    /* Create the APR socket address info */
    sprintf(Ips, "%s:%d", Addr, Port);
    status =
	apr_sockaddr_info_get(&SockAddr, NULL, APR_UNSPEC, Port, 0, Pool);
    if (status != APR_SUCCESS) {
	printf("Failed to set up sockaddr for %s", Addr);
	exit(1);
    }

    GetInput("Press return to continue:", ContBuf, sizeof(ContBuf) - 1, 0);

    /* Create the SPS socket */
    status =
	apache$sps_socket_create(&Socket, SockAddr->family, SOCK_STREAM,
				 Pool, Ips);
    if (status != APR_SUCCESS) {
	printf("Failed to get a socket for %s", Addr);
	exit(1);
    }

    GetInput("Press return to continue:", ContBuf, sizeof(ContBuf) - 1, 0);

    /* Bind SPS socket */
    status = apache$sps_socket_bind(Socket, SockAddr);
    if (status != APR_SUCCESS) {
	printf("Failed to bind socket for %s:%d", Addr, Port);
	exit(1);
    }

    /* Open the resource command file */
    CmdFile = fopen(CmdFileName, "w");
    if (!CmdFile) {
	printf("fopen: unable to open file %s\n", CmdFileName);
	exit(1);
    }

    /* Write the resources */
    apache$sps_write_resources(CmdFile);

    /* Close the resource command file */
    fclose(CmdFile);

    GetInput("Press return to continue:", ContBuf, sizeof(ContBuf) - 1, 0);

    /* Bind SPS socket */
    status = apache$sps_socket_close(Socket);
    if (status != APR_SUCCESS) {
	printf("Failed to close socket for %s:%d", Addr, Port);
	exit(1);
    }

    /* Open the resource command file */
    CmdFile = fopen(CmdFileName, "w");
    if (!CmdFile) {
	printf("fopen: unable to open file %s\n", CmdFileName);
	exit(1);
    }

    /* Write the resources */
    apache$sps_write_resources(CmdFile);

    /* Close the resource command file */
    fclose(CmdFile);

    GetInput("Press return to continue:", ContBuf, sizeof(ContBuf) - 1, 0);
    exit(1);
}

static void GetInput(char *Prompt, char *InputStr, int InputMax,
		     int *InputLen)
{
    char InputChr = { '\0' };
    int InputCtr = 0;

    printf("%s ", Prompt);

    while ((InputChr = fgetc(stdin)) != '\n') {
	if (InputCtr < InputMax) {
	    *InputStr = InputChr;
	    InputStr++;
	    InputCtr++;
	}
    }

    if (InputLen != 0)
	*InputLen = InputCtr;

}
#endif



apr_status_t apache$sps_socket_create(apr_socket_t ** Socket, int Family,
				      int Type, apr_pool_t * Pool,
				      char *Ips)
{
    struct dsc$descriptor SockDevDesc =
	{ 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    struct dsc$descriptor LntDesc = { 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    struct dsc$descriptor LnmDesc = { 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    char ResLnm[SPS_RES_LNM_MAX + 1];
    char ResEqv[SPS_RES_LNM_MAX + 1];
    unsigned short SockChan;
    SPS_RES *SpsResPtr;
    char ResDev[64 + 1];
    ILE3 DviItems[2], LnmItems[3], *Ile3Ptr;
    int RDlen = 0,
	SockDesc, RetryCtr, ResSize, EqvLen, status, On = 1, idx = -1, i;
    unsigned int attr = LNM$M_CASE_BLIND;
    IOSB iosb;

    /* Create the socket */

    status = apr_socket_create(Socket, Family, Type, APR_PROTO_TCP, Pool);
    if (status != APR_SUCCESS)
	return (status);

    /* If we're running as the control process and not as one process, then let's return now */

    if (apache$$sps_control_process() && !apache$$sps_one_process())
	return (status);

    /* If we're running as the parent process or as one process, then let's register the socket resource */

    if (apache$$sps_parent_process() || apache$$sps_one_process()) {
	/* Locate the socket resource, if not found create one */

	SpsResPtr = apache$$sps_resource_find(Ips);
	if (SpsResPtr == NULL) {
	    SpsResPtr = apache$$sps_alloc_res(Ips);

	    /* Insert the socket resource into the global resource queue */

	    RetryCtr = 0;
	    while (RetryCtr < SPS_MAX_QUEUE_RETRY &&
		   (status =
		    _INSQTI(SpsResPtr,
			    &SpsResQueue)) == _insqi_not_inserted)
		RetryCtr++;
	    if (RetryCtr >= SPS_MAX_QUEUE_RETRY) {
		decc$$translate(SS$_NOTQUEUED);
		return (errno);
	    }
	}

	/* Create the resource logical name string */

	sprintf(ResLnm, "APACHE$%s_SPS_%04d", apache$$sps_server_tag(),
		SpsResPtr->SpsIdx);
	if (SpsResPtr->SpsResLnm == NULL) {
	    SpsResPtr->SpsResLnm = strdup(ResLnm);
	}

	/* Get the socket channel number */

	SockChan = vaxc$get_sdc((*Socket)->socketdes);
	if (SockChan == 0)
	    return (errno);

	/* Get the Socket device name */

	ILE3_INIT(DviItems);
	ILE3_ADD(DVI$_ALLDEVNAM, sizeof(ResDev) - 1, ResDev, &RDlen);
	ILE3_TERM;

	/* Get the socket device name */

	status = SYS$GETDVIW(0, SockChan, 0, &DviItems, &iosb, 0, 0, 0);
	if (status & 1)
	    status = iosb.iosb$w_status;
	if (!(status & 1)) {
	    decc$$translate(status);
	    return (errno);
	}

	/* Null terminate the device name */

	ResDev[RDlen] = '\0';

	if (SpsResPtr->SpsResDev == NULL) {
	    SpsResPtr->SpsResDev = strdup(ResDev);
	}

	/* If we're running in one process, then we need to define the resource logical locally */

	if (apache$$sps_one_process()) {
	    /* Setup the resouce logical name table descriptor */

	    LntDesc.dsc$a_pointer = "LNM$PROCESS";
	    LntDesc.dsc$w_length = strlen(LntDesc.dsc$a_pointer);

	    /* Setup the resouce logical name descriptor */

	    LnmDesc.dsc$a_pointer = SpsResPtr->SpsResLnm;
	    LnmDesc.dsc$w_length = strlen(LnmDesc.dsc$a_pointer);

	    /* Setup the logical name items */

	    ILE3_INIT(LnmItems);
	    ILE3_ADD(LNM$_STRING, strlen(ResDev), ResDev, 0);
	    ILE3_ADD(LNM$_STRING, strlen(Ips), Ips, 0);
	    ILE3_TERM;

	    /* Define the resouce logical name in the current process */

	    status = SYS$CRELNM(0, &LntDesc, &LnmDesc, 0, &LnmItems);
	    if (!(status & 1)) {
		decc$$translate(status);
		return (errno);
	    }
	}
    } else {			/* This is the child process */

	/* Find the socket device name from logicals names */

	LntDesc.dsc$a_pointer = "LNM$PROCESS";
	LntDesc.dsc$w_length = strlen(LntDesc.dsc$a_pointer);

	for (idx = 1;; idx++) {
	    sprintf(ResLnm, "APACHE$%s_SPS_%04d", apache$$sps_server_tag(),
		    idx);
	    LnmDesc.dsc$a_pointer = ResLnm;
	    LnmDesc.dsc$w_length = strlen(LnmDesc.dsc$a_pointer);

	    i = 1;
	    EqvLen = 0;
	    ILE3_INIT(LnmItems);
	    ILE3_ADD(LNM$_INDEX, sizeof(int), &i, 0);
	    ILE3_ADD(LNM$_STRING, SPS_RES_LNM_MAX, ResEqv, &EqvLen);
	    ILE3_TERM;

	    status = SYS$TRNLNM(&attr, &LntDesc, &LnmDesc, 0, LnmItems);
	    if ((status & 1) == 0) {
		return (ENOTCONN);
	    }

	    ResEqv[EqvLen] = '\0';
	    if (strcasecmp(ResEqv, Ips) == 0) {
		i = 0;
		RDlen = 0;
		ILE3_INIT(LnmItems);
		ILE3_ADD(LNM$_INDEX, sizeof(int), &i, 0);
		ILE3_ADD(LNM$_STRING, SPS_RES_LNM_MAX, ResDev, &RDlen);
		ILE3_TERM;

		status =
		    SYS$TRNLNM(&attr, &LntDesc, &LnmDesc, 0, LnmItems);
		if ((status & 1) == 0) {
		    return (ENOTCONN);
		}
		break;
	    }
	}

	/* Assign the shared socket */

	SockDevDesc.dsc$a_pointer = ResDev;
	SockDevDesc.dsc$w_length = RDlen;
	status = SYS$ASSIGN(&SockDevDesc, &SockChan, 0, 0, 0);
	if (!(status & 1)) {
	    decc$$translate(status);
	    return (errno);
	}

	/* Convert the assigned socket channel to a descriptor */

	SockDesc = socket_fd(SockChan);
	if (SockDesc < 0) {
	    SYS$DASSGN(SockChan);
	    return (errno);
	}

	/* If there is already a socket created for this port, then close it and assign our socket */

	if ((*Socket)->socketdes)
	    close((*Socket)->socketdes);
	(*Socket)->socketdes = SockDesc;

	/* Set the device shared again */

	status = apr_socket_opt_set(*Socket, APR_VMS_SO_SHR, On);
	if (status != APR_SUCCESS)
	    return (status);
    }

    return (APR_SUCCESS);
}


apr_status_t apache$sps_socket_bind(apr_socket_t * Socket,
				    apr_sockaddr_t * SockAddr)
{
    int status;

    /* If we're the parent process or one process, then bind to the given socket resource */

    if (apache$$sps_parent_process() || apache$$sps_one_process()) {
	/* Bind the socket resource */

	status = apr_socket_bind(Socket, SockAddr);
	if (status != APR_SUCCESS)
	    return (status);
    }

    return (APR_SUCCESS);
}



apr_status_t apache$sps_socket_close(apr_socket_t * Socket)
{
    QUE_HDR _align(QUADWORD) TmpResQueue = {
    0, 0};
    char ResLnm[SPS_RES_LNM_MAX + 1];
    char *Host;
    char Ips[256];
    apr_port_t Port = 0;
    SPS_RES *FndResPtr, *SpsResPtr;
    int RetryCtr, ResSize, status;

    /* Save the socket hostname & port if we're the parent process or one process */

    if (apache$$sps_parent_process() || apache$$sps_one_process()) {
	Port = Socket->local_addr->port;
	if ((Socket->local_addr->hostname) &&
	    (strcmp(Socket->local_addr->hostname, "0.0.0.0") != 0)) {
	    Host = strdup(Socket->local_addr->hostname);
	    sprintf(Ips, "%s:%d", Host, Port);
	} else {
	    sprintf(Ips, "%d", Port);
	}
    }

    /* Close the socket resource */

    status = apr_socket_close(Socket);
    if (status != APR_SUCCESS)
	return (status);

    /* If we're the parent process or one process, then remove the socket resource it was found */

    if (apache$$sps_parent_process() || apache$$sps_one_process()) {
	/* Find the socket resource and get the logical name */

	FndResPtr = apache$$sps_resource_find(Ips);
	strcpy(ResLnm, FndResPtr->SpsResLnm);

	/* Remove the socket resource */

	RetryCtr = 0;
	while (RetryCtr < SPS_MAX_QUEUE_RETRY &&
	       (status =
		_REMQHI(&SpsResQueue, &SpsResPtr)) != _remqi_empty) {
	    /* If no entry was removed, try again */

	    if (status == _remqi_not_removed) {
		RetryCtr++;
		continue;
	    }

	    /* If this is the entry we're looking for, then free it and break out out of this loop */

	    if (SpsResPtr == FndResPtr) {
		if (SpsResPtr->SpsResLnm) {
		    free(SpsResPtr->SpsResLnm);
		    SpsResPtr->SpsResLnm = NULL;
		}
		if (SpsResPtr->SpsResDev) {
		    free(SpsResPtr->SpsResDev);
		    SpsResPtr->SpsResDev = NULL;
		}
		if (SpsResPtr->SpsLckMutex) {
		    apr_proc_mutex_cleanup(SpsResPtr->SpsLckMutex);
		    SpsResPtr->SpsLckMutex = NULL;
		}
		if (SpsResPtr->SpsIps) {
		    free(SpsResPtr->SpsIps);
		    SpsResPtr->SpsIps = NULL;
		}
		ResSize = sizeof(SPS_RES);
		LIB$FREE_VM(&ResSize, SpsResPtr);
		break;
	    }

	    /* If not the one, insert this entry into the temporary resource queue */

	    RetryCtr = 0;
	    while (RetryCtr < SPS_MAX_QUEUE_RETRY &&
		   (status =
		    _INSQHI(SpsResPtr,
			    &TmpResQueue)) == _insqi_not_inserted)
		RetryCtr++;
	    if (RetryCtr >= SPS_MAX_QUEUE_RETRY) {
		decc$$translate(SS$_NOTQUEUED);
		return (errno);
	    }
	    RetryCtr = 0;
	}

	/* Let's move any entries from the temporary resource queue back into the global resource queue */

	RetryCtr = 0;
	while (RetryCtr < SPS_MAX_QUEUE_RETRY &&
	       (status =
		_REMQHI(&TmpResQueue, &SpsResPtr)) != _remqi_empty) {
	    /* If no entry was removed, try again */

	    if (status == _remqi_not_removed) {
		RetryCtr++;
		continue;
	    }

	    /* Let's insert this entry into the global resource queue */

	    RetryCtr = 0;
	    while (RetryCtr < SPS_MAX_QUEUE_RETRY &&
		   (status =
		    _INSQHI(SpsResPtr,
			    &SpsResQueue)) == _insqi_not_inserted) {
		RetryCtr++;
		if (RetryCtr >= SPS_MAX_QUEUE_RETRY) {
		    decc$$translate(SS$_NOTQUEUED);
		    return (errno);
		}
	    }
	}
    }

    return (APR_SUCCESS);
}



apr_status_t apache$sps_port_lock(char *Ips)
{
    QUE_HDR _align(QUADWORD) TmpResQueue = {
    0, 0};
    apr_proc_mutex_t *SpsLckMutex = NULL;
    char MutexName[SPS_RES_LCK_MAX + 1];
    SPS_RES *SpsResPtr;
    SPS_RES *FndResPtr;
    int RetryCtr, ResSize, idx, rv, status;

    /* Create the resource pool (if necessary) */

    if (!SpsResPool) {
	status = apr_pool_create(&SpsResPool, NULL);
	if (status != APR_SUCCESS)
	    return (status);
    }

    /* Try to find this socket resource in the global queue */

    SpsResPtr = apache$$sps_resource_find(Ips);
    if (!SpsResPtr) {
	SpsResPtr = apache$$sps_alloc_res(Ips);
    }

    /* Build the mutex name */

    idx = SpsResPtr->SpsIdx;
    sprintf(MutexName, "APACHE$%08X_SPS_%04d", apr$parent_pid(), idx);

    /* If we didn't find this socket resource, then let's create a mutex for it */

    SpsLckMutex = SpsResPtr->SpsLckMutex;
    if (!SpsLckMutex) {
	/* Create the socket port lock */

	rv = apr_proc_mutex_create(&SpsLckMutex, MutexName,
				   APR_LOCK_VMSDLM, SpsResPool);
	if (rv != APR_SUCCESS) {
	    RetryCtr = 0;
	    while (RetryCtr < SPS_MAX_QUEUE_RETRY &&
		   (status =
		    _REMQHI(&SpsResQueue, &FndResPtr)) != _remqi_empty) {
		/* If no entry was removed, try again */

		if (status == _remqi_not_removed) {
		    RetryCtr++;
		    continue;
		}

		/* Let's insert this entry into the temporary resource queue */

		if (FndResPtr != SpsResPtr) {
		    RetryCtr = 0;
		    while (RetryCtr < SPS_MAX_QUEUE_RETRY &&
			   (status =
			    _INSQHI(SpsResPtr,
				    &TmpResQueue)) == _insqi_not_inserted)
			RetryCtr++;
		    if (RetryCtr >= SPS_MAX_QUEUE_RETRY) {
			decc$$translate(SS$_NOTQUEUED);
			return (errno);
		    }
		    RetryCtr = 0;
		}
	    }

	    ResSize = sizeof(SPS_RES);
	    LIB$FREE_VM(&ResSize, SpsResPtr);
	    return (rv);
	}

	/* Load the mutex into the resource */

	SpsResPtr->SpsLckMutex = SpsLckMutex;
    }

    /* Lock the socket port lock */
    return (apr_proc_mutex_lock(SpsLckMutex));
}


apr_status_t apache$sps_port_unlock(char *Ips)
{
    apr_proc_mutex_t *SpsLckMutex = NULL;
    SPS_RES *SpsResPtr;
    int status;

    /* Try to find this port lock in the global lock queue */

    SpsResPtr = apache$$sps_resource_find(Ips);
    if (SpsResPtr)
	SpsLckMutex = SpsResPtr->SpsLckMutex;

    /* If we couldn't find this lock to unlock it, then return an error */

    if (!SpsLckMutex)
	return (APR_EGENERAL);

    /* Unlock the socket port lock */
    return (apr_proc_mutex_unlock(SpsLckMutex));
}


int apache$sps_write_resources(FILE * FilePtr)
{
    QUE_HDR _align(QUADWORD) TmpResQueue = {
    0, 0};
    SPS_RES *SpsResPtr;
    int RetryCtr, status;

    /* Loop through the queued resources */

    RetryCtr = 0;
    while (RetryCtr < SPS_MAX_QUEUE_RETRY &&
	   (status = _REMQHI(&SpsResQueue, &SpsResPtr)) != _remqi_empty) {
	/* If no entry was removed, try again */

	if (status == _remqi_not_removed) {
	    RetryCtr++;
	    continue;
	}

	/* Let's insert this entry into the temporary resource queue */

	RetryCtr = 0;
	while (RetryCtr < SPS_MAX_QUEUE_RETRY &&
	       (status =
		_INSQHI(SpsResPtr, &TmpResQueue)) == _insqi_not_inserted)
	    RetryCtr++;
	if (RetryCtr >= SPS_MAX_QUEUE_RETRY) {
	    decc$$translate(SS$_NOTQUEUED);
	    return (errno);
	}
	RetryCtr = 0;

	/* Print the share process log resource */

	if (FilePtr)
	    if (SpsResPtr->SpsResLnm)
		fprintf(FilePtr, "$ Define/NoLog/Process %s %s,\"%s\"\n",
			SpsResPtr->SpsResLnm, SpsResPtr->SpsResDev,
			SpsResPtr->SpsIps);
    }
    if (RetryCtr >= SPS_MAX_QUEUE_RETRY) {
	decc$$translate(SS$_NOTQUEUED);
	return (errno);
    }

    /* Let's move any entries from the temporary resource queue back into the global resource queue */

    RetryCtr = 0;
    while (RetryCtr < SPS_MAX_QUEUE_RETRY &&
	   (status = _REMQHI(&TmpResQueue, &SpsResPtr)) != _remqi_empty) {
	/* If no entry was removed, try again */

	if (status == _remqi_not_removed) {
	    RetryCtr++;
	    continue;
	}

	/* Let's insert this entry into the global resource queue */

	RetryCtr = 0;
	while (RetryCtr < SPS_MAX_QUEUE_RETRY &&
	       (status =
		_INSQHI(SpsResPtr, &SpsResQueue)) == _insqi_not_inserted)
	    RetryCtr++;
	if (RetryCtr >= SPS_MAX_QUEUE_RETRY) {
	    decc$$translate(SS$_NOTQUEUED);
	    return (errno);
	}
	RetryCtr = 0;
    }
    if (RetryCtr >= SPS_MAX_QUEUE_RETRY) {
	decc$$translate(SS$_NOTQUEUED);
	return (errno);
    }

    return (0);			/* Success */
}



static SPS_RES *apache$$sps_resource_find(char *Ips)
{
    QUE_HDR _align(QUADWORD) TmpResQueue = {
    0, 0};
    SPS_RES *FndResPtr = NULL, *SpsResPtr;
    int RetryCtr, status;

    /* Locate the host/port */

    RetryCtr = 0;
    while (RetryCtr < SPS_MAX_QUEUE_RETRY &&
	   (status = _REMQHI(&SpsResQueue, &SpsResPtr)) != _remqi_empty) {
	/* If no entry was removed, try again */

	if (status == _remqi_not_removed) {
	    RetryCtr++;
	    continue;
	}

	/* Let's insert this entry into the temporary resource queue */

	RetryCtr = 0;
	while (RetryCtr < SPS_MAX_QUEUE_RETRY &&
	       (status =
		_INSQHI(SpsResPtr, &TmpResQueue)) == _insqi_not_inserted)
	    RetryCtr++;
	if (RetryCtr >= SPS_MAX_QUEUE_RETRY)
	    exit(SS$_NOTQUEUED);
	RetryCtr = 0;

	/* Compare the hostname/port to the current resource in the queue */

	if (strcasecmp(SpsResPtr->SpsIps, Ips) == 0) {
	    FndResPtr = SpsResPtr;
	    break;
	}
    }
    if (RetryCtr >= SPS_MAX_QUEUE_RETRY)
	exit(SS$_NOTQUEUED);

    /* Let's move any entries from the temporary resource queue back into the global resource queue */

    RetryCtr = 0;
    while (RetryCtr < SPS_MAX_QUEUE_RETRY &&
	   (status = _REMQHI(&TmpResQueue, &SpsResPtr)) != _remqi_empty) {
	/* If no entry was removed, try again */

	if (status == _remqi_not_removed) {
	    RetryCtr++;
	    continue;
	}

	/* Let's insert this entry into the global resource queue */

	RetryCtr = 0;
	while (RetryCtr < SPS_MAX_QUEUE_RETRY &&
	       (status =
		_INSQHI(SpsResPtr, &SpsResQueue)) == _insqi_not_inserted)
	    RetryCtr++;
	if (RetryCtr >= SPS_MAX_QUEUE_RETRY)
	    exit(SS$_NOTQUEUED);
	RetryCtr = 0;
    }
    if (RetryCtr >= SPS_MAX_QUEUE_RETRY)
	exit(SS$_NOTQUEUED);

    return (FndResPtr);
}



static SPS_RES *apache$$sps_alloc_res(char *Ips)
{
    int ResSize;
    int status;
    int idx;
    int RetryCtr;
    SPS_RES *SpsResPtr;

    /* Allocate the socket resource */

    ResSize = sizeof(SPS_RES);
    status = LIB$GET_VM(&ResSize, &SpsResPtr);
    if (!(status & 1))
	exit(status);

    /* Load the socket resource data */

    memset(SpsResPtr, 0, sizeof(SPS_RES));
    gettimeofday(&SpsResPtr->SpsResTime, NULL);
    SpsLckQueIdx += 1;
    idx = SpsLckQueIdx;
    SpsResPtr->SpsIdx = idx;
    SpsResPtr->SpsIps = strdup(Ips);

    /* Let's insert this entry into the global resource queue */

    RetryCtr = 0;
    while (RetryCtr < SPS_MAX_QUEUE_RETRY &&
	   (status =
	    _INSQHI(SpsResPtr, &SpsResQueue)) == _insqi_not_inserted) {
	RetryCtr++;
	if (RetryCtr >= SPS_MAX_QUEUE_RETRY)
	    exit(SS$_NOTQUEUED);
    }

    return (SpsResPtr);
}



static int apache$$sps_control_process(void)
{
#ifdef TEST_SPS
    static int ControlProcess = 1;
#else
    static int ControlProcess = -1;
#endif

    /* Establish if we're running as control process */

    if (ControlProcess < 0)
	ap_mpm_query(AP_MPMQ_IS_CONTROL_PROCESS, &ControlProcess);

    /* Return the control process indicator */
    return (ControlProcess);
}



static int apache$$sps_parent_process(void)
{
#ifdef TEST_SPS
    static int ParentProcess = 1;
#else
    static int ParentProcess = -1;
#endif

    /* Establish if we're running as parent process */

    if (ParentProcess < 0)
	ap_mpm_query(AP_MPMQ_IS_PARENT_PROCESS, &ParentProcess);

    /* Return the parent process indicator */
    return (ParentProcess);
}



static int apache$$sps_one_process(void)
{
#ifdef TEST_SPS
    static int OneProcess = 1;
#else
    static int OneProcess = -1;
#endif

    /* Establish if we're running as one process */

    if (OneProcess < 0)
	OneProcess = ap_exists_config_define("DEBUG") ||
	    ap_exists_config_define("ONE_PROCESS");

    /* Return the one process indicator */
    return (OneProcess);
}



static char *apache$$sps_server_tag(void)
{
    static char *ServerTag = NULL;
    int status;

    /* Determine the server tag */

    if (!ServerTag) {
	char *TmpTag = NULL;

	status = ap_mpm_query(AP_MPMQ_SERVER_TAG, (int *) &TmpTag);
	if (status == APR_SUCCESS)
	    ServerTag = strdup(TmpTag);
	else
	    ServerTag = "";
    }

    /* Return the address of the server tag */
    return (ServerTag);
}
