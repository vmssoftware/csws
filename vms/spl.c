#pragma module SPL "V1.01"

/* Important! We must use standard stat here because we're doing
** an fstat() call to get a mailbox name (which comes back in the
** std_dev field). */

#ifdef _USE_STD_STAT
#undef _USE_STD_STAT
#endif

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
**	These routines provide a simplified API for shared process logs.
**
**  AUTHOR:
**
**      Matthew Doremus
**
**
**  CREATION DATE:   February 3, 2003
**
**  MODIFICATION HISTORY:
**
**  V1.00 	        Matthew Doremus                 03-Feb-2003
**        Initial development.
**
**  V1.01 	        Matthew Doremus                 10-Mar-2003
**        Changed to interlocked queues.
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
#include <fcntl.h>
#include <ssdef.h>
#include <iodef.h>
#include <stat.h>

#include "apr.h"
#include "apr_arch_networkio.h"
#include "apr_arch_file_io.h"
#include "apr_strings.h"
#include "apr_errno.h"
#include "httpd.h"
#include "http_core.h"
#include "ap_mpm.h"

extern int apr$mbx_create(unsigned short int *MbxChan, ...);
extern int apr$mbx_delete(unsigned short int MbxChan);

#include "ilemac.h"
#include "spl.h"

#ifdef  __NEW_STARLET_SET
#undef  __NEW_STARLET_SET
#undef  __NEW_STARLET
#endif

/* Maximum queue retry count */
#define SPL_MAX_QUEUE_RETRY 	10

/* Resource types */
#define SPL_RES_FILE		1
#define SPL_RES_PIPE		2
#define SPL_RES_SOCK		3

#define SPL_RES_LNM_MAX		256

/* Message types */
#define SPL_MSG_WRITE		1
#define SPL_MSG_CLOSE		2
#define SPL_MSG_FLUSH		3
#define SPL_MSG_NEW		4


/* Defaults for message buffer size, message buffer count, and message flush interval */
#define SPL_MSG_BUF_MAX		1024
#define SPL_MSG_BUF_CNT		10
#define SPL_MSG_FLUSH_INTERVAL	256


/* Queue header */
typedef struct _que_hdr {
    int Flink;
    int Blink;
} QUE_HDR;


/* Resource structure */
typedef struct _spl_res {
    QUE_HDR SplResHdr;
    int SplResType;
    struct timeval SplResTime;
    char *SplResLnm;
    char *SplResDev;
    apr_file_t *SplResFile;
    __int64 SplResWrites;
    unsigned short int SplResMbxChan;
    IOSB SplResMbxIosb;
} SPL_RES;


/* Message structure */
typedef struct _spl_msg {
    QUE_HDR SplMsgHdr;
    int SplMsgType;
    struct timeval SplMsgTime;
    unsigned int SplMsgLen;
    char *SplMsgBuf;
    SPL_RES *SplResPtr;
} SPL_MSG;


static QUE_HDR _align(QUADWORD) SplResQueue =
{
0, 0};

static QUE_HDR _align(QUADWORD) SplMsgQueue =
{
0, 0};

static apr_pool_t *SplResPool = NULL;
static char *SplMsgBuf = NULL;


static SPL_RES *apache$$spl_resource_find(char *);
static void apache$$spl_resource_ast(SPL_RES *);
static int apache$$spl_compare_time(struct timeval *, struct timeval *);
static int apache$$spl_parent_process(void);
static int apache$$spl_one_process(void);
static int apache$$spl_max_message(void);
static int apache$$spl_max_buffers(void);
static int apache$$spl_flush_interval(void);
static char *apache$$spl_server_tag(void);
static int apache$$spl_disabled(void);

extern int decc$$translate();

#ifdef TEST_SPL
main(int argc, char *argv[])
{
    char *TestStr1 = "This is test string 1";
    char *TestStr2 = "This is test string 2";
    char *LogFileName = "spl.log";
    char *TmpFileName = "spl.tmp";
    char *CmdFileName = "spl.res";
    apr_file_t *LogFile;
    apr_file_t *TmpFile;
    apr_pool_t *pool;
    apr_size_t Bytes;
    FILE *CmdFile;
    int status;

    /* Initialize the APR environment */
    apr_initialize();

   /* Create the APR pool */
    status = apr_pool_create(&pool, NULL);
    if (status != APR_SUCCESS) {
	printf("apr_pool_create: unable to create pool\n");
	exit(1);
    }

    /* Open the log */
    status = apache$spl_file_open(&LogFile, LogFileName,
				  APR_APPEND | APR_READ | APR_WRITE |
				  APR_CREATE, APR_OS_DEFAULT, pool);
    if (status != APR_SUCCESS) {
	printf("apr_file_open: unable to open file %s\n", LogFileName);
	exit(1);
    }

    /* Write to the log file */
    Bytes = strlen(TestStr2);
    status = apr_file_write(LogFile, TestStr2, &Bytes);
    if (status != APR_SUCCESS) {
	printf("apr_file_write: unable to write to file %s\n",
	       LogFileName);
	exit(1);
    }

    /* Insert a Flush message */
    apache$spl_flush();

    /* Write to the log file */
    Bytes = strlen(TestStr1);
    status = apr_file_write(LogFile, TestStr1, &Bytes);
    if (status != APR_SUCCESS) {
	printf("apr_file_write: unable to write to file %s\n",
	       LogFileName);
	exit(1);
    }

    /* Insert a New message */
    apache$spl_new();

    /* Write to the log file */
    Bytes = strlen(TestStr1);
    status = apr_file_write(LogFile, TestStr1, &Bytes);
    if (status != APR_SUCCESS) {
	printf("apr_file_write: unable to write to file %s\n",
	       LogFileName);
	exit(1);
    }

    /* Insert a Flush message */
    apache$spl_flush();

    /* Write to the log file */
    Bytes = strlen(TestStr2);
    status = apr_file_write(LogFile, TestStr2, &Bytes);
    if (status != APR_SUCCESS) {
	printf("apr_file_write: unable to write to file %s\n",
	       LogFileName);
	exit(1);
    }

    /* Close the log file */
    apache$spl_file_close(LogFile);

    /* Open the log */
    status = apache$spl_file_open(&TmpFile, TmpFileName,
				  APR_APPEND | APR_READ | APR_WRITE |
				  APR_CREATE, APR_OS_DEFAULT, pool);
    if (status != APR_SUCCESS) {
	printf("apr_file_open: unable to open file %s\n", TmpFileName);
	exit(1);
    }

    /* Write to the log file */
    Bytes = strlen(TestStr1);
    status = apr_file_write(TmpFile, TestStr1, &Bytes);
    if (status != APR_SUCCESS) {
	printf("apr_file_write: unable to write to file %s\n",
	       TmpFileName);
	exit(1);
    }

    /* Close the tmp file */
    apache$spl_file_close(TmpFile);

    /* Open the resource command file */
    CmdFile = fopen(CmdFileName, "w");
    if (!CmdFile) {
	printf("fopen: unable to open file %s\n", CmdFileName);
	exit(1);
    }

    /* Write the resources */
    apache$spl_write_resources(CmdFile);

    /* Close the resource command file */
    fclose(CmdFile);

    /* Write the messages */
    apache$spl_write_messages();

    /* Open the resource command file */
    CmdFile = fopen(CmdFileName, "w");
    if (!CmdFile) {
	printf("fopen: unable to open file %s\n", CmdFileName);
	exit(1);
    }

    /* Write the resources */
    apache$spl_write_resources(CmdFile);

    /* Close the resource command file and exit */
    fclose(CmdFile);
    exit(1);
}
#endif



apr_status_t apache$spl_proc_create(apr_proc_t * new, const char *progname,
				    const char *const *args,
				    const char *const *env,
				    apr_procattr_t * attr,
				    apr_pool_t * pool)
{
    struct dsc$descriptor LntDesc = { 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    struct dsc$descriptor LnmDesc = { 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    char ResLnm[SPL_RES_LNM_MAX + 1];
    struct stat ResStat;
    SPL_RES *SplResPtr;
    ILE3 LnmItems[2], *Ile3Ptr;
    int RetryCtr, ResSize, status;

    /* If shared process logging is disabled, then simply return with APR call */
    if (apache$$spl_disabled()) {
	return (apr_proc_create(new, progname, args, env, attr, pool));
    }

    /* Get the resource statistics */

    status = stat(progname, &ResStat);
    if (status)
	return (status);

    /* Get the resource logical name */

    sprintf(ResLnm, "APACHE$%s_SPL_%s_%04X%04X%04X",
	    apache$$spl_server_tag(), ResStat.st_dev,
	    ResStat.st_ino[0], ResStat.st_ino[1], ResStat.st_ino[2]);

    /* If we're the parent or running in one process, then we need to register the resource. */

    if (apache$$spl_parent_process() || apache$$spl_one_process()) {
	/* Create the piped process */

	status = apr_proc_create(new, progname, args, env, attr, pool);
	if (status != APR_SUCCESS)
	    return (status);

	/* Get the resource mailbox name */

	status = fstat(new->in->filedes, &ResStat);
	if (status) {
	    return (status);
	}

	/* Setup the resouce logical name table descriptor */

	LntDesc.dsc$a_pointer = "LNM$PROCESS";
	LntDesc.dsc$w_length = strlen(LntDesc.dsc$a_pointer);

	/* Setup the resouce logical name descriptor */

	LnmDesc.dsc$a_pointer = ResLnm;
	LnmDesc.dsc$w_length = strlen(LnmDesc.dsc$a_pointer);

	/* Setup the logical name translation items */
	ILE3_INIT(LnmItems);
	ILE3_ADD(LNM$_STRING, strlen(ResStat.st_dev), ResStat.st_dev, 0);
	ILE3_TERM;

	/* Define the resouce logical name in the current process */
	status = SYS$CRELNM(0, &LntDesc, &LnmDesc, 0, &LnmItems);
	if (!(status & 1)) {
	    decc$$translate(status);
	    return (errno);
	}

	/* Find the associated file resource */

	SplResPtr = apache$$spl_resource_find(ResLnm);
	if (SplResPtr) {
	    gettimeofday(&SplResPtr->SplResTime, NULL);
	    if (SplResPtr->SplResLnm)
		free(SplResPtr->SplResLnm);
	    SplResPtr->SplResLnm = strdup(ResLnm);
	    if (SplResPtr->SplResDev)
		free(SplResPtr->SplResDev);
	    SplResPtr->SplResDev = strdup(ResStat.st_dev);
	} else {
	    /* Allocate the logging resource */

	    ResSize = sizeof(SPL_RES);
	    status = LIB$GET_VM(&ResSize, &SplResPtr);
	    if (!(status & 1)) {
		decc$$translate(status);
		return (errno);
	    }

	    /* Load the logging resource data */

	    memset(SplResPtr, 0, sizeof(SPL_RES));
	    SplResPtr->SplResType = SPL_RES_PIPE;
	    gettimeofday(&SplResPtr->SplResTime, NULL);
	    SplResPtr->SplResLnm = strdup(ResLnm);
	    SplResPtr->SplResDev = strdup(ResStat.st_dev);
	    SplResPtr->SplResMbxChan = 0;
	    SplResPtr->SplResFile = NULL;
	    SplResPtr->SplResWrites = 0;

	    /* Insert the logging resource into the global resource queue */

	    RetryCtr = 0;
	    while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
		   (status =
		    _INSQTI(SplResPtr,
			    &SplResQueue)) == _insqi_not_inserted)
		RetryCtr++;
	    if (RetryCtr >= SPL_MAX_QUEUE_RETRY) {
		if (SplResPtr->SplResLnm)
		    free(SplResPtr->SplResLnm);
		if (SplResPtr->SplResDev)
		    free(SplResPtr->SplResDev);
		LIB$FREE_VM(&ResSize, SplResPtr);
		decc$$translate(SS$_NOTQUEUED);
		return (errno);
	    }
	}
    } else {
	/* Open the pipe resource using APR */
	status = apr_file_open(&new->in, ResLnm,
			       APR_APPEND | APR_READ | APR_WRITE |
			       APR_CREATE, APR_OS_DEFAULT, pool);
	if (status != APR_SUCCESS)
	    return (status);
    }

    return (APR_SUCCESS);
}


apr_status_t apache$spl_socket_open(apr_file_t ** File,
				    const char *AddrPort,
				    apr_pool_t * pool)
{
    struct dsc$descriptor SockDevDesc =
	{ 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    struct dsc$descriptor LntDesc = { 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    struct dsc$descriptor LnmDesc = { 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    apr_interval_time_t SockTimeout = apr_time_from_sec(15);
    char ResLnm[SPL_RES_LNM_MAX + 1];
    apr_sockaddr_t *SockAddr = NULL;
    apr_socket_t *Sock = NULL;
    unsigned short SockChan;
    SPL_RES *SplResPtr;
    char ResDev[64 + 1];
    ILE3 DviItems[2], LnmItems[2], *Ile3Ptr;
    int ResDevLen = 0, RetryCtr, ResSize, status, Port;
    IOSB iosb;
    char *Addr, *Ptr;

    /* Get the resource logical name */

    sprintf(ResLnm, "APACHE$%s_SPL_%s", apache$$spl_server_tag(),
	    AddrPort);

    /* If we're the parent or running in one process, then we need to register the resource */

    if (apache$$spl_parent_process() || apache$$spl_one_process()) {
	/* Parse to the socket address/port */

	Addr = apr_pstrdup(pool, AddrPort);
	Ptr = strchr(Addr, ':');
	if (!Ptr)
	    return APR_EINVAL;
	*Ptr = '\0';
	Ptr++;
	Port = atoi(Ptr);

	/* Establish the socket address */

	status =
	    apr_sockaddr_info_get(&SockAddr, Addr, APR_UNSPEC, Port, 0,
				  pool);
	if (status != APR_SUCCESS)
	    return (status);

	/* Create a new socket */

	status =
	    apr_socket_create(&Sock, APR_UNSPEC, SOCK_STREAM,
			      APR_PROTO_TCP, pool);
	if (status != APR_SUCCESS)
	    return (status);

	/* Set the socket timeout */

	status = apr_socket_timeout_set(Sock, SockTimeout);
	if (status != APR_SUCCESS) {
	    apr_socket_close(Sock);
	    return (status);
	}

	/* Connect to the socket */

	status = apr_socket_connect(Sock, SockAddr);
	if (status != APR_SUCCESS) {
	    apr_socket_close(Sock);
	    return (status);
	}

	/* Set the socket options */

	status = apr_socket_opt_set(Sock, APR_SO_REUSEADDR, 1);
	if (status != APR_SUCCESS) {
	    apr_socket_close(Sock);
	    return (status);
	}
	status = apr_socket_opt_set(Sock, APR_VMS_SO_SHR, 1);
	if (status != APR_SUCCESS) {
	    apr_socket_close(Sock);
	    return (status);
	}

	/* Get the socket channel number */

	SockChan = vaxc$get_sdc(Sock->socketdes);
	if (SockChan == 0)
	    return (errno);

	/* Get the Socket device name */

	ILE3_INIT(DviItems);
	ILE3_ADD(DVI$_ALLDEVNAM, sizeof(ResDev) - 1, ResDev, &ResDevLen);
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

	ResDev[ResDevLen] = '\0';

	/* Setup the resouce logical name table descriptor */

	LntDesc.dsc$a_pointer = "LNM$PROCESS";
	LntDesc.dsc$w_length = strlen(LntDesc.dsc$a_pointer);

	/* Setup the resouce logical name descriptor */

	LnmDesc.dsc$a_pointer = ResLnm;
	LnmDesc.dsc$w_length = strlen(LnmDesc.dsc$a_pointer);

	/* Setup the logical name translation items */

	ILE3_INIT(LnmItems);
	ILE3_ADD(LNM$_STRING, strlen(ResDev), ResDev, 0);
	ILE3_TERM;

	/* Define the resouce logical name in the current process */

	status = SYS$CRELNM(0, &LntDesc, &LnmDesc, 0, &LnmItems);
	if (!(status & 1)) {
	    decc$$translate(status);
	    return (errno);
	}

	/* Find the associated file resource */

	SplResPtr = apache$$spl_resource_find(ResLnm);
	if (SplResPtr) {
	    gettimeofday(&SplResPtr->SplResTime, NULL);
	    if (SplResPtr->SplResLnm)
		free(SplResPtr->SplResLnm);
	    SplResPtr->SplResLnm = strdup(ResLnm);
	    if (SplResPtr->SplResDev)
		free(SplResPtr->SplResDev);
	    SplResPtr->SplResDev = strdup(ResDev);
	} else {
	    /* Allocate the logging resource */

	    ResSize = sizeof(SPL_RES);
	    status = LIB$GET_VM(&ResSize, &SplResPtr);
	    if (!(status & 1)) {
		decc$$translate(status);
		return (errno);
	    }

	    /* Load the logging resource data */

	    memset(SplResPtr, 0, sizeof(SPL_RES));
	    SplResPtr->SplResType = SPL_RES_SOCK;
	    gettimeofday(&SplResPtr->SplResTime, NULL);
	    SplResPtr->SplResLnm = strdup(ResLnm);
	    SplResPtr->SplResDev = strdup(ResDev);
	    SplResPtr->SplResMbxChan = 0;
	    SplResPtr->SplResFile = NULL;
	    SplResPtr->SplResWrites = 0;

	    /* Insert the logging resource into the global resource queue */

	    RetryCtr = 0;
	    while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
		   (status =
		    _INSQTI(SplResPtr,
			    &SplResQueue)) == _insqi_not_inserted)
		RetryCtr++;
	    if (RetryCtr >= SPL_MAX_QUEUE_RETRY) {
		if (SplResPtr->SplResLnm)
		    free(SplResPtr->SplResLnm);
		if (SplResPtr->SplResDev)
		    free(SplResPtr->SplResDev);
		LIB$FREE_VM(&ResSize, SplResPtr);
		decc$$translate(SS$_NOTQUEUED);
		return (errno);
	    }
	}
    } else {
	/* Create a new socket */
	status =
	    apr_socket_create(&Sock, APR_UNSPEC, SOCK_STREAM,
			      APR_PROTO_TCP, pool);
	if (status != APR_SUCCESS)
	    return (status);

	/* Setup the socket device descriptor */

	SockDevDesc.dsc$a_pointer = ResLnm;
	SockDevDesc.dsc$w_length = strlen(SockDevDesc.dsc$a_pointer);

	/* Assign the shared socket */

	status = SYS$ASSIGN(&SockDevDesc, &SockChan, 0, 0, 0);
	if (!(status & 1)) {
	    decc$$translate(status);
	    return (errno);
	}

	/* Convert the assigned socket channel to a descriptor */

	Sock->socketdes = socket_fd(SockChan);
	if (Sock->socketdes < 0) {
	    SYS$DASSGN(SockChan);
	    return (errno);
	}

	/* Set the device shared again */

	status = apr_socket_opt_set(Sock, APR_VMS_SO_SHR, 1);
	if (status != APR_SUCCESS)
	    return (status);
    }

    /* Open a file to the null device */

    status = apr_file_open(File, "NL:", APR_WRITE, APR_OS_DEFAULT, pool);
    if (status != APR_SUCCESS)
	return (status);

    /* Close the file descriptor and update the file structure with the socket information */

    close((*File)->filedes);
    (*File)->filedes = Sock->socketdes;
    (*File)->fname = apr_pstrdup(pool, AddrPort);
    (*File)->is_pipe = TRUE;
    (*File)->is_socket = TRUE;

    return (APR_SUCCESS);
}


apr_status_t apache$spl_file_open(apr_file_t ** new, const char *fname,
				  apr_int32_t flag, apr_fileperms_t perm,
				  apr_pool_t * pool)
{
    struct dsc$descriptor LntDesc = { 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    struct dsc$descriptor LnmDesc = { 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    unsigned short int ResDevChan = 0;
    char ResLnm[SPL_RES_LNM_MAX + 1];
    apr_file_t *ResFile = NULL;
    SPL_RES *SplResPtr = NULL;
    struct stat ResStat;
    char ResDev[64 + 1];
    int ResDesc = -1;
    ILE3 DviItems[2], LnmItems[2], *Ile3Ptr;
    int ResDevLen = 0, RetryCtr, ResSize, status;
    IOSB iosb;

    /* If shared process logging is disabled, then simply return with APR call */

    if (apache$$spl_disabled()) {
	return (apr_file_open(new, fname, flag, perm, pool));
    }

    /* Create the resource pool (if necessary) */

    if (!SplResPool) {
	status = apr_pool_create(&SplResPool, NULL);
	if (status != APR_SUCCESS)
	    return (status);
    }

    /* If we're the parent or running in one process, then we need to register the actual file. */

    if (apache$$spl_parent_process() || apache$$spl_one_process()) {
	/* Get the resource statistics */

	status = stat(fname, &ResStat);
	if (status == 0) {
	    /* Get the resource logical name */
	    sprintf(ResLnm, "APACHE$%s_SPL_%s_%04X%04X%04X",
		    apache$$spl_server_tag(), ResStat.st_dev,
		    ResStat.st_ino[0], ResStat.st_ino[1],
		    ResStat.st_ino[2]);

	    /* Find the associated file resource */

	    SplResPtr = apache$$spl_resource_find(ResLnm);

	    if (SplResPtr) {
		/* Open the file using APR */
		status =
		    apr_file_open(new, ResLnm, flag, perm, SplResPool);
		if (status != APR_SUCCESS)
		    return (status);

		return (APR_SUCCESS);
	    }
	}

	/* Open the file using APR */

	status = apr_file_open(&ResFile, fname, flag, perm, SplResPool);
	if (status != APR_SUCCESS)
	    return (status);

	/* Get the resource file info */

	status = fstat(ResFile->filedes, &ResStat);
	if (status) {
	    apr_file_close(ResFile);
	    return (status);
	}

	/* Generate the resource logical name */

	sprintf(ResLnm, "APACHE$%s_SPL_%s_%04X%04X%04X",
		apache$$spl_server_tag(), ResStat.st_dev,
		ResStat.st_ino[0], ResStat.st_ino[1], ResStat.st_ino[2]);

	/* Allocate the resource mailbox message buffer (if necessary) */
	if (!SplMsgBuf) {
	    SplMsgBuf = malloc(apache$$spl_max_message());
	    if (!SplMsgBuf) {
		apr_file_close(ResFile);
		return (errno);
	    }
	}

	/* Create the resource mailbox */

	status = apr$mbx_create(&ResDevChan,
				apache$$spl_max_message(),
				apache$$spl_max_message() *
				apache$$spl_max_buffers());
	if (!(status & 1)) {
	    apr_file_close(ResFile);
	    decc$$translate(status);
	    return (errno);
	}

	/* Get the Mailbox Unit Number */

	ILE3_INIT(DviItems);
	ILE3_ADD(DVI$_ALLDEVNAM, sizeof(ResDev) - 1, ResDev, &ResDevLen);
	ILE3_TERM;

	/* Get the resource mailbox name */

	status = SYS$GETDVIW(EFN$C_ENF,
			     ResDevChan, 0, &DviItems, &iosb, 0, 0, 0);
	if (status & 1)
	    status = iosb.iosb$w_status;
	if (!(status & 1)) {
	    apr_file_close(ResFile);
	    apr$mbx_delete(ResDevChan);
	    decc$$translate(status);
	    return (errno);
	}

	/* Null terminate the device name */

	ResDev[ResDevLen] = '\0';

	/* Setup the resouce logical name table descriptor */

	LntDesc.dsc$a_pointer = "LNM$PROCESS";
	LntDesc.dsc$w_length = strlen(LntDesc.dsc$a_pointer);

	/* Setup the resouce logical name descriptor */

	LnmDesc.dsc$a_pointer = ResLnm;
	LnmDesc.dsc$w_length = strlen(LnmDesc.dsc$a_pointer);

	/* Setup the logical name translation items */

	ILE3_INIT(LnmItems);
	ILE3_ADD(LNM$_STRING, strlen(ResDev), ResDev, 0);
	ILE3_TERM;

	/* Define the resouce logical name in the current process */

	status = SYS$CRELNM(0, &LntDesc, &LnmDesc, 0, &LnmItems);
	if (!(status & 1)) {
	    apr_file_close(ResFile);
	    apr$mbx_delete(ResDevChan);
	    decc$$translate(status);
	    return (errno);
	}

	/* Allocate the logging resource */

	ResSize = sizeof(SPL_RES);
	status = LIB$GET_VM(&ResSize, &SplResPtr);
	if (!(status & 1)) {
	    apr_file_close(ResFile);
	    apr$mbx_delete(ResDevChan);
	    decc$$translate(status);
	    return (errno);
	}

	/* Load the logging resource data */

	memset(SplResPtr, 0, sizeof(SPL_RES));
	SplResPtr->SplResType = SPL_RES_FILE;
	gettimeofday(&SplResPtr->SplResTime, NULL);
	SplResPtr->SplResLnm = strdup(ResLnm);
	SplResPtr->SplResDev = strdup(ResDev);
	SplResPtr->SplResMbxChan = ResDevChan;
	SplResPtr->SplResFile = ResFile;
	SplResPtr->SplResWrites = 0;

	/* Insert the logging resource into the global resource queue */

	RetryCtr = 0;
	while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
	       (status =
		_INSQTI(SplResPtr, &SplResQueue)) == _insqi_not_inserted)
	    RetryCtr++;
	if (RetryCtr >= SPL_MAX_QUEUE_RETRY) {
	    apr_file_close(ResFile);
	    apr$mbx_delete(ResDevChan);
	    if (SplResPtr->SplResLnm)
		free(SplResPtr->SplResLnm);
	    if (SplResPtr->SplResDev)
		free(SplResPtr->SplResDev);
	    LIB$FREE_VM(&ResSize, SplResPtr);
	    decc$$translate(SS$_NOTQUEUED);
	    return (errno);
	}

	/* Establish the logging resource mailbox AST */

	status = SYS$QIO(EFN$C_ENF,
			 SplResPtr->SplResMbxChan,
			 IO$_SETMODE | IO$M_WRTATTN,
			 &SplResPtr->SplResMbxIosb,
			 0, 0,
			 (void *) &apache$$spl_resource_ast,
			 (int) SplResPtr, 0, 0, 0, 0);
	if (!(status & 1)) {
	    apr_file_close(ResFile);
	    apr$mbx_delete(ResDevChan);
	    if (SplResPtr->SplResLnm)
		free(SplResPtr->SplResLnm);
	    if (SplResPtr->SplResDev)
		free(SplResPtr->SplResDev);
	    LIB$FREE_VM(&ResSize, SplResPtr);
	    decc$$translate(status);
	    return (errno);
	}

	/* Open the file resource using APR */

	status = apr_file_open(new, ResLnm, flag, perm, pool);
	if (status != APR_SUCCESS) {
	    apr_file_close(ResFile);
	    apr$mbx_delete(ResDevChan);
	    if (SplResPtr->SplResLnm)
		free(SplResPtr->SplResLnm);
	    if (SplResPtr->SplResDev)
		free(SplResPtr->SplResDev);
	    LIB$FREE_VM(&ResSize, SplResPtr);
	    return (status);
	}
    } else {
	/* Get the resource statistics */

	status = stat(fname, &ResStat);
	if (status) {
	    return (status);
        }

	/* Get the resource logical name */

	sprintf(ResLnm, "APACHE$%s_SPL_%s_%04X%04X%04X",
		apache$$spl_server_tag(), ResStat.st_dev,
		ResStat.st_ino[0], ResStat.st_ino[1], ResStat.st_ino[2]);

	/* Open the file resource using APR */

	status = apr_file_open(new, ResLnm, flag, perm, pool);
	if (status != APR_SUCCESS) {
	    return (status);
        }
    }

    return (APR_SUCCESS);
}


apr_status_t apache$spl_file_close(apr_file_t * file)
{
    SPL_RES *SplResPtr = NULL;
    SPL_MSG *SplMsgPtr = NULL;
    int RetryCtr, MsgSize, status;

    /* If shared process logging is disabled, then simply return with APR call */

    if (apache$$spl_disabled()) {
	return (apr_file_close(file));
    }

    /* If we're the parent or running in one process, then we need to insert the resouce close message into the
       queue so we can close the actual file itself. */

    if (apache$$spl_parent_process() || apache$$spl_one_process()) {

	/* Find the logging resource for the file to be closed */

	SplResPtr = apache$$spl_resource_find(file->fname);
	if (SplResPtr) {
	    /* Allocate the logging message buffer */

	    MsgSize = sizeof(SPL_MSG);
	    status = LIB$GET_VM(&MsgSize, &SplMsgPtr);
	    if (!(status & 1)) {
		decc$$translate(status);
		return (errno);
	    }

	    /* Load the logging message buffer */

	    memset(SplMsgPtr, 0, sizeof(SPL_MSG));
	    SplMsgPtr->SplMsgType = SPL_MSG_CLOSE;
	    gettimeofday(&SplMsgPtr->SplMsgTime, NULL);
	    SplMsgPtr->SplMsgLen = strlen(file->fname);
	    SplMsgPtr->SplMsgBuf = strdup(file->fname);
	    SplMsgPtr->SplResPtr = SplResPtr;

	    /* Insert the logging message into the global message queue */

	    RetryCtr = 0;
	    while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
		   (status =
		    _INSQTI(SplMsgPtr,
			    &SplMsgQueue)) == _insqi_not_inserted)
		RetryCtr++;
	    if (RetryCtr >= SPL_MAX_QUEUE_RETRY) {
		if (SplMsgPtr->SplMsgBuf)
		    free(SplMsgPtr->SplMsgBuf);
		LIB$FREE_VM(&MsgSize, SplMsgPtr);
		decc$$translate(SS$_NOTQUEUED);
		return (errno);
	    }
	}
    }

    /* Close the file resource using APR */

    status = apr_file_close(file);
    if (status != APR_SUCCESS)
	return (status);

    return (APR_SUCCESS);
}



int apache$spl_write_resources(FILE * FilePtr)
{
    QUE_HDR _align(QUADWORD) TmpResQueue = {
    0, 0};
    SPL_RES *SplResPtr;
    int RetryCtr, status;

    /* Loop through the queued resources */

    RetryCtr = 0;
    while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
	   (status = _REMQHI(&SplResQueue, &SplResPtr)) != _remqi_empty) {

	/* If no entry was removed, try again */

	if (status == _remqi_not_removed) {
	    RetryCtr++;
	    continue;
	}

	/* Let's insert this entry into the temporary resource queue */

	RetryCtr = 0;
	while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
	       (status =
		_INSQHI(SplResPtr, &TmpResQueue)) == _insqi_not_inserted)
	    RetryCtr++;
	if (RetryCtr >= SPL_MAX_QUEUE_RETRY) {
	    decc$$translate(SS$_NOTQUEUED);
	    return (errno);
	}
	RetryCtr = 0;

	if (FilePtr)
	    fprintf(FilePtr, "$ Define/NoLog/Process %s %s\n",
		    SplResPtr->SplResLnm, SplResPtr->SplResDev);
    }
    if (RetryCtr >= SPL_MAX_QUEUE_RETRY) {
	decc$$translate(SS$_NOTQUEUED);
	return (errno);
    }

    /* Let's move any entries from the temporary resource queue back into the global resource queue. */

    RetryCtr = 0;
    while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
	   (status = _REMQHI(&TmpResQueue, &SplResPtr)) != _remqi_empty) {

	/* If no entry was removed, try again */

	if (status == _remqi_not_removed) {
	    RetryCtr++;
	    continue;
	}

	/* Let's insert this entry into the global resource queue */

	RetryCtr = 0;
	while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
	       (status =
		_INSQHI(SplResPtr, &SplResQueue)) == _insqi_not_inserted)
	    RetryCtr++;
	if (RetryCtr >= SPL_MAX_QUEUE_RETRY) {
	    decc$$translate(SS$_NOTQUEUED);
	    return (errno);
	}
	RetryCtr = 0;
    }
    if (RetryCtr >= SPL_MAX_QUEUE_RETRY) {
	decc$$translate(SS$_NOTQUEUED);
	return (errno);
    }

    return (0);
}



int apache$spl_write_messages(void)
{
    QUE_HDR _align(QUADWORD) TmpResQueue = {
    0, 0};
    static char ResLnm[SPL_RES_LNM_MAX + 1];
    SPL_RES *SplResPtr = NULL;
    SPL_MSG *SplMsgPtr = NULL;
    struct stat NewStat;
    apr_file_t *NewFile;
    int RetryCtr, MsgSize, ResSize, status;

    /* Loop through the queued messages */

    RetryCtr = 0;
    while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
	   (status = _REMQHI(&SplMsgQueue, &SplMsgPtr)) != _remqi_empty) {
	/* If no entry was removed, try again */

	if (status == _remqi_not_removed) {
	    RetryCtr++;
	    continue;
	}

	/* Process the message based on the type */

	switch (SplMsgPtr->SplMsgType) {
	case SPL_MSG_WRITE:
	    /* Write the message to the associated resource file */

	    status = apr_file_write(SplMsgPtr->SplResPtr->SplResFile,
				    SplMsgPtr->SplMsgBuf,
				    &SplMsgPtr->SplMsgLen);
	    if (status != APR_SUCCESS)
		printf("ERROR (%d,%d,0x%08X) during write to file %s\n",
		       status, errno, vaxc$errno,
		       SplMsgPtr->SplResPtr->SplResFile->fname);

	    /* Increment the number resource file writes */
	    SplMsgPtr->SplResPtr->SplResWrites++;

	    /* If the SPL flush interval is active and we've done the specified number of writes, then flush the given file
               resource. */

	    if (apache$$spl_flush_interval() &&
		SplMsgPtr->SplResPtr->SplResWrites %
		apache$$spl_flush_interval() == 0) {

		/* Have APR flush it's buffer */

		status = apr_file_flush(SplMsgPtr->SplResPtr->SplResFile);
		if (status != APR_SUCCESS)
		    printf
			("ERROR (%d,%d,0x%08X) during apr_file_flush of file %s\n",
			 status, errno, vaxc$errno,
			 SplMsgPtr->SplResPtr->SplResFile->fname);

		/* Sync the RMS buffers out to disk */

		status = fsync(SplMsgPtr->SplResPtr->SplResFile->filedes);
		if (status < 0)
		    printf
			("ERROR (%d,0x%08X) during scheduled fsync of file %s\n",
			 errno, vaxc$errno,
			 SplMsgPtr->SplResPtr->SplResFile->fname);
	    }
	    break;

	case SPL_MSG_CLOSE:
	    /* Close the associated resource file */

	    status = apr_file_close(SplMsgPtr->SplResPtr->SplResFile);
	    if (status != APR_SUCCESS) {
		printf("ERROR (%d,%d,0x%08X) during close of file %s\n",
		       status, errno, vaxc$errno,
		       SplMsgPtr->SplResPtr->SplResFile->fname);
		break;
	    }

	    /* Remove the logging resource */

	    RetryCtr = 0;
	    while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
		   (status =
		    _REMQHI(&SplResQueue, &SplResPtr)) != _remqi_empty) {
		/* If no entry was removed, try again */

		if (status == _remqi_not_removed) {
		    RetryCtr++;
		    continue;
		}

		/* If this is the entry we're looking for, then free it and exit the loop */

		if (SplResPtr == SplMsgPtr->SplResPtr) {
		    apr$mbx_delete(SplResPtr->SplResMbxChan);
		    if (SplResPtr->SplResLnm)
			free(SplResPtr->SplResLnm);
		    if (SplResPtr->SplResDev)
			free(SplResPtr->SplResDev);
		    ResSize = sizeof(SPL_RES);
		    LIB$FREE_VM(&ResSize, SplResPtr);
		    break;
		} else {
		    /* Let's insert this entry into the temporary resource queue */

		    RetryCtr = 0;
		    while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
			   (status =
			    _INSQHI(SplResPtr,
				    &TmpResQueue)) == _insqi_not_inserted)
			RetryCtr++;
		    if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
			exit(SS$_NOTQUEUED);
		    RetryCtr = 0;
		}
	    }
	    if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
		exit(SS$_NOTQUEUED);

	    /* Let's move any entries from the temporary resource queue back into the global resource queue. */

	    RetryCtr = 0;
	    while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
		   (status =
		    _REMQHI(&TmpResQueue, &SplResPtr)) != _remqi_empty) {

		/* If no entry was removed, try again */

		if (status == _remqi_not_removed) {
		    RetryCtr++;
		    continue;
		}

		/* Let's insert this entry into the global resource queue */

		RetryCtr = 0;
		while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
		       (status =
			_INSQHI(SplResPtr,
				&SplResQueue)) == _insqi_not_inserted)
		    RetryCtr++;
		if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
		    exit(SS$_NOTQUEUED);
		RetryCtr = 0;
	    }
	    if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
		exit(SS$_NOTQUEUED);
	    break;

	case SPL_MSG_FLUSH:
	    /* Loop through the queued resources */

	    RetryCtr = 0;
	    while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
		   (status =
		    _REMQHI(&SplResQueue, &SplResPtr)) != _remqi_empty) {

		/* If no entry was removed, try again */

		if (status == _remqi_not_removed) {
		    RetryCtr++;
		    continue;
		}

		/* Let's insert this entry into the temporary resource queue */

		RetryCtr = 0;
		while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
		       (status =
			_INSQHI(SplResPtr,
				&TmpResQueue)) == _insqi_not_inserted)
		    RetryCtr++;
		if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
		    exit(SS$_NOTQUEUED);
		RetryCtr = 0;

		/* If this is the type of entry we're looking for, then flush and sync the logging resource. */

		if (SplResPtr->SplResType == SPL_RES_FILE &&
		    apache$$spl_compare_time(&SplResPtr->SplResTime,
					     &SplMsgPtr->SplMsgTime) <= 0)
		{
		    /* Have APR flush it's buffers */

		    status = apr_file_flush(SplResPtr->SplResFile);
		    if (status != APR_SUCCESS)
			printf
			    ("ERROR (%d,%d,0x%08X) during apr_file_flush of file %s\n",
			     status, errno, vaxc$errno,
			     SplResPtr->SplResFile->fname);

		    /* Sync the RMS buffers out to disk */

		    status = fsync(SplResPtr->SplResFile->filedes);
		    if (status < 0)
			printf
			    ("ERROR (%d,0x%08X) during fsync of file %s\n",
			     errno, vaxc$errno,
			     SplResPtr->SplResFile->fname);
		}
	    }
	    if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
		exit(SS$_NOTQUEUED);

	    /* Let's move any entries from the temporary resource queue back into the global resource queue */

	    RetryCtr = 0;
	    while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
		   (status =
		    _REMQHI(&TmpResQueue, &SplResPtr)) != _remqi_empty) {

		/* If no entry was removed, try again */

		if (status == _remqi_not_removed) {
		    RetryCtr++;
		    continue;
		}

		/* Let's insert this entry into the global resource queue */

		RetryCtr = 0;
		while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
		       (status =
			_INSQHI(SplResPtr,
				&SplResQueue)) == _insqi_not_inserted)
		    RetryCtr++;
		if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
		    exit(SS$_NOTQUEUED);
		RetryCtr = 0;
	    }
	    if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
		exit(SS$_NOTQUEUED);
	    break;

	/* Create New file resource entries */
	case SPL_MSG_NEW:
	    /* Loop through the queued resources */

	    RetryCtr = 0;
	    while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
		   (status =
		    _REMQHI(&SplResQueue, &SplResPtr)) != _remqi_empty) {

		/* If no entry was removed, try again */

		if (status == _remqi_not_removed) {
		    RetryCtr++;
		    continue;
		}

	        /* Let's insert this entry into the temporary resource queue */

		RetryCtr = 0;
		while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
		       (status =
			_INSQHI(SplResPtr,
				&TmpResQueue)) == _insqi_not_inserted)
		    RetryCtr++;
		if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
		    exit(SS$_NOTQUEUED);
		RetryCtr = 0;

		/* If this is the type of entry we're looking for, then flush and sync the logging resource */

		if (SplResPtr->SplResType == SPL_RES_FILE &&
		    apache$$spl_compare_time(&SplResPtr->SplResTime,
					     &SplMsgPtr->SplMsgTime) <= 0)
		{
		    status = apr_file_open(&NewFile,
					   SplResPtr->SplResFile->fname,
					   SplResPtr->SplResFile->
					   flags | APR_TRUNCATE,
					   APR_OS_DEFAULT, SplResPool);
		    if (status != APR_SUCCESS)
			printf
			    ("ERROR (%d,%d,0x%08X) during open of file %s\n",
			     status, errno, vaxc$errno,
			     SplResPtr->SplResFile->fname);
		    else {
			status = apr_file_close(SplResPtr->SplResFile);
			if (status != APR_SUCCESS)
			    printf
				("ERROR (%d,%d,0x%08X) during close of file %s\n",
				 status, errno, vaxc$errno,
				 SplResPtr->SplResFile->fname);
			status = fstat(NewFile->filedes, &NewStat);
			if (status)
			    printf
				("ERROR (%d,0x%08X) during fstat of file %s\n",
				 errno, vaxc$errno,
				 SplResPtr->SplResFile->fname);
			else {
			    if (SplResPtr->SplResLnm)
				free(SplResPtr->SplResLnm);
			    sprintf(ResLnm,
				    "APACHE$%s_SPL_%s_%04X%04X%04X",
				    apache$$spl_server_tag(),
				    NewStat.st_dev, NewStat.st_ino[0],
				    NewStat.st_ino[1], NewStat.st_ino[2]);
			    SplResPtr->SplResLnm = strdup(ResLnm);
			}
			SplResPtr->SplResFile = NewFile;
		    }
		}
	    }
	    if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
		exit(SS$_NOTQUEUED);

	    /* Let's move any entries from the temporary resource queue back into the global resource queue */

	    RetryCtr = 0;
	    while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
		   (status =
		    _REMQHI(&TmpResQueue, &SplResPtr)) != _remqi_empty) {

		/* If no entry was removed, try again */

		if (status == _remqi_not_removed) {
		    RetryCtr++;
		    continue;
		}

		/* Let's insert this entry into the global resource queue */

		RetryCtr = 0;
		while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
		       (status =
			_INSQHI(SplResPtr,
				&SplResQueue)) == _insqi_not_inserted)
		    RetryCtr++;
		if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
		    exit(SS$_NOTQUEUED);
		RetryCtr = 0;
	    }
	    if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
		exit(SS$_NOTQUEUED);
	    break;

	default:
	    break;
	}

	/* Free the message buffer */

	if (SplMsgPtr->SplMsgBuf)
	    free(SplMsgPtr->SplMsgBuf);
	MsgSize = sizeof(SPL_MSG);
	LIB$FREE_VM(&MsgSize, SplMsgPtr);
    }
    if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
	exit(SS$_NOTQUEUED);

    return (0);		/* Success */
}


void apache$spl_flush(void)
{
    SPL_MSG *SplMsgPtr;
    int RetryCtr, MsgSize, status;

    /* If SPL is currently disabled, then write warning and return */

    if (apache$$spl_disabled()) {
	fprintf(stderr,
		"WARNING: Shared Process Logging is currently disabled.\n");
	return;
    }

    /* Allocate the message buffer */

    MsgSize = sizeof(SPL_MSG);
    status = LIB$GET_VM(&MsgSize, &SplMsgPtr);
    if (!(status & 1)) {
	fprintf(stderr,
		"ERROR: Unable to allocate Shared Process Logging Flush message.\n");
	decc$$translate(status);
	return;
    }

    /* Load the message buffer */

    memset(SplMsgPtr, 0, sizeof(SPL_MSG));
    SplMsgPtr->SplMsgType = SPL_MSG_FLUSH;
    gettimeofday(&SplMsgPtr->SplMsgTime, NULL);
    SplMsgPtr->SplMsgLen = 0;
    SplMsgPtr->SplMsgBuf = NULL;
    SplMsgPtr->SplResPtr = NULL;

    /* Insert the logging message into the global message queue */

    RetryCtr = 0;
    while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
	   (status =
	    _INSQTI(SplMsgPtr, &SplMsgQueue)) == _insqi_not_inserted)
	RetryCtr++;
    if (RetryCtr >= SPL_MAX_QUEUE_RETRY) {
	fprintf(stderr,
		"ERROR: Unable to queue Shared Process Logging Flush message.\n");
	LIB$FREE_VM(&MsgSize, SplMsgPtr);
	decc$$translate(SS$_NOTQUEUED);
	return;
    }

}


void apache$spl_new(void)
{
    SPL_MSG *SplMsgPtr;
    int RetryCtr, MsgSize, status;

    /* If SPL is currently disabled, then write warning and return */

    if (apache$$spl_disabled()) {
	fprintf(stderr,
		"WARNING: Shared Process Logging is currently disabled.\n");
	return;
    }

    /* Allocate the message buffer */

    MsgSize = sizeof(SPL_MSG);
    status = LIB$GET_VM(&MsgSize, &SplMsgPtr);
    if (!(status & 1)) {
	fprintf(stderr,
		"ERROR: Unable to allocate Shared Process Logging New message.\n");
	decc$$translate(status);
	return;
    }

    /* Load the message buffer */

    memset(SplMsgPtr, 0, sizeof(SPL_MSG));
    SplMsgPtr->SplMsgType = SPL_MSG_NEW;
    gettimeofday(&SplMsgPtr->SplMsgTime, NULL);
    SplMsgPtr->SplMsgLen = 0;
    SplMsgPtr->SplMsgBuf = NULL;
    SplMsgPtr->SplResPtr = NULL;

    /* Insert the logging message into the global message queue */

    RetryCtr = 0;
    while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
	   (status =
	    _INSQTI(SplMsgPtr, &SplMsgQueue)) == _insqi_not_inserted)
	RetryCtr++;
    if (RetryCtr >= SPL_MAX_QUEUE_RETRY) {
	fprintf(stderr,
		"ERROR: Unable to queue Shared Process Logging New message.\n");
	LIB$FREE_VM(&MsgSize, SplMsgPtr);
	decc$$translate(SS$_NOTQUEUED);
	return;
    }

}



static SPL_RES *apache$$spl_resource_find(char *ResLnm)
{
    QUE_HDR _align(QUADWORD) TmpResQueue = {
    0, 0};
    SPL_RES *FndResPtr = NULL, *SplResPtr;
    int RetryCtr, status;

    /* Locate the logging resource */

    RetryCtr = 0;
    while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
	   (status = _REMQHI(&SplResQueue, &SplResPtr)) != _remqi_empty) {
	/* If no entry was removed, try again */
	if (status == _remqi_not_removed) {
	    RetryCtr++;
	    continue;
	}

	/* Let's insert this entry into the temporary resource queue */

	RetryCtr = 0;
	while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
	       (status =
		_INSQHI(SplResPtr, &TmpResQueue)) == _insqi_not_inserted)
	    RetryCtr++;
	if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
	    exit(SS$_NOTQUEUED);
	RetryCtr = 0;

	/* Compare the give resource name to the current resource in the queue */

	if (strcasecmp(SplResPtr->SplResLnm, ResLnm) == 0) {
	    FndResPtr = SplResPtr;
	    break;
	}
    }
    if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
	exit(SS$_NOTQUEUED);

    /* Let's move any entries from the temporary resource queue back into the global resource queue. */

    RetryCtr = 0;
    while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
	   (status = _REMQHI(&TmpResQueue, &SplResPtr)) != _remqi_empty) {
	/* If no entry was removed, try again */
	if (status == _remqi_not_removed) {
	    RetryCtr++;
	    continue;
	}

	/* Let's insert this entry into the global resource queue */

	RetryCtr = 0;
	while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
	       (status =
		_INSQHI(SplResPtr, &SplResQueue)) == _insqi_not_inserted)
	    RetryCtr++;
	if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
	    exit(SS$_NOTQUEUED);
	RetryCtr = 0;
    }
    if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
	exit(SS$_NOTQUEUED);

    /* Return resource */

    return (FndResPtr);
}


static void apache$$spl_resource_ast(SPL_RES * SplResPtr)
{
    SPL_MSG *SplMsgPtr;
    int RetryCtr, MsgSize, status;
    IOSB iosb;

    /* Return if no resource was provided or an abort has occurred */

    if (!SplResPtr || SplResPtr->SplResMbxIosb.iosb$w_status == SS$_ABORT)
	return;

    /* Clear the message buffer */

    memset(SplMsgBuf, 0, apache$$spl_max_message());

    /* Read the message from the mailbox */

    status = SYS$QIOW(EFN$C_ENF,
		      SplResPtr->SplResMbxChan,
		      IO$_READVBLK | IO$M_NOW,
		      &iosb,
		      0, 0,
		      SplMsgBuf, apache$$spl_max_message(), 0, 0, 0, 0);
    if (status & 1)
	status = iosb.iosb$w_status;
    if (!(status & 1) && status != SS$_ENDOFFILE)
	exit(status);

    /* Determine whether this is a valid message */

    if (strlen(SplMsgBuf)) {
	/* Allocate the logging message buffer */

	MsgSize = sizeof(SPL_MSG);
	status = LIB$GET_VM(&MsgSize, &SplMsgPtr);
	if (!(status & 1))
	    exit(status);

	/* Load the logging message buffer */

	memset(SplMsgPtr, 0, sizeof(SPL_MSG));
	SplMsgPtr->SplMsgType = SPL_MSG_WRITE;
	gettimeofday(&SplMsgPtr->SplMsgTime, NULL);
	SplMsgPtr->SplMsgLen = strlen(SplMsgBuf);
	SplMsgPtr->SplMsgBuf = strdup(SplMsgBuf);
	SplMsgPtr->SplResPtr = SplResPtr;

	/* Insert the logging message into the global message queue */

	RetryCtr = 0;
	while (RetryCtr < SPL_MAX_QUEUE_RETRY &&
	       (status =
		_INSQTI(SplMsgPtr, &SplMsgQueue)) == _insqi_not_inserted)
	    RetryCtr++;
	if (RetryCtr >= SPL_MAX_QUEUE_RETRY)
	    exit(SS$_NOTQUEUED);
    }

    /* Re-arm the AST */

    status = SYS$QIO(EFN$C_ENF,
		     SplResPtr->SplResMbxChan,
		     IO$_SETMODE | IO$M_WRTATTN,
		     &SplResPtr->SplResMbxIosb,
		     0, 0,
		     (void *) &apache$$spl_resource_ast,
		     (int) SplResPtr, 0, 0, 0, 0);
    if (!(status & 1))
	exit(status);

}


static int apache$$spl_compare_time(struct timeval *Time1,
				    struct timeval *Time2)
{
    /* Compare the seconds */

    if (Time1->tv_sec < Time2->tv_sec)
	return (-1);
    if (Time1->tv_sec > Time2->tv_sec)
	return (1);

    /* Compare the microseconds */

    if (Time1->tv_usec < Time2->tv_usec)
	return (-1);
    if (Time1->tv_usec > Time2->tv_usec)
	return (1);

    /* Times are equal */

    return (0);
}


static int apache$$spl_parent_process(void)
{
#ifdef TEST_SPL
    static int ParentProcess = 1;
#else
    static int ParentProcess = -1;
#endif

    /* Establish if we're running as parent process */

    if (ParentProcess < 0)
	ap_mpm_query(AP_MPMQ_IS_PARENT_PROCESS, &ParentProcess);

    return (ParentProcess);
}


static int apache$$spl_one_process(void)
{
#ifdef TEST_SPL
    static int OneProcess = 1;
#else
    static int OneProcess = -1;
#endif

    if (OneProcess < 0) {
        OneProcess = ap_exists_config_define("DEBUG") || ap_exists_config_define("ONE_PROCESS");
    }

    return (OneProcess);
}


static int apache$$spl_max_buffers(void)
{
    static int MaxBuffers = -1;
    char *EnvPtr;

    /* Determine the maximum number of clients allowed by this server */

    if (MaxBuffers < 0) {
	EnvPtr = getenv("APACHE$SPL_MAX_BUFFERS");
	if (EnvPtr)
	    MaxBuffers = atoi(EnvPtr);
	else
	    MaxBuffers = SPL_MSG_BUF_CNT;
    }

    return (MaxBuffers);
}


static int apache$$spl_max_message(void)
{
    static int MaxMessage = -1;
    char *EnvPtr;

    /* Determine the maximum size of the messages allowed by this server */

    if (MaxMessage < 0) {
	EnvPtr = getenv("APACHE$SPL_MAX_MESSAGE");
	if (EnvPtr)
	    MaxMessage = atoi(EnvPtr);
	else
	    MaxMessage = SPL_MSG_BUF_MAX;
    }

    return (MaxMessage);
}


static int apache$$spl_flush_interval(void)
{
    static int FlushInterval = -1;
    char *EnvPtr;

    /* Determine the flush interval for this server */

    if (FlushInterval < 0) {
	EnvPtr = getenv("APACHE$SPL_FLUSH_INTERVAL");
	if (EnvPtr)
	    FlushInterval = atoi(EnvPtr);
	else
	    FlushInterval = SPL_MSG_FLUSH_INTERVAL;
    }

    return (FlushInterval);
}


static char *apache$$spl_server_tag(void)
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

    return (ServerTag);
}


static int apache$$spl_disabled(void)
{
    static Disabled = -1;

    /* Determine if shared process logging is disabled */

    if (Disabled < 0) {
	if (getenv("APACHE$SPL_DISABLED"))
	    Disabled = 1;
	else
	    Disabled = 0;
    }

    return (Disabled);
}
