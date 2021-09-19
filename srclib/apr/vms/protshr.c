#pragma module PROTSHR "V1.00"

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
**	Interface for User Written System Services
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

#include <tcpip$inetdef.h>
#include <ioc_routines.h>
#include <sch_routines.h>
#include <lib$routines.h>
#include <va_rangedef.h>
#include <builtins.h>
#include <starlet.h>
#include <descrip.h>
#include <iosbdef.h>
#include <devdef.h>
#include <plvdef.h>
#include <prvdef.h>
#include <efndef.h>
#include <jpidef.h>
#include <iledef.h>
#include <stdlib.h>
#include <string.h>
#include <uicdef.h>
#include <uaidef.h>
#include <ucbdef.h>
#include <secdef.h>
#include <ccbdef.h>
#include <dyndef.h>
#include <iocdef.h>
#include <pcbdef.h>
#include <devdef.h>
#include <dvidef.h>
#include <socket.h>
#include <psldef.h>
#include <prdef.h>
#include <ssdef.h>
#include <iodef.h>
#include <vadef.h>
#include <netdb.h>
#include <pwd.h>
#include <in.h>
#ifdef TEST_PROTSHR
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#endif

#include "protshr.h"
#include "ilemac.h"

#ifdef  __NEW_STARLET_SET
#undef  __NEW_STARLET_SET
#undef  __NEW_STARLET
#endif

/*
** Define constants for True & False
*/
#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

/*
** Establish the APR function rights identifiers
*/
#define APR_ALL		  "APACHE$APR_ALL"
#define APR_CREMBX	  "APACHE$APR_CREMBX"
#define APR_GETPWNAM	  "APACHE$APR_GETPWNAM"
#define APR_SETSOCKOPT	  "APACHE$APR_SETSOCKOPT"
#define APR_SOCKET	  "APACHE$APR_SOCKET"
#define APR_AUTH_OPENVMS  "APACHE$APR_AUTH_OPENVMS"
#define APR_GALAXY_GBLSEC "APACHE$APR_GALAXY_GBLSEC"

/*
** Define the Socket Characteristics data structure
*/
typedef struct _sockchar {
    unsigned short protocol;	/* Socket Protocol                    */
    unsigned char type;		/* Socket Type                        */
    unsigned char af;		/* Address Format                     */
} SOCKCHAR;

/*
** Define the Socket Address data structure
*/
typedef struct _sockaddr {
    int buflen;			/* Buffer Length                      */
    void *bufadr;		/* Buffer Address                     */
    void *retadr;		/* Return Address                     */
    int lstend;			/* End of List                        */
} SOCKADDR;

/*
** Define the module global variables
*/
static unsigned int UaiCtx = -1;

/*
** Define the external routine prototypes
*/
extern int auth_princ_sysuaf_int(const char *, const int, const char *,
				 const int, const char *, const int,
				 unsigned int *);
extern int check_rights_list_int(const char *, int, const unsigned int *,
				 int, unsigned int *);

/*
** Define the local routine prototypes
*/
static int GetPid(int);
static int GetUic(void);
static int HaveIdentifier(char *);
static int HavePrivileges(PRVDEF *);
static int EnablePrivileges(PRVDEF *, PRVDEF *);
static int DisablePrivileges(PRVDEF *, PRVDEF *);
static int SetSockDevCcl(short int, int);
static int SetSockDevBuf(short int, int);
static size_t _strlen(char *str);
static int _strncmp(char *str_1, char *str_2, size_t maxchar);
static void *_memcpy(char *dest, char *source, size_t size);

#ifndef TEST_PROTSHR
/*
** Kernel & Exec rundown routine prototypes
*/
int krnl_rundown();
int exec_rundown();

/*
** Kernel routine table
*/
int (*(krnl_table[])) () = {
NULL};

/*
** Exec routine table
*/
int (*(exec_table[])) () = {
(int (*)()) apr$$crembx,
	(int (*)()) apr$$getpwnam,
	(int (*)()) apr$$setsockopt,
	(int (*)()) apr$$socket,
	(int (*)()) apr$$auth_princ_sysuaf,
	(int (*)()) apr$$check_rights_list,
	(int (*)()) apr$$create_galaxy_gblsec};

/*
** Kernel routine flags
*/
int krnl_flags[] = { 0, 0 };

/*
** Exec routine flags
*/
int exec_flags[] = { 0, 0 };

/*
** Kernel routine count
*/
#define KRNL_ROUTINE_COUNT sizeof (krnl_table) / sizeof (int *)

/*
** Exec routine count
*/
#define EXEC_ROUTINE_COUNT sizeof (exec_table) / sizeof (int *)

/*
** Now build and initialize the PLV structure.  Since the PLV must have
** the VEC psect attribute, and must be the first thing in that psect,
** we use the strict external ref-def model which allows us to put the
** PLV structure in its own psect.  This is like the globaldef
** extension in VAX C, where you can specify in what psect a global
** symbol may be found; unlike globaldef, it allows the declaration
** itself to be ANSI-compliant.  Note that the initialization here
** relies on the change-mode-specific portion (plv$r_cmod_data) of the
** PLV being declared before the portions of the PLV which are specific
** to message vector PLVs (plv$r_msg_data) and system service intercept
** PLVs (plv$r_ssi_data).
**
*/

#if defined(__ALPHA) || defined(__ia64) || defined(__x86_64)
#pragma extern_model save
#pragma extern_model strict_refdef "USER_SERVICES"
#endif

extern const PLV user_services = {
    PLV$C_TYP_CMOD,		/* type                               */
    0,				/* version                            */
    {
     {
      0,			/* # of kernel routines               */
      EXEC_ROUTINE_COUNT,	/* # of executive routines            */
      0,			/* kernel routine list                */
      exec_table,		/* exec routine list                  */
      0,			/* kernel rundown handler             */
      exec_rundown,		/* exec rundown handler               */
      0,			/* RMS dispatcher                     */
      0,			/* kernel routine flags               */
      0				/* exec routine flags                 */
      }
     }
};

#if defined(__ALPHA) || defined(__ia64) || defined(__x86_64)
#pragma extern_model restore
#endif
#endif				/* !TEST_PROTSHR */

#ifdef TEST_PROTSHR
int test_crembx();
int test_getpwnam();
int test_sockets();
void usage();
int ShowSockChar(short int, int);
void GetInput(char *, char *, int, int *);
extern decc$socket_fd();
/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
main(int argc, char *argv[])
{

    if (argc == 1)
	usage();

    if (strcasecmp(argv[1], "CREMBX") == 0)
	test_crembx(argc, argv);

    if (strcasecmp(argv[1], "GETPWNAM") == 0)
	test_getpwnam(argc, argv);

    if (strcasecmp(argv[1], "SOCKETS") == 0)
	test_sockets(argc, argv);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int test_crembx(int argc, char *argv[]
    )
{
    struct dsc$descriptor MbxLogNamDesc =
	{ 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    unsigned short MbxChan = 0;
    int status;

/*
** Print the test title
*/
    printf("Testing CREMBX function:\n\n");

    MbxLogNamDesc.dsc$a_pointer = "TEST_MBX_LNM";
    MbxLogNamDesc.dsc$w_length = strlen(MbxLogNamDesc.dsc$a_pointer);

    status = apr$$crembx(&MbxChan, 256, 1024, &MbxLogNamDesc);
    if (status != SS$_NORMAL) {
	fprintf(stderr, "apr$$crembx: error %08X\n", status);
	return (-1);
    }

    status = SYS$DELMBX(MbxChan);
    if (status != SS$_NORMAL) {
	fprintf(stderr, "SYS$DELMBX: error %08X\n", status);
	return (-1);
    }

    return (1);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int test_getpwnam(int argc, char *argv[]
    )
{
    struct dsc$descriptor UserDesc =
	{ 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    struct passwd pw;
    char pw_name[31 + 1];
    char pw_dir[31 + 63 + 1];
    char pw_shell[31 + 1];
    int status;

/*
** Print the test title
*/
    printf("Testing GETPWNAM function:\n\n");

    UserDesc.dsc$a_pointer = getlogin();
    UserDesc.dsc$w_length = strlen(UserDesc.dsc$a_pointer);

    pw.pw_name = pw_name;
    pw.pw_dir = pw_dir;
    pw.pw_shell = pw_shell;

    printf("Testing apr$$getpwnam using %s...\n", UserDesc.dsc$a_pointer);
    status = apr$$getpwnam(&UserDesc, &pw);
    if (status != SS$_NORMAL) {
	fprintf(stderr, "apr$$getpwnam: error %08X\n", status);
	return (-1);
    }
    printf("   pw_name: %s\n", pw.pw_name);
    printf("   pw_uid = %08X\n", pw.pw_uid);
    printf("   pw_gid = %08X\n", pw.pw_gid);
    printf("   pw_dir = %s\n", pw.pw_dir);
    printf("   pw_shell = %s\n", pw.pw_shell);

    return (1);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int test_sockets(int argc, char *argv[]
    )
{
    struct sockaddr_in sin;
    unsigned short chan;
    int port = 444;
    short size = 1024;
    int status, sock, off = 0, on = 1;

/*
** Print the test title
*/
    printf("Testing SOCKETS functions:\n\n");

/*
** Create the socket
*/
    printf("Testing apr$$socket using (PF_INET, SOCK_STREAM, 0):\n");
    status = apr$$socket(PF_INET, SOCK_STREAM, 0, &chan);
    if (status != SS$_NORMAL) {
	fprintf(stderr, "apr$$socket: error %08X\n", status);
	return (-1);
    }
    sock = decc$socket_fd(chan);

/*
** Set the socket options so that the socket can be reused.
*/
    status =
	setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char *) &on,
		   sizeof(on));
    if (status < 0) {
	perror("setsockopt");
	return (-1);
    }

    printf("Testing apr$$setsockopt using SET_SOCK_DEV_SHR:\n");
    ShowSockChar(vaxc$get_sdc(sock), SET_SOCK_DEV_SHR);
    status =
	apr$$setsockopt(vaxc$get_sdc(sock), SET_SOCK_DEV_SHR, &on,
			sizeof(on));
    if (status != SS$_NORMAL) {
	fprintf(stderr, "apr$$setsockopt: error %08X\n", status);
	return (-1);
    }
    ShowSockChar(vaxc$get_sdc(sock), SET_SOCK_DEV_SHR);

    printf("Testing apr$$setsockopt using SET_SOCK_DEV_CCL:\n");
    ShowSockChar(vaxc$get_sdc(sock), SET_SOCK_DEV_CCL);
    status =
	apr$$setsockopt(vaxc$get_sdc(sock), SET_SOCK_DEV_CCL, &on,
			sizeof(on));
    if (status != SS$_NORMAL) {
	fprintf(stderr, "apr$$setsockopt: error %08X\n", status);
	return (-1);
    }
    ShowSockChar(vaxc$get_sdc(sock), SET_SOCK_DEV_CCL);
    status =
	apr$$setsockopt(vaxc$get_sdc(sock), SET_SOCK_DEV_CCL, &off,
			sizeof(off));
    if (status != SS$_NORMAL) {
	fprintf(stderr, "apr$$setsockopt: error %08X\n", status);
	return (-1);
    }
    ShowSockChar(vaxc$get_sdc(sock), SET_SOCK_DEV_CCL);
    status =
	apr$$setsockopt(vaxc$get_sdc(sock), SET_SOCK_DEV_CCL, NULL, 0);
    if (status != SS$_NORMAL) {
	fprintf(stderr, "apr$$setsockopt: error %08X\n", status);
	return (-1);
    }
    ShowSockChar(vaxc$get_sdc(sock), SET_SOCK_DEV_CCL);

    printf("Testing apr$$setsockopt using SET_SOCK_DEV_BUF:\n");
    ShowSockChar(vaxc$get_sdc(sock), SET_SOCK_DEV_BUF);
    status =
	apr$$setsockopt(vaxc$get_sdc(sock), SET_SOCK_DEV_BUF, &size,
			sizeof(size));
    if (status != SS$_NORMAL) {
	fprintf(stderr, "apr$$setsockopt: error %08X\n", status);
	return (-1);
    }
    ShowSockChar(vaxc$get_sdc(sock), SET_SOCK_DEV_BUF);
    status =
	apr$$setsockopt(vaxc$get_sdc(sock), SET_SOCK_DEV_BUF, NULL, 0);
    if (status != SS$_NORMAL) {
	fprintf(stderr, "apr$$setsockopt: error %08X\n", status);
	return (-1);
    }
    ShowSockChar(vaxc$get_sdc(sock), SET_SOCK_DEV_BUF);

/*
** Setup the socket information
*/
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = 0;
    sin.sin_port = htons(port);

/*
** Bind to the socket
*/
    status = bind(sock, (struct sockaddr *) &sin, sizeof(sin));
    if (!(status & 1)) {
	perror("bind");
	return (-1);
    }

/*
** Listen on the socket with a backlog of 1
*/
    status = listen(sock, 1);
    if (status < 0) {
	perror("listen");
	return (-1);
    }

/*
** Close the socket
*/
    status = close(sock);
    if (status < 0) {
	perror("close");
	return (-1);
    }

    printf("\nCompleted !\n");

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
void GetInput(char *Prompt, char *InputStr, int InputMax, int *InputLen)
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

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int ShowSockChar(short int SockChan, int SockChar)
{
    unsigned int DevBufSiz;
    unsigned int DevPid;
    ILE3 DviItems[4], *Ile3Ptr;
    DEVDEF DevChar;
    int status;
    IOSB iosb;

/*
** Setup the Dvi item list
*/
    ILE3_INIT(DviItems);
    ILE3_ADD(DVI$_PID, sizeof(DevPid), &DevPid, 0);
    ILE3_ADD(DVI$_DEVCHAR, sizeof(DevChar), &DevChar, 0);
    ILE3_ADD(DVI$_DEVBUFSIZ, sizeof(DevBufSiz), &DevBufSiz, 0);
    ILE3_TERM;

/*
** Get the Dvi data
*/
    status = SYS$GETDVIW(EFN$C_ENF, SockChan, 0, DviItems, &iosb, 0, 0, 0);
    if (status & 1)
	status = iosb.iosb$w_status;
    if (!(status & 1))
	printf("ShowSockChar: error %08X\n", status);
    else
	switch (SockChar) {
	case SET_SOCK_DEV_CCL:
	    printf("   DVI$_DEVCHAR.DEV$V_CCL: %d\n", DevChar.dev$v_ccl);
	    break;
	case SET_SOCK_DEV_BUF:
	    printf("   DVI$_DEVBUFSIZ: %d\n", DevBufSiz);
	    break;
	case SET_SOCK_DEV_SHR:
	    printf("   DVI$_PID: %08X\n", DevPid);
	    break;
	}

/*
** Return sucess
*/
    return (TRUE);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
void usage()
{

    printf("Usage: PROTSHR test-name [...]\n");
    exit(1);

}
#endif				/* TEST_PROTSHR */

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int apr$$crembx(unsigned short *MbxChan,
		int MbxMaxMsg,
		int MbxBufQuo, struct dsc$descriptor *MbxLogNamDesc)
{
    register unsigned __int64 PrvMode = (__PAL_RD_PS() & PR$M_PS_PRVMOD);
    unsigned int MbxProMsk = 0x0000FF00;
    PRVDEF ReqPrivs, OldPrivs;
    int status;
    IOSB iosb;

/*
** Establish the required privileges for this function
*/
    memset(&ReqPrivs, 0, sizeof(ReqPrivs));
    ReqPrivs.prv$v_sysnam = 1;
    ReqPrivs.prv$v_grpnam = 1;

/*
** Validate that this user is authorized for this function
*/
    if (!HavePrivileges(&ReqPrivs) &&
	!HaveIdentifier(APR_ALL) && !HaveIdentifier(APR_CREMBX))
	return (SS$_NOPRIV);

/*
** Validate Access to the arguments
*/
    if (!__PAL_PROBEW(MbxChan, sizeof(unsigned short), PrvMode))
	return (SS$_ACCVIO);
    if (MbxLogNamDesc &&
	!__PAL_PROBER(MbxLogNamDesc->dsc$a_pointer,
		      MbxLogNamDesc->dsc$w_length, PrvMode))
	return (SS$_ACCVIO);

/*
** Turn On required privileges
*/
    if (!EnablePrivileges(&ReqPrivs, &OldPrivs)) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (SS$_NOPRIV);
    }

/*
** Create the mailbox
*/
    status = SYS$CREMBX(0,
			MbxChan,
			MbxMaxMsg,
			MbxBufQuo,
			MbxProMsk,
			PSL$C_USER,
			MbxLogNamDesc ? MbxLogNamDesc : NULL, 0);
    if (!(status & 1)) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (status);
    }

/*
** Turn Off required privileges
*/
    status = DisablePrivileges(&ReqPrivs, &OldPrivs);
    if (!(status & 1))
	return (status);

/*
** Return success
*/
    return (SS$_NORMAL);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int apr$$getpwnam(struct dsc$descriptor *NameDesc, struct passwd *pw)
{
    register unsigned __int64 PrvMode = (__PAL_RD_PS() & PR$M_PS_PRVMOD);
    char DefCliBuf[31 + 1], DefDevBuf[31 + 1], DefDirBuf[63 + 1];
    PRVDEF ReqPrivs, OldPrivs;
    ILE3 UaiItems[5], *Ile3Ptr;
    int DefCliLen, DefDevLen, DefDirLen, status;
    UICDEF Uic;

/*
** Establish the required privileges for this function
*/
    memset(&ReqPrivs, 0, sizeof(ReqPrivs));
    ReqPrivs.prv$v_sysprv = 1;

/*
** Validate that this user is authorized for this function
*/
    if (!HavePrivileges(&ReqPrivs) &&
	!HaveIdentifier(APR_ALL) && !HaveIdentifier(APR_GETPWNAM))
	return (SS$_NOPRIV);

/*
** Validate Access to the arguments
*/
    if (!__PAL_PROBER(NameDesc, sizeof(struct dsc$descriptor), PrvMode))
	return (SS$_ACCVIO);
    if (!__PAL_PROBER
	(NameDesc->dsc$a_pointer, NameDesc->dsc$w_length, PrvMode))
	return (SS$_ACCVIO);
    if (!__PAL_PROBEW(pw, sizeof(struct passwd), PrvMode))
	return (SS$_ACCVIO);

/*
** Turn On required privileges
*/
    if (!EnablePrivileges(&ReqPrivs, &OldPrivs)) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (SS$_NOPRIV);
    }

/*
** Setup the Uai item list
*/
    ILE3_INIT(UaiItems);
    ILE3_ADD(UAI$_UIC, sizeof(Uic.uic$l_uic), &Uic.uic$l_uic, 0);
    ILE3_ADD(UAI$_DEFDEV, sizeof(DefDevBuf), DefDevBuf, 0);
    ILE3_ADD(UAI$_DEFDIR, sizeof(DefDirBuf), DefDirBuf, 0);
    ILE3_ADD(UAI$_DEFCLI, sizeof(DefCliBuf), DefCliBuf, 0);
    ILE3_TERM;

/*
** Get the Uai data
*/
    status = SYS$GETUAI(0, &UaiCtx, NameDesc, UaiItems, 0, 0, 0);
    if (!(status & 1)) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (status);
    }

/*
** Turn Off required privileges
*/
    status = DisablePrivileges(&ReqPrivs, &OldPrivs);
    if (!(status & 1))
	return (status);

/*
** Get the lengths of the return data
*/
    DefDevLen = (int) DefDevBuf[0];
    DefDirLen = (int) DefDirBuf[0];
    DefCliLen = (int) DefCliBuf[0];

/*
** Validate Access to the arguments
*/
    if (!__PAL_PROBEW(pw->pw_name, NameDesc->dsc$w_length + 1, PrvMode))
	return (SS$_ACCVIO);
    if (!__PAL_PROBEW(pw->pw_dir, DefDevLen + DefDirLen + 1, PrvMode))
	return (SS$_ACCVIO);
    if (!__PAL_PROBEW(pw->pw_shell, DefCliLen + 1, PrvMode))
	return (SS$_ACCVIO);

/*
** Move the user information into the user structure
*/
    _memcpy(pw->pw_name, NameDesc->dsc$a_pointer, NameDesc->dsc$w_length);
    pw->pw_name[NameDesc->dsc$w_length] = '\0';
    pw->pw_uid = Uic.uic$l_uic;
    pw->pw_gid = Uic.uic$w_grp;
    _memcpy(pw->pw_dir, &DefDevBuf[1], DefDevLen);
    _memcpy(pw->pw_dir + DefDevLen, &DefDirBuf[1], DefDirLen);
    pw->pw_dir[DefDevLen + DefDirLen] = '\0';
    _memcpy(pw->pw_shell, &DefCliBuf[1], DefCliLen);
    pw->pw_shell[DefCliLen] = '\0';

/*
** Return success
*/
    return (SS$_NORMAL);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int apr$$socket(int af, int type, int protocol, unsigned short *SockChan)
{
    register unsigned __int64 PrvMode = (__PAL_RD_PS() & PR$M_PS_PRVMOD);
    static $DESCRIPTOR(SockDevDesc, "TCPIP$DEVICE");
    SOCKCHAR SockChar;
    PRVDEF ReqPrivs, OldPrivs;
    int status;
    IOSB iosb;

/*
** Establish the required privileges for this function
*/
    memset(&ReqPrivs, 0, sizeof(ReqPrivs));
    ReqPrivs.prv$v_sysprv = 1;

/*
** Validate that this user is authorized for this function
*/
    if (!HavePrivileges(&ReqPrivs) &&
	!HaveIdentifier(APR_ALL) && !HaveIdentifier(APR_SOCKET))
	return (SS$_NOPRIV);

/*
** Validate Access to the arguments
*/
    if (!__PAL_PROBER(SockChan, sizeof(short int), PrvMode))
	return (SS$_ACCVIO);

/*
** Turn On required privileges
*/
    if (!EnablePrivileges(&ReqPrivs, &OldPrivs)) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (SS$_NOPRIV);
    }

/*
** Create a socket device
*/
    status = SYS$ASSIGN(&SockDevDesc, SockChan, PSL$C_USER, 0, 0);
    if (!(status & 1)) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (status);
    }

/*
** Establish the device socket characteristics
*/
    SockChar.af = af;
    SockChar.type = type;
    SockChar.protocol = protocol;

/*
** Set the device socket characteristics
*/
    status = SYS$QIOW(EFN$C_ENF,
		      *SockChan,
		      IO$_SETMODE, &iosb, 0, 0, &SockChar, 0, 0, 0, 0, 0);
    if (!(status & 1)) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (status);
    }

/*
** Turn Off required privileges
*/
    status = DisablePrivileges(&ReqPrivs, &OldPrivs);
    if (!(status & 1))
	return (status);

/*
** Return success
*/
    return (SS$_NORMAL);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int apr$$setsockopt(short int SockChan,
		    int OptName, void *OptVal, int OptLen)
{
    register unsigned __int64 PrvMode = (__PAL_RD_PS() & PR$M_PS_PRVMOD);
    unsigned int ArgList[3];
    unsigned int DevPid;
    char DevNamBuf[256];
    ILE2 SetSockDevShrItem[1], SockOptItems[2], *Ile2Ptr;
    DEVDEF DevChar;
    UICDEF DevUic;
    PRVDEF ReqPrivs, OldPrivs;
    ILE3 DviItems[5], *Ile3Ptr;
    int ArgCtr = 0,
	DevNamLen, BufVal = 256, CclVal = -1, ShrVal = 1, status;
    IOSB iosb;

/*
** Establish the required privileges for this function
*/
    memset(&ReqPrivs, 0, sizeof(ReqPrivs));
    ReqPrivs.prv$v_cmkrnl = 1;

/*
** Validate that this user is authorized for this function
*/
    if (!HavePrivileges(&ReqPrivs) &&
	!HaveIdentifier(APR_ALL) && !HaveIdentifier(APR_SETSOCKOPT))
	return (SS$_NOPRIV);

/*
** Validate Access to the arguments
*/
    if (!__PAL_PROBER(OptVal, OptLen, PrvMode))
	return (SS$_ACCVIO);

/*
** Validate that the option is valid
*/
    switch (OptName) {
    case SET_SOCK_DEV_BUF:
	switch (OptLen) {
	case 4:
	    BufVal = *((int *) OptVal);
	    break;
	case 2:
	    BufVal = *((short *) OptVal);
	    break;
	case 1:
	    BufVal = *((char *) OptVal);
	    break;
	default:
	    break;
	}
	if (BufVal < 0 || BufVal > 65535)
	    return (SS$_BADPARAM);
	break;

    case SET_SOCK_DEV_CCL:
	switch (OptLen) {
	case 4:
	    CclVal = *((int *) OptVal);
	    break;
	case 2:
	    CclVal = *((short *) OptVal);
	    break;
	case 1:
	    CclVal = *((char *) OptVal);
	    break;
	default:
	    break;
	}
	if (CclVal < -1 || CclVal > 1)
	    return (SS$_BADPARAM);
	break;

    case SET_SOCK_DEV_SHR:
	switch (OptLen) {
	case 4:
	    ShrVal = *((int *) OptVal);
	    break;
	case 2:
	    ShrVal = *((short *) OptVal);
	    break;
	case 1:
	    ShrVal = *((char *) OptVal);
	    break;
	default:
	    break;
	}
	if (ShrVal < 0 || ShrVal > 1)
	    return (SS$_BADPARAM);
	break;

    default:
	return (SS$_BADPARAM);
	break;
    }

/*
** Turn On required privileges
*/
    if (!EnablePrivileges(&ReqPrivs, &OldPrivs)) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (SS$_NOPRIV);
    }

/*
** Setup the Dvi item list
*/
    ILE3_INIT(DviItems);
    ILE3_ADD(DVI$_PID, sizeof(DevPid), &DevPid, 0);
    ILE3_ADD(DVI$_OWNUIC, sizeof(DevUic.uic$l_uic), &DevUic.uic$l_uic, 0);
    ILE3_ADD(DVI$_DEVCHAR, sizeof(DevChar), &DevChar, 0);
    ILE3_ADD(DVI$_DISPLAY_DEVNAM, sizeof(DevNamBuf), DevNamBuf,
	     &DevNamLen);
    ILE3_TERM;

/*
** Get the Dvi data
*/
    status = SYS$GETDVIW(EFN$C_ENF, SockChan, 0, DviItems, &iosb, 0, 0, 0);
    if (status & 1)
	status = iosb.iosb$w_status;
    if (!(status & 1)) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (status);
    }

/*
** Verify the socket is owned by this process or it's parent
*/
    if ((DevPid == 0 && DevUic.uic$l_uic != GetUic()) ||
	(DevPid != GetPid(JPI$_PID) && DevPid != GetPid(JPI$_OWNER))) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (SS$_BADOWNER);
    }

/*
** Verify the socket is really a network BG device
*/
    if (!DevChar.dev$v_net || _strncmp(DevNamBuf, "BG", 2)) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (SS$_NOTNETDEV);
    }

/*
** Process the specified option name
*/
    switch (OptName) {
    case SET_SOCK_DEV_CCL:
	/*
	 ** Setup the argument list
	 */
	ArgList[++ArgCtr] = SockChan;
	ArgList[++ArgCtr] = CclVal;
	ArgList[0] = ArgCtr;

	/*
	 ** Set the socket option
	 */
	status = SYS$CMKRNL((int (*)()) SetSockDevCcl, ArgList);
	break;

    case SET_SOCK_DEV_BUF:
	/*
	 ** Setup the argument list
	 */
	ArgList[++ArgCtr] = SockChan;
	ArgList[++ArgCtr] = BufVal;
	ArgList[0] = ArgCtr;

	/*
	 ** Set the socket option
	 */
	status = SYS$CMKRNL((int (*)()) SetSockDevBuf, ArgList);
	break;

    case SET_SOCK_DEV_SHR:
	/*
	 ** Setup the item lists
	 */
	ILE2_INIT(SetSockDevShrItem);
	ILE2_ADD(TCPIP$M_SHARE, sizeof(ShrVal), &ShrVal);
	ILE2_INIT(SockOptItems);
	ILE2_ADD(TCPIP$C_SOCKOPT, sizeof(SetSockDevShrItem),
		 &SetSockDevShrItem);
	ILE2_TERM;

	/*
	 ** Set the socket option
	 */
	status = SYS$QIOW(EFN$C_ENF,
			  SockChan,
			  IO$_SETMODE,
			  &iosb,
			  0, 0, 0, 0, 0, 0, (__int64) & SockOptItems, 0);
	if (status & 1)
	    status = iosb.iosb$w_status;
	break;

    default:
	break;
    }
    if (!(status & 1)) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (status);
    }

/*
** Turn Off required privileges
*/
    status = DisablePrivileges(&ReqPrivs, &OldPrivs);
    if (!(status & 1))
	return (status);

/*
** Return success
*/
    return (SS$_NORMAL);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static int SetSockDevCcl(short int SockChan, int SockCcl)
{
    extern PCB *CTL$GL_PCB;
    int status;
    CCB *ccb;
    UCB *ucb;

/*
** Acquire the IO read lock for this PCB
*/
    sch_std$iolockr(CTL$GL_PCB);

/*
** Get channel's CCB address from the channel number
*/
    status = ioc$chan_to_ccb(SockChan, &ccb);
    if (status == SS$_NORMAL)
	ucb = ccb->ccb$l_ucb;

/*
** Sanity check. Valid channel has a valid access mode associated with it
*/
    if (status == SS$_NORMAL)
	if (ccb->ccb$b_amod == 0)
	    status = SS$_NOIOCHAN;

/*
** Sanity check. The CCB contains the channel number (Alpha && ia64 only)
*/
    if (status == SS$_NORMAL)
	if (ccb->ccb$w_chan != SockChan)
	    status = SS$_IVCHAN;

/*
** Get what should be the device's UCB and verify that it is a UCB.
*/
    if (status == SS$_NORMAL)
	if (ucb->ucb$b_type != DYN$C_UCB)
	    status = SS$_DEVFOREIGN;

/*
** Set the default buffer size
*/
    if (status == SS$_NORMAL)
	if (SockCcl < 0)
	    ucb->ucb$l_devchar ^= DEV$M_CCL;
	else
	    ((DEVDEF *) & ucb->ucb$l_devchar)->dev$v_ccl = SockCcl;

/*
** Release the IO mutex read lock
*/
    sch_std$iounlock(CTL$GL_PCB);

/*
** Return
*/
    return (status);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static int SetSockDevBuf(short int SockChan, int SockBuf)
{
    extern PCB *CTL$GL_PCB;
    int status;
    CCB *ccb;
    UCB *ucb;

/*
** Acquire the IO read lock for this PCB
*/
    sch_std$iolockr(CTL$GL_PCB);

/*
** Get channel's CCB address from the channel number
*/
    status = ioc$chan_to_ccb(SockChan, &ccb);
    if (status == SS$_NORMAL)
	ucb = ccb->ccb$l_ucb;

/*
** Sanity check. Valid channel has a valid access mode associated with it
*/
    if (status == SS$_NORMAL)
	if (ccb->ccb$b_amod == 0)
	    status = SS$_NOIOCHAN;

/*
** Sanity check. The CCB contains the channel number (Alpha & ia64 only)
*/
    if (status == SS$_NORMAL)
	if (ccb->ccb$w_chan != SockChan)
	    status = SS$_IVCHAN;

/*
** Get what should be the device's UCB and verify that it is a UCB.
*/
    if (status == SS$_NORMAL)
	if (ucb->ucb$b_type != DYN$C_UCB)
	    status = SS$_DEVFOREIGN;

/*
** Set the default buffer size
*/
    if (status == SS$_NORMAL)
	if (SockBuf <= 0)
	    ucb->ucb$w_devbufsiz = 256;
	else
	    ucb->ucb$w_devbufsiz = SockBuf;


/*
** Release the IO mutex read lock
*/
    sch_std$iounlock(CTL$GL_PCB);

/*
** Return
*/
    return (status);

}

#if 0
/******************************************************************************/
/*** The apr$$bind routine was not useful because it appears that TCPIP  ***/
/*** stores the privilege mask of the user at the time we create the socket ***/
/*** and thus using any privilege here will not affect our ability to bind  ***/
/*** the socket to a privileged port.  Without detailed information about   ***/
/*** TCPIP device definitions this routine was deemed not useful.  Instead  ***/
/*** we've chosen to use the apr$$socket routine.                        ***/
/******************************************************************************/
int apr$$bind(short int SockChan,
	      struct sockaddr_in *SockAddrBuf, int SockAddrLen)
{
    struct dsc$descriptor_s LocalAddrDesc =
	{ 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    register unsigned __int64 PrvMode = (__PAL_RD_PS() & PR$M_PS_PRVMOD);
    struct sockaddr_in LocalAddr;
    unsigned int DevPid;
    char DevNamBuf[256];
    PRVDEF ReqPrivs, OldPrivs;
    DEVDEF DevChar;
    ILE3 DviItems[5], *Ile3Ptr;
    UICDEF DevUic;
    int DevNamLen, status;
    IOSB iosb;

/*
** Establish the required privileges for this function
*/
    memset(&ReqPrivs, 0, sizeof(ReqPrivs));
    ReqPrivs.prv$v_sysprv = 1;

/*
** Validate that this user is authorized for this function
*/
    if (!HavePrivileges(&ReqPrivs) &&
	!HaveIdentifier(APR_ALL) && !HaveIdentifier(APR_BIND))
	return (SS$_NOPRIV);

/*
** Validate Access to the arguments
*/
    if (!__PAL_PROBER(SockAddrBuf, SockAddrLen, PrvMode))
	return (SS$_ACCVIO);

/*
** Turn On required privileges
*/
    if (!EnablePrivileges(&ReqPrivs, &OldPrivs)) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (SS$_NOPRIV);
    }

/*
** Setup the Uai item list
*/
    ILE3_INIT(DviItems);
    ILE3_ADD(DVI$_OWNUIC, sizeof(DevUic.uic$l_uic), &DevUic.uic$l_uic, 0);
    ILE3_ADD(DVI$_PID, sizeof(DevPid), &DevPid, 0);
    ILE3_ADD(DVI$_DEVCHAR, sizeof(DevChar), &DevChar, 0);
    ILE3_ADD(DVI$_DISPLAY_DEVNAM, sizeof(DevNamBuf), DevNamBuf,
	     &DevNamLen);
    ILE3_TERM;

/*
** Get the Dvi data
*/
    status = SYS$GETDVIW(EFN$C_ENF, SockChan, 0, DviItems, &iosb, 0, 0, 0);
    if (status & 1)
	status = iosb.iosb$w_status;
    if (!(status & 1)) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (status);
    }

/*
** Verify the socket is owned by this process or it's parent
*/
    if ((DevPid == 0 && DevUic.uic$l_uic != GetUic()) ||
	(DevPid != GetPid(JPI$_PID) && DevPid != GetPid(JPI$_OWNER))) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (SS$_BADOWNER);
    }

/*
** Verify the socket is really a network BG device
*/
    if (!DevChar.dev$v_net || _strncmp(DevNamBuf, "BG", 2)) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (SS$_NOTNETDEV);
    }

/*
** Setup the local address structure
*/
    memset(&LocalAddr, 0, sizeof(LocalAddr));
    LocalAddr.sin_family = SockAddrBuf->sin_family;
    LocalAddr.sin_addr.s_addr = SockAddrBuf->sin_addr.s_addr;
    LocalAddr.sin_port = SockAddrBuf->sin_port;

/*
** Setup the local address structure descriptor
*/
    LocalAddrDesc.dsc$a_pointer = (char *) &LocalAddr;
    LocalAddrDesc.dsc$w_length = (long) sizeof(LocalAddr);

/*
** Bind the socket to the port
*/
    status = SYS$QIOW(EFN$C_ENF,
		      SockChan,
		      IO$_SETMODE,
		      &iosb,
		      0, 0, 0, 0, (__int64) & LocalAddrDesc, 0, 0, 0);
    if (status & 1)
	status = iosb.iosb$w_status;
    if (!(status & 1)) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (status);
    }

/*
** Turn Off required privileges
*/
    status = DisablePrivileges(&ReqPrivs, &OldPrivs);
    if (!(status & 1))
	return (status);

/*
** Return success
*/
    return (SS$_NORMAL);

}
#endif

/******************************************************************************/
/*** The vms_auth_princ_sysuaf() function authenticates a principal using   ***/
/*** the SYSUAF data.  The function uses the $GETUAI, $HASH_PASSWORD, and   ***/
/*** $SCAN_INTRUSION system service calls.                                  ***/
/******************************************************************************/
int apr$$auth_princ_sysuaf(const char *pcszUsername,
			   int ccUsername,
			   const char *pcszPassword,
			   int ccPassword,
			   const char *pcszHostName, int ccHostName)
{
    register unsigned __int64 PrvMode = (__PAL_RD_PS() & PR$M_PS_PRVMOD);
    PRVDEF ReqPrivs, OldPrivs;

/*
** Establish the required privileges for this function
*/
    memset(&ReqPrivs, 0, sizeof(ReqPrivs));
    ReqPrivs.prv$v_sysprv = 1;
    ReqPrivs.prv$v_security = 1;

/*
** Validate that this user is authorized for this function
*/
    if (!HavePrivileges(&ReqPrivs) &&
	!HaveIdentifier(APR_ALL) && !HaveIdentifier(APR_AUTH_OPENVMS))
	return (SS$_NOPRIV);

/*
** Validate the access to the parameters
*/
    if ((!pcszUsername) || (ccUsername == 0))
	return (SS$_INVARG);
    if ((!pcszPassword) || (ccPassword == 0))
	return (SS$_INVARG);
    if ((!pcszHostName) || (ccHostName == 0))
	return (SS$_INVARG);
    if (__PAL_PROBER(pcszUsername, ccUsername, PrvMode) == 0)
	return (SS$_ACCVIO);
    if (__PAL_PROBER(pcszPassword, ccPassword, PrvMode) == 0)
	return (SS$_ACCVIO);
    if (__PAL_PROBER(pcszHostName, ccHostName, PrvMode) == 0)
	return (SS$_ACCVIO);

/*
** Call the internal authorize principal from sysuaf routine
*/
    return (auth_princ_sysuaf_int(pcszUsername, ccUsername,
				  pcszPassword, ccPassword,
				  pcszHostName, ccHostName, &UaiCtx));

}

/******************************************************************************/
/*** The apr$$check_rights_list function checks a given user and list of ***/
/*** identifiers to see if the UIC, group code, or any of the identifiers   ***/
/*** that have been granted to the user are in the list.  The function uses ***/
/*** the $GETUAI and $FIND_HELD system service calls.                       ***/
/******************************************************************************/
int apr$$check_rights_list(const char *pcszUsername,
			   int ccUsername,
			   const unsigned int *pcuiIdentList,
			   int ccIdentList)
{
    register unsigned __int64 PrvMode = (__PAL_RD_PS() & PR$M_PS_PRVMOD);
    PRVDEF ReqPrivs, OldPrivs;

/*
** Establish the required privileges for this function
*/
    memset(&ReqPrivs, 0, sizeof(ReqPrivs));
    ReqPrivs.prv$v_sysprv = 1;

/*
** Validate that this user is authorized for this function
*/
    if (!HavePrivileges(&ReqPrivs) &&
	!HaveIdentifier(APR_ALL) && !HaveIdentifier(APR_AUTH_OPENVMS))
	return (SS$_NOPRIV);

/*
** Validate the access to the parameters
*/
    if ((!pcszUsername) || (ccUsername == 0))
	return (SS$_INVARG);
    if (((ccIdentList == 0) && (pcuiIdentList != (void *) 0)) ||
	((ccIdentList > 0) && (pcuiIdentList == (void *) 0)) ||
	(ccIdentList < 0))
	return (SS$_INVARG);
    if (__PAL_PROBER(pcszUsername, ccUsername, PrvMode) == 0)
	return (SS$_ACCVIO);
    if ((ccIdentList > 0)
	&&
	(__PAL_PROBER(pcuiIdentList, (ccIdentList * sizeof(int)), PrvMode)
	 == 0))
	return (SS$_ACCVIO);

/*
** Call the internal check rights list routine
*/
    return (check_rights_list_int(pcszUsername, ccUsername,
				  pcuiIdentList, ccIdentList, &UaiCtx));

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int apr$$create_galaxy_gblsec(struct dsc$descriptor *SectionNameDesc,
			      __int64 SectionBytes,
			      VA_RANGE * SectionVaRange)
{
    register unsigned __int64 PrvMode = (__PAL_RD_PS() & PR$M_PS_PRVMOD);
    __int64 SectionIdent = SEC$K_MATALL;
    __int64 SectionRegion = VA$C_P0;
    __int64 SectionLength = 0;
    PRVDEF ReqPrivs, OldPrivs;
    int status;

/*
** Establish the required privileges for this function
*/
    memset(&ReqPrivs, 0, sizeof(ReqPrivs));
    ReqPrivs.prv$v_shmem = 1;

/*
** Validate that this user is authorized for this function
*/
    if (!HavePrivileges(&ReqPrivs) &&
	!HaveIdentifier(APR_ALL) && !HaveIdentifier(APR_GALAXY_GBLSEC))
	return (SS$_NOPRIV);

/*
** Validate the access to the parameters
*/
    if (!SectionNameDesc)
	return (SS$_ACCVIO);
    if (!__PAL_PROBER
	(SectionNameDesc, sizeof(struct dsc$descriptor), PrvMode))
	return (SS$_ACCVIO);
    if (!__PAL_PROBER
	(SectionNameDesc->dsc$a_pointer, SectionNameDesc->dsc$w_length,
	 PrvMode))
	return (SS$_ACCVIO);
    if (!SectionVaRange)
	return (SS$_ACCVIO);
    if (!__PAL_PROBEW(SectionVaRange, sizeof(VA_RANGE), PrvMode))
	return (SS$_ACCVIO);

/*
** Turn On required privileges
*/
    if (!EnablePrivileges(&ReqPrivs, &OldPrivs)) {
	DisablePrivileges(&ReqPrivs, &OldPrivs);
	return (SS$_NOPRIV);
    }

/*
** Create and map the galaxy global section
*/
    status = SYS$CRMPSC_GDZRO_64(SectionNameDesc,
				 (struct _secid *) &SectionIdent,
				 0x0FF00,
				 SectionBytes,
				 (struct _generic_64 *) &SectionRegion,
				 0,
				 PSL$C_USER,
				 SEC$M_WRT | SEC$M_GBL | SEC$M_SHMGS |
				 SEC$M_EXPREG, (void *) SectionVaRange,
				 (unsigned __int64 *) &SectionLength, 0, 0,
				 0, 0);

/*
** Turn Off required privileges
*/
    DisablePrivileges(&ReqPrivs, &OldPrivs);

/*
** Update the section end virtual address if successful
*/
    if (status & 1)
	SectionVaRange->va_range$ps_end_va = (void *)
	    ((__int64) SectionVaRange->va_range$ps_start_va +
	     SectionLength - 1);

/*
** Return status
*/
    return (status);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static int HaveIdentifier(char *IdentPtr)
{
    struct dsc$descriptor IdentDesc =
	{ 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    unsigned int Holder[2] = { 0, 0 };
    unsigned int RdbCtx = 0;
    unsigned int RdbIdent;
    int HasIdent = FALSE;
    unsigned int Ident;
    ILE3 JpiItems[2], *Ile3Ptr;
    int status;
    IOSB iosb;

/*
** Setup the identifier descriptor
*/
    IdentDesc.dsc$w_length = _strlen(IdentPtr);
    IdentDesc.dsc$a_pointer = IdentPtr;

/*
** Convert the identifier string to a value
*/
    status = sys$asctoid(&IdentDesc, &Ident, 0);
    if (!(status & 1))
	return (FALSE);

/*
** Setup the the Jpi item list
*/
    ILE3_INIT(JpiItems);
    ILE3_ADD(JPI$_UIC, sizeof(Holder[0]), &Holder[0], 0);
    ILE3_TERM;

/*
** Get the current persona's rights list size
*/
    status = SYS$GETJPIW(EFN$C_ENF, 0, 0, &JpiItems, &iosb, 0, 0);
    if (!(status & 1))
	return (FALSE);

/*
** Loop through all the rights this user is authorized to hold
*/
    while (TRUE) {
	/*
	 ** Find the rights identifiers held by this user and see if this they have
	 ** requested rights identifier
	 */
	status = SYS$FIND_HELD((struct _generic_64 *) Holder,
			       &RdbIdent, 0, &RdbCtx);
	if (!(status & 1))
	    break;
	if (Ident == RdbIdent) {
	    HasIdent = TRUE;
	    break;
	}
    }

/*
** Cleanup the rights identifier search context
*/
    status = SYS$FINISH_RDB(&RdbCtx);
    if (!(status & 1))
	return (FALSE);

/*
** Return HasIdent indicator
*/
    return (HasIdent);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static int HavePrivileges(PRVDEF * ReqPrivs)
{
    __int64 CurPrivs;
    int status;

/*
** Get the current privileges
*/
    status = SYS$SETPRV(1,
			(struct _generic_64 *) NULL,
			PRV$K_PRVMASK_WORKING,
			(struct _generic_64 *) &CurPrivs);
    if (!(status & 1))
	memset((void *) &CurPrivs, 0, sizeof(CurPrivs));

/*
** Check to see if the required privs are already enabled
*/
    if ((*((__int64 *) ReqPrivs) & CurPrivs) == *((__int64 *) ReqPrivs))
	return (TRUE);
    else
	return (FALSE);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static int EnablePrivileges(PRVDEF * NewPrivs, PRVDEF * OldPrivs)
{
    int status;

/*
** Enable the new privileges
*/
    status = SYS$SETPRV(1,
			(struct _generic_64 *) NewPrivs,
			PRV$K_PRVMASK_WORKING,
			(struct _generic_64 *) OldPrivs);
    if (status & 1)
	return (TRUE);
    else
	return (FALSE);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static int DisablePrivileges(PRVDEF * NewPrivs, PRVDEF * OldPrivs)
{
    __int64 SetPrivs;
    int status;

/*
** If no set privileges need to turned off, then return success
*/
    SetPrivs = *((__int64 *) NewPrivs) & ~(*((__int64 *) OldPrivs));
    if (!SetPrivs)
	return (TRUE);

/*
** Disable the set privileges
*/
    status = SYS$SETPRV(0,
			(struct _generic_64 *) &SetPrivs,
			PRV$K_PRVMASK_WORKING,
			(struct _generic_64 *) NULL);
    if (status & 1)
	return (TRUE);
    else
	return (FALSE);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static int GetPid(int PidType)
{
    unsigned int Pid;
    ILE3 JpiItems[2], *Ile3Ptr;
    int status;
    IOSB iosb;

    ILE3_INIT(JpiItems);
    ILE3_ADD(PidType, sizeof(Pid), &Pid, 0);
    ILE3_TERM;

/*
** Get the requested PID
*/
    status = SYS$GETJPIW(EFN$C_ENF, 0, 0, &JpiItems, &iosb, 0, 0);
    if (status & 1)
	status = iosb.iosb$w_status;
    if (!(status & 1))
	return (0);

/*
** Return the requested PID
*/
    return (Pid);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static int GetUic()
{
    ILE3 JpiItems[2], *Ile3Ptr;
    UICDEF Uic;
    int status;
    IOSB iosb;

    ILE3_INIT(JpiItems);
    ILE3_ADD(JPI$_UIC, sizeof(Uic.uic$l_uic), &Uic.uic$l_uic, 0);
    ILE3_TERM;

/*
** Get the current callers UIC
*/
    status = SYS$GETJPIW(EFN$C_ENF, 0, 0, &JpiItems, &iosb, 0, 0);
    if (status & 1)
	status = iosb.iosb$w_status;
    if (!(status & 1))
	return (0);

/*
** Return the current callers UIC
*/
    return (Uic.uic$l_uic);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static size_t _strlen(char *str)
{
    size_t i;

    for (i = 0; i < 0xFFFF; i++)
	if (str[i] == '\0')
	    break;

    return (i);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static void *_memcpy(char *dest, char *source, size_t size)
{
    size_t i;

    for (i = 0; i < size; i++)
	dest[i] = source[i];

    return (dest);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static int _strncmp(char *str_1, char *str_2, size_t maxchar)
{
    size_t i;

    for (i = 0; i < maxchar; i++)
	if (str_1[i] != str_2[i])
	    break;

    if (i == maxchar)
	return (0);
    if (str_1[i] < str_2[i])
	return (-1);
    else
	return (1);

}

/******************************************************************************/
/***                                                                        ***/
/*** Rundown handlers are invoked before any system rundown is performed.   ***/
/***                                                                        ***/
/*** These routines are provided simply as placeholders for real rundown    ***/
/*** handlers.  A user-written rundown handler should not invoke any RMS    ***/
/*** services or RTL routines, and must not signal any exceptions.  User-   ***/
/*** written rundown handlers can invoke most system services except those  ***/
/*** that use RMS (i.e.  $PUTMSG).                                          ***/
/***                                                                        ***/
/******************************************************************************/
int krnl_rundown()
{

/*
** Indicate success
*/
    return SS$_NORMAL;

}

int exec_rundown()
{

/*
** Indicate success
*/
    return SS$_NORMAL;

}
