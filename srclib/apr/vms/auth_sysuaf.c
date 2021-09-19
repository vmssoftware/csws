#pragma module AUTH_SYSUAF "V1.00"

/*
**  FACILITY:
**	Compaq Secure Web Server for OpenVMS Alpha
**
**  ABSTRACT:
**	This module provides a basic routine for authenticating a user based
**	on a username and password contained in the SYSUAF records.
**
**	NOTE: This routine is designed to be called by a user-written
**	system service (i.e. protected shareable image).  However, the PLV
**	wrapper is in a separate module so that this module can include the
**	STARLET LGIDEF.H instead of the SYS$LIB_C LGIDEF.H.
**
**  RESTRICTION:
**	There is no support for secondary passwords or external authentication
**	at this time.  (The MOD_AUTH_xxx services only provide one password.)
**
**  CREATION DATE:  November 1, 2000
**
**  AUTHORS:
**	Kevin D. O'Kelley
**
**  MODIFICATION HISTORY:
**
**	000	KDO/01-Nov-2000
**		Created.
**
**	001	KDO/27-Nov-2000
**		Code review input.  In particular, fix the SETPRV calls so that
**		we disable only those bits that were not previously enabled.
**		Return only SS$_NORMAL, SS$_INVARG, or LGI$_NOTVALID.
**
**	002	KDO/29-Nov-2000
**		Add an optional parameter to vms_auth_princ_sysuaf_int():
**		pointer to the $GETUAI context.
**
**	003	KDO/01-Dec-2000
**		Fix the network access and password expiration checks.
**
**	004	KDO/12-Dec-2000
**		Fix call to SYS$SCAN_INTRUSION: Only include failed passwords.
**
**	005	RJB/02-Feb-2001
**		Fix call to SYS$SCAN_INTRUSION so that proper auditing takes
**		place and intrusion records are created when username is
**		blank.
**
**	006	KDO/18-Sep-2001
**		Return LGI$_PWDEXPIR if the password has expired.
**
**	007	KDO/02-Nov-2001
**		Return LGI$_NOSUCHUSER if the SYSUAF record doesn't exist.
**		Add support for hidden accounts identified by the optional
**		APACHE$MOD_AUTH_OPENVMS_{ENABLE,DISABLE} identifier.
**
**	008	MPD/15-Jan-2003
**		Added Apache 2.0 API changes
**
**
**      009     RJB/04-Dec-2003
**              Allow EXTAUTH accounts to use SYSUAF-based authentication
**              if APACHE$MODAUTHVMS_EXTAUTH_USE_SYSUAF (exec-mode) logical
**              name is defined.
*/
#define __NEW_STARLET   1


/*
**  Include files
*/
#include <ctype.h>
#include <types.h>
#include <limits.h>


/*
**  OpenVMS include files
*/
#include <utcblkdef>
#include <acmedef>
#include <stdlib.h>
#include <ssdef.h>
#include <ciadef.h>
#include <iledef.h>
#include <jpidef.h>
#include <lgidef.h>
#include <prvdef.h>
#include <stsdef.h>
#include <uaidef.h>
#include <lnmdef.h>
#include <psldef.h>
#include <descrip.h>
#include <starlet.h>
#include <builtins.h>
#include <secsrvmsgdef.h>
#include <efndef.h>
#include <string.h>
#include <iledef>

/*
**  Conditional asssembly macros
*/
#ifndef __VMS_AUTH_SYSUAF_DEBUG
#define __VMS_AUTH_SYSUAF_DEBUG 0		/* 1= debugging */
#endif
#if     __VMS_AUTH_SYSUAF_DEBUG
#define DEBUG_PRINTF(ARGS) printf ARGS
#define DEBUG_OFF (0)
#define DEBUG_ON (1)
#include <stdio.h>
#include <string.h>
#else
#define DEBUG_PRINTF(ARGS)
#define DEBUG_OFF (1)
#define DEBUG_ON (0)
#endif


/*
**  Definitions
*/
#ifndef IN
#define IN
#endif
#ifndef OUT
#define OUT
#endif
#ifndef INTERNAL
#define INTERNAL static
#endif
#ifndef NULL
#define NULL (void *) 0
#endif
#ifndef alloca
#define alloca __ALLOCA
#endif
#ifndef memcpy
#define memcpy __MEMCPY
#endif
#ifndef memset
#define memset __MEMSET
#endif
#define MIN(A,B) ((A) < (B) ? (A) : (B))
#define MAX(A,B) ((A) > (B) ? (A) : (B))

#ifdef EXIT_SUCCESS
#undef EXIT_SUCCESS
#endif
#ifdef EXIT_FAILURE
#undef EXIT_FAILURE
#endif
#define EXIT_SUCCESS SS$_NORMAL
#define EXIT_FAILURE SS$_ABORT
#define BAD_STATUS(I) (((I) & STS$M_SUCCESS) == 0)
#define GOOD_STATUS(I) (((I) & STS$M_SUCCESS) == 1)
#define STATUS_OK SS$_NORMAL


/*
**  Structures
*/
typedef signed   char   CHAR;
typedef unsigned char   BYTE, UCHAR;
typedef signed   short  SHORT;
typedef unsigned short  USHORT;
typedef signed   int    INT;
typedef unsigned int    UINT, VMS_STATUS;
typedef signed   long   LONG;
typedef unsigned long   ULONG;
typedef signed   __int64 LONGLONG;
typedef unsigned __int64 ULONGLONG;

typedef struct dsc$descriptor_s DSC_S;
typedef struct dsc$descriptor_d DSC_D;
#define DSC_S_INIT(STR) \
        { (sizeof(STR) - 1), DSC$K_DTYPE_T, DSC$K_CLASS_S, (char *) STR }

// #ifndef ILE3
// #define ILE3 ile3
// #endif


/*
**  Authentication information acquired by SYS$GETUAI.
*/
typedef struct
{
    ULONGLONG expiration;		/* expiration date/time */
    ULONGLONG pwd, pwd2;		/* hashed passwords */
    ULONG flags;			/* UAIDEF flags */
    ULONG primedays;			/* primary versus secondary days */
    struct				/* internal flags */
    {
        unsigned fPwdExpired : 1;	/* password has expired */
        unsigned fAcctHidden : 1;	/* account is hidden */
        unsigned : 30;			/* reserved */
    }
    stFlags;
    USHORT salt;			/* encryption salt */
    UCHAR encrypt, encrypt2;		/* encryption algorithms */
    UCHAR network_access_p[3];		/* primary times access is allowed */
    UCHAR network_access_s[3];		/* secondary times access is allowed */
}
SYSUAF_INFO;


/*
**  The context block is the process-wide global data for the transaction.
*/
typedef struct				/* context block */
{
    ULONGLONG now;			/* current date/time */
    VMS_STATUS stLoginFailure;		/* LGI$_xxx status code */
    DSC_S dscsUsername;			/* user name in upper case */
    DSC_S dscsPassword;			/* plaintext password in upper case */
    SYSUAF_INFO sysuaf;			/* authentication information */
    DSC_S dscsHostName;			/* DNS name in upper case */
}
CTXBLK;


/*
**  Function prototypes
*/
INTERNAL int get_sysuaf_info(IN CTXBLK *pctx, IN OUT UINT *puiUafContext);
INTERNAL int check_sysuaf_info(IN CTXBLK *pctx);
INTERNAL int check_sysuaf_hours(IN CTXBLK *pctx);
INTERNAL int check_sysuaf_password(IN CTXBLK *pctx);
INTERNAL int scan_intrusion_db(IN CTXBLK *pctx);
INTERNAL int get_weekday(IN ULONGLONG ullTime);
INTERNAL int get_hour_of_day(IN ULONGLONG ullTime);
INTERNAL int is_account_hidden(IN UINT uiUic);
INTERNAL int set_sysuaf_info(IN CTXBLK *pctx);


/*
**  Global variables
*/
int g_vms_auth_sysuaf_fInit = 0;	/* 0= not initialized */
int g_vms_auth_sysuaf_fHide;		/* 0= allow account to be used */
unsigned int g_vms_auth_sysuaf_uiIdent;	/* identifier (if any) to look for */


/*
**  v m s _ a u t h _ s y s u a f _ i n i t
**
**  Initialize this module, particularly the global variables.
**  Return with SS$_NORMAL (always).
*/
VMS_STATUS vms_auth_sysuaf_init()
{
    VMS_STATUS st;
    unsigned int uiId;
    static const char acIdEnable[]  = "APACHE$MOD_AUTH_OPENVMS_ENABLE";
    static const char acIdDisable[] = "APACHE$MOD_AUTH_OPENVMS_DISABLE";
    static const DSC_S dscsIdEnable = DSC_S_INIT(acIdEnable);
    static const DSC_S dscsIdDisable = DSC_S_INIT(acIdDisable);


    /*
    **  If the identifier is defined to identify individual accounts that can
    **  be used, then save that value, and clear the flag to hide accounts.
    **  If not, check to see if the identifier is defined that identifies
    **  accounts that are disabled.  If neither is defined, assume that all
    **  accounts are valid.
    */
    st = sys$asctoid((DSC_S *) &dscsIdEnable, &uiId, NULL);
    if (GOOD_STATUS(st))
    {
        g_vms_auth_sysuaf_uiIdent = uiId;
        g_vms_auth_sysuaf_fHide = 0;
    }
    else
    {
        st = sys$asctoid((DSC_S *) &dscsIdDisable, &uiId, NULL);
        if (GOOD_STATUS(st))
        {
            g_vms_auth_sysuaf_uiIdent = uiId;
            g_vms_auth_sysuaf_fHide = -1;
        }
        else
        {
            g_vms_auth_sysuaf_uiIdent = 0;
            g_vms_auth_sysuaf_fHide = 0;
        }
    }
    g_vms_auth_sysuaf_fInit++;
    return(SS$_NORMAL);
}



#ifndef OKAY
#define OKAY(STATUS) (((STATUS) & 1) != 0)
#endif

static unsigned int acm_validate(char *username, char *password)
{
    unsigned int status = SS$_NORMAL;
    ACMESB sblk;
    ACMEFC func;
    unsigned int type = ACME$K_NETWORK;
    ILE3 list[6];
    unsigned int idx;

    if ((username == NULL) || (password == NULL)) {
	return (SS$_NOPRIV);
    }

    idx = 0;
    memset(&list, 0, sizeof(list));

    func.acmefc$v_function = ACME$_FC_AUTHENTICATE_PRINCIPAL;
    func.acmefc$v_modifiers = 0;

    list[idx].ile3$w_code = ACME$_LOGON_TYPE;
    list[idx].ile3$ps_bufaddr = &type;
    list[idx].ile3$w_length = sizeof(type);
    idx++;

    list[idx].ile3$w_code = ACME$_PRINCIPAL_NAME_IN;
    list[idx].ile3$ps_bufaddr = username;
    list[idx].ile3$w_length = strlen(username);
    idx++;

    list[idx].ile3$w_code = ACME$_PASSWORD_1;
    list[idx].ile3$ps_bufaddr = password;
    list[idx].ile3$w_length = strlen(password);
    idx++;

    status = sys$acmw(0, func.acmefc$l_fcode_struct, 0, list, &sblk, 0, 0);

    if (OKAY(status)) {
	status = sblk.acmesb$l_status;
    }

    return (status);
}


/*
**  a u t h _ p r i n c _ s y s u a f _ i n t
**
**  Authenticate a username and password.
**  Return with an OpenVMSstatus code:
**
**      SS$_NORMAL
**	SS$_INVARG
**	SS$_NOPRIV
**	LGI$_NOTVALID
**	LGI$_NOSUCHUSER
**	LGI$_PWDEXPIR
*/
VMS_STATUS auth_princ_sysuaf_int
(
    IN const char *pcszUsername, IN const int ccUsername,
    IN const char *pcszPassword, IN const int ccPassword,
    IN const char *pcszHostName, IN const int ccHostName,
    IN OUT UINT *puiUafContext
)
{
    int i, iReturn;
    VMS_STATUS st;
    char *pcSrc, *pcDest;
    CTXBLK ctxBlock, *pctx = &ctxBlock;
    char *UAF_LOGINUPDATE_RATE=NULL;
    static int acm = -1;

    if (acm == -1) {
        if (getenv("apache$auth_use_acm")) {
	    acm = 1;
	} else {
	   acm = 0;
	}
    }

    if (g_vms_auth_sysuaf_fInit == 0)
        (void) vms_auth_sysuaf_init();

    memset((void *) pctx, 0, sizeof(*pctx));
    if (pcszUsername == NULL)
        return(SS$_INVARG);
    if ((ccUsername == 0) || (ccUsername > UCHAR_MAX))
        return(SS$_INVARG);
    if (pcszPassword == NULL)
        return(SS$_INVARG);
    if ((ccPassword == 0) || (ccPassword > UCHAR_MAX))
        return(SS$_INVARG);

    if (acm == 1) {
	return (acm_validate((char *) pcszUsername, (char *) pcszPassword));
    }

    if (pcszHostName == NULL)
        return(SS$_INVARG);
    if ((ccHostName == 0) || (ccHostName > UCHAR_MAX))
        return(SS$_INVARG);
    sys$gettim((struct _generic_64 *) &pctx->now);


    pctx->dscsUsername.dsc$w_length = ccUsername;
    pctx->dscsUsername.dsc$a_pointer =
            alloca(pctx->dscsUsername.dsc$w_length + 1);
    pctx->dscsUsername.dsc$b_dtype = DSC$K_DTYPE_T;
    pctx->dscsUsername.dsc$b_class = DSC$K_CLASS_S;
    pcSrc = (char *) pcszUsername;
    pcDest = pctx->dscsUsername.dsc$a_pointer;
    for (i = 0; i < ccUsername; i++)
    {
        if (*pcSrc == '\0')
        {
            memset((void *) pctx->dscsPassword.dsc$a_pointer, 0,
                    pctx->dscsPassword.dsc$w_length);
            return(SS$_INVARG);
        }
        *pcDest++ = ((*pcSrc) >= 'a' && (*pcSrc) <= 'z') ?
                (*pcSrc++) & 0xDF : (*pcSrc++);
    }
    *pcDest = '\0';

    pctx->dscsHostName.dsc$w_length = ccHostName;
    pctx->dscsHostName.dsc$a_pointer =
            alloca(pctx->dscsHostName.dsc$w_length + 1);
    pctx->dscsHostName.dsc$b_dtype = DSC$K_DTYPE_T;
    pctx->dscsHostName.dsc$b_class = DSC$K_CLASS_S;
    pcSrc = (char *) pcszHostName;
    pcDest = pctx->dscsHostName.dsc$a_pointer;
    for (i = 0; i < ccHostName; i++)
    {
        if (*pcSrc == '\0')
        {
            memset((void *) pctx->dscsHostName.dsc$a_pointer, 0,
                    pctx->dscsHostName.dsc$w_length);
            return(SS$_INVARG);
        }
        *pcDest++ = ((*pcSrc) >= 'a' && (*pcSrc) <= 'z') ?
                (*pcSrc++) & 0xDF : (*pcSrc++);
    }
    *pcDest = '\0';


    DEBUG_PRINTF(("(auth_princ_sysuaf_int) username=\"%s\"\n",
            pctx->dscsUsername.dsc$a_pointer));
    iReturn = get_sysuaf_info(pctx, puiUafContext);

    /* only change the case of the password after getting the uai flags
     */

    pctx->dscsPassword.dsc$w_length = ccPassword;
    pctx->dscsPassword.dsc$a_pointer =
            alloca(pctx->dscsPassword.dsc$w_length + 1);
    pctx->dscsPassword.dsc$b_dtype = DSC$K_DTYPE_T;
    pctx->dscsPassword.dsc$b_class = DSC$K_CLASS_S;
    pcSrc = (char *) pcszPassword;
    pcDest = pctx->dscsPassword.dsc$a_pointer;
    for (i = 0; i < ccPassword; i++)
    {
        if (*pcSrc == '\0')
        {
            memset((void *) pctx->dscsPassword.dsc$a_pointer, 0,
                    pctx->dscsPassword.dsc$w_length);
            return(SS$_INVARG);
        }
        /*
        ** If mixed case passwords are enabled for this account,
        ** send the password as it is. Convert to upper case otherwise.
        */
        if ((pctx->sysuaf.flags & UAI$M_PWDMIX) != 0)
        {
            *pcDest++ = *pcSrc++;
        }
        else
        {
            *pcDest++ = ((*pcSrc) >= 'a' && (*pcSrc) <= 'z') ?
                        (*pcSrc++) & 0xDF : (*pcSrc++);
        }
    }
    *pcDest = '\0';

    if (iReturn == 0)
        iReturn = check_sysuaf_info(pctx);
    if (iReturn == 0)
        iReturn = check_sysuaf_password(pctx);
    iReturn = iReturn | scan_intrusion_db(pctx);
    memset((void *) pctx->dscsPassword.dsc$a_pointer, 0,
            pctx->dscsPassword.dsc$w_length);
    if (iReturn != 0)
    {
        st = pctx->stLoginFailure;
        if (pctx->sysuaf.stFlags.fAcctHidden)
            st = LGI$_NOSUCHUSER;
        DEBUG_PRINTF(("(auth_princ_sysuaf_int) FAILED, st=0x%08x\n", st));
        memset((void *) pctx, 0, sizeof(*pctx));
        if ((st == SS$_NOPRIV) || (st == LGI$_NOSUCHUSER))
            return(st);
        return(LGI$_NOTVALID);
    }
    if (pctx->sysuaf.stFlags.fPwdExpired != 0)
    {
        memset((void *) pctx, 0, sizeof(*pctx));
        DEBUG_PRINTF(("(auth_princ_sysuaf_int) LGI$_PWDEXPIR\n"));
        return(LGI$_PWDEXPIR);
    }

    /*
    **Set the last non-interactive login time in sysuaf
    ** set only when below logical in not define or defined to 0
    **
    */
    UAF_LOGINUPDATE_RATE=getenv("APACHE$UAF_LOGINUPDATE_RATE");
    if((UAF_LOGINUPDATE_RATE==NULL) || !(strcmp(UAF_LOGINUPDATE_RATE,"0")))
    {
     iReturn = set_sysuaf_info(pctx);
     if (iReturn !=0 )
     {
       DEBUG_PRINTF(("Non-interactive login time update failed\n"));
     }
    }
    memset((void *) pctx, 0, sizeof(*pctx));
    DEBUG_PRINTF(("(auth_princ_sysuaf_int) OK\n"));
    return(SS$_NORMAL);
}


/*
**  g e t _ s y s u a f _ i n f o
**
**  Read the necessary authentication information from the SYSUAF file.
**  Return with zero for success or minus one for failure.
*/
INTERNAL int get_sysuaf_info(IN CTXBLK *pctx, IN OUT UINT *puiUafContext)
{
    VMS_STATUS st;
    ULONGLONG ullOldPriv;
    ULONGLONG ullPwdDate;
    LONGLONG llPwdLifetime;
    unsigned int uiUic;
    ILE3 itmlst[14], *pitmlst;
    USHORT usTimLen;
    DSC_D dscdTimBuf;
    char acTimBuf[32];
    static const ULONGLONG ullNewPriv = PRV$M_SYSPRV;


    pitmlst = &itmlst[0];
    pitmlst->ile3$w_code = UAI$_ENCRYPT;
    pitmlst->ile3$w_length = sizeof(pctx->sysuaf.encrypt);
    pitmlst->ile3$ps_bufaddr = &pctx->sysuaf.encrypt;
    pitmlst->ile3$ps_retlen_addr = NULL;
    pitmlst++;
    pitmlst->ile3$w_code = UAI$_ENCRYPT2;
    pitmlst->ile3$w_length = sizeof(pctx->sysuaf.encrypt2);
    pitmlst->ile3$ps_bufaddr = &pctx->sysuaf.encrypt2;
    pitmlst->ile3$ps_retlen_addr = NULL;
    pitmlst++;
    pitmlst->ile3$w_code = UAI$_EXPIRATION;
    pitmlst->ile3$w_length = sizeof(pctx->sysuaf.expiration);
    pitmlst->ile3$ps_bufaddr = &pctx->sysuaf.expiration;
    pitmlst->ile3$ps_retlen_addr = NULL;
    pitmlst++;
    pitmlst->ile3$w_code = UAI$_FLAGS;
    pitmlst->ile3$w_length = sizeof(pctx->sysuaf.flags);
    pitmlst->ile3$ps_bufaddr = &pctx->sysuaf.flags;
    pitmlst->ile3$ps_retlen_addr = NULL;
    pitmlst++;
    pitmlst->ile3$w_code = UAI$_NETWORK_ACCESS_P;
    pitmlst->ile3$w_length = sizeof(pctx->sysuaf.network_access_p);
    pitmlst->ile3$ps_bufaddr = &pctx->sysuaf.network_access_p;
    pitmlst->ile3$ps_retlen_addr = NULL;
    pitmlst++;
    pitmlst->ile3$w_code = UAI$_NETWORK_ACCESS_S;
    pitmlst->ile3$w_length = sizeof(pctx->sysuaf.network_access_s);
    pitmlst->ile3$ps_bufaddr = &pctx->sysuaf.network_access_s;
    pitmlst->ile3$ps_retlen_addr = NULL;
    pitmlst++;
    pitmlst->ile3$w_code = UAI$_PRIMEDAYS;
    pitmlst->ile3$w_length = sizeof(pctx->sysuaf.primedays);
    pitmlst->ile3$ps_bufaddr = &pctx->sysuaf.primedays;
    pitmlst->ile3$ps_retlen_addr = NULL;
    pitmlst++;
    pitmlst->ile3$w_code = UAI$_PWD;
    pitmlst->ile3$w_length = sizeof(pctx->sysuaf.pwd);
    pitmlst->ile3$ps_bufaddr = &pctx->sysuaf.pwd;
    pitmlst->ile3$ps_retlen_addr = NULL;
    pitmlst++;
    pitmlst->ile3$w_code = UAI$_PWD2;
    pitmlst->ile3$w_length = sizeof(pctx->sysuaf.pwd2);
    pitmlst->ile3$ps_bufaddr = &pctx->sysuaf.pwd2;
    pitmlst->ile3$ps_retlen_addr = NULL;
    pitmlst++;
    pitmlst->ile3$w_code = UAI$_SALT;
    pitmlst->ile3$w_length = sizeof(pctx->sysuaf.salt);
    pitmlst->ile3$ps_bufaddr = &pctx->sysuaf.salt;
    pitmlst->ile3$ps_retlen_addr = NULL;
    pitmlst++;
    pitmlst->ile3$w_code = UAI$_PWD_DATE;
    pitmlst->ile3$w_length = sizeof(ullPwdDate);
    pitmlst->ile3$ps_bufaddr = &ullPwdDate;
    pitmlst->ile3$ps_retlen_addr = NULL;
    pitmlst++;
    pitmlst->ile3$w_code = UAI$_PWD_LIFETIME;
    pitmlst->ile3$w_length = sizeof(llPwdLifetime);
    pitmlst->ile3$ps_bufaddr = &llPwdLifetime;
    pitmlst->ile3$ps_retlen_addr = NULL;
    pitmlst++;
    pitmlst->ile3$w_code = UAI$_UIC;
    pitmlst->ile3$w_length = sizeof(uiUic);
    pitmlst->ile3$ps_bufaddr = &uiUic;
    pitmlst->ile3$ps_retlen_addr = NULL;
    pitmlst++;
    memset(pitmlst, 0, sizeof(*pitmlst));


    st = sys$setprv(1, (struct _generic_64 *) &ullNewPriv, 0, (struct _generic_64 *) &ullOldPriv);
    if (BAD_STATUS(st))
    {
        pctx->stLoginFailure = st;
        return(-1);
    }
    st = sys$getuai(0, puiUafContext, &pctx->dscsUsername, &itmlst[0],
            NULL, NULL, 0);
    if (GOOD_STATUS(st) && (g_vms_auth_sysuaf_uiIdent != 0))
    {
        if (is_account_hidden(uiUic))
            pctx->sysuaf.stFlags.fAcctHidden = 1;
    }
    ullOldPriv = (ullOldPriv ^ ullNewPriv) & ullNewPriv;
    if (ullOldPriv != 0)
        sys$setprv(0, (struct _generic_64 *) &ullOldPriv, 0, 0);
    if (BAD_STATUS(st))
    {
        DEBUG_PRINTF(("(get_sysuaf_info) sys$getuai()=0x%x\n", st));
        pctx->stLoginFailure = LGI$_NOSUCHUSER;
        return(-1);
    }
    DEBUG_PRINTF(("(get_sysuaf_info) sys$getuai() OK\n"));
    if (pctx->sysuaf.stFlags.fAcctHidden)
    {
        DEBUG_PRINTF(("(get_sysuaf_info) LGI$_RESTRICT\n", st));
        pctx->stLoginFailure = LGI$_RESTRICT;
        return(-1);
    }


    if (llPwdLifetime == 0)
    {
        DEBUG_PRINTF(("(get_sysuaf_info) Password never expires.\n"));
        return(0);
    }
    if (ullPwdDate == (unsigned int) -1)
    {
        DEBUG_PRINTF(("(get_sysuaf_info) Password is pre-expired.\n"));
        pctx->sysuaf.stFlags.fPwdExpired = 1;
        return(0);
    }
    if (DEBUG_ON)
    {
        dscdTimBuf.dsc$b_dtype = DSC$K_DTYPE_T;
        dscdTimBuf.dsc$b_class = DSC$K_CLASS_S;
        dscdTimBuf.dsc$w_length = sizeof(acTimBuf);
        dscdTimBuf.dsc$a_pointer = acTimBuf;
        st = sys$asctim(&usTimLen, &dscdTimBuf, (struct _generic_64 *) &ullPwdDate, 0);
        if (GOOD_STATUS(st))
            acTimBuf[usTimLen] = '\0';
        else
            acTimBuf[0] = '\0';
        DEBUG_PRINTF(("(get_sysuaf_info) pwd_date=0x%016llx %s\n",
                ullPwdDate, acTimBuf));
        st = sys$asctim(&usTimLen, &dscdTimBuf, (struct _generic_64 *) &llPwdLifetime, 0);
        if (GOOD_STATUS(st))
            acTimBuf[usTimLen] = '\0';
        else
            acTimBuf[0] = '\0';
        DEBUG_PRINTF(("(get_sysuaf_info) pwd_lifetime=%ld seconds %s\n",
                (-llPwdLifetime / (10 * 1000 * 1000)), acTimBuf));
    }
    ullPwdDate -= llPwdLifetime;
    if (ullPwdDate <= pctx->now)
        pctx->sysuaf.stFlags.fPwdExpired = 1;
    if (DEBUG_ON)
    {
        st = sys$asctim(&usTimLen, &dscdTimBuf, (struct _generic_64 *) &ullPwdDate, 0);
        if (GOOD_STATUS(st))
            acTimBuf[usTimLen] = '\0';
        else
            acTimBuf[0] = '\0';
        DEBUG_PRINTF(("(get_sysuaf_info) expiration=0x%016llx %s\n",
                ullPwdDate, acTimBuf));
    }
    return(0);
}

/*
**  extauth_use_sysuaf
**
**  EXTAUTH accounts use SYSUAF-based authentication if the
**  exec-mode logical name APACHE$MODAUTHVMS_EXTUATH_USE_SYSUAF is
**  defined (to anything).
*/
INTERNAL int extauth_use_sysuaf(void)
{
    int status;
    struct { short length, code; char *buffer; int *ret_len; } item[2];
    static $DESCRIPTOR(logical_name, "APACHE$MODAUTHVMS_EXTAUTH_USE_SYSUAF");
    static $DESCRIPTOR(name_table, "LNM$FILE_DEV");
    unsigned char acmode = PSL$C_EXEC;
    unsigned int attr = LNM$M_CASE_BLIND;
    int equiv_len = 0;
    char equiv_str[16];

    item[0].length = 16;
    item[0].code = LNM$_STRING;
    item[0].buffer = equiv_str;
    item[0].ret_len = &equiv_len;
    equiv_len = 0;

    item[1].code = item[1].length = 0;          /* terminate list */

    status = SYS$TRNLNM (&attr, &name_table, &logical_name, &acmode, item);
    if (((status&1) == 1) && (equiv_len > 0)) return(1);

    return(0);
}

/*
**  c h e c k _ s y s u a f _ i n f o
**
**  Sanity check the authentication information from the SYSUAF file.
**  Return with zero for success or minus one for failure.
*/
INTERNAL int check_sysuaf_info(IN CTXBLK *pctx)
{
    int iReturn = 0;


    if ((pctx->sysuaf.flags & UAI$M_EXTAUTH) != 0)
    {
        if (!extauth_use_sysuaf())
        {
            DEBUG_PRINTF(("(check_sysuaf_info) UAI$M_EXTAUTH\n"));
            pctx->stLoginFailure = LGI$_NOEXTAUTH;
            iReturn = -1;
        }
    }
    if ((pctx->sysuaf.flags & UAI$M_PWD_EXPIRED) != 0)
    {
        DEBUG_PRINTF(("(check_sysuaf_info) UAI$M_PWD_EXPIRED\n"));
        pctx->stLoginFailure = LGI$_PWDEXPIR;
        iReturn = -1;
    }
    if ((pctx->sysuaf.flags & UAI$M_DISACNT) != 0)
    {
        DEBUG_PRINTF(("(check_sysuaf_info) UAI$M_DISACNT\n"));
        pctx->stLoginFailure = LGI$_DISUSER;
        iReturn = -1;
    }
    if (pctx->sysuaf.pwd == 0)
    {
        DEBUG_PRINTF(("(check_sysuaf_info) no primary password\n"));
        pctx->stLoginFailure = LGI$_INVPWD;
        iReturn = -1;
    }
    if (pctx->sysuaf.pwd2 != 0)
    {
        DEBUG_PRINTF(("(check_sysuaf_info) secondary password set\n"));
        pctx->stLoginFailure = LGI$_INVPWD;
        iReturn = -1;
    }
    if ((pctx->sysuaf.expiration != 0) &&
            (pctx->sysuaf.expiration <= pctx->now))
    {
        DEBUG_PRINTF(("(check_sysuaf_info) expired\n"));
        pctx->stLoginFailure = LGI$_ACNTEXPIR;
        iReturn = -1;
    }
    if (check_sysuaf_hours(pctx) != 0)
        iReturn = -1;
    if (DEBUG_ON && (iReturn == 0))
        DEBUG_PRINTF(("(check_sysuaf_info) OK\n"));
    return(iReturn);
}


/*
**  c h e c k _ s y s u a f _ h o u r s
**
**  Check to see if the user is allowed access at this time.  Specifically,
**  check the access masks for the primary and secondary days.  Each bit
**  represents a 1-hour period with bit 0 representing midnight to 1 a.m.
**  and bit 23 representing 11 p.m. to midnight.  If the bit is zero, then
**  access is allowed.
**  Return with zero for success or minus one for failure.
*/
INTERNAL int check_sysuaf_hours(IN CTXBLK *pctx)
{
    LONG iHour, iWeekday;
    ULONG aulAccessMasks[] = { 0, 0 };	/* primary, second access masks */
    int iAccess = 0;			/* flag and index into access masks */
					/* 0= primary day */


    iWeekday = get_weekday(pctx->now);
    iHour = get_hour_of_day(pctx->now);
    if ((pctx->sysuaf.primedays & (1 << (iWeekday - 1))) != 0)
        iAccess = 1;			/* 1= secondary day */
    DEBUG_PRINTF(("(check_sysuaf_hours) primedays=0x%x weekday=%d (%s) hours=%d\n",
            pctx->sysuaf.primedays, iWeekday,
            ((iAccess == 0) ? "prime" : "secondary"), iHour));
    memcpy((void *) &aulAccessMasks[0],
            (const void *) &pctx->sysuaf.network_access_p,
            sizeof(pctx->sysuaf.network_access_p));
    memcpy((void *) &aulAccessMasks[1],
            (const void *) &pctx->sysuaf.network_access_s,
            sizeof(pctx->sysuaf.network_access_s));
    DEBUG_PRINTF(("(check_sysuaf_hours) network_access_p=0x%08x "
            "network_access_s=0x%08x\n",
            aulAccessMasks[0], aulAccessMasks[1]));
    if ((aulAccessMasks[iAccess] & (1 << iHour)) == 0)
    {
        DEBUG_PRINTF(("(check_sysuaf_hours) OK\n"));
        return(0);
    }
    if (aulAccessMasks[iAccess] != 0xffffff)
    {
        DEBUG_PRINTF(("(check_sysuaf_hours) LGI$_BADHOUR\n"));
        pctx->stLoginFailure = LGI$_BADHOUR;
        return(-1);
    }
    if (aulAccessMasks[iAccess ^ 1] != 0xffffff)
    {
        DEBUG_PRINTF(("(check_sysuaf_hours) LGI$_BADDAY\n"));
        pctx->stLoginFailure = LGI$_BADDAY;
        return(-1);
    }
    DEBUG_PRINTF(("(check_sysuaf_hours) LGI$_RESTRICT\n"));
    pctx->stLoginFailure = LGI$_RESTRICT;
    return(-1);
}


/*
**  c h e c k _ s y s u a f _ p a s s w o r d
**
**  Test the password against the SYSUAF info.
**  Return with zero for success or minus one for failure.
*/
INTERNAL int check_sysuaf_password(IN CTXBLK *pctx)
{
    VMS_STATUS st;
    ULONGLONG ullHash;


    st = sys$hash_password(&pctx->dscsPassword, pctx->sysuaf.encrypt,
            pctx->sysuaf.salt, &pctx->dscsUsername, (struct _generic_64 *) &ullHash);
    if (BAD_STATUS(st))
    {
        DEBUG_PRINTF(("(check_sysuaf_password) sys$hash_password()=0x%x\n", st));
        pctx->stLoginFailure = st;
        return(-1);
    }
    DEBUG_PRINTF(("(check_sysuaf_password) hash_value=0x%016llx\n", ullHash));
    if (ullHash != pctx->sysuaf.pwd)
    {
        ullHash = 0;
        DEBUG_PRINTF(("(check_sysuaf_password)  expecting=0x%016llx\n",
                pctx->sysuaf.pwd));
        pctx->stLoginFailure = LGI$_INVPWD;
        return(-1);
    }
    ullHash = 0;
    DEBUG_PRINTF(("(check_sysuaf_password) OK\n"));
    return(0);
}


/*
**  s c a n _ i n t r u s i o n _ d b
**
**  Scan the intrusion database for intruders, audit login failures, and
**  update the intrusion database.
**  Return with zero for success or minus one for failure.
**
**  Intrusion records generated by this routine have the format
**  "host-name::user-name" where <host-name> is the IP-address of
**  the client (supplied by the caller) and <user-name> is a valid
**  username or "<invalid>". Because <host-name> is a component of
**  the intrusion string, a nefarious caller could supply a unique
**  address for a sequence of calls to avoid intrusion detection,
**  But these privileged routines are restricted to callers running
**  under the APACHE$WWW username, so we trust it.
**
**  NOTE! For proxy servers where multiple clients come in from the same
**  host name, a single client can lock-out a given username by
**  triggering break-in evasion.
*/
INTERNAL int scan_intrusion_db(IN CTXBLK *pctx)
{
    VMS_STATUS st;
    ULONG ulFlags = 0;
    ULONGLONG ullOldPriv;
    DSC_S *pdscsInvPwd = NULL;
    static const ULONGLONG ullNewPriv = PRV$M_SECURITY;
    static const DSC_S dscsSrcInvUser = DSC_S_INIT("<invalid>");

    DEBUG_PRINTF(("(scan_intrusion_db) status=0x%08x\n", pctx->stLoginFailure));
    if (pctx->stLoginFailure == 0)
        pctx->stLoginFailure = SS$_NORMAL;
    if (pctx->stLoginFailure != LGI$_NOSUCHUSER)
        ulFlags = CIA$M_REAL_USERNAME;
    if (pctx->stLoginFailure == LGI$_INVPWD)
        pdscsInvPwd = &pctx->dscsPassword;
    st = sys$setprv(1, (struct _generic_64 *) &ullNewPriv, 0, (struct _generic_64 *) &ullOldPriv);
    if (BAD_STATUS(st))
    {
        pctx->stLoginFailure = st;
        return(-1);
    }
    st = sys$scan_intrusion(
            pctx->stLoginFailure,
            &pctx->dscsUsername,
            JPI$K_NETWORK,
            NULL,
            &pctx->dscsHostName,
            (DSC_S *) (ulFlags && CIA$M_REAL_USERNAME ?
                    &pctx->dscsUsername : &dscsSrcInvUser),
            NULL,
            pdscsInvPwd,
            NULL,
            0,
            ulFlags);
    ullOldPriv = (ullOldPriv ^ ullNewPriv) & ullNewPriv;
    if (ullOldPriv != 0)
        sys$setprv(0, (struct _generic_64 *) &ullOldPriv, 0, 0);
    if (st == SECSRV$_INTRUDER)
    {
        DEBUG_PRINTF(("(scan_intrusion_db) SECSRV$_INTRUDER\n"));
        pctx->stLoginFailure = LGI$_NOTVALID;
        return(-1);
    }
    DEBUG_PRINTF(("(scan_intrusion_db) sys$scan_intrusion()=0x%08x\n", st));
    if (BAD_STATUS(st))
        return(-1);
    return(0);
}


/*
**  g e t _ w e e k d a y
**
**  Given a system time, return with the day of the week, where Monday is one
**  and Sunday is seven.  LIB$DAY_OF_WEEK is not used because the routine may
**  not be available in executive mode, and this module can be called from a
**  user-written system service.
**
**  The input arument is the system time as the number of 100-nanosecond
**  (i.e. 10 millisecond) clock ticks since November 17, 1858 (Wednesday).
*/
INTERNAL int get_weekday(IN ULONGLONG ullTime)
{
    int iDay;
    static const ULONGLONG ullTicksInSecond = 10 * 1000 * 1000;
    static const ULONGLONG ullSecondsInDay = 24 * 60 * 60;


    ullTime = ullTime / ullTicksInSecond;
    ullTime = ullTime / ullSecondsInDay;
    iDay = ((int) ullTime + 2) % 7;
    return(iDay + 1);
}


/*
**  g e t _ h o u r _ o f _ d a y
**
**  Given a system time, return with the hour of the day (from zero to
**  twenty-three).  LIB$CVT_FROM_INTERNAL_TIME is not used because the
**  routine may not be available in executive mode, and this module can be
**  called from a user-written system service.
**
**  The input arument is the system time as the number of 100-nanosecond
**  (i.e. 10 millisecond) clock ticks since November 17, 1858 (Wednesday).
*/
INTERNAL int get_hour_of_day(IN ULONGLONG ullTime)
{
    int iHour;
    ULONGLONG ullTemp;
    static const ULONGLONG ullTicksInSecond = 10 * 1000 * 1000;
    static const ULONGLONG ullSecondsInHour = 60 * 60;


    ullTemp = ullTime / ullTicksInSecond;
    ullTemp = ullTemp / ullSecondsInHour;
    iHour = ullTemp % 24;
    return(iHour);
}


/*
**  i s _ a c c o u n t _ h i d d e n
**
**  Check the rights list for the specified UIC to determine whether or not
**  the account is hidden.
**  Return with zero for false and minus one for true.
**
**  NOTE: This routine requires privileges.
*/
INTERNAL int is_account_hidden(IN UINT uiUic)
{
    VMS_STATUS st;
    unsigned int auiHolder[2], uiId;
    unsigned int uiFindContext;


    auiHolder[0] = uiUic;
    auiHolder[1] = 0;
    uiFindContext = 0;
    while (1)
    {
        st = sys$find_held((struct _generic_64 *) auiHolder, &uiId, NULL, &uiFindContext);
        if (BAD_STATUS(st))
            break;
        if (uiId == g_vms_auth_sysuaf_uiIdent)
        {
            (void) sys$finish_rdb(&uiFindContext);
            DEBUG_PRINTF(("(is_account_hidden) id=0x%08x, %s\n", uiId,
                    (g_vms_auth_sysuaf_fHide ? "hidden" : "not hidden")));
            return(g_vms_auth_sysuaf_fHide);
        }
        DEBUG_PRINTF(("(is_account_hidden) id=0x%08x\n", uiId));
    }
    (void) sys$finish_rdb(&uiFindContext);
    DEBUG_PRINTF(("(is_account_hidden) %s\n",
            (g_vms_auth_sysuaf_fHide ? "not hidden" : "hidden")));
    return(~g_vms_auth_sysuaf_fHide);
}

/*
**  s e t _ s y s u a f _ i n f o
**
**  Update the last non-interactive login time in SYSUAF
**
*/
INTERNAL int set_sysuaf_info(IN CTXBLK *pctx)
{
    VMS_STATUS st;
    int retVal = -1;
    ULONGLONG ullOldPriv;
    ILE3 itmlst[2], *pitmlst;
    static const ULONGLONG ullNewPriv = PRV$M_SYSPRV;


    pitmlst = &itmlst[0];
    pitmlst->ile3$w_code = UAI$_LASTLOGIN_N;
    pitmlst->ile3$w_length = sizeof(pctx->now);
    pitmlst->ile3$ps_bufaddr = &pctx->now;
    pitmlst->ile3$ps_retlen_addr = NULL;
    pitmlst++;
    memset(pitmlst, 0, sizeof(*pitmlst));



    st = sys$setprv(1, (struct _generic_64 *) &ullNewPriv, 0, (struct _generic_64 *) &ullOldPriv);
    if (BAD_STATUS(st))
    {
        pctx->stLoginFailure = st;
        return(retVal);
    }
    st = sys$setuai(0,NULL, &pctx->dscsUsername, &itmlst[0],
            NULL, NULL, 0);
    if (GOOD_STATUS(st))
    {
       retVal=0;
    }
    ullOldPriv = (ullOldPriv ^ ullNewPriv) & ullNewPriv;
    if (ullOldPriv != 0)
        sys$setprv(0, (struct _generic_64 *) &ullOldPriv, 0, 0);

    return(retVal);
}




#ifndef __VMS_AUTH_SYSUAF_MAIN
#define __VMS_AUTH_SYSUAF_MAIN 0
#endif

#if     __VMS_AUTH_SYSUAF_MAIN

#include <stdio.h>
#include <limits.h>
#include <string.h>

/*
**  m a i n _ r o u t i n e
**
**  Main routine used for testing purposes.
*/
int main(int argc, char **argv)
{
    VMS_STATUS st;
    ULONGLONG ullOldPriv;
    static const ULONGLONG ullAllPriv = __UINT64_MAX;
    static const ULONGLONG ullNewPriv = PRV$M_NETMBX | PRV$M_TMPMBX;


    if (argc <= 3)
    {
        printf("ERROR: Insufficient arguments: %s <username> <password> <host-name>\n",
                argv[0]);
        return(EXIT_FAILURE);
    }
    (void) sys$setprv(0, &ullAllPriv, 0, &ullOldPriv);
    (void) sys$setprv(1, &ullNewPriv, 0, 0);
    st = auth_princ_sysuaf_int(
            (const char *) argv[1], strlen(argv[1]),
            (const char *) argv[2], strlen(argv[2]),
            (const char *) argv[3], strlen(argv[3]),
            NULL);
    printf("[ auth_princ_sysuaf_int()=0x%08x ]\n", st);
    (void) sys$setprv(1, &ullOldPriv, 0, 0);
    if (BAD_STATUS(st))
        return(EXIT_FAILURE);
    return(EXIT_SUCCESS);
}



#endif  /* __VMS_AUTH_SYSUAF_MAIN */

