#pragma module CHECK_RIGHTS "V1.00"

/*
**  FACILITY:
**	Compaq Secure Web Server for OpenVMS Alpha
**
**  ABSTRACT:
**	This module provides a routine to return a list of identifiers
**	held by a specified user.
**
**	NOTE: This routine is designed to be called by a user-written
**	system service (i.e. protected shareable image).  However, the PLV
**	wrapper is in a separate module so that this module can include the
**	STARLET LGIDEF.H instead of the SYS$LIB_C LGIDEF.H.
**
**  CREATION DATE:  August 17,2001
**
**  AUTHORS:
**	Kevin D. O'Kelley
**
**  MODIFICATION HISTORY:
**
**      000     KDO/17-Aug-2001
**		Created.
**
**	001	MPD/15-Jan-2003
**		Added Apache 2.0 API changes
**
*/



/*
**  Include files
*/
#include <ctype.h>
#include <errno.h>
#include <types.h>
#include <limits.h>


/*
**  OpenVMS include files
*/
#include <ssdef.h>
#include <iledef.h>
#include <jpidef.h>
#include <kgbdef.h>
#include <lgidef.h>
#include <prvdef.h>
#include <stsdef.h>
#include <uaidef.h>
#include <uicdef.h>
#include <descrip.h>
#include <starlet.h>
#include <builtins.h>


/*
**  Conditional asssembly macros
*/
#ifndef __VMS_CHECK_RIGHTS_DEBUG
#define __VMS_CHECK_RIGHTS_DEBUG 0	/* 1= debugging */
#endif
#if     __VMS_CHECK_RIGHTS_DEBUG
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
typedef signed char CHAR;
typedef unsigned char BYTE, UCHAR;
typedef signed short SHORT;
typedef unsigned short USHORT;
typedef signed int INT;
typedef unsigned int UINT, VMS_STATUS;
typedef signed long LONG;
typedef unsigned long ULONG;
typedef signed __int64 LONGLONG;
typedef unsigned __int64 ULONGLONG;

typedef struct dsc$descriptor_s DSC_S;
typedef struct dsc$descriptor_d DSC_D;
#define DSC_S_INIT(STR) \
        { (sizeof(STR) - 1), DSC$K_DTYPE_T, DSC$K_CLASS_S, (char *) STR }

#ifndef ILE3
#define ILE3 ile3
#endif


/*
**  Function prototypes
*/
INTERNAL int is_member_of_list(IN unsigned int uiId,
			       IN const unsigned int *pcuiIdentList,
			       IN int ccIdentList);

VMS_STATUS vms_auth_sysuaf_init();


/*
**  Global variables
*/
extern int g_vms_auth_sysuaf_fInit;	/* 0= not initialized */
extern int g_vms_auth_sysuaf_fHide;	/* 0= allow account to be used */
extern unsigned int g_vms_auth_sysuaf_uiIdent;	/* identifier (if any) */


/*
**  c h e c k _ r i g h t s _ l i s t _ i n t
**
**  For a given user and list of identifiers, see if any of those identifiers
**  have been granted to the user.
**  Return with the OpenVMS status code:
**
**      SS$_NORMAL
**	SS$_INVARG
**	SS$_NOPRIV
**	SS$_NOCALLPRIV
**	LGI$_NOSUCHUSER
**
**  NOTE: We return with SS$_NOCALLPRIV instead of SS$_NOPRIV to differentiate
**  a rejection by this routine versus a case where this routine lacks the
**  necessary privileges to make the test.
**
**  NOTE: If the number of identifiers is zero, then the purpose of the call
**  is to determine whether or not the account is valid (i.e. not hidden).
*/
VMS_STATUS check_rights_list_int
    (IN const char *pcszUsername, IN int ccUsername,
     IN const unsigned int *pcuiIdentList, IN int ccIdentList,
     IN OUT UINT * puiUafContext) {
    int i;
    VMS_STATUS st;
    int iListMatch;
    unsigned int uiUic, auiHolder[2], uiId;
    DSC_S dscsUsername;
    ULONGLONG ullOldPriv;
    static const ULONGLONG ullNewPriv = PRV$M_SYSPRV;
    static const char *pcszThis = "check_rights_list_int";
    ILE3 itmlst[2], *pitmlst;
    static unsigned int uiUaiContext = (unsigned int) -1;
    static unsigned int uiFindContext;
    enum {
	ACCOUNT_E_UNKNOWN, ACCOUNT_E_VALID, ACCOUNT_E_INVALID
    } eAccountState;


    /*
     **  This argument checking is assumed to have already been done.
     */
#if 0
    if (pcszUsername == NULL)
	return (SS$_INVARG);
    if ((ccUsername == 0) || (ccUsername > UCHAR_MAX))
	return (SS$_INVARG);
    if (((ccIdentList > 0) && (pcuiIdentList == NULL)) ||
	((ccIdentList == 0) && (pcuiIdentList != NULL)) ||
	(ccIdentList < 0))
	return (SS$_INVARG);
#endif


    /*
     **  Initialization.
     */
    if (g_vms_auth_sysuaf_fInit == 0)
	(void) vms_auth_sysuaf_init();
    if (puiUafContext == NULL)
	puiUafContext = &uiUaiContext;
    dscsUsername.dsc$b_dtype = DSC$K_DTYPE_T;
    dscsUsername.dsc$b_class = DSC$K_CLASS_S;
    dscsUsername.dsc$w_length = ccUsername;
    dscsUsername.dsc$a_pointer = (char *) pcszUsername;
    DEBUG_PRINTF(("(%s) username=\"%.*s\"\n", pcszThis,
		  dscsUsername.dsc$w_length, dscsUsername.dsc$a_pointer));


    /*
     **  Convert the user name to a UIC.
     */
    pitmlst = &itmlst[0];
    pitmlst->ile3$w_code = UAI$_UIC;
    pitmlst->ile3$w_length = sizeof(uiUic);
    pitmlst->ile3$ps_bufaddr = &uiUic;
    pitmlst->ile3$ps_retlen_addr = NULL;
    pitmlst++;
    memset(pitmlst, 0, sizeof(*pitmlst));
    st = sys$setprv(1, &ullNewPriv, 0, &ullOldPriv);
    if (BAD_STATUS(st))
	return (SS$_NOPRIV);
    ullOldPriv = (ullOldPriv ^ ullNewPriv) & ullNewPriv;
    st = sys$getuai(NULL, puiUafContext, &dscsUsername, &itmlst[0],
		    NULL, NULL, NULL);
    if (BAD_STATUS(st)) {
	if (ullOldPriv != 0)
	    (void) sys$setprv(0, &ullOldPriv, 0, 0);
	DEBUG_PRINTF(("(%s) sys$getuai failed, st=0x%x\n", pcszThis, st));
	return (LGI$_NOSUCHUSER);
    }
    DEBUG_PRINTF(("(%s) sys$getuai() OK, uic=0x%08x\n", pcszThis, uiUic));


    /*
     **  If the list of identifiers is empty, then pretend there is a match
     **  (i.e. any valid account is OK).  Otherwise, use the group portion of
     **  the UIC to create a group identifier and see if that is in the list.
     */
    eAccountState = (g_vms_auth_sysuaf_uiIdent != 0) ?
	ACCOUNT_E_UNKNOWN : ACCOUNT_E_VALID;
    iListMatch = 0;
    if (ccIdentList == 0)
	iListMatch++;
    else if (is_member_of_list
	     ((uiUic | 0xffff), pcuiIdentList, ccIdentList))
	iListMatch++;
    if ((iListMatch > 0) && (eAccountState == ACCOUNT_E_VALID)) {
	if (ullOldPriv != 0)
	    (void) sys$setprv(0, &ullOldPriv, 0, 0);
	return (SS$_NORMAL);
    }


    /*
     **  Loop through all of the identifiers held by the user.  We will exit
     **  the loop when one of the following is true:
     **      1.  The account has no more identifiers to test.
     **      2.  We find the DISABLE identifier (i.e. the account is hidden).
     **      3.  We find the ENABLE identifier after a matching rights list
     **          identifier has been found.
     **      4.  We find a matching rights identifier for a valid account.
     */
    auiHolder[0] = uiUic;
    auiHolder[1] = 0;
    uiFindContext = 0;
    while (1) {
	st = sys$find_held(auiHolder, &uiId, NULL, &uiFindContext);
	if (BAD_STATUS(st))
	    break;
	if (uiId == 0)
	    continue;
	if (uiId == g_vms_auth_sysuaf_uiIdent) {
	    if (g_vms_auth_sysuaf_fHide != 0) {
		eAccountState = ACCOUNT_E_INVALID;
		break;
	    }
	    eAccountState = ACCOUNT_E_VALID;
	    if (iListMatch > 0)
		break;
	} else if (iListMatch == 0) {
	    if (is_member_of_list(uiId, pcuiIdentList, ccIdentList)) {
		iListMatch++;
		if (eAccountState != ACCOUNT_E_UNKNOWN)
		    break;
	    }
	}
    }
    (void) sys$finish_rdb(&uiFindContext);
    if (ullOldPriv != 0)
	(void) sys$setprv(0, &ullOldPriv, 0, 0);
    if ((eAccountState == ACCOUNT_E_UNKNOWN)
	&& (g_vms_auth_sysuaf_fHide == 0))
	eAccountState = ACCOUNT_E_INVALID;
    if (eAccountState == ACCOUNT_E_INVALID) {
	DEBUG_PRINTF(("(%s) LGI$_NOSUCHUSER\n", pcszThis));
	return (LGI$_NOSUCHUSER);
    }
    DEBUG_PRINTF(("(%s) %s\n", pcszThis,
		  ((iListMatch > 0) ? "OK" : "SS$_NOCALLPRIV")));
    if (iListMatch <= 0)
	return (SS$_NOCALLPRIV);
    return (SS$_NORMAL);
}


/*
**  i s _ m e m b e r _ o f _ l i s t
**
**  Check to see if the specified identifier is a member of the list.
**  Return with minus one for true or zero for false.
*/
INTERNAL int is_member_of_list(IN unsigned int uiId,
			       IN const unsigned int *pcuiIdentList,
			       IN int ccIdentList)
{
    int i;


    for (i = 0; i < ccIdentList; i++) {
	if (uiId == pcuiIdentList[i]) {
	    DEBUG_PRINTF(("(is_member_of_list) 0x%08x OK\n", uiId));
	    return (-1);
	}
    }
    DEBUG_PRINTF(("(is_member_of_list) 0x%08x not found\n", uiId));
    return (0);
}


#ifndef __VMS_CHECK_RIGHTS_MAIN
#define __VMS_CHECK_RIGHTS_MAIN 0
#endif

#if     __VMS_CHECK_RIGHTS_MAIN

#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <unistd.h>


/*
**  c v t _ n a m e s _ t o _ i d s
**
**  Convert an array of ASCIZ strings to list of identifiers.
**  Return with zero for success, minus one for failure.
**
**  NOTE: If the number of strings does not equal the number of identifiers
**  returned, then some of the strings are not identifiers.  This is not a
**  fatal error.
*/
INTERNAL int cvt_names_to_ids(IN const int iArg, IN const char **ppszArg,
			      OUT int *piIds,
			      OUT unsigned int **ppuiBuffer)
{
    int i;
    VMS_STATUS st;
    unsigned int uiId;
    unsigned int uiAttrib;
    DSC_S dscsName;
    static const char *pcszThis = "cvt_names_to_ids";
    static unsigned int uiRdbCtx = 0;
    struct {
	int iUsed;		/* number of identifiers */
	int iAllocated;		/* number of slots allocated */
	int aiIds[10];		/* initial allocation of space */
    } *pstIds, *pstIdsNew;


    pstIds = malloc(sizeof(*pstIds));
    if (pstIds == NULL)
	return (-1);
    pstIds->iUsed = 0;
    pstIds->iAllocated = sizeof(pstIds->aiIds) / sizeof(pstIds->aiIds[0]);
    dscsName.dsc$b_dtype = DSC$K_DTYPE_T;
    dscsName.dsc$b_class = DSC$K_CLASS_S;


    for (i = 0; i < iArg; i++) {
	dscsName.dsc$a_pointer = ppszArg[i];
	dscsName.dsc$w_length = strlen(dscsName.dsc$a_pointer);
	if (dscsName.dsc$w_length == 0)
	    continue;
	if (dscsName.dsc$w_length > CHAR_MAX) {
	    errno = EINVAL;
	    return (-1);
	}
	st = sys$asctoid(&dscsName, &uiId, &uiAttrib);
	if (BAD_STATUS(st)) {
	    DEBUG_PRINTF(("(%s) sys$asctoid(\"%.*s\") status=0x%x\n",
			  pcszThis, dscsName.dsc$w_length,
			  dscsName.dsc$a_pointer, st));
	    continue;
	}
	DEBUG_PRINTF(("(%s) \"%.*s\" = 0x%08x (attrib=0x%08x)\n",
		      pcszThis, dscsName.dsc$w_length,
		      dscsName.dsc$a_pointer, uiId, uiAttrib));


	/*
	 **  Only keep resource identifiers and plain identifiers, including
	 **  group ids and UICs.
	 */
	if (((uiAttrib & KGB$M_RESOURCE) == 0) && (uiAttrib != 0))
	    continue;


	/*
	 **  If the structure is filled, then re-allocate additional memory.
	 **  The additional amount will be the same size as the initial
	 **  allocation.
	 */
	if (pstIds->iUsed >= pstIds->iAllocated) {
	    pstIdsNew = realloc(pstIds, sizeof(*pstIds) +
				(pstIds->iAllocated *
				 sizeof(pstIds->aiIds[0])));
	    if (pstIdsNew == NULL) {
		free(pstIds);
		return (-1);
	    }
	    pstIds = pstIdsNew;
	    pstIds->iAllocated +=
		sizeof(pstIds->aiIds) / sizeof(pstIds->aiIds[0]);
	}
	pstIds->aiIds[pstIds->iUsed++] = uiId;
    }


    /*
     **  Set the return values.  The output buffer is malloc'd space and
     **  must be free'd by the called.
     */
    if (piIds != NULL)
	*piIds = pstIds->iUsed;
    if (ppuiBuffer != NULL) {
	*ppuiBuffer = malloc(pstIds->iUsed * sizeof(int));
	if (*ppuiBuffer == NULL) {
	    free(pstIds);
	    return (-1);
	}
	memcpy(*ppuiBuffer, pstIds->aiIds, (pstIds->iUsed * sizeof(int)));
    }
    free(pstIds);
    return (0);
}


/*
**  m a i n _ r o u t i n e
**
**  Main routine used for testing purposes.
*/
int main(int argc, char **argv)
{
    int iIds;
    VMS_STATUS st = SS$_NORMAL;
    unsigned int *puiIds;
    char *pszUsername;
    ULONGLONG ullOldPriv;
    static const ULONGLONG ullAllPriv = __UINT64_MAX;
    static const ULONGLONG ullNewPriv = PRV$M_NETMBX | PRV$M_TMPMBX;


    (void) sys$setprv(0, &ullAllPriv, 0, &ullOldPriv);
    (void) sys$setprv(1, &ullNewPriv, 0, 0);
    if (cvt_names_to_ids((argc - 1), &argv[1], &iIds, &puiIds))
	puiIds = NULL;
    pszUsername = getlogin();
    st = check_rights_list_int((const char *) pszUsername,
			       strlen(pszUsername), (const int *) puiIds,
			       iIds, NULL);
    (void) sys$setprv(1, &ullOldPriv, 0, 0);
    if (BAD_STATUS(st)) {
	printf("check_rights_list_int(%s) = 0x%x\n", pszUsername, st);
	return (EXIT_FAILURE);
    }
    printf("vms_check_rights_list(%s) OK\n", pszUsername);
    return (EXIT_SUCCESS);
}
#endif				/* __VMS_CHECK_RIGHTS_MAIN */
