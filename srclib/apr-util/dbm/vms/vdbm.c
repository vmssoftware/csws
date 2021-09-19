/*
**  FACILITY:
**	Compaq Secure Web Server for OpenVMS Alpha
**
**  ABSTRACT:
**	This module provides an RMS equivalent to the NDBM/SDBM routines
**	data storage routines.
**
**	In particular, the VDBM routines allow a user to save records 
**	using very large variable-length keys, something that RMS does not 
**	support.  Therefore, we store a fixed-length portion of the key and
**	a hash value.
**
**      Performance comparison for a series of tests with medium-length
**	text keys and 5000 records:
**
**                                           SDBM port     VDBM version
**      Writing the Records                   1019.745          121.813
**      Reading with firstkey/nextkey           52.672            2.107
**      Reading with sequential keys             8.446            5.627
**      Reading in random order                  5.521            5.382
**      Replacing 501 records                    5.380            5.498
**      Deleting 501 records                     5.454            7.798
**      Total elapsed time                    1284.506          183.720
**
**  CREATION DATE:  April 6, 2001
**
**  AUTHORS:
**	Kevin D. O'Kelley
**
**  MODIFICATION HISTORY:
**
**	000	KDO/06-Apr-2001
**		Created.
**
**	001	KDO/05-Feb-2002
**		Allow UNIX-style file specifications.
**
**	002	KDO/12-Feb-2002
**		Add ODS5 long-filename support.  In particular, look at the
**		length of the file name.  If it is longer than 255 characters,
**		then use the new NAML blocks.  This, of course, assumes that
**		the user is using long file names on an operating system that
**		can accomodate it.  It also assumes that the user will not
**		use a bizzare UNIX file specification on anything but an ODS5
**		file system.  However, if the user fails to do that, then the
**		user will get a reasonable error code, and that is fair.
**
**	003	KDO/30-Apr-2002
**		Add more debugging logic.
**
**	004	SAL/27-Jul-2005
**		Add more support for ODS-5 by escaping '.' in directory
**		and filenames.
*/


/*
**  Include files 
*/
#include <file.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <limits.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <types.h>

#include "vdbm.h"

/*
**  OpenVMS include files
*/
#include <ssdef.h>
#include <iledef.h>
#include <stsdef.h>
#include <descrip.h>
#include <starlet.h>
#include <builtins.h>
#include <rms.h>


/*
**  Conditional asssembly macros
*/
#ifndef __VDBM_DEBUG
#define __VDBM_DEBUG 0		/* 1= debugging */
#endif
#if     __VDBM_DEBUG
#define DEBUG_PRINTF(ARGS) vms_debug_printf ARGS
#define DEBUG_OFF (0)
#define DEBUG_ON (1)
#include <stdio.h>
#include <string.h>
#else
#define DEBUG_PRINTF(ARGS) 
#define DEBUG_OFF (1)
#define DEBUG_ON (0)
#endif

#ifndef __VDBM_LONG_FILENAMES
#define __VDBM_LONG_FILENAMES 1	/* 1= ODS5 long filename support */
#endif
#if     __VDBM_FILENAMES
#ifdef  PATH_MAX
#undef  PATH_MAX
#endif
#define PATH_MAX NAML$C_MAXRSS
#endif


/*
**  Definitions
*/
#define ROP_RDONLY (RAB$M_WAT | RAB$M_RRL | RAB$M_NLK) /* read record */
#define ROP_RDWR (RAB$M_WAT | RAB$M_RLK) /* lock record */

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
typedef unsigned int    UINT;
typedef signed   long   LONG;
typedef unsigned long   ULONG, VMS_STATUS;
typedef signed   __int64 LONGLONG;
typedef unsigned __int64 ULONGLONG;

typedef struct dsc$descriptor_s DSC_S;
typedef struct dsc$descriptor_d DSC_D;
#define DSC_S_INIT(STR) \
        { (sizeof(STR) - 1), DSC$K_DTYPE_T, DSC$K_CLASS_S, STR }

#ifndef ILE2
#define ILE2 ile2
#endif

#ifndef ILE3
#define ILE3 ile3
#endif


/*
**  Global variables
*/
extern DATUM nullitem = {NULL, 0};


/*
**  This data structure represents a primary key
**
**  NOTE: The format of a record is: (1) fixed key, (2) remainder of key and
**  (3) value.  Note that the fixed portion is small but large enough to fit
**  a very long integer.  Also notice that the fixed portion of the key must
**  be at the end of this structure because the remainder of the key follows
**  in the record, and we don't want to move the bytes around or copy them to
**  assemble a key datum.
*/
#define KEYBLK_FIXEDSIZE (sizeof(ULONGLONG) * 1) // size of fixed portion 
#define KEYBLK_OVERHEAD (sizeof(KEYBLK) - KEYBLK_FIXEDSIZE)
typedef struct KEYBLK
{
    INT iSize;				/* size of the entire key */
    ULONG ulCrc;			/* CRC of the entire key */
    BYTE abFixed[KEYBLK_FIXEDSIZE];	/* last: fixed portion of text */
}
KEYBLK;


/*
**  This data structure is the internal representation of the file.
**
**  NOTE: The user is given the address of the DBM structure.  However, that
**  address is also the address of the VMSBLK structure, that contains the
**  necesssary RMS extensions.
*/
#define VMSBLK_TYPE 0xdb0101		/* identification of VMSBLK */
typedef struct VMSBLK
{
    VDBM dbmUser;			/* DBM structure (must be first) */ 
    UINT uiType;			/* block type (VMSBLK_TYPE) */
    VDBM *pdbmUser;			/* address of dbmUser */
    VMS_STATUS st;			/* status code */
    struct FAB fab;			/* RMS File Access Block */
    struct RAB rabkey;			/* RMS Record Access Block (indexed) */
    struct RAB rabseq;			/* RMS Record Access Block (seq) */
    struct XABKEY xabkey;		/* RMS primary key */
    struct XABPRO xabpro;		/* RMS protection */
    INT iBufferSize;			/* I/O buffer size */
    BYTE *pbBuffer;			/* I/O buffer */
#if __VDBM_LONG_FILENAMES
    struct NAML naml;			/* RMS Long Name Block (optional) */
#endif
}
VMSBLK;


/*
**  Function prototypes
*/
INTERNAL VMSBLK *vms_malloc_vmsblk();
INTERNAL VMSBLK *vms_verify_vmsblk(VMSBLK *pvms);
INTERNAL void vms_free_vmsblk(VMSBLK *pvms);
INTERNAL DATUM vms_make_keyblk(KEYBLK *pkey, DATUM dtKey);
INTERNAL int vms_find_recd(VMSBLK *pvms, KEYBLK *pkey, DATUM dtKeyExtra);
INTERNAL unsigned short vms_make_xabpro(int iMask);
INTERNAL int vms_cvt_path(char *pszPath, char *pszResult);
INTERNAL int vms_debug_key(const char *pcszBegin, const char *pcszEnd,
        int ccKey, const char *pcsKey);
INTERNAL int vms_debug_key_value(const char *pcszBegin, const char *pcszEnd,
        int ccKey, const char *pcsKey, int ccValue, const char *pcsValue);
INTERNAL int vms_debug_recd(const char *pcszBegin, const char *pcszEnd,
        size_t ccRecd, const char *pcbRecd);
INTERNAL void vms_debug_render(int ccData, const char *pcszData, char *pszText);

#if __VDBM_DEBUG
INTERNAL vms_debug_printf(const char *format, ...);
#endif

extern int decc$$translate(int vms_error_code);


/*
**  v d b m _ o p e n 
**
**  Open an indexed data file.
**  Return with the address of the DBM structure or NULL for failure.
*/
VDBM *vdbm_open(char *pszName, int iFlags, int iMode)
{
    char *pcTemp;
    char acPFile[PATH_MAX + 1];


    DEBUG_PRINTF(("(vdbm_open) pszName=\"%s\"\n", pszName));
    if (pszName == NULL)
    {
        (void) decc$$translate(SS$_INVARG);
        return(NULL);
    }
    strcpy(acPFile, pszName);


    /*
    **  Guard against double extensions.  Find the end of the directory
    **  specification, then see if the file name already has a period.
    **  If it does, then don't add this extension.  Instead, append to the
    **  existing extension with an underscore ("_").
    **
    **  Removed by SAL 27-Jul-2005 - instead, the Unix -> VMS
    **  translate escapes the other '.' characters.  See routine
    **  vms_cvt_path() below.  This is possible by the new restriction
    **  that CSWS *MUST* run on an ODS-5 device now (V2.x).
    */
    pcTemp = strrchr((const char *) acPFile, '/');
    if (pcTemp == NULL)
        pcTemp = strrchr((const char *) acPFile, ']');
    if (pcTemp == NULL)
        pcTemp = acPFile;
/*    pcTemp = strchr((const char *) pcTemp, '.');  */
    strcat(acPFile, VDBM_DIRFEXT);
/*    if (pcTemp != NULL)  */
/*        acPFile[strlen(pszName)] = '_';  */
    return(vdbm_prep(acPFile, NULL, iFlags, iMode));
}


/*
**  v d b m _ p r e p 
**
**  Open an indexed data file.  This routine allows the user to specify the
**  directory file specifcation.  However, there is no directory file in 
**  these routines.
**  Return with the address of the DBM structure or NULL for failure.
**
**  NOTE: This routine is part of the VDBM interface but not part of NDBM.
*/
VDBM *vdbm_prep(char *pszPFile, char *pszDFile, int iFlags, int iMode)
{
    int ccPFile;
    VMSBLK *pvms;
    char *pcTemp;
    char acKey0Name[32];
    char acPFile[PATH_MAX + 1];


    /*
    **  If the file contains a slash ("/"), then treat it as a UNIX-style
    **  path specification.  Otherwise check for a version number.  If it
    **  is missing, then add ";" to specify latest-and-greatest.
    */
    DEBUG_PRINTF(("(vdbm_prep) pszPFile=\"%s\"\n", pszPFile));
    if (pszPFile == NULL)
    {
        (void) decc$$translate(SS$_INVARG);
        return(NULL);
    }
    if ((pvms = vms_malloc_vmsblk()) == NULL)
    {
        (void) decc$$translate(SS$_INSFMEM);
        return(NULL);
    }
    if (strchr(pszPFile, '/') != NULL)
    {
        if (vms_cvt_path(pszPFile, acPFile))
        {
            vms_free_vmsblk(pvms);
            return(NULL);
        }
        pszPFile = acPFile;
    }
    else
    {
        pcTemp = strrchr((const char *) acPFile, ']');
        if (pcTemp == NULL)
            pcTemp = pszPFile;
        pcTemp = strchr((const char *) pcTemp, ';');
        if (pcTemp == NULL)
        {
            strcpy(acPFile, (const char *) pszPFile);
            strcat(acPFile, ";");
            pszPFile = acPFile;
        }
    }


    /*
    **  Initialize the RMS structures.
    */
    pvms->fab = cc$rms_fab;		/* RMS File Access Block */
    pvms->fab.fab$l_fop = FAB$M_CIF | FAB$M_CBT;
    if (iFlags & (O_WRONLY | O_RDWR))
        pvms->fab.fab$b_fac = FAB$M_GET | FAB$M_PUT | FAB$M_DEL | FAB$M_UPD;
    else
    {
        pvms->fab.fab$b_fac = FAB$M_GET;
        /* [kdo][todo] set DBM flags to DBM_RDONLY */
    }
    pvms->fab.fab$b_shr = FAB$M_SHRGET | FAB$M_SHRPUT | FAB$M_SHRDEL | 
            FAB$M_SHRUPD | FAB$M_MSE;
    pvms->fab.fab$b_org = FAB$C_IDX;
    pvms->fab.fab$b_rfm = FAB$C_VAR;
    pvms->fab.fab$w_mrs = (PAIRMAX * 2) + KEYBLK_OVERHEAD;
    ccPFile = strlen(pszPFile);
    if (ccPFile <= NAM$C_MAXRSS)
    {
        pvms->fab.fab$l_fna = (char *) pszPFile;
        pvms->fab.fab$b_fns = ccPFile;
    }
    else
#if __VDBM_LONG_FILENAMES
    {
        pvms->fab.fab$l_fna = (char *) -1;
        pvms->fab.fab$l_naml = &pvms->naml;
        pvms->naml = cc$rms_naml;
        pvms->naml.naml$l_long_filename = (char *) pszPFile;
        pvms->naml.naml$l_long_filename_size = ccPFile;
    }
#else
    {
        vms_free_vmsblk(pvms);
        (void) decc$$translate(SS$_INVARG);
        return(NULL);
    }
#endif
    pvms->fab.fab$l_xab = (char *) &pvms->xabkey;
    pvms->xabkey = cc$rms_xabkey;	/* RMS primary key */
    pvms->xabkey.xab$l_knm = acKey0Name;
    pvms->xabkey.xab$b_ref = 0;
    pvms->xabkey.xab$w_pos0 = 0;
    pvms->xabkey.xab$b_siz0 = sizeof(KEYBLK);
    pvms->xabkey.xab$b_dtp = XAB$C_STG;
    pvms->xabkey.xab$b_flg = XAB$M_DUP;
    pvms->xabkey.xab$l_nxt = &pvms->xabpro;
    pvms->xabpro = cc$rms_xabpro;	/* RMS protection */
    pvms->xabpro.xab$w_pro = vms_make_xabpro(iMode);


    memset(acKey0Name, 0, sizeof(acKey0Name));
    strcpy(acKey0Name, "FIXED");
    if (iFlags & O_CREAT)
    {
        pvms->st = sys$create(&pvms->fab);
        if (BAD_STATUS(pvms->st))
        {
            DEBUG_PRINTF(("(vdbm_prep) sys$open() failed, "
                    "st=0x%x\n", pvms->st));
            (void) decc$$translate(pvms->st);
            vms_free_vmsblk(pvms);
            return(NULL);
        }
    }
    else
    {
        pvms->st = sys$open(&pvms->fab);
        if (BAD_STATUS(pvms->st))
        {
            DEBUG_PRINTF(("(vdbm_prep) sys$open() failed, "
                    "st=0x%x\n", pvms->st));
            (void) decc$$translate(pvms->st);
            vms_free_vmsblk(pvms);
            return(NULL);
        }
    }


    pvms->rabkey = cc$rms_rab;		/* RMS Record Access Block */
    pvms->rabkey.rab$b_rac = RAB$C_KEY;
    pvms->rabkey.rab$l_rop = ROP_RDONLY;
    pvms->rabkey.rab$l_fab = &pvms->fab;
    pvms->st = sys$connect(&pvms->rabkey);
    if (BAD_STATUS(pvms->st))
    {
        DEBUG_PRINTF(("(vdbm_prep) sys$connect(rabkey) failed, "
                "st=0x%x\n", pvms->st));
        (void) decc$$translate(pvms->st);
        vms_free_vmsblk(pvms);
        return(NULL);
    }


    pvms->rabseq = cc$rms_rab;		/* RMS Record Access Block */
    pvms->rabseq.rab$b_rac = RAB$C_SEQ;
    pvms->rabseq.rab$l_rop = ROP_RDONLY;
    pvms->rabseq.rab$l_fab = &pvms->fab;
    pvms->st = sys$connect(&pvms->rabseq);
    if (BAD_STATUS(pvms->st))
    {
        DEBUG_PRINTF(("(vdbm_prep) sys$connect(rabseq) failed, "
                "st=0x%x\n", pvms->st));
        (void) decc$$translate(pvms->st);
        vms_free_vmsblk(pvms);
        return(NULL);
    }


    pvms->iBufferSize = pvms->fab.fab$w_mrs;
    pvms->pbBuffer = malloc(pvms->iBufferSize);
    if (pvms->pbBuffer == NULL)
    {
        (void) decc$$translate(SS$_INSFMEM);
        vms_free_vmsblk(pvms);
        return(NULL);
    }
    DEBUG_PRINTF(("(vdbm_prep) OK\n"));
    return((VDBM *) pvms);
}


/*
**  v d b m _ c l o s e 
**
**  Close the file and deallocate the data structure.
*/
void vdbm_close(VDBM *pdbmUser)
{
    VMSBLK *pvms;


    pvms = vms_verify_vmsblk((VMSBLK *) pdbmUser);
    if (vms_verify_vmsblk(pvms) == NULL)
        return;
    if (pvms->fab.fab$l_stv != 0)
    {
        pvms->st = sys$close(&pvms->fab);
        if (BAD_STATUS(pvms->st))
            DEBUG_PRINTF(("(vdbm_close) sys$close() failed, "
                    "st=0x%x\n", pvms->st));
        else
            DEBUG_PRINTF(("(vdbm_close) sys$close() OK\n"));
    }
    vms_free_vmsblk(pvms);
}

/*
**  v d b m _ s t o r e 
**
**  Write a new record or update an existing record.
*/
int vdbm_store(VDBM *pdbmUser, DATUM dtKey, DATUM dtValue, int iFlags)
{
    int ccRecd;
    VMSBLK *pvms;
    DATUM dtKeyExtra;
    BYTE *pbBuffer;


    /*
    **  Build the KEYBLK structure and the record to be written.
    */
    if (DEBUG_ON)
        vms_debug_key("(vdbm_store)", NULL, dtKey.dsize,
                (const char *) dtKey.dptr);
    pvms = vms_verify_vmsblk((VMSBLK *) pdbmUser);
    if (vms_verify_vmsblk(pvms) == NULL)
        return(-1);
    if ((iFlags != VDBM_INSERT) && (iFlags != VDBM_REPLACE))
    {
        (void) decc$$translate(SS$_INVARG);
        return(-1);
    }
    pbBuffer = (BYTE *) alloca(sizeof(KEYBLK) + dtKey.dsize + dtValue.dsize);
    dtKeyExtra = vms_make_keyblk((KEYBLK *) pbBuffer, dtKey);
    if (dtKeyExtra.dsize > 0)
        memcpy(&pbBuffer[sizeof(KEYBLK)], dtKeyExtra.dptr, dtKeyExtra.dsize);
    if (dtValue.dsize > 0)
        memcpy(&pbBuffer[sizeof(KEYBLK) + dtKeyExtra.dsize], dtValue.dptr, 
                dtValue.dsize);


    /*
    **  Check to see if the record already exists.  If it does, then this
    **  operation must be an update or an error.
    */
    pvms->rabkey.rab$b_rac = RAB$C_KEY;
    pvms->rabkey.rab$l_rop = ROP_RDWR;
    pvms->rabkey.rab$b_krf = 0;
    pvms->rabkey.rab$l_kbf = (char *) pbBuffer;
    pvms->rabkey.rab$b_ksz = sizeof(KEYBLK);
    pvms->rabkey.rab$l_ubf = (char *) pvms->pbBuffer;
    pvms->rabkey.rab$w_usz = pvms->iBufferSize;
    if (vms_find_recd(pvms, (KEYBLK *) pbBuffer, dtKeyExtra) == 0)
    {
        if (iFlags == VDBM_INSERT)
        {
            DEBUG_PRINTF(("(vdbm_store) Insert failed: record exists, "
                    "key-size=%d value-size=%d rfa=0x%x\n",
                    ((KEYBLK *) pbBuffer)->iSize, dtValue.dsize,
                    pvms->rabkey.rab$w_rfa));
            (void) sys$free(&pvms->rabkey);
            (void) decc$$translate(RMS$_DUP);
            return(-1);
        }
        pvms->rabkey.rab$l_rbf = (char *) pbBuffer;
        pvms->rabkey.rab$w_rsz = sizeof(KEYBLK) + dtKeyExtra.dsize + 
                dtValue.dsize;
        pvms->st = sys$update(&pvms->rabkey);
        if (BAD_STATUS(pvms->st))
        {
            DEBUG_PRINTF(("(vdbm_store) sys$update() failed, "
                    "st=0x%x\n", pvms->st));
            (void) decc$$translate(pvms->st);
            return(-1);
        }
        if (DEBUG_ON)
        {
            vms_debug_recd("(vdbm_store)", "(overwrite)", 
                    pvms->rabkey.rab$w_rsz, pvms->rabkey.rab$l_rbf);
        }
        return(0);
    }


    /*
    **  Insert a new record.
    */
    pvms->rabkey.rab$l_rbf = (char *) pbBuffer;
    pvms->rabkey.rab$w_rsz = sizeof(KEYBLK) + dtKeyExtra.dsize + dtValue.dsize;
    pvms->st = sys$put(&pvms->rabkey);
    if (BAD_STATUS(pvms->st))
    {
        DEBUG_PRINTF(("(vdbm_store) sys$put() failed, "
                "st=0x%x\n", pvms->st));
        (void) decc$$translate(pvms->st);
        return(-1);
    }
    if (DEBUG_ON)
    {
        vms_debug_recd("(vdbm_store)", "(new)", pvms->rabkey.rab$w_rsz, 
                pvms->rabkey.rab$l_rbf);
    }
    return(0);
}


/*
**  v d b m _ f e t c h 
**
**  For a given key, return with the value.
*/
DATUM vdbm_fetch(VDBM *pdbmUser, DATUM dtKey)
{
    VMSBLK *pvms;
    KEYBLK keyblk;
    DATUM dtKeyExtra, dtValue;


    if (DEBUG_ON)
        vms_debug_key("(vdbm_fetch)", NULL, dtKey.dsize, 
                (const char *) dtKey.dptr);
    pvms = vms_verify_vmsblk((VMSBLK *) pdbmUser);
    if (vms_verify_vmsblk(pvms) == NULL)
        return(nullitem);
    if ((dtKey.dsize < 0) || ((dtKey.dsize > 0) && (dtKey.dptr == NULL)))
    {
        (void) decc$$translate(SS$_INVARG);
        return(nullitem);
    }
    dtKeyExtra = vms_make_keyblk(&keyblk, dtKey);
    pvms->rabkey.rab$b_rac = RAB$C_KEY;
    pvms->rabkey.rab$l_rop = ROP_RDONLY;
    pvms->rabkey.rab$b_krf = 0;
    pvms->rabkey.rab$l_kbf = (char *) &keyblk;
    pvms->rabkey.rab$b_ksz = sizeof(keyblk);
    pvms->rabkey.rab$l_ubf = (char *) pvms->pbBuffer;
    pvms->rabkey.rab$w_usz = pvms->iBufferSize;
    if (vms_find_recd(pvms, &keyblk, dtKeyExtra))
    {
        DEBUG_PRINTF(("(vdbm_fetch) Not found\n"));
        errno = 0;
        return(nullitem);
    }
    dtValue.dsize = pvms->rabkey.rab$w_rsz - sizeof(KEYBLK) - dtKeyExtra.dsize;
    dtValue.dptr = (char *) &(pvms->pbBuffer)[sizeof(KEYBLK) + dtKeyExtra.dsize];
    if (DEBUG_ON)
        vms_debug_key_value("(vdbm_fetch)", NULL, dtKey.dsize, 
                (const char *) dtKey.dptr, dtValue.dsize,
                (const char *) dtValue.dptr);
    return(dtValue);
}


/*
**  v d b m _ f i r s t k e y 
**
**  For a sequential search of the file, read the first record and return 
**  with the first key.
*/
DATUM vdbm_firstkey(VDBM *pdbmUser)
{
    VMSBLK *pvms;


    pvms = vms_verify_vmsblk((VMSBLK *) pdbmUser);
    if (vms_verify_vmsblk(pvms) == NULL)
        return(nullitem);
    pvms->st = sys$rewind(&pvms->rabseq);
    if (BAD_STATUS(pvms->st))
    {
        DEBUG_PRINTF(("(vdbm_firstkey) sys$rewind() failed, st=0x%x\n", 
                pvms->st));
        (void) decc$$translate(pvms->st);
    }
    pvms->rabseq.rab$b_rac = RAB$C_SEQ;
    pvms->rabseq.rab$l_rop = ROP_RDONLY;
    pvms->rabseq.rab$l_ubf = (char *) pvms->pbBuffer;
    pvms->rabseq.rab$w_usz = pvms->iBufferSize;
    return(vdbm_nextkey(pdbmUser));
}



/*
**  v d b m _ n e x t k e y 
**
**  Read the next record and return with the key.
*/
DATUM vdbm_nextkey(VDBM *pdbmUser)
{
    VMSBLK *pvms;
    DATUM dtKey;


    pvms = vms_verify_vmsblk((VMSBLK *) pdbmUser);
    if (vms_verify_vmsblk(pvms) == NULL)
        return(nullitem);
    pvms->st = sys$get(&pvms->rabseq);
    if (BAD_STATUS(pvms->st))
    {
        if (pvms->st != RMS$_EOF)
        {
            DEBUG_PRINTF(("(vdbm_nextkey) sys$get() failed, st=0x%x\n", 
                    pvms->st));
            (void) decc$$translate(pvms->st);
        }
        else
            errno = 0;

        return(nullitem);
    }
    dtKey.dsize = ((KEYBLK *)(pvms->pbBuffer))->iSize;
    dtKey.dptr = (char *) &((KEYBLK *)(pvms->pbBuffer))->abFixed[0];
    if (DEBUG_ON)
    {
        vms_debug_recd("(vdbm_nextkey)", NULL, dtKey.dsize, 
                (const char *) pvms->pbBuffer);
    }
    return(dtKey);
}


/*
**  v d b m _ d e l e t e 
**
**  Delete a record.
*/
int vdbm_delete(VDBM *pdbmUser, DATUM dtKey)
{
    VMSBLK *pvms;
    DATUM  dtKeyExtra;
    KEYBLK keyblk;


    if (DEBUG_ON)
        vms_debug_key("(vdbm_delete)", NULL, dtKey.dsize, 
                (const char *) dtKey.dptr);
    pvms = vms_verify_vmsblk((VMSBLK *) pdbmUser);
    if (vms_verify_vmsblk(pvms) == NULL)
        return(-1);
    if ((dtKey.dsize < 0) || ((dtKey.dsize > 0) && (dtKey.dptr == NULL)))
    {
        (void) decc$$translate(SS$_INVARG);
        return(-1);
    }
    dtKeyExtra = vms_make_keyblk(&keyblk, dtKey);
    pvms->rabkey.rab$b_rac = RAB$C_KEY;
    pvms->rabkey.rab$l_rop = ROP_RDWR;
    pvms->rabkey.rab$b_krf = 0;
    pvms->rabkey.rab$l_kbf = (char *) &keyblk;
    pvms->rabkey.rab$b_ksz = sizeof(keyblk);
    pvms->rabkey.rab$l_ubf = (char *) pvms->pbBuffer;
    pvms->rabkey.rab$w_usz = pvms->iBufferSize;
    if (vms_find_recd(pvms, &keyblk, dtKeyExtra))
        return(-1);
    pvms->st = sys$delete(&pvms->rabkey);
    if (BAD_STATUS(pvms->st))
    {
        DEBUG_PRINTF(("(vdbm_delete) sys$delete() failed, "
                "st=0x%x\n", pvms->st));
        (void) decc$$translate(pvms->st);
        return(-1);
    }
    DEBUG_PRINTF(("(vdbm_delete) sys$delete() OK\n"));
    return(0);
}


/*
**  v d b m _ h a s h 
**
**  Calculate the hash value for a given string.
**  NOTE: This routine is part of the VDBM interface but not part of NDBM.
*/
long vdbm_hash(char *pucData, int iSize)
{
    int i, j, iCarry;
    unsigned long ulEntry;
    register unsigned long ulResult;
    register unsigned long ulTemp;
    static int iOnce = 0;
    static unsigned long aulTable[256];


    /*
    **  Initialize the CRC-32 table (if necessary).
    */
    if (iOnce == 0)
    {
        iOnce++;
        for (i = 0; i < 256; i++)
        {
            ulEntry = i;
            for (j = 0; j < 8; j++)
            {
                iCarry = ulEntry & 1;
                ulEntry >>= 1;
                if (iCarry)
                    ulEntry ^= 0xedb88320;
            }
            aulTable[i] = ulEntry;
        }
    }


    /*
    **  Now calculate the CRC-32.
    */
    if ((iSize < 0) || ((iSize > 0) && (pucData == NULL)))
    {
        (void) decc$$translate(SS$_INVARG);
        return(-1);
    }
    ulResult = -1;
    while (iSize-- > 0)
    {
        ulTemp = (*pucData++ ^ ulResult) & UCHAR_MAX;
        ulResult >>= 8;
        ulResult ^= aulTable[ulTemp];
    }
    return (long)(~ulResult);
}


/*
**  v d b m _ l o c k 
**
**  Lock a file
*/
int vdbm_lock (VDBM *pdbmUser, int LockType)
{

return (0);

}

/*
**  v d b m _ u n l o c k 
**
**  Unlock a file
*/
int vdbm_unlock (VDBM *pdbmUser)
{

return (0);

}

/*
**  v d b m _ c l e a r e r r r
**
**  Clear the errno value.
**  Return with zero for success or minus one for failure.
*/
int vdbm_clearerr(VDBM *pdbmUser)
{
    VMSBLK *pvms;


    pvms = vms_verify_vmsblk((VMSBLK *) pdbmUser);
    if (vms_verify_vmsblk(pvms) == NULL)
        return(-1);
    pvms->st = STATUS_OK;
    return(0);
}


/*
**  v d b m _ e r r o r 
**
**  Return with zero for no error or minus one for failure.
*/
int vdbm_error(VDBM *pdbmUser)
{
    VMSBLK *pvms;


    pvms = vms_verify_vmsblk((VMSBLK *) pdbmUser);
    if (vms_verify_vmsblk(pvms) == NULL)
        return(-1);
    if (BAD_STATUS(pvms->st))
        return(-1);
    return(0);
}

/*
**  v d b m _ c l e a n u p _ c a l l b a c k 
**
**  Routine called by the pool handler to cleanup the database.
*/
int vdbm_cleanup_callback (void *pv)
{
    VMSBLK *pvms = vms_verify_vmsblk((VMSBLK *) pv);
    if (vms_verify_vmsblk(pvms) == NULL)
        return(-1);
    vms_free_vmsblk(pvms);
    return (0);
}


/*
**  v m s _ d e b u g _ k e y 
**
**  Routine to format and print the key.
**  Return with zero for success (always).
*/
INTERNAL int vms_debug_key(const char *pcszBegin, const char *pcszEnd,
        int ccKey, const char *pcsKey)
{
    char *pszKey;


    if (DEBUG_OFF)
        return(0);
    pszKey = alloca((3 * ccKey) + 1);
    vms_debug_render(ccKey, pcsKey, pszKey);
    DEBUG_PRINTF(("%s key=\"%s\" (%d) %s\n", pcszBegin, pszKey, ccKey, 
            ((pcszEnd) ? pcszEnd : "")));
    return(0);
}


/*
**  v m s _ d e b u g _ k e y _ v a l u e 
**
**  Routine to format and print the key and value.
**  Return with zero for success (always).
*/
INTERNAL int vms_debug_key_value(const char *pcszBegin, const char *pcszEnd,
        int ccKey, const char *pcsKey, int ccValue, const char *pcsValue)
{
    char *pszKey, *pszValue;


    if (DEBUG_OFF)
        return(0);
    pszKey = alloca((3 * ccKey) + 1);
    vms_debug_render(ccKey, pcsKey, pszKey);
    pszValue = alloca((3 * ccValue) + 1);
    vms_debug_render(ccValue, pcsValue, pszValue);
    DEBUG_PRINTF(("%s key=\"%s\" (%d) value=\"%s\" (%d) %s\n", pcszBegin, 
            pszKey, ccKey, pszValue, ccValue, ((pcszEnd) ? pcszEnd : "")));
    return(0);
}


/*
**  v m s _ d e b u g _ r e c d 
**
**  Routine to format and print a variable-length record.
**  Return with zero for success (always).
*/
INTERNAL int vms_debug_recd(const char *pcszBegin, const char *pcszEnd,
        size_t ccRecd, const char *pcbRecd)
{
    int ccKey, ccValue;
    KEYBLK *pkey;


    if (DEBUG_OFF)
        return(0);
    pkey = (KEYBLK *) pcbRecd;
    ccKey = MAX(pkey->iSize, KEYBLK_FIXEDSIZE);
    ccValue = ccRecd - KEYBLK_OVERHEAD - ccKey;
    return(vms_debug_key_value(pcszBegin, pcszEnd, pkey->iSize,
            (const char *) pkey->abFixed, ccValue,
            (const char *) &pkey->abFixed[pkey->iSize]));
}


/*
**  v m s _ d e b u g _ r e n d e r 
**
**  Routine to format character strings for the other debugging routines.
**  Return with zero for success (always).
*/
INTERNAL void vms_debug_render(int ccData, const char *pcszData, char *pszText)
{
    int i;
    static const char *accHex = "0123456789abcdef";


    for (i = 0; i < ccData; i++)
    {
        if (!isprint(*pcszData))
        {
            *pszText++ = '\\';
            *pszText++ = accHex[*pcszData / 16];
            *pszText++ = accHex[*pcszData % 16];
        }
        else
        {
            if (*pcszData == '\\')
                *pszText++ = '\\';
            *pszText++ = *pcszData;
        }
        pcszData++;
    }
    *pszText++ = '\0';
}


/*
**  v m s _ d e b u g _ p r i n t f 
**
**  Routine to format and output the debugging messages.
**  Return with zero for success or minus one for failure.
*/
INTERNAL vms_debug_printf(const char *pcszFormat, ...)
{
   int iReturn;
   va_list pArgList;


   va_start(pArgList, pcszFormat);
   iReturn = vfprintf(stderr, pcszFormat, pArgList);
   va_end(pArgList);
   return(iReturn);
}


/*
**  v m s _ m a l l o c _ v m s b l k 
**
**  Routine to allocate and initialize a VMSBLK structure.
**  Return with the address for success or NULL for failure.
*/
INTERNAL VMSBLK *vms_malloc_vmsblk()
{
    VMSBLK *pvms = (VMSBLK *) malloc(sizeof(VMSBLK));
    if (pvms == NULL)
    {
        (void) decc$$translate(SS$_INSFMEM);
        return(NULL);
    }
    pvms->uiType = VMSBLK_TYPE;
    pvms->pdbmUser = (VDBM *) pvms;
    pvms->pbBuffer = NULL;
    return(pvms);
}



/*
**  v m s _ v e r i f y _ v m s b l k 
**
**  Verify that the specified address points to a valid VMSBLK structure.
**  Return with the address for success or NULL for failure.
*/
INTERNAL VMSBLK *vms_verify_vmsblk(VMSBLK *pvms)
{
    if ((pvms != NULL) && (pvms->uiType == VMSBLK_TYPE) && 
            (pvms->pdbmUser == (VDBM *) pvms))
        return(pvms);
    DEBUG_PRINTF(("(vms_verify_vmsblk) Invalid VMSBLK, address=0x%x\n", pvms));
    (void) decc$$translate(SS$_INVARG);
    return(NULL);
}



/*
**  v m s _ f r e e _ v m s b l k 
**
**  Deallocate a VMSBLK structure.
*/
INTERNAL void vms_free_vmsblk(VMSBLK *pvms)
{
    if (vms_verify_vmsblk(pvms) == NULL)
        return;
    if (pvms->pbBuffer != NULL)
    {
        free(pvms->pbBuffer);
        pvms->pbBuffer = NULL;
    }
    free(pvms);
}


/*
**  v m s _ m a k e _ k e y b l k 
**
**  Build the KEYBLK structure for the specified key DATUM.
**  Return with the new key DATUM structure (fixed portion removed).
*/
INTERNAL DATUM vms_make_keyblk(KEYBLK *pkey, DATUM dtKey)
{
    int i;
    BYTE *pbSrc = (BYTE *) dtKey.dptr;
    BYTE *pbDest = pkey->abFixed;


    pkey->iSize = dtKey.dsize;
    if (dtKey.dsize > KEYBLK_FIXEDSIZE)
        pkey->ulCrc = vdbm_hash(dtKey.dptr, dtKey.dsize);
    else
        pkey->ulCrc = 0;
    for (i = 0; i < MIN(dtKey.dsize, KEYBLK_FIXEDSIZE); i++)
    {
        *pbDest++ = *pbSrc++;
    }
    dtKey.dsize -= i;
    dtKey.dptr = (char *) pbSrc;
    for (; i < KEYBLK_FIXEDSIZE; i++)
    {
        *pbDest++ = '\0';
    }
    return(dtKey);
}



/*
**  v m s _ f i n d _ r e c d 
**
**  Find the specified record based on the key.  The trick is that there may
**  be multiple records with the same primary key, and we have to read them
**  one-at-a-time to find the one we want.
**  Return with zero for success or minus one for failure.
*/
INTERNAL int vms_find_recd(VMSBLK *pvms, KEYBLK *pkey, DATUM dtKeyExtra)
{
    pvms->st = sys$get(&pvms->rabkey);
    pvms->rabkey.rab$b_rac = RAB$C_SEQ;
    pvms->rabkey.rab$l_rop |= RAB$M_LIM;
    while (GOOD_STATUS(pvms->st))
    {
        if ((pkey->iSize <= 0) || (memcmp((const void *) dtKeyExtra.dptr,
                (const void *) &(pvms->pbBuffer)[sizeof(KEYBLK)], 
                dtKeyExtra.dsize) == 0))
        {
            if (DEBUG_ON)
                vms_debug_recd("(vms_find_recd)", NULL, 
                        pvms->rabkey.rab$w_rsz,
                        (const char *) pvms->pbBuffer);
            return(0);
        }
        if (DEBUG_ON)
            vms_debug_recd("(vms_find_recd)", "(skip)", 
                    pvms->rabkey.rab$w_rsz, (const char *) pvms->pbBuffer);
        pvms->st = sys$get(&pvms->rabkey);
    }
    DEBUG_PRINTF(("(vms_find_recd) Not found\n"));
    return(-1);
}


/*
**  v m s _ m a k e _ x a b p r o 
**
**  Build the XAB$W_PRO bitmask based on the UNIX bits.
**  Return with the XAB$W_PRO bitmask.
*/
INTERNAL unsigned short vms_make_xabpro(int iMask)
{
    unsigned short usReturn;
    unsigned char aucUnixToXab[8] =
    {
        XAB$M_NOREAD|XAB$M_NOWRITE|XAB$M_NODEL|XAB$M_NOEXE, /* 0: no access */
        XAB$M_NOREAD|XAB$M_NOWRITE|XAB$M_NODEL,	/* 1: execute */
        XAB$M_NOREAD|XAB$M_NOEXE,		/* 2: write */
        XAB$M_NOREAD,				/* 3: write, execute */
        XAB$M_NOWRITE|XAB$M_NODEL|XAB$M_NOEXE,	/* 4: read */
        XAB$M_NOWRITE|XAB$M_NODEL,		/* 5: read, execute  */
        XAB$M_NOEXE,				/* 6: read, write */
        0,					/* 7: all access */
    };

    usReturn = aucUnixToXab[iMask & 7];	/* world (other) bits */
    iMask = iMask >> 3;			/* group bits */
    usReturn = (usReturn << 4) | aucUnixToXab[iMask & 7];
    iMask = iMask >> 3;			/* owner bits */
    usReturn = (usReturn << 4) | aucUnixToXab[iMask & 7];
    usReturn = (usReturn << 4) | aucUnixToXab[iMask & 7];
    return(usReturn);
}


/*
**  v m s _ c v t _ p a t h 
**
**  Simple routine to convert a UNIX path into an OpenVMS file specification.
**  Return with zero for success or minus one for failure.
**
**  NOTE: This routine assumes that if the path starts with root ("/"), then
**  the first portion is a device name of logical name.
*/
INTERNAL int vms_cvt_path(char *pszPath, char *pszResult)
{
    int iDepth = 0;
    register char *pcEnd;
    register char *pcSrc = pszPath;
    register char *pcDest = pszResult;
    char *tmp;

    /*
    **  If the first character is a slash ("/"), then the first element must
    **  be a device name or logical name.  If that is the only element (i.e.
    **  "/name" or "/name/"), then just copy the logical name.  Otherwise
    **  copy the logical name, add a colon (":") as a separator, and look
    **  for a directory (below).
    */
    DEBUG_PRINTF(("(vms_cvt_path) in:  \"%s\"\n", pszPath));
    if (*pszPath == '/')
    {
        pcSrc++;
        pcEnd = strchr(pcSrc, '/');
        if ((pcEnd == NULL) || (pcEnd[1] == '\0'))
        {
            strcpy(pszResult, pcSrc);
            if (pcEnd != NULL)
                pszResult[strlen(pszResult) - 1] = '\0';
            DEBUG_PRINTF(("(vms_cvt_path) out: \"%s\"\n", pszResult));
            return(0);
        }
        while (pcSrc != pcEnd)
        {
            *pcDest++ = *pcSrc++;
        }
        *pcDest++ = ':';
        pcSrc++;
    }


    /*
    **  Look for a directory specification (another slash).
    **
    **  SAL 27-Jul-2005 - Escape all '.' in directory names.
    */
    while ((pcEnd = strchr(pcSrc, '/')) != NULL)
    {
        if (pcSrc == pcEnd)
        {
            *pcSrc++;
            continue;
        }
        if (iDepth++ == 0)
            *pcDest++ = '[';
        else
            *pcDest++ = '.';
        if (strncmp((const char *) pcSrc, "./", 2) == 0)
        {
            pcSrc = &pcSrc[2];
            continue;
        }
        if (strncmp((const char *) pcSrc, "../", 3) == 0)
        {
            pcSrc = &pcSrc[3];
            *pcDest++ = '-';
            continue;
        }
        while (pcSrc != pcEnd)
        {
            if (*pcSrc == '.') {
                *pcDest++ = '^';
            }
            *pcDest++ = *pcSrc++;
        }
        pcSrc++;
    }
    if (iDepth++ > 0)
        *pcDest++ = ']';


    /*
    **  Copy the file name and extension.  If a version number is not
    **  specified add ";" to signify latest-and-greatest.
    **
    **  SAL 27-Jul-2005 - Check for multiple '.' and escape all but
    **  the last one.
    */
    if (pcSrc != NULL)
    {
        pcEnd = strchr((const char *) pcSrc, ';');
        while (*pcSrc != '\0')
        {
            if (*pcSrc == '.')
            {
                tmp = pcSrc + 1;
                if (strchr(tmp,'.') != NULL)
                {
                    *pcDest++ = '^';
                }
            }
            *pcDest++ = *pcSrc++;
        }
        if (pcEnd == NULL)
            *pcDest++ = ';';
    }
    *pcDest = '\0';
    DEBUG_PRINTF(("(vms_cvt_path) out: \"%s\"\n", pszResult));
    return(0);
}

