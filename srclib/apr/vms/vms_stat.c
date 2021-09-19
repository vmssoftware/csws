#pragma module VMS_STAT "V1.0"

#ifndef _USE_STD_STAT
#define _USE_STD_STAT 1
#endif

#ifndef __NEW_STARLET
#define __NEW_STARLET
#define __NEW_STARLET_SET
#endif

#include <starlet.h>
#include <descrip.h>
#include <iosbdef.h>
#include <stdarg.h>
#include <efndef.h>
#include <atrdef.h>
#include <fiddef.h>
#include <fibdef.h>
#include <string.h>
#include <stdlib.h>
#include <iledef.h>
#include <lnmdef.h>
#include <fatdef.h>
#include <fchdef.h>
#include <ssdef.h>
#include <iodef.h>
#include <stdio.h>
#include <errno.h>
#include <time.h>
#include <stat.h>
#include <rms.h>
#include <dvidef.h>
#include <assert.h>

#include "ilemac.h"
#include "cvtfnm.h"

#ifdef  __NEW_STARLET_SET
#undef  __NEW_STARLET_SET
#undef  __NEW_STARLET
#endif

#define ST_M_ALL        -1
#define ST_M_EXISTS     0
#define ST_M_DEV        1
#define ST_M_INO        2
#define ST_M_MODE       4
#define ST_M_NLINK      8
#define ST_M_UID        16
#define ST_M_GID        32
#define ST_M_RDEV       64
#define ST_M_SIZE       128
#define ST_M_ATIME      256
#define ST_M_MTIME      512
#define ST_M_CTIME      1024
#define ST_M_FAB_RFM    2048
#define ST_M_FAB_RAT    4096
#define ST_M_FAB_FSZ    8192
#define ST_M_FAB_MRS    16384

#ifndef PROT_BITS
#define PROT_BITS       0x7770
#endif

#ifndef RMS_BLOCKSIZE
#define RMS_BLOCKSIZE   512
#endif

extern int decc$fix_time();
extern int decc$$translate();

static int SwapBits(int);
static char *ConvertDev(char *);
static char *ConvertDir(char *);
static int FileIsDevice(char *);
static void FreeParseCtx(struct FAB *);
static void ParseDir(char *, int, char **, int *, char **, int *);


int apr$stat(char *FileName, stat_t * StatBuffer, ...)
{
    struct dsc$descriptor DevDesc = { 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0 };
    struct dsc$descriptor FibDesc = { 0, 0, 0, 0 };
    unsigned __int64 credate, revdate;
    int FileNameIsDevice = FALSE;
    static char dvi[NAML$C_DVI];
    char esa[NAML$C_MAXRSS];
    unsigned int owner, group, world, uchar, uic;
    unsigned short DevChan, fpro;
    char *TmpFileName = NULL, *VmsFileName = NULL, *VmsFileNameDef = NULL;
    ILE2 AtrItems[7];
    ILE2 *Ile2Ptr;
    struct FAB fab;
    struct NAML naml;
    struct tm *pt;
    va_list argp;
    FAT recattr;
    FIBDEF Fib;
    int StatItems = ST_M_ALL, status, argc;
    IOSB iosb;

    va_start(argp, StatBuffer);
    va_count(argc);
    if (argc > 2)
	VmsFileNameDef = va_arg(argp, char *);
    if (argc > 3)
	StatItems = va_arg(argp, int);
    va_end(argp);

    if (FileName == NULL) {
	errno = EFAULT;
	return (-1);
    }

    if (VmsFileNameDef)
	VmsFileName = FileName;
    else if (! apr$cvt_fnm(CVT_FNM_UNIX_TO_VMS, FileName, &VmsFileName))
	VmsFileName = FileName;

    if (!VmsFileNameDef &&
	strlen(VmsFileName) &&
	strncmp(&VmsFileName[strlen(VmsFileName) - 1], ":", 1) == 0) {
	TmpFileName = ConvertDev(VmsFileName);
	if (TmpFileName) {
	    if (VmsFileName && VmsFileName != FileName)
		free(VmsFileName);
	    VmsFileName = TmpFileName;
	}
    }


    if (!VmsFileNameDef &&
	strlen(VmsFileName) &&
	strncmp(&VmsFileName[strlen(VmsFileName) - 1], "]", 1) == 0) {
	TmpFileName = ConvertDir(VmsFileName);
	if (TmpFileName) {
	    if (VmsFileName && VmsFileName != FileName)
		free(VmsFileName);
	    VmsFileName = TmpFileName;
	}
    }


    fab = cc$rms_fab;
    naml = cc$rms_naml;
    fab.fab$l_fna = (char *) -1;
    naml.naml$l_long_filename = VmsFileName;
    naml.naml$l_long_filename_size = strlen(VmsFileName);
    naml.naml$l_long_expand = esa;
    naml.naml$l_long_expand_alloc = sizeof(esa);
    if (VmsFileNameDef) {
	fab.fab$l_dna = (char *) -1;
	naml.naml$l_long_defname = VmsFileNameDef;
	naml.naml$l_long_defname_size = strlen(VmsFileNameDef);
    }
    fab.fab$l_naml = &naml;


    status = SYS$PARSE(&fab);
    if (!(status & 1)) {
	if (VmsFileName && VmsFileName != FileName)
	    free(VmsFileName);
	decc$$translate(status);
	return (-1);
    }


    status = SYS$SEARCH(&fab);
    if (!(status & 1)) {
	if (VmsFileName && VmsFileName != FileName)
	    free(VmsFileName);
	FreeParseCtx(&fab);
	decc$$translate(status);
	return (-1);
    }


    if (naml.naml$v_wildcard) {
	if (VmsFileName && VmsFileName != FileName)
	    free(VmsFileName);
	FreeParseCtx(&fab);
	decc$$translate(status);
	errno = ENOENT;
	return (-1);
    }


    ILE2_INIT(AtrItems);
    if (StatItems & ST_M_UID || StatItems & ST_M_GID)
	ILE2_ADD(ATR$C_UIC, ATR$S_UIC, &uic);
    if (StatItems & ST_M_MODE) {
	ILE2_ADD(ATR$C_FPRO, ATR$S_FPRO, &fpro);
	ILE2_ADD(ATR$C_UCHAR, ATR$S_UCHAR, &uchar);
    }
    if (StatItems & ST_M_CTIME)
	ILE2_ADD(ATR$C_CREDATE, ATR$S_CREDATE, &credate);
    if (StatItems & ST_M_ATIME || StatItems & ST_M_MTIME)
	ILE2_ADD(ATR$C_REVDATE, ATR$S_REVDATE, &revdate);
    if (StatItems & ST_M_SIZE ||
	StatItems & ST_M_FAB_RFM || StatItems & ST_M_FAB_RAT ||
	StatItems & ST_M_FAB_FSZ || StatItems & ST_M_FAB_MRS)
	ILE2_ADD(ATR$C_RECATTR, ATR$S_RECATTR, &recattr);
    ILE2_TERM;


    if (!ILE2_EMPTY(AtrItems)) {
	DevDesc.dsc$w_length = naml.naml$t_dvi[0];
	DevDesc.dsc$a_pointer = &naml.naml$t_dvi[1];

	status = SYS$ASSIGN(&DevDesc, &DevChan, 0, 0);
	if (!(status & 1)) {
	    if (VmsFileName && VmsFileName != FileName)
		free(VmsFileName);
	    FreeParseCtx(&fab);
	    decc$$translate(status);
	    return (-1);
	}

	FibDesc.dsc$w_length = FIB$C_ACCDATA;
	FibDesc.dsc$a_pointer = (void *) &Fib;

	memset(&Fib, 0, sizeof(Fib));
	Fib.fib$w_fid_num = naml.naml$w_fid_num;
	Fib.fib$w_fid_seq = naml.naml$w_fid_seq;
	Fib.fib$w_fid_rvn = naml.naml$w_fid_rvn;


	status = SYS$QIOW(EFN$C_ENF,	/* event flag number                    */
			  DevChan,	/* I/O channel to device                */
			  IO$_ACCESS,	/* function code                        */
			  &iosb,	/* I/O Status Block                     */
			  0,	/* AST address - unused                 */
			  0,	/* AST parameter - unused               */
			  &FibDesc,	/* p1 - File info block descriptor      */
			  0, 0, 0,	/* p2, p3, p4 - Unused                  */
			  (__int64) & AtrItems,	/* p5 - attributes item list            */
			  0);	/* p6 - unused                          */

	if (status & 1)
	    status = iosb.iosb$w_status;
	if (!(status & 1)) {
	    if (!FileIsDevice(FileName)) {
		if (VmsFileName && VmsFileName != FileName)
		    free(VmsFileName);
		FreeParseCtx(&fab);
		decc$$translate(status);
		SYS$DASSGN(DevChan);
		if (status != SS$_NOPRIV)
		    return (-1);
		else
		    return (-2);
	    } else
		FileNameIsDevice = TRUE;
	}

	SYS$DASSGN(DevChan);
    }


    if (StatItems & ST_M_DEV) {
        StatBuffer->st_dev = 0; 		/* Could do better */
    } else
	StatBuffer->st_dev = 0;

    if (StatItems & ST_M_INO && !FileNameIsDevice) {
        StatBuffer->st_ino = naml.naml$w_fid_num | naml.naml$w_fid_seq << 16 | ((unsigned long long) naml.naml$w_fid_rvn) << 32;
    } else {
	StatBuffer->st_ino = 0;
    }

    StatBuffer->st_mode = 0;
    if (StatItems & ST_M_MODE && !FileNameIsDevice) {
	fpro ^= PROT_BITS;
	owner = SwapBits(((fpro / 16) % 8)) * 64;
	group = SwapBits(((fpro / 256) % 8)) * 8;
	world = SwapBits((fpro / 4096) % 8);
	StatBuffer->st_mode = owner + group + world;
	if (uchar & FCH$M_DIRECTORY)
	    StatBuffer->st_mode |= S_IFDIR;
	else
	    StatBuffer->st_mode |= S_IFREG;
    }
    if (StatItems & ST_M_MODE && FileNameIsDevice)
	StatBuffer->st_mode |= S_IFDIR;

    StatBuffer->st_nlink = 0;

    if (StatItems & ST_M_UID && !FileNameIsDevice)
	StatBuffer->st_uid = uic;
    else
	StatBuffer->st_uid = 0;

    if (StatItems & ST_M_GID && !FileNameIsDevice)
	StatBuffer->st_gid = uic / 65536;
    else
	StatBuffer->st_gid = 0;

    StatBuffer->st_rdev = 0;

    if (StatItems & ST_M_SIZE && !FileNameIsDevice) {
	if (recattr.fat$l_efblk != 0)
	    StatBuffer->st_size =
		(((recattr.fat$w_efblkh * 65536) + recattr.fat$w_efblkl -
		  1) * RMS_BLOCKSIZE) + recattr.fat$w_ffbyte;
	else
	    StatBuffer->st_size =
		(((recattr.fat$w_hiblkh * 65536) +
		  recattr.fat$w_hiblkl) * RMS_BLOCKSIZE) +
		recattr.fat$w_ffbyte;
    } else
	StatBuffer->st_size = 0;

    if (StatItems & ST_M_CTIME && !FileNameIsDevice) {
	StatBuffer->st_ctime = decc$fix_time(&credate);
	pt = gmtime(&StatBuffer->st_ctime);
	if (!pt) {
	    if (VmsFileName && VmsFileName != FileName)
		free(VmsFileName);
	    FreeParseCtx(&fab);
	    return (-1);
	}
	pt->tm_isdst = -1;
	if ((signed int) (StatBuffer->st_ctime = mktime(pt)) == -1) {
	    if (VmsFileName && VmsFileName != FileName)
		free(VmsFileName);
	    FreeParseCtx(&fab);
	    return (-1);
	}
    } else
	StatBuffer->st_ctime = 0;

    if (StatItems & ST_M_MTIME && !FileNameIsDevice) {
	StatBuffer->st_mtime = decc$fix_time(&revdate);
	pt = gmtime(&StatBuffer->st_mtime);
	if (!pt) {
	    if (VmsFileName && VmsFileName != FileName)
		free(VmsFileName);
	    FreeParseCtx(&fab);
	    return (-1);
	}
	pt->tm_isdst = -1;
	if ((signed int) (StatBuffer->st_mtime = mktime(pt)) == -1) {
	    if (VmsFileName && VmsFileName != FileName)
		free(VmsFileName);
	    FreeParseCtx(&fab);
	    return (-1);
	}
    } else
	StatBuffer->st_mtime = 0;

    if (StatItems & ST_M_ATIME && !FileNameIsDevice) {
	if (StatItems & ST_M_MTIME)
	    StatBuffer->st_atime = StatBuffer->st_mtime;
	else {
	    StatBuffer->st_atime = decc$fix_time(&revdate);
	    pt = gmtime(&StatBuffer->st_atime);
	    if (!pt) {
		if (VmsFileName && VmsFileName != FileName)
		    free(VmsFileName);
		FreeParseCtx(&fab);
		return (-1);
	    }
	    pt->tm_isdst = -1;
	    if ((signed int) (StatBuffer->st_atime = mktime(pt)) == -1) {
		if (VmsFileName && VmsFileName != FileName)
		    free(VmsFileName);
		FreeParseCtx(&fab);
		return (-1);
	    }
	}
    } else
	StatBuffer->st_atime = 0;

    if (StatItems & ST_M_FAB_RFM && !FileNameIsDevice)
	StatBuffer->st_fab_rfm = recattr.fat$b_rtype & 0x0F;
    else
	StatBuffer->st_fab_rfm = 0;

    if (StatItems & ST_M_FAB_RAT && !FileNameIsDevice)
	StatBuffer->st_fab_rat = recattr.fat$b_rattrib;
    else
	StatBuffer->st_fab_rat = 0;

    if (StatItems & ST_M_FAB_FSZ && !FileNameIsDevice)
	StatBuffer->st_fab_fsz = recattr.fat$b_vfcsize;
    else
	StatBuffer->st_fab_fsz = 0;

    if (StatItems & ST_M_FAB_MRS && !FileNameIsDevice)
	StatBuffer->st_fab_mrs = recattr.fat$w_maxrec;
    else
	StatBuffer->st_fab_mrs = 0;


    if (VmsFileName && VmsFileName != FileName)
	free(VmsFileName);
    FreeParseCtx(&fab);
    return (0);

}


static void FreeParseCtx(struct FAB *fab)
{
    struct NAML *naml = fab->fab$l_naml;

    if (!naml->naml$l_wcc && !naml->naml$v_wildcard)
	return;

    naml->naml$l_long_filename = NULL;
    naml->naml$l_long_filename_size = 0;
    naml->naml$l_long_defname = NULL;
    naml->naml$l_long_defname_size = 0;
    naml->naml$b_nop |= NAM$M_SYNCHK;
    naml->naml$l_long_expand = NULL;
    naml->naml$l_long_expand_alloc = 0;
    naml->naml$l_long_result = NULL;
    naml->naml$l_long_result_alloc = 0;

    SYS$PARSE(fab);

}


static int SwapBits(int val)
{
    switch (val) {
    case 1:
	return (4);
    case 3:
	return (6);
    case 4:
	return (1);
    case 6:
	return (3);
    default:
	return (val);
    }

}


static char *ConvertDev(char *DevSpec)
{
    unsigned int LnmAttr = LNM$M_CASE_BLIND;
    char LnmString[255 + 1];
    unsigned short LnmLen;
    ILE3 LnmItems[2];
    ILE3 *Ile3Ptr;
    char *TmpSpec;
    int status;
    $DESCRIPTOR(TblDesc, "LNM$FILE_DEV");
    $DESCRIPTOR(LnmDesc, "");


    if (strncmp(&DevSpec[strlen(DevSpec) - 1], ":", 1) == 0)
	LnmDesc.dsc$w_length = strlen(DevSpec) - 1;
    else
	LnmDesc.dsc$w_length = strlen(DevSpec);
    LnmDesc.dsc$a_pointer = DevSpec;


    ILE3_INIT(LnmItems);
    ILE3_ADD(LNM$_STRING, sizeof(LnmString) - 1, LnmString, &LnmLen);
    ILE3_TERM;

    status = SYS$TRNLNM(&LnmAttr, &TblDesc, &LnmDesc, 0, &LnmItems);
    if (!(status & 1))
	return (NULL);
    else
	LnmString[LnmLen] = '\0';

    TmpSpec = malloc(strlen(LnmString) + 1);
    strcpy(TmpSpec, LnmString);
    return (TmpSpec);

}


static char *ConvertDir(char *DirSpec)
{
    char DirString[NAML$C_MAXRSS];
    char esa[NAML$C_MAXRSS];
    char *OutDirPtr = NULL;
    int OutDirLen = 0;
    char *InpDirPtr;
    int InpDirLen;
    int DirLvlMax = 0, DirLvlCnt;
    char *TmpSpec;
    struct FAB fab;
    struct NAML naml;
    int status;


    if (!(InpDirPtr = strstr(DirSpec, "[")))
	return (NULL);
    InpDirLen = strlen(InpDirPtr);


    if (strcmp(InpDirPtr, "[000000]") == 0) {
	fab = cc$rms_fab;
	naml = cc$rms_naml;
	fab.fab$l_fna = (char *) -1;
	naml.naml$b_nop = NAML$M_NOCONCEAL;
	naml.naml$l_long_filename = DirSpec;
	naml.naml$l_long_filename_size = strlen(DirSpec);
	naml.naml$l_long_expand = esa;
	naml.naml$l_long_expand_alloc = sizeof(esa);
	fab.fab$l_naml = &naml;

	status = SYS$PARSE(&fab);
	if (status & 1)
	    if (naml.naml$v_root_dir) {
		DirSpec = naml.naml$l_long_dev;
		DirSpec[naml.naml$l_long_dev_size +
			naml.naml$l_long_dir_size] = '\0';
		InpDirPtr = naml.naml$l_long_dir;
		InpDirLen = naml.naml$l_long_dir_size;
		if (strncmp(&InpDirPtr[InpDirLen - 10], ".][000000]", 10)
		    == 0) {
		    strcpy(&InpDirPtr[InpDirLen - 10], "]");
		    InpDirLen = strlen(InpDirPtr);
		} else
		    if (strncmp(&InpDirPtr[InpDirLen - 2], ".]", 2) == 0) {
		    strcpy(&InpDirPtr[InpDirLen - 10], "]");
		    InpDirLen = strlen(InpDirPtr);
		}
	    }
	FreeParseCtx(&fab);
    }


    while (InpDirLen > 0) {
	ParseDir(InpDirPtr, InpDirLen,
		 &OutDirPtr, &OutDirLen, &InpDirPtr, &InpDirLen);
	if (OutDirLen)
	    DirLvlMax++;
    }


    InpDirPtr = strstr(DirSpec, "[");
    InpDirLen = strlen(InpDirPtr);


    strncpy(DirString, DirSpec, strlen(DirSpec) - InpDirLen);
    DirString[strlen(DirSpec) - InpDirLen] = '\0';
    if (DirLvlMax == 1)
	strcat(DirString, "[000000");
    else
	strcat(DirString, "[");

    for (DirLvlCnt = 0; DirLvlCnt < DirLvlMax; DirLvlCnt++) {
	ParseDir(InpDirPtr, InpDirLen,
		 &OutDirPtr, &OutDirLen, &InpDirPtr, &InpDirLen);

	if (DirLvlCnt == DirLvlMax - 1)
	    sprintf(DirString + strlen(DirString), "]%-*.*s.DIR",
		    OutDirLen, OutDirLen, OutDirPtr);
	else if (DirLvlCnt == 0)
	    sprintf(DirString + strlen(DirString), "%-*.*s",
		    OutDirLen, OutDirLen, OutDirPtr);
	else
	    sprintf(DirString + strlen(DirString), ".%-*.*s",
		    OutDirLen, OutDirLen, OutDirPtr);
    }


    TmpSpec = malloc(strlen(DirString) + 1);
    strcpy(TmpSpec, DirString);
    return (TmpSpec);
}


static void ParseDir(char *InpDirPtr,
		     int InpDirLen,
		     char **OutDirPtr,
		     int *OutDirLen, char **NxtDirPtr, int *NxtDirLen)
{
    char *DirPtr, *PrvPtr;
    int DirLen, PrvCtr;


    if (*InpDirPtr == '[') {
	*OutDirPtr = ++InpDirPtr;
	InpDirLen--;
    } else
	*OutDirPtr = InpDirPtr;


    DirPtr = *OutDirPtr;
    DirLen = 0;


    while (DirLen < InpDirLen) {
	if (*DirPtr == '.' || *DirPtr == ']') {
	    PrvPtr = DirPtr - 1;
	    PrvCtr = 0;
	    while (PrvPtr >= *OutDirPtr) {
		if (*PrvPtr != '^')
		    break;
		PrvPtr--;
		PrvCtr++;
	    }
	    if (!(PrvCtr % 2)) {
		*OutDirLen = DirLen;
		*NxtDirLen = InpDirLen - *OutDirLen - 1;
		*NxtDirPtr = (char *) ((int) *OutDirPtr + *OutDirLen + 1);
		return;
	    }
	}
	DirLen++;
	DirPtr++;
    }


    *OutDirLen = DirLen;
    *NxtDirLen = InpDirLen - *OutDirLen - 1;
    *NxtDirPtr = (char *) ((int) *OutDirPtr + *OutDirLen + 1);

}

#if 0
static int FileIsDevice(char *FileName)
{
    int FileIsDevice = FALSE;
    char esa[NAML$C_MAXRSS];
    struct FAB fab;
    struct NAML naml;
    char *DevName;
    int status;


    if (!FileName)
	return (FALSE);

    if (*FileName == '/' && strrchr(FileName, '/') != FileName)
	return (FALSE);


    if (*FileName == '/' && strrchr(FileName, '/') == FileName)
	DevName = &FileName[1];
    else
	DevName = FileName;


    fab = cc$rms_fab;
    naml = cc$rms_naml;
    fab.fab$l_fna = (char *) -1;
    fab.fab$l_dna = (char *) -1;
    fab.fab$l_naml = &naml;
    naml.naml$b_nop = NAML$M_NOCONCEAL;
    naml.naml$l_long_filename = DevName;
    naml.naml$l_long_filename_size = strlen(DevName);
    naml.naml$l_long_expand = esa;
    naml.naml$l_long_expand_alloc = sizeof(esa);
    naml.naml$l_long_defname = "[000000]000000.DIR";
    naml.naml$l_long_defname_size = strlen(naml.naml$l_long_defname);

    status = SYS$PARSE(&fab);
    if (status & 1)
	if (strncmp
	    (naml.naml$l_long_dir, naml.naml$l_long_defname,
	     naml.naml$l_long_defname_size) == 0)
	    FileIsDevice = TRUE;

    FreeParseCtx(&fab);

    return (FileIsDevice);
}
# endif


#ifndef OKAY
#define OKAY(STATUS) (((STATUS) & 1) != 0)
#endif

static int FileIsDevice(char *name)
{
    unsigned int devchar, dfs_access, remote_device;
    short int buflen;
    char volnam[13];
    struct dsc$descriptor_s device_name;
    unsigned long status;
    char *dev;

    struct _ile3 getdvi_itm[9] = {
        {sizeof(dfs_access), DVI$_DFS_ACCESS, (char *) &dfs_access, 0},
        {sizeof(remote_device), DVI$_REMOTE_DEVICE,
         (char *) &remote_device, 0},
        {sizeof(devchar), DVI$_DEVCHAR, (char *) &devchar, 0},
        {sizeof(volnam), DVI$_VOLNAM, (char *) volnam, 0},
        {0, 0, 0, 0}
    };

    assert(name);

    if (*name == '/' && strrchr(name, '/') != name) {
	return (0);
    }

    if (*name == '/' && strrchr(name, '/') == name) {
	dev = &name[1];
    } else {
	dev = &name[0];
    }

    /* We could possibly decide here that we have a device, but let's check for sure... */

    device_name.dsc$w_length = strlen(dev);
    device_name.dsc$b_dtype = DSC$K_DTYPE_T;
    device_name.dsc$b_class = DSC$K_CLASS_S;
    device_name.dsc$a_pointer = (char *) dev;

    status = sys$getdviw(0, 0, &device_name, &getdvi_itm, 0, 0, 0, 0);

    if (OKAY(status)) {
	return (1);
    } else {
	return (0);
    }

}

