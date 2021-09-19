#pragma module FLOCK "V1.01"

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
**	Interface for flock
**
**  AUTHOR:
**
**      Matthew Doremus
**
**
**  CREATION DATE:   August 29, 2001
**
**  MODIFICATION HISTORY:
**
**  V1.00 	        Matthew Doremus                 29-Aug-2001
**        Initial development.
**
**  V1.01 	        Matthew Doremus                 07-Nov-2001
**        Changed AST processing to Event Flags
**
*/

#ifndef __NEW_STARLET
#define __NEW_STARLET
#define __NEW_STARLET_SET
#endif

#include <lib$routines.h>
#include <errnodef.h>
#include <descrip.h>
#include <starlet.h>
#include <lksbdef.h>
#include <unixio.h>
#include <stdlib.h>
#include <efndef.h>
#include <string.h>
#include <lckdef.h>
#include <psldef.h>
#include <ssdef.h>
#include <fcntl.h>
#include <errno.h>
#include <stat.h>

#include "flock.h"

#ifdef  __NEW_STARLET_SET
#undef  __NEW_STARLET_SET
#undef  __NEW_STARLET
#endif

/*
** Define Success & Failure for these functions
*/
#ifdef	SUCCESS
#undef	SUCCESS
#endif
#define	SUCCESS	0
#ifdef	FAILURE
#undef	FAILURE
#endif
#define	FAILURE -1

/*
** Define the NonBlocking timeout wait
*/
#ifndef TIMEOUT_WAIT
#define TIMEOUT_WAIT	3
#endif

/*
** Define the lock prefix
*/
#ifndef LOCK_PREFIX
#define LOCK_PREFIX "APL"
#endif

/*
** Define a File Lock Entry structure
*/
typedef struct _FLE {
    struct _FLE		*next;
    unsigned int	file_id;
    unsigned int	ref_cnt;
    unsigned int	lock_id;
    unsigned int	lock_mode;
    } FLE;

/*
** Define the File Lock Entry list root
*/
static FLE *FLE_Root = NULL;

/*
** Define prototypes for external functions
*/
extern int decc$$translate ();

/*
** Define prototypes for local functions
*/
static FLE *LocateFLE (FLE *, unsigned int);
static FLE *InsertFLE (FLE **, unsigned int);
static FLE *RemoveFLE (FLE **, FLE *);
static int AcquireLock (char *, unsigned int *, int);
static int ReleaseLock (unsigned int);
static int GetEventFlags (unsigned int *, unsigned int *, unsigned int *);
#ifdef TEST_FLOCK
static void GetInput (char *, char *, int, int *);
static int ParseLockOperation (char *);
#endif

#ifdef TEST_FLOCK
/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
main (int argc, char *argv[])
{
char LockFileBuf[256];
char LockOperBuf[256];
int LockFileLen = 0,
    LockOperLen = 0,
    LockOperVal,
    status,
    fd;

/*
** Get the path name
*/
if (argc == 1)
    {
    /*
    ** Ask for the lock operation
    */
    GetInput ("File Name:", LockFileBuf, sizeof (LockFileBuf) - 1,
	      &LockFileLen);
    LockFileBuf[LockFileLen] = '\0';
    }
else
if (argc == 2)
    {
    strcpy (LockFileBuf, argv[1]);
    }
else
    {
    printf ("Usage: FLOCK [file]\n");
    exit (1);
    }

/*
** Open the file
*/
fd = open (LockFileBuf, O_RDWR | O_CREAT, 0777, "shr=get,del,put,upd");
if (fd == FAILURE)
    {
    perror ("open");
    exit (1);
    }

/*
** Lock the file
*/
do  {
    /*
    ** Ask for the lock operation
    */
    GetInput ("Lock Operation:", LockOperBuf, sizeof (LockOperBuf) - 1,
	      &LockOperLen);
    LockOperBuf[LockOperLen] = '\0';

    /*
    ** If the lock operation value was valid, the process it
    */
    if (LockOperVal = ParseLockOperation (LockOperBuf))
	{
	status = flock (fd, LockOperVal);
	if (status == FAILURE)
	    {
	    perror ("flock");
	    printf ("\r\n");
	    }
	}
     } while (strlen (LockOperBuf));

/*
** Unlock any locks before exiting
*/
while (flock (fd, LOCK_UN) == SUCCESS);

/*
** Close the file
*/
close (fd);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static void GetInput (char *Prompt, char *InputStr, int InputMax, int *InputLen)
{
char InputChr = {'\0'};
int InputCtr = 0;

printf ("%s ", Prompt);

while ((InputChr = fgetc (stdin)) != '\n')
    {
    if (InputCtr < InputMax)
        {
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
static int ParseLockOperation (char *LockOperBuf)
{
int LockOperErr = FALSE,
    LockOperVal = 0;
char *LockOperTmp,
     *LockOperPtr;

/*
** Allocate a temporary buffer
*/
LockOperTmp = strdup (LockOperBuf);

/*
** Convert the lock operation buffer string to upper case
*/
while (LockOperPtr = strsep (&LockOperTmp, " ,|"))
    {
    if (strcasecmp (LockOperPtr, "LOCK_SH") == 0)
	LockOperVal |= LOCK_SH;
    else
    if (strcasecmp (LockOperPtr, "LOCK_EX") == 0)
	LockOperVal |= LOCK_EX;
    else
    if (strcasecmp (LockOperPtr, "LOCK_NB") == 0)
	LockOperVal |= LOCK_NB;
    else
    if (strcasecmp (LockOperPtr, "LOCK_UN") == 0)
	LockOperVal |= LOCK_UN;
    else
    if (strlen (LockOperPtr))
	LockOperErr = TRUE;
    }

/*
** Free the temporary buffer
*/
free (LockOperTmp);

/*
** Print out instructional note if we've had an error
*/
if (LockOperErr)
    {
    printf ("Note: The valid lock operations are LOCK_SH, LOCK_EX, LOCK_NB, and LOCK_UN.\n");
    printf ("      Any combination of these operations are considered as being a logical OR\n");
    printf ("      and must be separated by a space, comma, or vertical bar.\n");
    }

/*
** Return the lock operation value
*/
return (LockOperVal);

}
#endif

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int flock (int fd, int operation)
{
char LockName[31 + 1];
FLE *FLE_Ptr = NULL;
stat_t buf;
int status;

/*
** Validate the flock operation value range
*/
if ((operation < 0) || (operation > (LOCK_SH | LOCK_EX | LOCK_NB | LOCK_UN)))
    {
    errno = EINVAL;
    return (FAILURE);
    }

/*
** Validate the flock operation value bits
*/
if ((operation & LOCK_SH && operation & LOCK_EX) ||
    (operation & LOCK_SH && operation & LOCK_UN) ||
    (operation & LOCK_EX && operation & LOCK_UN) ||
   !(operation & LOCK_SH || operation & LOCK_EX || operation & LOCK_UN))
    {
    errno = EINVAL;
    return (FAILURE);
    }

/*
** Locate the file lock entry for this fd, if it doesn't exist then create one
*/
FLE_Ptr = LocateFLE (FLE_Root, fd);
if (! FLE_Ptr)
    {
    if (operation & LOCK_UN)
	{
	errno = ENOLCK;
	return (FAILURE);
	}

    FLE_Ptr = InsertFLE (&FLE_Root, fd);
    if (! FLE_Ptr)
	{
	errno = EBADF;
	return (FAILURE);
	}
    }

/*
** If the resource is already locked, then we may need to release the current
** lock before we acquire a new one
*/
if (FLE_Ptr->lock_id)
    {
    /*
    ** If we're looking to relock a lock with the same mode, then increment the
    ** lock reference count and return SUCCESS.  Otherwise, if we're trying to
    ** unlock a lock with a reference count greater than 1, then decrement the
    ** reference count and return SUCCESS.
    */
    if (operation & LOCK_SH && FLE_Ptr->lock_mode == LOCK_SH)
        {
	FLE_Ptr->ref_cnt++;
	return (SUCCESS);
        }
    if (operation & LOCK_EX && FLE_Ptr->lock_mode == LOCK_EX)
        {
	FLE_Ptr->ref_cnt++;
	return (SUCCESS);
	}
    if (operation & LOCK_UN && FLE_Ptr->ref_cnt > 1)
	{
	FLE_Ptr->ref_cnt--;
	return (SUCCESS);
	}

    /*
    ** Release the current lock if the reference count is 1, otherwise return
    ** Failure with error status EWOULDBLOCK since we surely would have dead-
    ** locked ourselves with this request.
    */
    if (FLE_Ptr->ref_cnt == 1)
	{
	status = ReleaseLock (FLE_Ptr->lock_id);
	if (! (status & 1))
	    {
	    decc$$translate (status);
	    return (FAILURE);
	    }
	FLE_Ptr->lock_id = 0;
	FLE_Ptr->lock_mode = 0;
	}
    else
	{
	errno = EWOULDBLOCK;
	return (FAILURE);
	}

    /*
    ** If we're simply unlocking this lock then remove the File Lock Entry and
    ** free it before returning SUCCESS.
    */
    if (operation & LOCK_UN)
	{
	FLE_Ptr = RemoveFLE (&FLE_Root, FLE_Ptr);
	if (FLE_Ptr)
	    free (FLE_Ptr);
	return (SUCCESS);
	}
    }

/*
** If the resource is not already locked, then we need to acquire a lock for
** the requested operation.  First, we try to acquire a system wide lock and if
** that fails, then we create a group wide lock.
*/
if (! FLE_Ptr->lock_id)
    {
    /*
    ** Let's fstat the file descriptor
    */
    if (fstat (fd, &buf) != SUCCESS)
	return (FAILURE);

    /*
    ** Construct the lock resource name
    */
#ifdef _USE_STD_STAT
    sprintf (LockName, "%s_%012llx_%012llx", LOCK_PREFIX, buf.st_dev, buf.st_ino);
#else
    sprintf (LockName, "%s%s%04X%04X%04X", LOCK_PREFIX, buf.st_dev,
	     buf.st_ino[0], buf.st_ino[1], buf.st_ino[2]);
#endif

    /*
    ** Acquire the lock
    */
    status = AcquireLock (LockName, &FLE_Ptr->lock_id, operation);
    if (! (status & 1))
	{
	FLE_Ptr = RemoveFLE (&FLE_Root, FLE_Ptr);
	if (FLE_Ptr)
	    free (FLE_Ptr);
	decc$$translate (status);
	return (FAILURE);
	}

    /*
    ** Set the lock reference count to one
    */
    FLE_Ptr->ref_cnt = 1;

    /*
    ** Update the lock mode
    */
    if (operation & LOCK_SH)
	FLE_Ptr->lock_mode = LOCK_SH;
    if (operation & LOCK_EX)
	FLE_Ptr->lock_mode = LOCK_EX;
    }

/*
** Return success
*/
return (SUCCESS);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static FLE *LocateFLE (FLE *FLE_Root, unsigned int fd)
{
FLE *FLE_List = FLE_Root;

while (FLE_List)
    {
    if (FLE_List->file_id == fd)
	return (FLE_List);
    FLE_List = FLE_List->next;
    }

return (NULL);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static FLE *InsertFLE (FLE **FLE_Root, unsigned int fd)
{
FLE *FLE_List = *FLE_Root;
FLE *FLE_Ptr;

FLE_Ptr = malloc (sizeof (FLE));
if (FLE_Ptr)
    {
    FLE_Ptr->next = NULL;
    FLE_Ptr->file_id = fd;
    FLE_Ptr->ref_cnt = 0;
    FLE_Ptr->lock_id = 0;
    FLE_Ptr->lock_mode = 0;
    }

if (*FLE_Root == NULL)
    *FLE_Root = FLE_Ptr;
else
    while (FLE_List)
	{
	if (FLE_List->next == NULL)
	    {
	    FLE_List->next = FLE_Ptr;
	    break;
	    }
	FLE_List = FLE_List->next;
	}

return (FLE_Ptr);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static FLE *RemoveFLE (FLE **FLE_Root, FLE *FLE_Ptr)
{
FLE *FLE_List = *FLE_Root;

if (*FLE_Root == FLE_Ptr)
    *FLE_Root = FLE_Ptr->next;
else
    while (FLE_List)
	{
	if (FLE_List->next == FLE_Ptr)
	    {
	    FLE_List->next = FLE_Ptr->next;
	    break;
	    }
	FLE_List = FLE_List->next;
	}

return (NULL);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static int AcquireLock (char *LockBuf, unsigned int *LockId, int Mode)
{
struct dsc$descriptor AscTimeDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
struct dsc$descriptor MutexDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
unsigned int LockEF = 0,
	     TimeEF = 0,
	     EFMask = 0;
char AscTime[23+1];
int MutexFlags = 0,
    MutexMode = 0;
__int64 BinTime;
LKSB MutexLKSB;
int status;

/*
** Create the lock resouce name descriptor
*/
MutexDesc.dsc$w_length = strlen (LockBuf);
MutexDesc.dsc$a_pointer = LockBuf;

/*
** Clear the lock status block
*/
memset (&MutexLKSB, 0, sizeof (MutexLKSB));

/*
** Establish the appropriate lock mode
*/
if (Mode & LOCK_SH)
    MutexMode = LCK$K_CRMODE;
if (Mode & LOCK_EX)
    MutexMode = LCK$K_EXMODE;

/*
** If the lock mode contains the non-blocking qualifier, then let's setup a
** timer which will cancel our lock request if it is not satisfied within
** TIMEOUT_WAIT seconds
*/
if (Mode & LOCK_NB)
    {
    /*
    ** Get the lock event flag, timer event flag, and an event flag mask
    */
    status = GetEventFlags (&LockEF, &TimeEF, &EFMask);
    if (! (status & 1))
	return (status);

    /*
    ** Create a binary timeout value
    */
    sprintf (AscTime, "0 00:00:%02d.00", TIMEOUT_WAIT);
    AscTimeDesc.dsc$w_length = strlen (AscTime);
    AscTimeDesc.dsc$a_pointer = AscTime;
    status = SYS$BINTIM (&AscTimeDesc,
			 (struct _generic_64 *) &BinTime);
    if (! (status & 1))
	return (status);

    /*
    ** Set the timeout for the non-blocking mode
    */
    status = SYS$SETIMR (TimeEF,
			 (struct _generic_64 *) &BinTime,
                         0,
                         (unsigned __int64) &MutexLKSB.lksb$l_lkid,
                         0);
    if (! (status & 1))
	return (status);
    }
else
    {
    /*
    ** Get the lock event flag
    */
    status = GetEventFlags (&LockEF, NULL, NULL);
    if (! (status & 1))
	return (status);
    }

/*
** Enqueue the lock
*/
status = SYS$ENQ  (LockEF, 		/* Lock event flag		*/
		   MutexMode,		/* Lock mode			*/
		   &MutexLKSB,		/* Lock Status Block		*/
		   0,			/* No lock flags		*/
		   &MutexDesc,		/* Resource name descriptor	*/
		   0,			/* No parent			*/
		   0,			/* No AST routine		*/
		   0,			/* No AST parameter		*/
		   0,			/* No blocking AST		*/
		   PSL$C_USER,		/* Access mode = user		*/
		   0,			/* Resource domain ID		*/
		   0,			/* Range			*/
		   0);			/* No priority			*/
if (! (status & 1))
    {
    /*
    ** If the lock failed to be queued, then cancel the timer, free the
    ** event flags and return the $ENQ status.
    */
    LIB$FREE_EF (&LockEF);
    if (TimeEF)
	{
	SYS$CANTIM ((unsigned __int64) &MutexLKSB.lksb$l_lkid, 0);
	LIB$FREE_EF (&TimeEF);
	}
    return (status);
    }

/*
** If the the non-blocking mode was specified then cancel the established timer
*/
if (Mode & LOCK_NB)
    {
    /*
    ** Wait for the lock event flag or the timer event flag
    */
    status = SYS$WFLOR (LockEF, EFMask);
    if (! (status & 1))
        {
	SYS$DEQ (MutexLKSB.lksb$l_lkid, 0, PSL$C_USER, LCK$M_CANCEL);
        LIB$FREE_EF (&LockEF);
	SYS$CANTIM ((unsigned __int64) &MutexLKSB.lksb$l_lkid, 0);
	LIB$FREE_EF (&TimeEF);
        return (status);
        }

    /*
    ** If lock event flag was clear, then the timer expired so let's cancel the
    ** lock request and send back C$_EWOULDBLOCK instead.
    */
    status = SYS$CLREF (LockEF);
    if (status == SS$_WASCLR)
        {
	SYS$DEQ (MutexLKSB.lksb$l_lkid, 0, PSL$C_USER, LCK$M_CANCEL);
        LIB$FREE_EF (&LockEF);
        LIB$FREE_EF (&TimeEF);
	return (C$_EWOULDBLOCK);
        }

    /*
    ** Free the timer event flag and cancel the timer itself.
    */
    SYS$CANTIM ((unsigned __int64) &MutexLKSB.lksb$l_lkid, 0);
    LIB$FREE_EF (&TimeEF);
    }
else
    {
    /*
    ** Wait for the lock event flag
    */
    status = SYS$WAITFR (LockEF);
    if (! (status & 1))
        {
        LIB$FREE_EF (&LockEF);
        return (status);
        }
    }

/*
** Free the lock event flag
*/
LIB$FREE_EF (&LockEF);

/*
** Check the status in the lock status block
*/
if (! (MutexLKSB.lksb$w_status & 1))
    return (MutexLKSB.lksb$w_status);

/*
** Save the termination lock id
*/
*LockId = MutexLKSB.lksb$l_lkid;

/*
** Return normal
*/
return (SS$_NORMAL);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static int ReleaseLock (unsigned int LockId)
{
int status;

/*
** Dequeue the lock
*/
status = SYS$DEQ (LockId,		/* Lock ID 			*/
		  0,			/* No value block		*/
		  PSL$C_USER,		/* Access mode = user		*/
		  0);			/* No lock Flags 		*/
if (! (status & 1))
    return (status);

/*
** Return normal
*/
return (SS$_NORMAL);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static int GetEventFlags (unsigned int *LockEF, unsigned int *TimeEF,
			  unsigned int *EFMask)
{
unsigned int SaveEF = 0,
	     Shift1 = 0,
	     Shift2 = 0;
int status;

/*
** Allocate the lock event flag
*/
status = LIB$GET_EF (LockEF);
if (! (status & 1))
    return (status);

/*
** If we need to create a mask of flags and the event flag number is on the
** border between two groups of event flags, then let's save off the allocated
** event flag and get another so that we can create the mask of flags.
*/
if (EFMask && *LockEF == 32)
    {
    SaveEF = *LockEF;
    *LockEF = 0;
    status = LIB$GET_EF (LockEF);
    if (! (status & 1))
        {
        LIB$FREE_EF (&SaveEF);
        return (status);
        }
    }

/*
** Allocate the timer event flag
*/
if (TimeEF)
    {
    status = LIB$GET_EF (TimeEF);
    if (! (status & 1))
        {
	if (LockEF)
	    LIB$FREE_EF (LockEF);
        if (SaveEF)
            LIB$FREE_EF (&SaveEF);
        return (status);
        }
    }

/*
** Create the mask of the two event flags
*/
if (EFMask)
    {
    Shift1 = *TimeEF;
    Shift2 = *LockEF;

    if (*TimeEF > 31)
	{
        Shift1 -= 32;
        Shift2 -= 32;
        }

    *EFMask = (1 << Shift1) | (1 << Shift2);
    }

/*
** Free the saved event flag if necessary
*/
if (SaveEF)
    LIB$FREE_EF (&SaveEF);

/*
** Return success
*/
return (SS$_NORMAL);

}
