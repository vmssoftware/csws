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

#ifndef __FLOCK_H
#define __FLOCK_H 1

/*
** Define the flock operations
*/
#ifndef	 LOCK_SH
#define  LOCK_SH   0x01   /* shared file lock */
#define  LOCK_EX   0x02   /* exclusive file lock */
#define  LOCK_NB   0x04   /* do not block when locking */
#define  LOCK_UN   0x08   /* unlock file */
#endif

/*
** Define the flock function prototypes
*/
int flock (int, int);

#endif /* __FLOCK_H */
