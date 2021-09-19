#ifndef _APRSHRP_H_
#define _APRSHRP_H_

#ifndef __NEW_STARLET
#define __NEW_STARLET
#define __NEW_STARLET_SET_PROTSHR
#endif

#include <va_rangedef.h>

#ifdef  __NEW_STARLET_SET_PROTSHR
#undef  __NEW_STARLET_SET_PROTSHR
#undef  __NEW_STARLET
#endif

#include <descrip.h>
#include <socket.h>
#include <pwd.h>
#include <in.h>

/*
** Define the privileged APR routine prototypes
*/
int apr$$crembx (unsigned short *, int, int, struct dsc$descriptor *);
int apr$$getpwnam (struct dsc$descriptor *, struct passwd *);
int apr$$setsockopt (short int, int, void *, int);
int apr$$socket (int, int, int, unsigned short *);
int apr$$auth_princ_sysuaf (const char *, int, const char *, int, const char *, int);
int apr$$check_rights_list (const char *, int, const unsigned int *, int);
int apr$$create_galaxy_gblsec (struct dsc$descriptor	*, __int64, VA_RANGE *);

/*
** Define the apr$$setsockopt options
*/
#define SET_SOCK_DEV_CCL	1
#define SET_SOCK_DEV_BUF	2
#define SET_SOCK_DEV_SHR	3

#endif /* _APRSHRP_H_ */
