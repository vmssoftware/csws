#pragma module SLEEP "V1.00"

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
**	This routine impliments a sub second sleep
**
**  AUTHOR:
**
**      Matthew Doremus
**
**
**  CREATION DATE:   February 1, 2001
**
**  MODIFICATION HISTORY:
**
**  V1.00 	        Matthew Doremus                 01-Feb-2001
**        Initial development.
**
*/

#ifndef __NEW_STARLET
#define __NEW_STARLET
#define __NEW_STARLET_SET
#endif

#include <lib$routines.h>
#include <descrip.h>
#include <starlet.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#ifdef  __NEW_STARLET_SET
#undef  __NEW_STARLET_SET
#undef  __NEW_STARLET
#endif

extern int decc$$translate();

#ifdef TEST_SLEEP
int TimeDif(struct timeval, struct timeval);

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
main(int argc, char *argv[])
{
    struct timeval StartTime, EndTime;
    __time_t sec;
    int status;
    long usec;

/*
** Set the sleep value
*/
    sec = 3;
    usec = 0;

/*
** Get the start time
*/

/*
** Sleep ...
*/
    printf("Starting test of apr$sleep (%d) ...\n", sec);
    gettimeofday(&StartTime, 0);
    status = apr$sleep(sec);
    gettimeofday(&EndTime, 0);
    if (status)
	fprintf(stderr, "ERROR (%d,%08X) Calling apr$sleep\n", errno,
		vaxc$errno);
    else
	printf("Elapsed time: %6d.%03d seconds\n\n",
	       TimeDif(EndTime, StartTime) / 1000, TimeDif(EndTime,
							   StartTime) %
	       1000);

/*
** Set the sleep value
*/
    sec = 0;
    usec = 30000;

/*
** Sleep ...
*/
    printf("Starting test of apr$sleep (%d, %d) ...\n", sec, usec);
    gettimeofday(&StartTime, 0);
    status = apr$sleep(sec, usec);
    gettimeofday(&EndTime, 0);
    if (status)
	fprintf(stderr, "ERROR (%d,%08X) Calling apr$sleep\n", errno,
		vaxc$errno);
    else
	printf("Elapsed time: %6d.%03d seconds\n\n",
	       TimeDif(EndTime, StartTime) / 1000, TimeDif(EndTime,
							   StartTime) %
	       1000);

/*
** Exit
*/
    exit(1);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int TimeDif(struct timeval a, struct timeval b)
{
    register int us, s;

    us = a.tv_usec - b.tv_usec;
    us /= 1000;
    s = a.tv_sec - b.tv_sec;
    s *= 1000;

    return s + us;

}
#endif

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int apr$sleep(__time_t sec, ...)
{
    struct dsc$descriptor DeltaTimeDesc =
	{ 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, NULL };
    unsigned __int64 DeltaTime;
    unsigned int TimeoutEF = 0;
    char DeltaTimeBuff[32 + 1];
    long usec = 0;
    va_list argp;
    int dd = 0, hh = 0, mm = 0, ss = 0, cc = 0, status, argc;

/*
** Process the optional parameter
*/
    va_start(argp, sec);
    va_count(argc);
    if (argc > 1)
	usec = va_arg(argp, long);
    va_end(argp);

/*
** Establish the seconds and hundreths of seconds
*/
    ss = sec;
    cc = usec / 10000;

/*
** Establish the delta time values
*/
    if (cc >= 100) {
	ss = ss + (cc / 100);
	cc = cc % 100;
    }
    if (ss >= 60) {
	mm = ss / 60;
	ss = ss % 60;
    }
    if (mm >= 60) {
	hh = mm / 60;
	mm = mm % 60;
    }
    if (hh > 24) {
	dd = hh / 24;
	hh = hh % 24;
    }

/*
** Establish the delta time descriptor
*/
    sprintf(DeltaTimeBuff, "%d %02d:%02d:%02d.%02d", dd, hh, mm, ss, cc);
    DeltaTimeDesc.dsc$w_length = strlen(DeltaTimeBuff);
    DeltaTimeDesc.dsc$a_pointer = DeltaTimeBuff;

/*
** Convert the delta time to binary
*/
    status = SYS$BINTIM(&DeltaTimeDesc, (struct _generic_64 *) &DeltaTime);
    if (!(status & 1)) {
	decc$$translate(status);
	return (errno);
    }

/*
** Get a timer event flag
*/
    status = LIB$GET_EF(&TimeoutEF);
    if (!(status & 1)) {
	decc$$translate(status);
	return (errno);
    }

/*
** Clear the timer event flag
*/
    status = SYS$CLREF(TimeoutEF);
    if (!(status & 1)) {
	decc$$translate(status);
	LIB$FREE_EF(&TimeoutEF);
	return (errno);
    }

/*
** Set the timer
*/
    status =
	SYS$SETIMR(TimeoutEF, (struct _generic_64 *) &DeltaTime, 0, 0, 0);
    if (!(status & 1)) {
	decc$$translate(status);
	LIB$FREE_EF(&TimeoutEF);
	return (errno);
    }

/*
** Wait for the timeout
*/
    status = SYS$WAITFR(TimeoutEF);
    if (!(status & 1)) {
	decc$$translate(status);
	LIB$FREE_EF(&TimeoutEF);
	return (errno);
    }

/*
** Free timer event flag
*/
    status = LIB$FREE_EF(&TimeoutEF);
    if (!(status & 1)) {
	decc$$translate(status);
	return (errno);
    }

    return (0);
}
