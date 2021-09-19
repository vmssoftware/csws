/*
   +----------------------------------------------------------------------+
   | PHP version 4.0                                                      |
   +----------------------------------------------------------------------+
   | Copyright (c) 2001 The PHP Group                   		  |
   +----------------------------------------------------------------------+
   | This source file is subject to version 2.02 of the PHP license,      |
   | that is bundled with this package in the file LICENSE, and is        |
   | available at through the world-wide-web at                           |
   | http://www.php.net/license/2_02.txt.                                 |
   | If you did not receive a copy of the PHP license and are unable to   |
   | obtain it through the world-wide-web, please send a note to          |
   | license@php.net so we can mail you a copy immediately.               |
   +----------------------------------------------------------------------+
   | Authors: Matthew Doremus <Matthew.Doremus@compaq.com>                |
   +----------------------------------------------------------------------+
*/

#ifndef __ILEMAC_LOADED
#define __ILEMAC_LOADED 1

/*
** Item List 2 Macros
*/
#define ILE2_INIT(ile2)						\
    {								\
    Ile2Ptr = (ILE2 *) &ile2;					\
    }
#define ILE2_ADD(itmcod, buflen, bufptr)			\
    {								\
    Ile2Ptr->ile2$w_length = buflen;				\
    Ile2Ptr->ile2$w_code = itmcod;				\
    Ile2Ptr->ile2$ps_bufaddr = (void *) bufptr;			\
    Ile2Ptr++;							\
    }
#define ILE2_TERM						\
    {								\
    Ile2Ptr->ile2$w_length = 0;					\
    Ile2Ptr->ile2$w_code = 0;					\
    Ile2Ptr->ile2$ps_bufaddr = (void *) 0;			\
    }
#define ILE2_EMPTY(ile2)					\
    (ile2[0].ile2$w_length == 0 &&				\
     ile2[0].ile2$w_code == 0 && 				\
     ile2[0].ile2$ps_bufaddr == NULL ? 1 : 0)

/*
** Item List 3 Macros
*/
#define ILE3_INIT(ile3)						\
    {								\
    Ile3Ptr= (struct _ile3 *) &ile3;				\
    }
#define ILE3_ADD(itmcod, buflen, bufptr, retlen)		\
    {								\
    Ile3Ptr->ile3$w_length = buflen;				\
    Ile3Ptr->ile3$w_code = itmcod;				\
    Ile3Ptr->ile3$ps_bufaddr = (void *) bufptr;			\
    Ile3Ptr->ile3$ps_retlen_addr = (unsigned short *) retlen;	\
    Ile3Ptr++;							\
    }
#define ILE3_TERM						\
    {								\
    Ile3Ptr->ile3$w_length = 0;					\
    Ile3Ptr->ile3$w_code = 0;					\
    Ile3Ptr->ile3$ps_bufaddr = (void *) 0;			\
    Ile3Ptr->ile3$ps_retlen_addr = (unsigned short *) 0;	\
    }
#define ILE3_EMPTY(ile3)					\
    (ile3[0].ile3$w_length == 0 &&				\
     ile3[0].ile3$w_code == 0 && 				\
     ile3[0].ile3$ps_bufaddr == NULL &&				\
     ile3[0].ile3$ps_retlen_addr == NULL) ? 1 : 0)

#endif /* __ILEMAC_LOADED */
