#pragma module VMSSHM "V1.09"

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
**	Shared memory API's using global sections.
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
**  V1.01 	        Matthew Doremus                 22-Jun-2001
**        Added changes for Galaxy Shared Memory
**
**  V1.02 	        Matthew Doremus                 26-Jun-2001
**        Improved shared memory processing
**
**  V1.03 	        Matthew Doremus                 17-Sep-2001
**        Fixed Apr$Shm_Create Session Cache Name Descriptor
**
**  V1.04 	        Matthew Doremus                 19-Sep-2001
**        Fixed shared memory backlink bug
**
**  V1.05 	        Matthew Doremus                 20-Sep-2001
**        Added Shared Memory fixes
**
**  V1.06 	        Matthew Doremus                 31-Oct-2001
**        Added privileged Galaxy GblSec create
**
**  V1.07 	        Matthew Doremus                 31-Oct-2001
**        Added support for use of CGI data in Global Sections
**
**  V1.08 	        Matthew Doremus                 08-Nov-2001
**        Fixed shared memory create typo
**
**  V1.09 	        Matthew Doremus                 08-Jan-2002
**        Fixed SSL Shared Memory Addressing
**
*/

#ifndef __NEW_STARLET
#define __NEW_STARLET
#define __NEW_STARLET_SET
#endif

#include <va_rangedef.h>
#include <descrip.h>
#include <starlet.h>
#include <lksbdef.h>
#include <iosbdef.h>
#include <efndef.h>
#include <syidef.h>
#include <jpidef.h>
#include <lckdef.h>
#include <psldef.h>
#include <iledef.h>
#include <secdef.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <ssdef.h>
#include <vadef.h>

#include "apr_arch_shm.h"
#include "protshr.h"
#include "ilemac.h"

#ifdef  __NEW_STARLET_SET
#undef  __NEW_STARLET_SET
#undef  __NEW_STARLET
#endif

/*
** Define the local shared memory prototypes
*/
static int apr$$shm_deltva(SHM_CTX * shm_ctx);
static void apr$$shm_lock(SHM_CTX *);
static void apr$$shm_unlock(SHM_CTX *);
static SHM_STS *apr$$shm_stats(SHM_CTX *, int);
static void apr$$shm_dump(SHM_CTX *, int);
static void apr$$shm_split(SHM_CTX *, SHM_SEG *, int);
static void apr$$shm_merge(SHM_CTX *);
static SHM_SEG *apr$$shm_header(SHM_CTX *, void *, int);
static void apr$$shm_insque(SHM_CTX *, SHM_SEG *, int);
static void apr$$shm_remque(SHM_CTX *, SHM_SEG *, int);

/*
** Define the external service prototypes
*/
extern int decc$$translate();

/*
** Define the local general service prototypes
*/
static int MemoryPageSize(void);

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
SHM_CTX *apr$shm_create(void *SectionName, size_t SectionSize,
			int SectionLocking, ...)
{
    $DESCRIPTOR(SectionNameDesc, "");
    VA_RANGE iaddr = { (void *) 0x2000, NULL }, oaddr = {
    NULL, NULL};
    SHM_CTX *shm_ctx = NULL;
    SHM_HDR *shm_hdr = NULL;
    int GalaxySection = 0, MaxSectionSize = 0;
    SHM_SEG *shm_seg;
    LKSB shm_lksb;
    va_list argp;
    int pagcnt = 0, pagsiz = 0, status, argc;

/*
** Process the optional parameters
*/
    va_start(argp, SectionLocking);
    va_count(argc);
    if (argc > 3)
	GalaxySection = va_arg(argp, int);
    if (argc > 4)
	MaxSectionSize = va_arg(argp, int);
    va_end(argp);

/*
** Allocate a share memory context
*/
    shm_ctx = malloc(sizeof(SHM_CTX));
    if (!shm_ctx) {
	fprintf(stderr,
		"ERROR (%d, 0x%08X): Unable to allocate shared memory context\n",
		errno, vaxc$errno);
	return (NULL);
    }
    memset(shm_ctx, 0, sizeof(SHM_CTX));

/*
** Update the section name descriptor
*/
    SectionNameDesc.dsc$w_length = strlen(SectionName);
    SectionNameDesc.dsc$a_pointer = SectionName;

/*
** Determine the section page size and count
*/
    pagsiz = (GalaxySection ? MemoryPageSize() : 512);
    pagcnt =
	(SectionSize + sizeof(SHM_HDR) + sizeof(SHM_SEG) +
	 (pagsiz - 1)) / pagsiz;

/*
** Create either a Galaxy/Local Global Section
*/
    if (GalaxySection) {
	__int64 SectionBytes = pagcnt * pagsiz;

	/*
	 ** Create and map the galaxy global section
	 */
	status =
	    apr$$create_galaxy_gblsec((struct dsc$descriptor *)
				      &SectionNameDesc, SectionBytes,
				      &oaddr);
	if (!(status & 1)) {
	    fprintf(stderr,
		    "ERROR (0x%08X): Creating and mapping galaxy section: %s (%x:%x)\n",
		    status, (char *) SectionName,
		    oaddr.va_range$ps_start_va, oaddr.va_range$ps_end_va);
	    free(shm_ctx);
	    decc$$translate(status);
	    return (NULL);
	} else
	    shm_ctx->ctx_status = status;
    } else {
	/*
	 ** Create and map the global section
	 **
	 ** Since SEC$M_EXPREG is specified, the use of the hex 2000 address in
	 ** iaddr is only to indicate the region (P0 or P1) in which to map the
	 ** desired section.  Thus the section is mapped into the P0 region.
	 */
	status = SYS$CRMPSC(&iaddr,
			    &oaddr,
			    0,
			    SEC$M_WRT | SEC$M_GBL | SEC$M_PAGFIL |
			    SEC$M_EXPREG, &SectionNameDesc, 0, 0, 0,
			    pagcnt, 0, 0x0FF00, 0);
	if (!(status & 1)) {
	    fprintf(stderr,
		    "ERROR (0x%08X): Creating and mapping section: %s (%x:%x)\n",
		    status, (char *) SectionName,
		    oaddr.va_range$ps_start_va, oaddr.va_range$ps_end_va);
	    free(shm_ctx);
	    decc$$translate(status);
	    return (NULL);
	} else
	    shm_ctx->ctx_status = status;
    }

/*
** Point to the global section
*/
    shm_ctx->ctx_va_range = oaddr;
    shm_hdr = (SHM_HDR *) oaddr.va_range$ps_start_va;
    shm_hdr->shm_galaxy = GalaxySection;

/*
** If we're the first to create the global section, then let's initialize
*/
    if (shm_ctx->ctx_status == SS$_CREATED) {
	/*
	 ** Initialize the shared memory header
	 */
	shm_hdr->shm_avail = 0;
	shm_hdr->shm_alloc = 0;
	shm_hdr->shm_bsize =
	    ((int) oaddr.va_range$ps_end_va -
	     (int) oaddr.va_range$ps_start_va) + 1;
	shm_hdr->shm_asize =
	    (MaxSectionSize ? shm_hdr->shm_bsize -
	     (sizeof(SHM_SEG) + sizeof(SHM_HDR)) : SectionSize);
	shm_hdr->shm_flags = 0;
	if (SectionLocking == SHM_M_LOCKING)
	    shm_hdr->shm_locking = 1;

	/*
	 ** Initialize the segment header
	 */
	shm_seg = (SHM_SEG *) ((int) shm_hdr + sizeof(SHM_HDR));
	shm_seg->seg_flink = 0;
	shm_seg->seg_blink = 0;
	shm_seg->seg_asize = 0;
	shm_seg->seg_bsize = shm_hdr->shm_asize;

	/*
	 ** Update the shared memory header
	 */
	shm_hdr->shm_avail = (int) shm_seg - (int) shm_hdr;
    }

/*
** If we're doing our own locking on this share memory, then let's create our
** lock resource.
*/
    if (shm_hdr->shm_locking) {
	/*
	 ** Clear the lock status block
	 */
	memset(&shm_lksb, 0, sizeof(shm_lksb));

	/*
	 ** Create the lock with a mode of Null.
	 */
	status = SYS$ENQW(EFN$C_ENF,	/* No event flag                */
			  LCK$K_NLMODE,	/* Null mode                    */
			  &shm_lksb,	/* Lock Status Block            */
			  LCK$M_EXPEDITE,	/* Expedite lock flag           */
			  &SectionNameDesc,	/* resource name descriptor     */
			  0,	/* No parent                    */
			  0,	/* No AST routine               */
			  0,	/* No AST parameter             */
			  0,	/* No blocking AST              */
			  PSL$C_USER,	/* Access mode = user           */
			  0,	/* Resource domain ID           */
			  0,	/* Range                        */
			  0);	/* No priority                  */
	if (status == SS$_NORMAL)
	    status = shm_lksb.lksb$w_status;
	if (status != SS$_NORMAL) {
	    fprintf(stderr,
		    "ERROR (0x%08X): Unable to create shared memory lock resource %s\n",
		    status, (char *) SectionName);
	    apr$shm_destroy(shm_ctx);
	    free(shm_ctx);
	    decc$$translate(status);
	    return (NULL);
	}

	/*
	 ** Save the lock id
	 */
	shm_ctx->ctx_lockid = shm_lksb.lksb$l_lkid;
    }

/*
** Return the shared memory addres
*/
    return (shm_ctx);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
SHM_CTX *apr$shm_attach(void *SectionName, int SectionLocking, ...)
{
    $DESCRIPTOR(SectionNameDesc, "");
    VA_RANGE iaddr = { NULL, NULL }, oaddr = {
    NULL, NULL};
    SHM_CTX *shm_ctx = NULL;
    SHM_HDR *shm_hdr = NULL;
    int GalaxySection = 0;
    SHM_SEG *shm_seg;
    LKSB shm_lksb;
    va_list argp;
    int status, argc;

/*
** Process the optional parameters
*/
    va_start(argp, SectionLocking);
    va_count(argc);
    if (argc > 3)
	GalaxySection = va_arg(argp, int);
    va_end(argp);

/*
** Allocate a share memory context
*/
    shm_ctx = malloc(sizeof(SHM_CTX));
    if (!shm_ctx) {
	fprintf(stderr,
		"ERROR (%d, 0x%08X): Unable to allocate shared memory context\n",
		errno, vaxc$errno);
	return (NULL);
    }
    memset(shm_ctx, 0, sizeof(SHM_CTX));

/*
** Update the section name descriptor
*/
    SectionNameDesc.dsc$w_length = strlen(SectionName);
    SectionNameDesc.dsc$a_pointer = SectionName;

/*
** Create either a Galaxy/Local Global Section
*/
    if (GalaxySection) {
#if 0
	/*
	 ** Load the privileged library
	 */
	load_privileged_library();

	/*
	 ** Create and map the galaxy global section
	 */
	status = vms_attach_galaxy_gblsec(&SectionNameDesc, &oaddr);
	if (!(status & 1)) {
	    fprintf(stderr,
		    "ERROR (0x%08X): Creating and mapping galaxy section: %s (%x:%x)\n",
		    status, (char *) SectionName,
		    oaddr.va_range$ps_start_va, oaddr.va_range$ps_end_va);
	    free(shm_ctx);
	    decc$$translate(status);
	    return (NULL);
	} else
	    shm_ctx->ctx_status = status;
#endif
    } else {
	/*
	 ** Since SEC$M_EXPREG is specified, the use of the SectionName address in
	 ** iaddr is only to indicate the region (P0 or P1) in which to map the
	 ** desired section.  Thus the section is mapped into the same region as
	 ** is the given section name.
	 */
	iaddr.va_range$ps_start_va = SectionName;

	/*
	 ** Create and map the global section
	 */
	status = SYS$MGBLSC(&iaddr,
			    &oaddr,
			    0,
			    SEC$M_WRT | SEC$M_GBL | SEC$M_PAGFIL |
			    SEC$M_EXPREG, &SectionNameDesc, 0, 0);
	if (!(status & 1)) {
	    fprintf(stderr,
		    "ERROR (0x%08X): Creating and mapping section: %s (%x:%x)\n",
		    status, (char *) SectionName,
		    oaddr.va_range$ps_start_va, oaddr.va_range$ps_end_va);
	    free(shm_ctx);
	    decc$$translate(status);
	    return (NULL);
	} else
	    shm_ctx->ctx_status = status;
    }

/*
** Point to the global section
*/
    shm_ctx->ctx_va_range = oaddr;
    shm_hdr = (SHM_HDR *) oaddr.va_range$ps_start_va;
    shm_hdr->shm_galaxy = GalaxySection;

/*
** If we're doing our own locking on this share memory, then let's create our
** lock resource.
*/
    if (shm_hdr->shm_locking) {
	/*
	 ** Clear the lock status block
	 */
	memset(&shm_lksb, 0, sizeof(shm_lksb));

	/*
	 ** Create the lock with a mode of Null.
	 */
	status = SYS$ENQW(EFN$C_ENF,	/* No event flag                */
			  LCK$K_NLMODE,	/* Null mode                    */
			  &shm_lksb,	/* Lock Status Block            */
			  LCK$M_EXPEDITE,	/* Expedite lock flag           */
			  &SectionNameDesc,	/* resource name descriptor     */
			  0,	/* No parent                    */
			  0,	/* No AST routine               */
			  0,	/* No AST parameter             */
			  0,	/* No blocking AST              */
			  PSL$C_USER,	/* Access mode = user           */
			  0,	/* Resource domain ID           */
			  0,	/* Range                        */
			  0);	/* No priority                  */
	if (status == SS$_NORMAL)
	    status = shm_lksb.lksb$w_status;
	if (status != SS$_NORMAL) {
	    fprintf(stderr,
		    "ERROR (0x%08X): Unable to create shared memory lock resource %s\n",
		    status, (char *) SectionName);
	    apr$shm_destroy(shm_ctx);
	    free(shm_ctx);
	    decc$$translate(status);
	    return (NULL);
	}

	/*
	 ** Save the lock id
	 */
	shm_ctx->ctx_lockid = shm_lksb.lksb$l_lkid;
    }

/*
** Return the shared memory addres
*/
    return (shm_ctx);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
void *apr$shm_get_common_ctx(SHM_CTX * shm_ctx)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    SHM_SEG *shm_seg;
    void *ptr;

/*
** Initialize the context status
*/
    shm_ctx->ctx_status = SS$_NORMAL;

/*
** Lock the shared memory section
*/
    apr$$shm_lock(shm_ctx);
    if (!(shm_ctx->ctx_status & 1))
	return (NULL);

/*
** If no context has been established yet, return NULL
*/
    if (!shm_hdr->shm_common_ctx) {
	apr$$shm_unlock(shm_ctx);
	return (NULL);
    }

/*
** Calculate the address of the common context memory
*/
    shm_seg = (SHM_SEG *) ((int) shm_hdr + shm_hdr->shm_common_ctx);
    ptr = (void *) ((int) shm_seg + sizeof(SHM_SEG));

/*
** Unlock the shared memory section
*/
    apr$$shm_unlock(shm_ctx);

/*
** Return the common context memory
*/
    return (ptr);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
void apr$shm_set_common_ctx(SHM_CTX * shm_ctx, void *ptr)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    SHM_SEG *shm_seg;

/*
** Initialize the context status
*/
    shm_ctx->ctx_status = SS$_NORMAL;

/*
** Lock the shared memory section
*/
    apr$$shm_lock(shm_ctx);
    if (!(shm_ctx->ctx_status & 1))
	return;

/*
** Search the allocated headers looking for the given segment address
*/
    shm_seg = (SHM_SEG *) ((int) shm_hdr + shm_hdr->shm_alloc);
    while (shm_seg) {
	if (ptr == (void *) ((int) shm_seg + sizeof(SHM_SEG))) {
	    shm_hdr->shm_common_ctx = (int) shm_seg - (int) shm_hdr;
	    break;
	}

	if (!shm_seg->seg_flink)
	    break;
	shm_seg = (SHM_SEG *) ((int) shm_hdr + shm_seg->seg_flink);
    }

/*
** Unlock the shared memory section
*/
    apr$$shm_unlock(shm_ctx);

/*
** Return
*/
    return;

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int apr$shm_allocated(SHM_CTX * shm_ctx)
{
    SHM_STS *shm_sts = NULL;
    int asize;

/*
** Initialize the context status
*/
    shm_ctx->ctx_status = SS$_NORMAL;

/*
** Lock the shared memory section
*/
    apr$$shm_lock(shm_ctx);
    if (!(shm_ctx->ctx_status & 1))
	return (-1);

/*
** Get the allocated memory statistics
*/
    shm_sts = apr$$shm_stats(shm_ctx, SHM_M_ALLOC);
    if (!shm_sts) {
	apr$$shm_unlock(shm_ctx);
	return (-1);
    }

/*
** Unlock the shared memory section
*/
    apr$$shm_unlock(shm_ctx);

/*
** Save the allocated memory size and free the statistics block
*/
    asize = shm_sts->sts_asize;
    free(shm_sts);

/*
** Return the allocated memory size
*/
    return (asize);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int apr$shm_available(SHM_CTX * shm_ctx)
{
    SHM_STS *shm_sts = NULL;
    int bsize;

/*
** Initialize the context status
*/
    shm_ctx->ctx_status = SS$_NORMAL;

/*
** Lock the shared memory section
*/
    apr$$shm_lock(shm_ctx);
    if (!(shm_ctx->ctx_status & 1))
	return (-1);

/*
** Get the available memory statistics
*/
    shm_sts = apr$$shm_stats(shm_ctx, SHM_M_AVAIL);
    if (!shm_sts) {
	apr$$shm_unlock(shm_ctx);
	return (-1);
    }

/*
** Unlock the shared memory section
*/
    apr$$shm_unlock(shm_ctx);

/*
** Save the available memory size and free the statistics block
*/
    bsize = shm_sts->sts_bsize;
    free(shm_sts);

/*
** Return the available memory size
*/
    return (bsize);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
void *apr$shm_malloc(SHM_CTX * shm_ctx, size_t size)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    SHM_SEG *shm_seg = NULL, *use_seg = NULL;
    int shm_merge = 0, rsize, i;

/*
** Initialize the context status
*/
    shm_ctx->ctx_status = SS$_NORMAL;

/*
** Lock the shared memory section
*/
    apr$$shm_lock(shm_ctx);
    if (!(shm_ctx->ctx_status & 1))
	return (NULL);

/*
** Round the allocation size to be longword aligned and accommodate any
** zero length allocations.
*/
    size = (size == 0) ? SHM_C_ALIGN : size;
    rsize =
	size + ((size % SHM_C_ALIGN) ? SHM_C_ALIGN -
		size % SHM_C_ALIGN : 0);

/*
** Process the available headers for a good size fit
*/
    for (i = 0; i < 2 && !use_seg; i++) {
	if (shm_hdr->shm_avail)
	    shm_seg = (SHM_SEG *) ((int) shm_hdr + shm_hdr->shm_avail);
	while (shm_seg) {
	    if (shm_seg->seg_bsize >= rsize)
		if (!use_seg)
		    use_seg = shm_seg;
		else if (use_seg->seg_bsize > shm_seg->seg_bsize)
		    use_seg = shm_seg;

	    if (!shm_seg->seg_flink)
		break;

	    shm_seg = (SHM_SEG *) ((int) shm_hdr + shm_seg->seg_flink);
	}

	/*
	 ** If no available segment was found, then try to merge any contiguous
	 ** available segments and trying again.
	 */
	if (!use_seg && !shm_merge) {
	    apr$$shm_merge(shm_ctx);
	    shm_merge++;
	}
    }

/*
** If no available memory was found, then return NULL.
*/
    if (!use_seg) {
	apr$$shm_unlock(shm_ctx);
	return (NULL);
    }

/*
** If this segment is large enough to hold another segment with data then let's
** try to split it.
*/
    use_seg->seg_asize = size;
    apr$$shm_split(shm_ctx, use_seg, rsize);
    apr$$shm_remque(shm_ctx, use_seg, SHM_M_AVAIL);
    apr$$shm_insque(shm_ctx, use_seg, SHM_M_ALLOC);

/*
** Unlock the shared memory section
*/
    apr$$shm_unlock(shm_ctx);

/*
** Return the shared memory address
*/
    return ((void *) ((int) use_seg + sizeof(SHM_SEG)));

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
void *apr$shm_calloc(SHM_CTX * shm_ctx, size_t number, size_t size)
{
    void *ret_ptr;

/*
** Simply call the malloc routine and clear the memory returned
*/
    ret_ptr = apr$shm_malloc(shm_ctx, number * size);
    if (ret_ptr)
	memset(ret_ptr, 0, number * size);

/*
** Return the shared memory address
*/
    return (ret_ptr);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
void *apr$shm_realloc(SHM_CTX * shm_ctx, void *ptr, size_t size)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    void *tmp_ptr = NULL, *ret_ptr = NULL;
    SHM_SEG *shm_seg, *prv_seg = NULL, *nxt_seg = NULL, *tmp_seg;
    int tmp_len = 0, bsize = 0, rsize = 0;

/*
** If the pointer provided is NULL, then simply call the malloc routine
*/
    if (!ptr)
	return (apr$shm_malloc(shm_ctx, size));

/*
** Initialize the context status
*/
    shm_ctx->ctx_status = SS$_NORMAL;

/*
** Lock the shared memory section
*/
    apr$$shm_lock(shm_ctx);
    if (!(shm_ctx->ctx_status & 1))
	return (NULL);

/*
** Get the memory segment header but if it couldn't be found then return NULL
*/
    shm_seg = apr$$shm_header(shm_ctx, ptr, SHM_M_ALLOC);
    if (!shm_seg) {
	apr$$shm_unlock(shm_ctx);
	return (NULL);
    }

/*
** if the requested size is smaller than the allocated size, see if we can
** split the segment and then update the segment size.
*/
    if (size < shm_seg->seg_asize) {
	shm_seg->seg_asize = size;
	bsize = shm_seg->seg_bsize;
	rsize = (size == 0) ? SHM_C_ALIGN : size;
	rsize = rsize +
	    ((rsize % SHM_C_ALIGN) ? SHM_C_ALIGN -
	     rsize % SHM_C_ALIGN : 0);
	apr$$shm_split(shm_ctx, shm_seg, rsize);
	if (bsize != shm_seg->seg_bsize) {
	    tmp_seg = (SHM_SEG *)
		((int) shm_seg + sizeof(SHM_SEG) + shm_seg->seg_bsize);
	    apr$$shm_remque(shm_ctx, tmp_seg, SHM_M_ALLOC);
	    apr$$shm_insque(shm_ctx, tmp_seg, SHM_M_AVAIL);
	}
	apr$$shm_unlock(shm_ctx);
	return (ptr);
    }

/*
** if the requested size is smaller than the blocked size, then updated the
** segment size.
*/
    if (size <= shm_seg->seg_bsize) {
	shm_seg->seg_asize = size;
	apr$$shm_unlock(shm_ctx);
	return (ptr);
    }

/*
** If we can simply allocate this memory then copy the data at the given address
** to the new memory, free the given address and return.
*/
    ret_ptr = apr$shm_calloc(shm_ctx, 1, size);
    if (ret_ptr) {
	memcpy(ret_ptr, ptr,
	       (size > shm_seg->seg_asize) ? shm_seg->seg_asize : size);
	apr$shm_free(shm_ctx, ptr);
	apr$$shm_unlock(shm_ctx);
	return (ret_ptr);
    }

/*
** If we've got here then we're unable to allocate the contiguous memory for
** the requested size and a merge has occurred as a result of our allocation
** failure.  Now, we'll try to see if there is memory at either end of the
** existing memory which we can merge into our current allocation.  Point to
** the first available segment to begin looking.
*/
    if (shm_hdr->shm_avail)
	tmp_seg = (SHM_SEG *) ((int) shm_hdr + shm_hdr->shm_avail);

/*
** Locate any available contiguous segments
*/
    while (tmp_seg) {
	if (((int) tmp_seg - (int) shm_hdr) ==
	    (((int) shm_seg - (int) shm_hdr) +
	     shm_seg->seg_bsize + sizeof(SHM_SEG))) {
	    nxt_seg = tmp_seg;
	} else
	    if (((int) shm_seg - (int) shm_hdr) ==
		(((int) tmp_seg - (int) shm_hdr) +
		 tmp_seg->seg_bsize + sizeof(SHM_SEG))) {
	    prv_seg = tmp_seg;
	}

	if (!tmp_seg->seg_flink)
	    break;
	tmp_seg = (SHM_SEG *) ((int) shm_hdr + tmp_seg->seg_flink);
    }

/*
** See if we can merge with the next contiguous available segment to satisfy
** the requested size.
*/
    if (nxt_seg &&
	((nxt_seg->seg_bsize + sizeof(SHM_SEG) + shm_seg->seg_bsize) >=
	 size)) {
	/*
	 ** Merge the next contiguous segment by updating the given segment with
	 ** the data sizes.  We also remove the next segment from the available list
	 ** since it is now combined with the given segment.  Clear any unused
	 ** memory in the segment block.
	 */
	shm_seg->seg_asize = size;
	shm_seg->seg_bsize = shm_seg->seg_bsize +
	    nxt_seg->seg_bsize + sizeof(SHM_SEG);
	apr$$shm_remque(shm_ctx, nxt_seg, SHM_M_AVAIL);
	memset(nxt_seg, 0, nxt_seg->seg_bsize + sizeof(SHM_SEG));

	/*
	 ** See if we can free up any unused space by calling split.
	 */
	bsize = shm_seg->seg_bsize;
	rsize = (size == 0) ? SHM_C_ALIGN : size;
	rsize = rsize +
	    ((rsize % SHM_C_ALIGN) ? SHM_C_ALIGN -
	     rsize % SHM_C_ALIGN : 0);
	apr$$shm_split(shm_ctx, shm_seg, rsize);
	if (bsize != shm_seg->seg_bsize) {
	    tmp_seg = (SHM_SEG *)
		((int) shm_seg + sizeof(SHM_SEG) + shm_seg->seg_bsize);
	    apr$$shm_remque(shm_ctx, tmp_seg, SHM_M_ALLOC);
	    apr$$shm_insque(shm_ctx, tmp_seg, SHM_M_AVAIL);
	}

	/*
	 ** Unlock the shared memory section and return the reallocated address.
	 */
	apr$$shm_unlock(shm_ctx);
	return (ptr);
    }

/*
** See if we can merge with the previous contiguous available segment to
** satisfy the requested size.
*/
    if (prv_seg &&
	((prv_seg->seg_bsize + sizeof(SHM_SEG) + shm_seg->seg_bsize) >=
	 size)) {
	/*
	 ** Merge the previous contiguous segment by removing it from the available
	 ** list and updating the previous segment with the data sizes.  We also
	 ** remove the given segment from the allocated list since it is now
	 ** contained within the previous segment.  Copy the data from the given
	 ** segment into the previous segment and clear any unused memory in the
	 ** segment block.  We then insert the merge previous segment into the
	 ** allocated memory list.
	 */
	tmp_len = shm_seg->seg_asize;
	apr$$shm_remque(shm_ctx, prv_seg, SHM_M_AVAIL);
	prv_seg->seg_asize = size;
	prv_seg->seg_bsize = prv_seg->seg_bsize +
	    shm_seg->seg_bsize + sizeof(SHM_SEG);
	apr$$shm_remque(shm_ctx, shm_seg, SHM_M_ALLOC);
	ret_ptr = (void *) ((int) prv_seg + sizeof(SHM_SEG));
	memcpy(ret_ptr, ptr, tmp_len);
	tmp_ptr = (char *) ((int) ret_ptr + tmp_len);
	memset(tmp_ptr, 0, prv_seg->seg_bsize - tmp_len);
	apr$$shm_insque(shm_ctx, prv_seg, SHM_M_ALLOC);

	/*
	 ** See if we can free up any unused space by calling split.
	 */
	bsize = prv_seg->seg_bsize;
	rsize = (size == 0) ? SHM_C_ALIGN : size;
	rsize = rsize +
	    ((rsize % SHM_C_ALIGN) ? SHM_C_ALIGN -
	     rsize % SHM_C_ALIGN : 0);
	apr$$shm_split(shm_ctx, prv_seg, rsize);
	if (bsize != prv_seg->seg_bsize) {
	    tmp_seg = (SHM_SEG *)
		((int) prv_seg + sizeof(SHM_SEG) + prv_seg->seg_bsize);
	    apr$$shm_remque(shm_ctx, tmp_seg, SHM_M_ALLOC);
	    apr$$shm_insque(shm_ctx, tmp_seg, SHM_M_AVAIL);
	}

	/*
	 ** Unlock the shared memory section and return the reallocated address.
	 */
	apr$$shm_unlock(shm_ctx);
	return (ret_ptr);
    }

/*
** See if we can merge with the previous and next contiguous available segments
** to satisfy the requested size.
*/
    if (nxt_seg && prv_seg &&
	((nxt_seg->seg_bsize + sizeof(SHM_SEG) +
	  prv_seg->seg_bsize + sizeof(SHM_SEG) + shm_seg->seg_bsize) >=
	 size)) {
	/*
	 ** Merge the previous and next contiguous segments by removing them from
	 ** the available list and updating the previous segment with the data sizes.
	 ** We also remove the given segment from the allocated list since it is now
	 ** contained within the previous segment.  Copy the data from the given
	 ** segment into the previous segment and clear any unused memory in the
	 ** segment block.  We then insert the merge previous segment into the
	 ** allocated memory list.
	 */
	tmp_len = shm_seg->seg_asize;
	apr$$shm_remque(shm_ctx, prv_seg, SHM_M_AVAIL);
	apr$$shm_remque(shm_ctx, nxt_seg, SHM_M_AVAIL);
	prv_seg->seg_asize = size;
	prv_seg->seg_bsize = prv_seg->seg_bsize + sizeof(SHM_SEG) +
	    nxt_seg->seg_bsize + sizeof(SHM_SEG) + shm_seg->seg_bsize;
	apr$$shm_remque(shm_ctx, shm_seg, SHM_M_ALLOC);
	ret_ptr = (void *) ((int) prv_seg + sizeof(SHM_SEG));
	memcpy(ret_ptr, ptr, tmp_len);
	tmp_ptr = (char *) ((int) ret_ptr + tmp_len);
	memset(tmp_ptr, 0, prv_seg->seg_bsize - tmp_len);
	apr$$shm_insque(shm_ctx, prv_seg, SHM_M_ALLOC);

	/*
	 ** See if we can free up any unused space by calling split.
	 */
	bsize = prv_seg->seg_bsize;
	rsize = (size == 0) ? SHM_C_ALIGN : size;
	rsize = rsize +
	    ((rsize % SHM_C_ALIGN) ? SHM_C_ALIGN -
	     rsize % SHM_C_ALIGN : 0);
	apr$$shm_split(shm_ctx, prv_seg, rsize);
	if (bsize != prv_seg->seg_bsize) {
	    tmp_seg = (SHM_SEG *)
		((int) prv_seg + sizeof(SHM_SEG) + prv_seg->seg_bsize);
	    apr$$shm_remque(shm_ctx, tmp_seg, SHM_M_ALLOC);
	    apr$$shm_insque(shm_ctx, tmp_seg, SHM_M_AVAIL);
	}

	/*
	 ** Unlock the shared memory section and return the reallocated address.
	 */
	apr$$shm_unlock(shm_ctx);
	return (ret_ptr);
    }

/*
** Unlock the shared memory section
*/
    apr$$shm_unlock(shm_ctx);

/*
** Return the shared memory address
*/
    return (ret_ptr);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
void *apr$shm_pointer(SHM_CTX * shm_ctx, int off)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    void *ptr = NULL;

/*
** Initialize the context status
*/
    shm_ctx->ctx_status = SS$_NORMAL;

/*
** Calculate the pointer of the given offset
*/
    if (off > 0 && off <= shm_hdr->shm_bsize)
	ptr = (void *) ((int) shm_hdr + off);

/*
** Return the pointer
*/
    return (ptr);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int apr$shm_offset(SHM_CTX * shm_ctx, void *ptr)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    int off = 0;

/*
** Initialize the context status
*/
    shm_ctx->ctx_status = SS$_NORMAL;

/*
** Calculate the offset of the given pointer
*/
    if (ptr > (void *) shm_hdr
	&& ptr <= (void *) ((int) shm_hdr + shm_hdr->shm_bsize))
	off = (int) ptr - (int) shm_hdr;

/*
** Return the offset
*/
    return (off);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
void *apr$shm_strdup(SHM_CTX * shm_ctx, void *ptr)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    void *tmp_ptr = NULL;
    int tmp_len = 0;

/*
** Initialize the context status
*/
    shm_ctx->ctx_status = SS$_NORMAL;

/*
** Lock the shared memory section
*/
    apr$$shm_lock(shm_ctx);
    if (!(shm_ctx->ctx_status & 1))
	return (NULL);

/*
** Get the size of the requested memory, but if it couldn't be found then
** use strlen to get it's length.
*/
    tmp_len = apr$shm_sizeof(shm_ctx, ptr);
    if (tmp_len == -1)
	tmp_len = strlen(ptr);

/*
** Allocate the necessary shared memory and copy the contents
*/
    tmp_ptr = apr$shm_malloc(shm_ctx, tmp_len);
    if (!tmp_ptr) {
	apr$$shm_unlock(shm_ctx);
	return (NULL);
    }
    memcpy(tmp_ptr, ptr, tmp_len);

/*
** Unlock the shared memory section
*/
    apr$$shm_unlock(shm_ctx);

/*
** Return the shared memory address
*/
    return (tmp_ptr);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
int apr$shm_sizeof(SHM_CTX * shm_ctx, void *ptr)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    SHM_SEG *shm_seg = NULL;

/*
** Locate and return the allocated size of this address.  If the address was
** not found then return zero size
*/
    shm_seg = apr$$shm_header(shm_ctx, ptr, SHM_M_ALLOC);
    if (shm_seg)
	return (shm_seg->seg_asize);
    else
	return (-1);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
void apr$shm_free(SHM_CTX * shm_ctx, void *ptr)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    SHM_SEG *shm_seg, *use_seg;

/*
** Initialize the context status
*/
    shm_ctx->ctx_status = SS$_NORMAL;

/*
** Lock the shared memory section
*/
    apr$$shm_lock(shm_ctx);
    if (!(shm_ctx->ctx_status & 1))
	return;

/*
** Get the given segment header
*/
    shm_seg = apr$$shm_header(shm_ctx, ptr, SHM_M_ALLOC);
    if (shm_seg) {
	apr$$shm_remque(shm_ctx, shm_seg, SHM_M_ALLOC);
	apr$$shm_insque(shm_ctx, shm_seg, SHM_M_AVAIL);
	shm_seg->seg_asize = 0;
	if (shm_hdr->shm_common_ctx)
	    if (ptr == (void *) ((int) shm_hdr + shm_hdr->shm_common_ctx))
		shm_hdr->shm_common_ctx = 0;
    }

/*
** Unlock the shared memory section
*/
    apr$$shm_unlock(shm_ctx);

/*
** Return
*/
    return;

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
void apr$shm_detach(SHM_CTX * shm_ctx)
{
    int status;

/*
** delete the global section references
*/
    status = apr$$shm_deltva(shm_ctx);
    if (status != SS$_NORMAL)
	decc$$translate(status);

/*
** Clear the shared memory context
*/
    memset(shm_ctx, 0, sizeof(SHM_CTX));

/*
** Return
*/
    return;

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
void apr$shm_destroy(SHM_CTX * shm_ctx)
{
    int status;

/*
** delete the global section references
*/
    status = apr$$shm_deltva(shm_ctx);
    if (status != SS$_NORMAL)
	decc$$translate(status);

/*
** Free the shared memory context
*/
    free(shm_ctx);

/*
** Return
*/
    return;

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static int apr$$shm_deltva(SHM_CTX * shm_ctx)
{
    int DeqFlags = LCK$M_DEQALL, status = SS$_NORMAL;
    SHM_HDR *shm_hdr;

/*
** If there is virtual address range then just return
*/
    if (!(shm_ctx->ctx_va_range.va_range$ps_start_va &&
	  shm_ctx->ctx_va_range.va_range$ps_end_va))
	return (status);

/*
** Point to the shared memory header
*/
    shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;

/*
** If we're doing our own locking on this share memory, then let's delete our
** lock resource.
*/
    if (shm_hdr->shm_locking) {
	/*
	 ** Dequeue the lock
	 */
	status = SYS$DEQ(shm_ctx->ctx_lockid,	/* Lock ID                      */
			 0,	/* No value block               */
			 PSL$C_USER,	/* Access mode = user           */
			 DeqFlags);	/* Flags                        */
	if (status != SS$_NORMAL)
	    fprintf(stderr,
		    "ERROR (0x%08X): Unable to dequeue shared memory lock resource\n",
		    status);
    }

/*
** Destroy either the galaxy/local shared memory section
*/
    if (shm_hdr->shm_galaxy) {
	__int64 SectionRegion = VA$C_P0;
	__int64 SectionLength =
	    ((int) shm_ctx->ctx_va_range.va_range$ps_end_va -
	     (int) shm_ctx->ctx_va_range.va_range$ps_start_va) + 1;

	/*
	 ** Destroy the galaxy shared memory section
	 */
	status = SYS$DELTVA_64((void *) &SectionRegion,
			       shm_ctx->ctx_va_range.va_range$ps_start_va,
			       SectionLength,
			       PSL$C_USER,
			       (void *) &shm_ctx->ctx_va_range.
			       va_range$ps_end_va,
			       (void *) &SectionLength);
	if (status != SS$_NORMAL)
	    fprintf(stderr,
		    "ERROR (0x%08X): Unable to delete galaxy shared memory\n",
		    status);
    } else {
	/*
	 ** Destroy the local shared memory section
	 */
	status = SYS$DELTVA(&shm_ctx->ctx_va_range, 0, PSL$C_USER);
	if (status != SS$_NORMAL)
	    fprintf(stderr,
		    "ERROR (0x%08X): Unable to delete shared memory\n",
		    status);
    }

/*
** Return status
*/
    return (status);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static SHM_SEG *apr$$shm_header(SHM_CTX * shm_ctx, void *ptr, int seg_lst)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    SHM_SEG *shm_seg;

/*
** Point to the appropriate segment list
*/
    if (seg_lst == SHM_M_ALLOC)
	shm_seg = (SHM_SEG *) ((int) shm_hdr + shm_hdr->shm_alloc);
    else
	shm_seg = (SHM_SEG *) ((int) shm_hdr + shm_hdr->shm_avail);

/*
** Locate and return the segment header for this address
*/
    while (shm_seg) {
	if (ptr == (void *) ((int) shm_seg + sizeof(SHM_SEG)))
	    return (shm_seg);

	if (!shm_seg->seg_flink)
	    break;
	shm_seg = (SHM_SEG *) ((int) shm_hdr + shm_seg->seg_flink);
    }

/*
** If the address was not found then return zero size
*/
    return (NULL);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static void apr$$shm_merge(SHM_CTX * shm_ctx)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    SHM_SEG *shm_seg = NULL, *tmp_seg;

/*
** Point to the first available segment
*/
    if (shm_hdr->shm_avail)
	shm_seg = (SHM_SEG *) ((int) shm_hdr + shm_hdr->shm_avail);

/*
** Bubble sort the available headers looking for contiguous headers to merge
*/
    while (shm_seg) {
	if (!shm_seg->seg_flink)
	    break;
	tmp_seg = (SHM_SEG *) ((int) shm_hdr + shm_seg->seg_flink);

	while (tmp_seg) {
	    if (((int) tmp_seg - (int) shm_hdr) ==
		(((int) shm_seg - (int) shm_hdr) + shm_seg->seg_bsize +
		 sizeof(SHM_SEG))) {
		apr$$shm_remque(shm_ctx, tmp_seg, SHM_M_AVAIL);
		shm_seg->seg_bsize = shm_seg->seg_bsize +
		    tmp_seg->seg_bsize + sizeof(SHM_SEG);
	    } else
		if (((int) shm_seg - (int) shm_hdr) ==
		    (((int) tmp_seg - (int) shm_hdr) + tmp_seg->seg_bsize +
		     sizeof(SHM_SEG))) {
		apr$$shm_remque(shm_ctx, shm_seg, SHM_M_AVAIL);
		tmp_seg->seg_bsize = tmp_seg->seg_bsize +
		    shm_seg->seg_bsize + sizeof(SHM_SEG);
	    }

	    if (!tmp_seg->seg_flink)
		break;
	    tmp_seg = (SHM_SEG *) ((int) shm_hdr + tmp_seg->seg_flink);
	}

	if (!shm_seg->seg_flink)
	    break;
	shm_seg = (SHM_SEG *) ((int) shm_hdr + shm_seg->seg_flink);
    }

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static void apr$$shm_split(SHM_CTX * shm_ctx, SHM_SEG * shm_seg, int size)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    SHM_SEG *tmp_seg1, *tmp_seg2;

/*
** If no split is required then simply return
*/
    if ((shm_seg->seg_bsize - size) < (sizeof(SHM_SEG) + SHM_C_ALIGN))
	return;

/*
** If we're not at the end of the available list, then save the forward
** link to the next segment.
*/
    if (shm_seg->seg_flink)
	tmp_seg1 = (SHM_SEG *) ((int) shm_hdr + shm_seg->seg_flink);
    else
	tmp_seg1 = NULL;

/*
** Initialize the new segment in this split
*/
    tmp_seg2 = (SHM_SEG *) ((int) shm_seg + sizeof(SHM_SEG) + size);
    tmp_seg2->seg_flink = shm_seg->seg_flink;
    tmp_seg2->seg_blink = (int) shm_seg - (int) shm_hdr;
    tmp_seg2->seg_asize = 0;
    tmp_seg2->seg_bsize = shm_seg->seg_bsize - (sizeof(SHM_SEG) + size);

/*
** update the initial segment's forward link & size
*/
    shm_seg->seg_flink = (int) tmp_seg2 - (int) shm_hdr;
    shm_seg->seg_bsize = size;

/*
** If we're not at the end of the available list, then update the backward
** link of the next segment.
*/
    if (tmp_seg1)
	tmp_seg1->seg_blink = (int) tmp_seg2 - (int) shm_hdr;

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static void apr$$shm_remque(SHM_CTX * shm_ctx, SHM_SEG * shm_seg,
			    int seg_lst)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    SHM_SEG *tmp_seg;

/*
** Update the previous segment
*/
    if (shm_seg->seg_blink) {
	tmp_seg = (SHM_SEG *) ((int) shm_hdr + shm_seg->seg_blink);
	tmp_seg->seg_flink = shm_seg->seg_flink;
    } else if (seg_lst == SHM_M_ALLOC)
	shm_hdr->shm_alloc = shm_seg->seg_flink;
    else
	shm_hdr->shm_avail = shm_seg->seg_flink;

/*
** Update the following segment
*/
    if (shm_seg->seg_flink) {
	tmp_seg = (SHM_SEG *) ((int) shm_hdr + shm_seg->seg_flink);
	tmp_seg->seg_blink = shm_seg->seg_blink;
    }

/*
** Initialize the segment links
*/
    shm_seg->seg_flink = 0;
    shm_seg->seg_blink = 0;

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static void apr$$shm_insque(SHM_CTX * shm_ctx, SHM_SEG * shm_seg,
			    int seg_lst)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    SHM_SEG *tmp_seg;

/*
** Initialize the segment links
*/
    shm_seg->seg_flink = 0;
    shm_seg->seg_blink = 0;

/*
** Insert the segment into the appropriate list
*/
    if (seg_lst == SHM_M_ALLOC) {
	if (shm_hdr->shm_alloc) {
	    tmp_seg = (SHM_SEG *) ((int) shm_hdr + shm_hdr->shm_alloc);
	    tmp_seg->seg_blink = ((int) shm_seg - (int) shm_hdr);
	    shm_seg->seg_flink = shm_hdr->shm_alloc;
	}
	shm_hdr->shm_alloc = (int) shm_seg - (int) shm_hdr;
    } else {
	if (shm_hdr->shm_avail) {
	    tmp_seg = (SHM_SEG *) ((int) shm_hdr + shm_hdr->shm_avail);
	    tmp_seg->seg_blink = ((int) shm_seg - (int) shm_hdr);
	    shm_seg->seg_flink = shm_hdr->shm_avail;
	}
	shm_hdr->shm_avail = (int) shm_seg - (int) shm_hdr;
    }

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static SHM_STS *apr$$shm_stats(SHM_CTX * shm_ctx, int seg_lst)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    SHM_SEG *shm_seg = NULL;
    SHM_STS *shm_sts = NULL;

/*
** Allocate and initialize the status block
*/
    shm_sts = malloc(sizeof(SHM_STS));
    if (!shm_sts)
	return (NULL);
    memset(shm_sts, 0, sizeof(SHM_STS));

/*
** Point to the allocated or available list
*/
    if (seg_lst == SHM_M_ALLOC) {
	if (shm_hdr->shm_alloc)
	    shm_seg = (SHM_SEG *) ((int) shm_hdr + shm_hdr->shm_alloc);
    } else {
	if (shm_hdr->shm_avail)
	    shm_seg = (SHM_SEG *) ((int) shm_hdr + shm_hdr->shm_avail);
    }

/*
** Gather the statistics for the requested list
*/
    while (shm_seg) {
	shm_sts->sts_segct++;
	shm_sts->sts_asize += shm_seg->seg_asize;
	shm_sts->sts_bsize += shm_seg->seg_bsize;
	if (shm_seg->seg_flink)
	    shm_seg = (SHM_SEG *) ((int) shm_hdr + shm_seg->seg_flink);
	else
	    shm_seg = NULL;
    }

/*
** Return the shared memory statistics
*/
    return (shm_sts);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static void apr$$shm_dump(SHM_CTX * shm_ctx, int seg_lst)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    SHM_SEG *shm_seg = NULL;
    int ctr = 1;

/*
** Point to the allocated or available list
*/
    if (seg_lst == SHM_M_ALLOC) {
	if (shm_hdr->shm_alloc)
	    shm_seg = (SHM_SEG *) ((int) shm_hdr + shm_hdr->shm_alloc);
    } else {
	if (shm_hdr->shm_avail)
	    shm_seg = (SHM_SEG *) ((int) shm_hdr + shm_hdr->shm_avail);
    }

/*
** Print the dump list header
*/
    printf("%s Segment List:\n",
	   (seg_lst == SHM_M_ALLOC) ? "Allocated" : "Available");

/*
** Dump the segments in the requested list
*/
    while (shm_seg) {
	printf("   seg_addr:  %d\n", (int) shm_seg - (int) shm_hdr);
	printf("   seg_flink: %d\n", shm_seg->seg_flink);
	printf("   seg_blink: %d\n", shm_seg->seg_blink);
	printf("   seg_asize: %d\n", shm_seg->seg_asize);
	printf("   seg_bsize: %d\n", shm_seg->seg_bsize);
	printf("   seg_data:  '%-*.*s'\n", shm_seg->seg_asize,
	       shm_seg->seg_asize,
	       (char *) ((int) shm_seg + sizeof(SHM_SEG)));
	if (shm_seg->seg_flink)
	    shm_seg = (SHM_SEG *) ((int) shm_hdr + shm_seg->seg_flink);
	else
	    shm_seg = NULL;
	printf("\n");
    }

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static void apr$$shm_lock(SHM_CTX * shm_ctx)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    LKSB shm_lksb;

/*
** If we're not doing our own locking then just return
*/
    if (!shm_hdr->shm_locking)
	return;

/*
** If we're not doing to the first lock, just increment the count and
** return success
*/
    if (shm_ctx->ctx_lockcnt != 0) {
	shm_ctx->ctx_lockcnt++;
	return;
    }

/*
** Clear the lock status block and load it with our lock id
*/
    memset(&shm_lksb, 0, sizeof(shm_lksb));
    shm_lksb.lksb$l_lkid = shm_ctx->ctx_lockid;

/*
** Convert the lock to Exclusive
*/
    shm_ctx->ctx_status = SYS$ENQW(EFN$C_ENF,	/* No event flag              */
				   LCK$K_EXMODE,	/* Null mode                  */
				   &shm_lksb,	/* Lock Status Block          */
				   LCK$M_CONVERT,	/* Convert Flags              */
				   0,	/* No resource name desc      */
				   0,	/* No parent                  */
				   0,	/* No AST routine             */
				   0,	/* No AST parameter           */
				   0,	/* No blocking AST            */
				   PSL$C_USER,	/* Access mode = user         */
				   0,	/* Resource domain ID         */
				   0,	/* Range                      */
				   0);	/* No priority                */
    if (shm_ctx->ctx_status == SS$_NORMAL)
	shm_ctx->ctx_status = shm_lksb.lksb$w_status;

/*
** Set the lock count to 0
*/
    shm_ctx->ctx_lockcnt = 1;

/*
** Return
*/
    return;

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static void apr$$shm_unlock(SHM_CTX * shm_ctx)
{
    SHM_HDR *shm_hdr = shm_ctx->ctx_va_range.va_range$ps_start_va;
    LKSB shm_lksb;

/*
** If we're not doing our own locking then just return
*/
    if (!shm_hdr->shm_locking)
	return;

/*
** If we're not down to the last unlock, just decrement the count and
** return success
*/
    if (shm_ctx->ctx_lockcnt != 1) {
	if (shm_ctx->ctx_lockcnt)
	    shm_ctx->ctx_lockcnt--;
	return;
    }

/*
** Clear the lock status block and load it with our lock id
*/
    memset(&shm_lksb, 0, sizeof(shm_lksb));
    shm_lksb.lksb$l_lkid = shm_ctx->ctx_lockid;

/*
** Convert the lock to Null
*/
    shm_ctx->ctx_status = SYS$ENQW(EFN$C_ENF,	/* No event flag              */
				   LCK$K_NLMODE,	/* Null mode                  */
				   &shm_lksb,	/* Lock Status Block          */
				   LCK$M_CONVERT,	/* Convert Flags              */
				   0,	/* No resource name desc      */
				   0,	/* No parent                  */
				   0,	/* No AST routine             */
				   0,	/* No AST parameter           */
				   0,	/* No blocking AST            */
				   PSL$C_USER,	/* Access mode = user         */
				   0,	/* Resource domain ID         */
				   0,	/* Range                      */
				   0);	/* No priority                */
    if (shm_ctx->ctx_status == SS$_NORMAL)
	shm_ctx->ctx_status = shm_lksb.lksb$w_status;

/*
** Set the lock count to 0
*/
    shm_ctx->ctx_lockcnt = 0;

/*
** Return
*/
    return;

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static int MemoryPageSize(void)
{
    ILE3 SyiItems[2];
    ILE3 *Ile3Ptr;
    int PageSize, status;
    IOSB iosb;

/*
** Setup the SYI item list
*/
    ILE3_INIT(SyiItems);
    ILE3_ADD(SYI$_PAGE_SIZE, sizeof(PageSize), &PageSize, 0);
    ILE3_TERM;

/*
** Get the SYI information
*/
    status = SYS$GETSYIW(EFN$C_ENF,	/* No event flag              */
			 0,	/* No CSID address            */
			 0,	/* No node name               */
			 &SyiItems,	/* Item list                  */
			 &iosb,	/* IOSB                       */
			 0,	/* No AST routine             */
			 0);	/* No AST parameter           */
    if (status & 1)
	status = iosb.iosb$w_status;
    if (!(status & 1)) {
	fprintf(stderr, "ERROR (0x%08X): Unable to system page size\n",
		status);
	PageSize = 0;
    }

/*
** Return the Memory Pages Size
*/
    return (PageSize);

}
