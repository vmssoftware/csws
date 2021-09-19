/* Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef SHM_H
#define SHM_H

#include "apr.h"
#include "apr_private.h"
#include "apr_general.h"
#include "apr_lib.h"
#include "apr_shm.h"
#include "apr_pools.h"
#include "apr_file_io.h"
#include "apr_network_io.h"
#include "apr_portable.h"

#if APR_HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif
#ifdef HAVE_SYS_IPC_H
#include <sys/ipc.h>
#endif
#ifdef HAVE_SYS_MUTEX_H
#include <sys/mutex.h>
#endif
#ifdef HAVE_SYS_SHM_H
#include <sys/shm.h>
#endif
#if !defined(SHM_R)
#define SHM_R 0400
#endif
#if !defined(SHM_W)
#define SHM_W 0200
#endif
#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif

/* Not all systems seem to have MAP_FAILED defined, but it should always
 * just be (void *)-1. */
#ifndef MAP_FAILED
#define MAP_FAILED ((void *)-1)
#endif

struct apr_shm_t {
    apr_pool_t *pool;
    void *base;          /* base real address */
    void *usable;        /* base usable address */
    apr_size_t reqsize;  /* requested segment size */
    apr_size_t realsize; /* actual segment size */
    const char *filename;      /* NULL if anonymous */
#if APR_USE_SHMEM_SHMGET || APR_USE_SHMEM_SHMGET_ANON
    int shmid;          /* shmem ID returned from shmget() */
    key_t shmkey;       /* shmem key IPC_ANON or returned from ftok() */
#endif
};

#ifdef __VMS
#ifndef __NEW_STARLET
#define __NEW_STARLET
#define __NEW_STARLET_SET
#endif
#include <va_rangedef.h>
#ifdef  __NEW_STARLET_SET
#undef  __NEW_STARLET_SET
#undef  __NEW_STARLET
#endif

#include <types.h>

/*
** Define the shared memory locking directives
*/
#define SHM_M_NOLOCKS   0
#define SHM_M_LOCKING   1

/*
** Define the shared memory name constants
*/
#define SHM_NAME_MAX    43

/*
** Definitions to destinguish the memory lists
*/
#define SHM_M_ALLOC     0
#define SHM_M_AVAIL     1

/*
** Define the shared memory alignment factor
*/
#define SHM_C_ALIGN     8

/*
** Shared memory context structure.
*/
typedef struct _shm_ctx {
    VA_RANGE            ctx_va_range;   /* Shared memory range          */
    unsigned int        ctx_lockid;     /* Shared memory lockid         */
    unsigned int        ctx_lockcnt;    /* Shared memory lock count     */
    unsigned int        ctx_status;     /* Shared memory status         */
    } SHM_CTX;

/*
** Shared memory section header.  Make sure this structure is quadword aligned
*/
typedef struct _shm_hdr {
    unsigned int        shm_alloc;      /* Allocated segment list       */
    unsigned int        shm_avail;      /* Available segment list       */
    unsigned int        shm_common_ctx; /* Section common context       */
    unsigned int        shm_asize;      /* Section alloc size           */
    unsigned int        shm_bsize;      /* Section block size           */
    __union  {
        unsigned int    shm_flags;      /* Section block flags          */
        __struct  {
            unsigned    shm_locking : 1;/* Section locking flag         */
            unsigned    shm_galaxy : 1; /* Section is a Galaxy section  */
            unsigned    shm_filler : 30;
            } shm_flags_bits;
        } shm_flags_overlay;
    } SHM_HDR;

#if !defined(__VAXC)
#define shm_flags shm_flags_overlay.shm_flags
#define shm_locking shm_flags_overlay.shm_flags_bits.shm_locking
#define shm_galaxy shm_flags_overlay.shm_flags_bits.shm_galaxy
#endif

/*
** Shared memory segment header.  Make sure this structure is quadword aligned
*/
typedef struct _shm_seg {
    unsigned int        seg_flink;      /* Segment forward link         */
    unsigned int        seg_blink;      /* Segment backward link        */
    unsigned int        seg_asize;      /* Segment alloc size           */
    unsigned int        seg_bsize;      /* Segment block size           */
    } SHM_SEG;

/*
** Shared memory status block.
*/
typedef struct _shm_sts {
    unsigned int        sts_segct;      /* Total segment count          */
    unsigned int        sts_asize;      /* Total segment alloc size     */
    unsigned int        sts_bsize;      /* Total segment block size     */
    } SHM_STS;

/*
** Define the shared memory structure as Shared Memory Context structure
*/
#ifndef APR_SHM_H
typedef SHM_CTX apr_shm_t;
#endif

/*
** Define the global shared memory prototypes
*/
extern SHM_CTX *apr$shm_create (void *, size_t, int, ...);
extern SHM_CTX *apr$shm_attach (void *, int, ...);
extern void apr$shm_set_common_ctx (SHM_CTX *, void *);
extern void *apr$shm_get_common_ctx (SHM_CTX *);
extern int apr$shm_available (SHM_CTX *);
extern int apr$shm_allocated (SHM_CTX *);
extern void *apr$shm_malloc (SHM_CTX *, size_t);
extern void *apr$shm_calloc (SHM_CTX *, size_t, size_t);
extern void *apr$shm_realloc (SHM_CTX *, void *, size_t);
extern int apr$shm_sizeof (SHM_CTX *, void *);
extern void *apr$shm_pointer (SHM_CTX *, int);
extern int apr$shm_offset (SHM_CTX *, void *);
extern void *apr$shm_strdup (SHM_CTX *, void *);
extern void apr$shm_free (SHM_CTX *, void *);
extern void apr$shm_detach (SHM_CTX *);
extern void apr$shm_destroy (SHM_CTX *);

#endif

#endif /* SHM_H */
