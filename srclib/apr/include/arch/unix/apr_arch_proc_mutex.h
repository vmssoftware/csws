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

#ifndef PROC_MUTEX_H
#define PROC_MUTEX_H

#include "apr.h"
#include "apr_private.h"
#include "apr_general.h"
#include "apr_lib.h"
#include "apr_proc_mutex.h"
#include "apr_pools.h"
#include "apr_portable.h"
#include "apr_file_io.h"
#include "apr_arch_file_io.h"
#include "apr_time.h"

/* System headers required by Locks library */
#if APR_HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if APR_HAVE_STDIO_H
#include <stdio.h>
#endif
#if APR_HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_SYS_IPC_H
#include <sys/ipc.h>
#endif
#ifdef HAVE_SYS_SEM_H
#include <sys/sem.h>
#endif
#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#if APR_HAVE_STDLIB_H
#include <stdlib.h>
#endif
#if APR_HAVE_UNISTD_H
#include <unistd.h>
#endif
#if APR_HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif
#if APR_HAVE_PTHREAD_H
#include <pthread.h>
#endif
/* End System Headers */

struct apr_proc_mutex_unix_lock_methods_t {
    unsigned int flags;
    apr_status_t (*create)(apr_proc_mutex_t *, const char *);
    apr_status_t (*acquire)(apr_proc_mutex_t *);
    apr_status_t (*tryacquire)(apr_proc_mutex_t *);
    apr_status_t (*timedacquire)(apr_proc_mutex_t *, apr_interval_time_t);
    apr_status_t (*release)(apr_proc_mutex_t *);
    apr_status_t (*cleanup)(void *);
    apr_status_t (*child_init)(apr_proc_mutex_t **, apr_pool_t *, const char *);
    apr_status_t (*perms_set)(apr_proc_mutex_t *, apr_fileperms_t, apr_uid_t, apr_gid_t);
    apr_lockmech_e mech;
    const char *name;
};
typedef struct apr_proc_mutex_unix_lock_methods_t apr_proc_mutex_unix_lock_methods_t;

/* bit values for flags field in apr_unix_lock_methods_t */
#define APR_PROCESS_LOCK_MECH_IS_GLOBAL          1

#if !APR_HAVE_UNION_SEMUN && defined(APR_HAS_SYSVSEM_SERIALIZE)
union semun {
    int val;
    struct semid_ds *buf;
    unsigned short *array;
};
#endif

struct apr_proc_mutex_t {
    apr_pool_t *pool;
    const apr_proc_mutex_unix_lock_methods_t *meth;
    int curr_locked;
    char *fname;

    apr_os_proc_mutex_t os;     /* Native mutex holder. */

#if APR_HAS_FCNTL_SERIALIZE || APR_HAS_FLOCK_SERIALIZE
    apr_file_t *interproc;      /* For apr_file_ calls on native fd. */
    int interproc_closing;      /* whether the native fd is opened/closed with
                                 * 'interproc' or apr_os_file_put()ed (hence
                                 * needing an an explicit close for consistency
                                 * with other methods).
                                 */
#endif
#if APR_HAS_PROC_PTHREAD_SERIALIZE
    int pthread_refcounting;    /* Whether the native mutex is refcounted or
                                 * apr_os_proc_mutex_put()ed, which makes
                                 * refcounting impossible/undesirable.
                                 */
#endif
#ifdef __VMS
    unsigned int lockid;
#endif
};

void apr_proc_mutex_unix_setup_lock(void);

#endif  /* PROC_MUTEX_H */

