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

#include "apr_arch_threadproc.h"
#include "apr_strings.h"
#include "apr_portable.h"
#include "apr_signal.h"
#include "apr_random.h"

#ifdef __VMS
#ifndef __NEW_STARLET
#define __NEW_STARLET
#define __NEW_STARLET_SET
#endif
#include <lib$routines.h>
#include <starlet.h>
#include <iosbdef.h>
#include <iledef.h>
#include <descrip.h>
#include <unixlib.h>
#include <starlet.h>
#include <unixio.h>
#include <string.h>
#include <jpidef.h>
#include <lnmdef.h>
#include <efndef.h>
#include <stdarg.h>
#include <stdio.h>
#include <errno.h>
#include <ssdef.h>
#ifdef __NEW_STARLET_SET
#undef __NEW_STARLET_SET
#undef __NEW_STARLET
#endif

#include "ilemac.h"
#include "apr_arch_shm.h"
#include "protshr.h"

/*
** Define the active server data structure
*/
typedef struct _ActiveSvrData
    {
    int pid;
    int terminated;
    int terminated_status;
    } ACTIVE_SVR_DATA;

static ACTIVE_SVR_DATA *ActiveServerList = NULL;
static int ActiveServerSize = 0;

typedef struct _dcl_prc_data_ctx {
    char        *DclPrcHdl;
    int         DclPrcCtx;
    int         DclArgSiz;
    char        *DclArgBuf;
    SHM_CTX     *DclArgCtx;
    int         DclEnvSiz;
    char        *DclEnvBuf;
    SHM_CTX     *DclEnvCtx;
    } DCL_PRC_DATA_CTX;

static DCL_PRC_DATA_CTX *DclPrcDataCtx = NULL;
static int apr$$dcl_proc_ctx = 0;
static int DclPrcDataMax = 0;

typedef struct _dcl_shm_data_ctx {
    SHM_CTX     *shm_ctx;
    void        *shm_ptr;
    int         shm_len;
    } DCL_SHM_DATA_CTX;

/*
** Local routine prototypes
*/
static int apr$$dcl_exec_wrapper (const char *, const char * const *, const char * const *, apr_procattr_t *, apr_pool_t *);
static SHM_CTX *apr$$dcl_shm_create (char *, char **, int *);
static int apr$$dcl_shm_delete (SHM_CTX **);
static int apr$$dcl_sync_run (unsigned int);

/*
** Global routine prototypes
*/
int apr$dcl_pass_env (char *[], ...);
int apr$dcl_pass_arg (char *[], ...);
void *apr$dcl_shm_open (char *);
int apr$dcl_shm_read (void *, size_t, size_t, void *);
int apr$dcl_shm_rewind (void    *);
int apr$dcl_shm_close (void     *);
pid_t apr$asl_waitpid (pid_t process_id, int *status_location, int options);

/*
** External routine prototypes
*/
extern int decc$set_child_standard_streams (int, int, int);
extern int decc$$translate (int);

#define fork vfork
#endif          /* __VMS */


/* Heavy on no'ops, here's what we want to pass if there is APR_NO_FILE
 * requested for a specific child handle;
 */
static apr_file_t no_file = { NULL, -1, };

APR_DECLARE(apr_status_t) apr_procattr_create(apr_procattr_t **new,
#ifdef __VMS
                                              apr_pool_t *pool, ...)
#else
                                              apr_pool_t *pool)
#endif
{
#ifdef __VMS
    static int ProcHandleCtr = 0;
    char *PrcRequestType = NULL;
    int UseSocketPipes = FALSE;
    char ProcHandleBuf[32+1];
    va_list argp;
    int argc;

    va_start (argp, pool);
    va_count (argc);
    if (argc > 2)
        PrcRequestType = va_arg (argp, char *);
    if (argc > 3)
        UseSocketPipes = va_arg (argp, int);
    va_end (argp);

    (*new) = (apr_procattr_t *)apr_pcalloc(pool, sizeof(apr_procattr_t));
    if ((*new) == NULL)
        return APR_ENOMEM;

    (*new)->UseSocketPipes = UseSocketPipes;
    if (! PrcRequestType) {
        sprintf (ProcHandleBuf, "ProcHandle-%08X", ProcHandleCtr++);
        (*new)->PrcRequestType = apr_pstrdup (pool, ProcHandleBuf);
    }
    else
        (*new)->PrcRequestType = apr_pstrdup (pool, PrcRequestType);

#else
    (*new) = (apr_procattr_t *)apr_pcalloc(pool, sizeof(apr_procattr_t));

    if ((*new) == NULL) {
        return APR_ENOMEM;
    }
#endif
    (*new)->pool = pool;
    (*new)->cmdtype = APR_PROGRAM;
    (*new)->uid = (*new)->gid = -1;
    return APR_SUCCESS;
}

APR_DECLARE(apr_status_t) apr_procattr_io_set(apr_procattr_t *attr,
                                              apr_int32_t in,
                                              apr_int32_t out,
                                              apr_int32_t err)
{
#ifdef __VMS
    int on = 1;
#endif
    apr_status_t rv;

    if ((in != APR_NO_PIPE) && (in != APR_NO_FILE)) {
        /* APR_CHILD_BLOCK maps to APR_WRITE_BLOCK, while
         * APR_PARENT_BLOCK maps to APR_READ_BLOCK, so transpose
         * the CHILD/PARENT blocking flags for the stdin pipe.
         * stdout/stderr map to the correct mode by default.
         */
        if (in == APR_CHILD_BLOCK)
            in = APR_READ_BLOCK;
        else if (in == APR_PARENT_BLOCK)
            in = APR_WRITE_BLOCK;

#ifdef __VMS
        if ((rv = apr_file_pipe_create_ex(&attr->child_in, &attr->parent_in,
                                          in, attr->pool, attr->UseSocketPipes)) == APR_SUCCESS)
#else
        if ((rv = apr_file_pipe_create_ex(&attr->child_in, &attr->parent_in,
                                          in, attr->pool)) == APR_SUCCESS)
#endif
            rv = apr_file_inherit_unset(attr->parent_in);
        if (rv != APR_SUCCESS)
            return rv;
    }
    else if (in == APR_NO_FILE)
        attr->child_in = &no_file;

    if ((out != APR_NO_PIPE) && (out != APR_NO_FILE)) {
#ifdef __VMS
        if ((rv = apr_file_pipe_create_ex(&attr->parent_out, &attr->child_out,
                                          out, attr->pool, attr->UseSocketPipes)) == APR_SUCCESS)
#else
        if ((rv = apr_file_pipe_create_ex(&attr->parent_out, &attr->child_out,
                                          out, attr->pool)) == APR_SUCCESS)
#endif
            rv = apr_file_inherit_unset(attr->parent_out);
        if (rv != APR_SUCCESS)
            return rv;
#ifdef __VMS
        /*
        ** Turn on the CCL bit for output socket pipes
        */
        if (attr->UseSocketPipes)
            {
            rv = apr$$setsockopt (vaxc$get_sdc (attr->parent_out->filedes), SET_SOCK_DEV_CCL, &on, sizeof (on));
            if (! (rv & 1))
                {
                decc$$translate (rv);
                return (errno);
                }
            rv = apr$$setsockopt (vaxc$get_sdc (attr->child_out->filedes), SET_SOCK_DEV_CCL, &on, sizeof (on));
            if (! (rv & 1))
                {
                decc$$translate (rv);
                return (errno);
                }
            }
#endif
    }
    else if (out == APR_NO_FILE)
        attr->child_out = &no_file;

    if ((err != APR_NO_PIPE) && (err != APR_NO_FILE)) {
#ifdef __VMS
        if ((rv = apr_file_pipe_create_ex(&attr->parent_err, &attr->child_err,
                                          err, attr->pool, attr->UseSocketPipes)) == APR_SUCCESS)
#else
        if ((rv = apr_file_pipe_create_ex(&attr->parent_err, &attr->child_err,
                                          err, attr->pool)) == APR_SUCCESS)
#endif
            rv = apr_file_inherit_unset(attr->parent_err);
        if (rv != APR_SUCCESS)
            return rv;
#ifdef __VMS
        /*
        ** Turn on the CCL bit for error socket pipes
        */
        if (attr->UseSocketPipes)
            {
            rv = apr$$setsockopt (vaxc$get_sdc (attr->parent_err->filedes), SET_SOCK_DEV_CCL, &on, sizeof (on));
            if (! (rv & 1))
                {
                decc$$translate (rv);
                return (errno);
                }
            rv = apr$$setsockopt (vaxc$get_sdc (attr->child_err->filedes), SET_SOCK_DEV_CCL, &on, sizeof (on));
            if (! (rv & 1))
                {
                decc$$translate (rv);
                return (errno);
                }
            }
#endif
    }
    else if (err == APR_NO_FILE)
        attr->child_err = &no_file;

    return APR_SUCCESS;
}


APR_DECLARE(apr_status_t) apr_procattr_child_in_set(apr_procattr_t *attr,
                                                    apr_file_t *child_in,
                                                    apr_file_t *parent_in)
{
    apr_status_t rv = APR_SUCCESS;

    if (attr->child_in == NULL && attr->parent_in == NULL
           && child_in == NULL && parent_in == NULL)
#ifdef __VMS
        if ((rv = apr_file_pipe_create(&attr->child_in, &attr->parent_in,
                                       attr->pool, attr->UseSocketPipes)) == APR_SUCCESS)
#else
        if ((rv = apr_file_pipe_create(&attr->child_in, &attr->parent_in,
                                       attr->pool)) == APR_SUCCESS)
#endif
            rv = apr_file_inherit_unset(attr->parent_in);

    if (child_in != NULL && rv == APR_SUCCESS) {
        if (attr->child_in && (attr->child_in->filedes != -1))
            rv = apr_file_dup2(attr->child_in, child_in, attr->pool);
        else {
            attr->child_in = NULL;
            if ((rv = apr_file_dup(&attr->child_in, child_in, attr->pool))
                    == APR_SUCCESS)
                rv = apr_file_inherit_set(attr->child_in);
        }
    }

    if (parent_in != NULL && rv == APR_SUCCESS) {
        if (attr->parent_in)
            rv = apr_file_dup2(attr->parent_in, parent_in, attr->pool);
        else
            rv = apr_file_dup(&attr->parent_in, parent_in, attr->pool);
    }

    return rv;
}


APR_DECLARE(apr_status_t) apr_procattr_child_out_set(apr_procattr_t *attr,
                                                     apr_file_t *child_out,
                                                     apr_file_t *parent_out)
{
#ifdef __VMS
    int status, on = 1;
#endif
    apr_status_t rv = APR_SUCCESS;

    if (attr->child_out == NULL && attr->parent_out == NULL
           && child_out == NULL && parent_out == NULL)
#ifdef __VMS
        if ((rv = apr_file_pipe_create(&attr->parent_out, &attr->child_out,
                                       attr->pool, attr->UseSocketPipes)) == APR_SUCCESS)
#else
        if ((rv = apr_file_pipe_create(&attr->parent_out, &attr->child_out,
                                       attr->pool)) == APR_SUCCESS)
#endif
            rv = apr_file_inherit_unset(attr->parent_out);

    if (child_out != NULL && rv == APR_SUCCESS) {
#ifdef __VMS
        /*
        ** Turn on the CCL bit for child output socket pipe
        */
        if (attr->UseSocketPipes)
            {
            status = apr$$setsockopt (vaxc$get_sdc (attr->child_out->filedes), SET_SOCK_DEV_CCL, &on, sizeof (on));
            if (! (status & 1))
                {
                decc$$translate (status);
                return (errno);
                }
            }
#else
        if (attr->child_out && (attr->child_out->filedes != -1))
            rv = apr_file_dup2(attr->child_out, child_out, attr->pool);
        else {
            attr->child_out = NULL;
            if ((rv = apr_file_dup(&attr->child_out, child_out, attr->pool))
                    == APR_SUCCESS)
                rv = apr_file_inherit_set(attr->child_out);
        }
#endif
    }

    if (parent_out != NULL && rv == APR_SUCCESS) {
#ifdef __VMS
        /*
        ** Turn on the CCL bit for parent output socket pipe
        */
        if (attr->UseSocketPipes)
            {
            status = apr$$setsockopt (vaxc$get_sdc (attr->parent_out->filedes), SET_SOCK_DEV_CCL, &on, sizeof (on));
            if (! (status & 1))
                {
                decc$$translate (status);
                return (errno);
                }
            }
#else
        if (attr->parent_out)
            rv = apr_file_dup2(attr->parent_out, parent_out, attr->pool);
        else
            rv = apr_file_dup(&attr->parent_out, parent_out, attr->pool);
#endif
    }

    return rv;
}


APR_DECLARE(apr_status_t) apr_procattr_child_err_set(apr_procattr_t *attr,
                                                     apr_file_t *child_err,
                                                     apr_file_t *parent_err)
{
#ifdef __VMS
    int status, on = 1;
#endif
    apr_status_t rv = APR_SUCCESS;

    if (attr->child_err == NULL && attr->parent_err == NULL
           && child_err == NULL && parent_err == NULL)
#ifdef __VMS
        if ((rv = apr_file_pipe_create(&attr->parent_err, &attr->child_err,
                                      attr->pool, attr->UseSocketPipes)) == APR_SUCCESS)
#else
        if ((rv = apr_file_pipe_create(&attr->parent_err, &attr->child_err,
                                      attr->pool)) == APR_SUCCESS)
#endif
            rv = apr_file_inherit_unset(attr->parent_err);

    if (child_err != NULL && rv == APR_SUCCESS) {
#ifdef __VMS
        /*
        ** Turn on the CCL bit for child error socket pipe
        */
        if (attr->UseSocketPipes)
            {
            status = apr$$setsockopt (vaxc$get_sdc (attr->child_err->filedes), SET_SOCK_DEV_CCL, &on, sizeof (on));
            if (! (status & 1))
                {
                decc$$translate (status);
                return (errno);
                }
            }
#else
        if (attr->child_err && (attr->child_err->filedes != -1))
            rv = apr_file_dup2(attr->child_err, child_err, attr->pool);
        else {
            attr->child_err = NULL;
            if ((rv = apr_file_dup(&attr->child_err, child_err, attr->pool))
                    == APR_SUCCESS)
                rv = apr_file_inherit_set(attr->child_err);
        }
#endif
    }
    if (parent_err != NULL && rv == APR_SUCCESS) {
#ifdef __VMS
        /*
        ** Turn on the CCL bit for parent error socket pipe
        */
        if (attr->UseSocketPipes)
            {
            status = apr$$setsockopt (vaxc$get_sdc (attr->parent_err->filedes), SET_SOCK_DEV_CCL, &on, sizeof (on));
            if (! (status & 1))
                {
                decc$$translate (status);
                return (errno);
                }
            }
#else
        if (attr->parent_err)
            rv = apr_file_dup2(attr->parent_err, parent_err, attr->pool);
        else
            rv = apr_file_dup(&attr->parent_err, parent_err, attr->pool);
#endif
    }

    return rv;
}


APR_DECLARE(apr_status_t) apr_procattr_dir_set(apr_procattr_t *attr,
                                               const char *dir)
{
    attr->currdir = apr_pstrdup(attr->pool, dir);
    if (attr->currdir) {
        return APR_SUCCESS;
    }

    return APR_ENOMEM;
}

APR_DECLARE(apr_status_t) apr_procattr_cmdtype_set(apr_procattr_t *attr,
                                                   apr_cmdtype_e cmd)
{
    attr->cmdtype = cmd;
    return APR_SUCCESS;
}

APR_DECLARE(apr_status_t) apr_procattr_detach_set(apr_procattr_t *attr,
                                                  apr_int32_t detach)
{
    attr->detached = detach;
    return APR_SUCCESS;
}

APR_DECLARE(apr_status_t) apr_proc_fork(apr_proc_t *proc, apr_pool_t *pool)
{
    int pid;

    memset(proc, 0, sizeof(apr_proc_t));

    if ((pid = fork()) < 0) {
        return errno;
    }
    else if (pid == 0) {
        proc->pid = getpid();

        apr_random_after_fork(proc);

        return APR_INCHILD;
    }

    proc->pid = pid;

    return APR_INPARENT;
}

static apr_status_t limit_proc(apr_procattr_t *attr)
{
#if APR_HAVE_STRUCT_RLIMIT && APR_HAVE_SETRLIMIT
#ifdef RLIMIT_CPU
    if (attr->limit_cpu != NULL) {
        if ((setrlimit(RLIMIT_CPU, attr->limit_cpu)) != 0) {
            return errno;
        }
    }
#endif
#ifdef RLIMIT_NPROC
    if (attr->limit_nproc != NULL) {
        if ((setrlimit(RLIMIT_NPROC, attr->limit_nproc)) != 0) {
            return errno;
        }
    }
#endif
#ifdef RLIMIT_NOFILE
    if (attr->limit_nofile != NULL) {
        if ((setrlimit(RLIMIT_NOFILE, attr->limit_nofile)) != 0) {
            return errno;
        }
    }
#endif
#if defined(RLIMIT_AS)
    if (attr->limit_mem != NULL) {
        if ((setrlimit(RLIMIT_AS, attr->limit_mem)) != 0) {
            return errno;
        }
    }
#elif defined(RLIMIT_DATA)
    if (attr->limit_mem != NULL) {
        if ((setrlimit(RLIMIT_DATA, attr->limit_mem)) != 0) {
            return errno;
        }
    }
#elif defined(RLIMIT_VMEM)
    if (attr->limit_mem != NULL) {
        if ((setrlimit(RLIMIT_VMEM, attr->limit_mem)) != 0) {
            return errno;
        }
    }
#endif
#else
    /*
     * Maybe make a note in error_log that setrlimit isn't supported??
     */

#endif
    return APR_SUCCESS;
}

APR_DECLARE(apr_status_t) apr_procattr_child_errfn_set(apr_procattr_t *attr,
                                                       apr_child_errfn_t *errfn)
{
    attr->errfn = errfn;
    return APR_SUCCESS;
}

APR_DECLARE(apr_status_t) apr_procattr_error_check_set(apr_procattr_t *attr,
                                                       apr_int32_t chk)
{
    attr->errchk = chk;
    return APR_SUCCESS;
}

APR_DECLARE(apr_status_t) apr_procattr_addrspace_set(apr_procattr_t *attr,
                                                       apr_int32_t addrspace)
{
    /* won't ever be used on this platform, so don't save the flag */
    return APR_SUCCESS;
}

APR_DECLARE(apr_status_t) apr_procattr_user_set(apr_procattr_t *attr,
                                                const char *username,
                                                const char *password)
{
    apr_status_t rv;
    apr_gid_t    gid;

    if ((rv = apr_uid_get(&attr->uid, &gid, username,
                          attr->pool)) != APR_SUCCESS) {
        attr->uid = -1;
        return rv;
    }

    /* Use default user group if not already set */
    if (attr->gid == -1) {
        attr->gid = gid;
    }
    return APR_SUCCESS;
}

APR_DECLARE(apr_status_t) apr_procattr_group_set(apr_procattr_t *attr,
                                                 const char *groupname)
{
    apr_status_t rv;

    if ((rv = apr_gid_get(&attr->gid, groupname, attr->pool)) != APR_SUCCESS)
        attr->gid = -1;
    return rv;
}

APR_DECLARE(apr_status_t) apr_proc_create(apr_proc_t *new,
                                          const char *progname,
                                          const char * const *args,
                                          const char * const *env,
                                          apr_procattr_t *attr,
                                          apr_pool_t *pool)
{
#ifdef __VMS
    char *DefPath;
    int NullDev = 0;
#endif
    int i;
    const char * const empty_envp[] = {NULL};

    if (!env) { /* Specs require an empty array instead of NULL;
                 * Purify will trigger a failure, even if many
                 * implementations don't.
                 */
        env = empty_envp;
    }

    new->in = attr->parent_in;
    new->err = attr->parent_err;
    new->out = attr->parent_out;

    if (attr->errchk) {
        if (attr->currdir) {
            if (access(attr->currdir, X_OK) == -1) {
                /* chdir() in child wouldn't have worked */
                return errno;
            }
        }

        if (attr->cmdtype == APR_PROGRAM ||
            attr->cmdtype == APR_PROGRAM_ENV ||
            *progname == '/') {
            /* for both of these values of cmdtype, caller must pass
             * full path, so it is easy to check;
             * caller can choose to pass full path for other
             * values of cmdtype
             */
            if (access(progname, X_OK) == -1) {
                /* exec*() in child wouldn't have worked */
                return errno;
            }
        }
        else {
            /* todo: search PATH for progname then try to access it */
        }
    }

#ifdef __VMS
    /*
    ** If we haven't already created the null device descriptor, do it now
    */
    if (! attr->child_in || ! attr->child_out || ! attr->child_err)
        NullDev = open ("NLA0:", O_RDWR);

    /*
    ** For VMS, we need to get the currect directory
    */
    apr_filepath_get (&DefPath, 0, pool);
#endif

    if ((new->pid = fork()) < 0) {
        return errno;
    }
    else if (new->pid == 0) {
        /* child process */
#ifdef __VMS
        /*
        ** Don't let the child process inherit the parent's side of the pipes
        */
        if (attr->parent_in)
            fcntl (attr->parent_in->filedes, F_SETFD, FD_CLOEXEC);
        if (attr->parent_out)
            fcntl (attr->parent_out->filedes, F_SETFD, FD_CLOEXEC);
        if (attr->parent_err)
            fcntl (attr->parent_err->filedes, F_SETFD, FD_CLOEXEC);

        /*
        ** Set the child standard streams
        */
        decc$set_child_standard_streams (attr->child_in  ? attr->child_in->filedes  : NullDev,
                                         attr->child_out ? attr->child_out->filedes : NullDev,
                                         attr->child_err ? attr->child_err->filedes : NullDev);
#else
        /*
         * If we do exec cleanup before the dup2() calls to set up pipes
         * on 0-2, we accidentally close the pipes used by programs like
         * mod_cgid.
         *
         * If we do exec cleanup after the dup2() calls, cleanup can accidentally
         * close our pipes which replaced any files which previously had
         * descriptors 0-2.
         *
         * The solution is to kill the cleanup for the pipes, then do
         * exec cleanup, then do the dup2() calls.
         */

        if (attr->child_in) {
            apr_pool_cleanup_kill(apr_file_pool_get(attr->child_in),
                                  attr->child_in, apr_unix_file_cleanup);
        }

        if (attr->child_out) {
            apr_pool_cleanup_kill(apr_file_pool_get(attr->child_out),
                                  attr->child_out, apr_unix_file_cleanup);
        }

        if (attr->child_err) {
            apr_pool_cleanup_kill(apr_file_pool_get(attr->child_err),
                                  attr->child_err, apr_unix_file_cleanup);
        }

        apr_pool_cleanup_for_exec();

        if ((attr->child_in) && (attr->child_in->filedes == -1)) {
            close(STDIN_FILENO);
        }
        else if (attr->child_in &&
                 attr->child_in->filedes != STDIN_FILENO) {
            dup2(attr->child_in->filedes, STDIN_FILENO);
            apr_file_close(attr->child_in);
        }

        if ((attr->child_out) && (attr->child_out->filedes == -1)) {
            close(STDOUT_FILENO);
        }
        else if (attr->child_out &&
                 attr->child_out->filedes != STDOUT_FILENO) {
            dup2(attr->child_out->filedes, STDOUT_FILENO);
            apr_file_close(attr->child_out);
        }

        if ((attr->child_err) && (attr->child_err->filedes == -1)) {
            close(STDERR_FILENO);
        }
        else if (attr->child_err &&
                 attr->child_err->filedes != STDERR_FILENO) {
            dup2(attr->child_err->filedes, STDERR_FILENO);
            apr_file_close(attr->child_err);
        }

        apr_signal(SIGCHLD, SIG_DFL); /* not sure if this is needed or not */
#endif

        if (attr->currdir != NULL) {
            if (chdir(attr->currdir) == -1) {
                if (attr->errfn) {
                    attr->errfn(pool, errno, "change of working directory failed");
                }
                _exit(-1);   /* We have big problems, the child should exit. */
            }
        }

        if (!geteuid()) {
            apr_procattr_pscb_t *c = attr->perms_set_callbacks;

            while (c) {
                apr_status_t r;
                r = (*c->perms_set_fn)((void *)c->data, c->perms,
                                       attr->uid, attr->gid);
                if (r != APR_SUCCESS && r != APR_ENOTIMPL) {
                    _exit(-1);
                }
                c = c->next;
            }
        }
        /* Only try to switch if we are running as root */
        if (attr->gid != -1 && !geteuid()) {
            if (setgid(attr->gid)) {
                if (attr->errfn) {
                    attr->errfn(pool, errno, "setting of group failed");
                }
                _exit(-1);   /* We have big problems, the child should exit. */
            }
        }

        if (attr->uid != -1 && !geteuid()) {
            if (setuid(attr->uid)) {
                if (attr->errfn) {
                    attr->errfn(pool, errno, "setting of user failed");
                }
                _exit(-1);   /* We have big problems, the child should exit. */
            }
        }

        if (limit_proc(attr) != APR_SUCCESS) {
            if (attr->errfn) {
                attr->errfn(pool, errno, "setting of resource limits failed");
            }
            _exit(-1);   /* We have big problems, the child should exit. */
        }

#ifdef __VMS
        /*
        ** Call the exec wrapper routine
        */
        apr$$dcl_exec_wrapper (progname, args, env, attr, pool);
#else
        if (attr->cmdtype == APR_SHELLCMD ||
            attr->cmdtype == APR_SHELLCMD_ENV) {
            int onearg_len = 0;
            const char *newargs[4];

            newargs[0] = SHELL_PATH;
            newargs[1] = "-c";

            i = 0;
            while (args[i]) {
                onearg_len += strlen(args[i]);
                onearg_len++; /* for space delimiter */
                i++;
            }

            switch(i) {
            case 0:
                /* bad parameters; we're doomed */
                break;
            case 1:
                /* no args, or caller already built a single string from
                 * progname and args
                 */
                newargs[2] = args[0];
                break;
            default:
            {
                char *ch, *onearg;

                ch = onearg = apr_palloc(pool, onearg_len);
                i = 0;
                while (args[i]) {
                    size_t len = strlen(args[i]);

                    memcpy(ch, args[i], len);
                    ch += len;
                    *ch = ' ';
                    ++ch;
                    ++i;
                }
                --ch; /* back up to trailing blank */
                *ch = '\0';
                newargs[2] = onearg;
            }
            }

            newargs[3] = NULL;

            if (attr->detached) {
                apr_proc_detach(APR_PROC_DETACH_DAEMONIZE);
            }

            if (attr->cmdtype == APR_SHELLCMD) {
                execve(SHELL_PATH, (char * const *) newargs, (char * const *)env);
            }
            else {
                execv(SHELL_PATH, (char * const *)newargs);
            }
        }
        else if (attr->cmdtype == APR_PROGRAM) {
            if (attr->detached) {
                apr_proc_detach(APR_PROC_DETACH_DAEMONIZE);
            }

            execve(progname, (char * const *)args, (char * const *)env);
        }
        else if (attr->cmdtype == APR_PROGRAM_ENV) {
            if (attr->detached) {
                apr_proc_detach(APR_PROC_DETACH_DAEMONIZE);
            }

            execv(progname, (char * const *)args);
        }
        else {
            /* APR_PROGRAM_PATH */
            if (attr->detached) {
                apr_proc_detach(APR_PROC_DETACH_DAEMONIZE);
            }

            execvp(progname, (char * const *)args);
        }
        if (attr->errfn) {
            char *desc;

            desc = apr_psprintf(pool, "exec of '%s' failed",
                                progname);
            attr->errfn(pool, errno, desc);
        }
#endif
        _exit(-1);  /* if we get here, there is a problem, so exit with an
                     * error code. */
    }

    /* Parent process */
#ifdef __VMS
    /*
    ** For VMS, we need to reset out previous directory
    */
    apr_filepath_set (DefPath, pool);

    /*
    ** Let's wait for the process to signal it's ready before proceeding
    */
    apr$$dcl_sync_run (new->pid);

    /*
    ** Close the null device
    */
    if (NullDev)
        close (NullDev);
#endif

    if (attr->child_in && (attr->child_in->filedes != -1)) {
        apr_file_close(attr->child_in);
    }

    if (attr->child_out && (attr->child_out->filedes != -1)) {
        apr_file_close(attr->child_out);
    }

    if (attr->child_err && (attr->child_err->filedes != -1)) {
        apr_file_close(attr->child_err);
    }

    return APR_SUCCESS;
}

APR_DECLARE(apr_status_t) apr_proc_wait_all_procs(apr_proc_t *proc,
                                                  int *exitcode,
                                                  apr_exit_why_e *exitwhy,
                                                  apr_wait_how_e waithow,
                                                  apr_pool_t *p)
{
    proc->pid = -1;
    return apr_proc_wait(proc, exitcode, exitwhy, waithow);
}

APR_DECLARE(apr_status_t) apr_proc_wait(apr_proc_t *proc,
                                        int *exitcode, apr_exit_why_e *exitwhy,
                                        apr_wait_how_e waithow)
{
    pid_t pstatus;
    int waitpid_options = WUNTRACED;
    int exit_int;
    int ignore;
    apr_exit_why_e ignorewhy;

    if (exitcode == NULL) {
        exitcode = &ignore;
    }

    if (exitwhy == NULL) {
        exitwhy = &ignorewhy;
    }

    if (waithow != APR_WAIT) {
        waitpid_options |= WNOHANG;
    }

    do {
#ifdef __VMS
        pstatus = apr$asl_waitpid (proc->pid, &exit_int, WNOHANG);
        if (pstatus <= 0)
#endif
        pstatus = waitpid(proc->pid, &exit_int, waitpid_options);
    } while (pstatus < 0 && errno == EINTR);

    if (pstatus > 0) {
#ifdef __VMS
asl_wait_return:
#endif
        proc->pid = pstatus;

        if (WIFEXITED(exit_int)) {
            *exitwhy = APR_PROC_EXIT;
            *exitcode = WEXITSTATUS(exit_int);
        }
        else if (WIFSIGNALED(exit_int)) {
            *exitwhy = APR_PROC_SIGNAL;

#ifdef WCOREDUMP
            if (WCOREDUMP(exit_int)) {
                *exitwhy |= APR_PROC_SIGNAL_CORE;
            }
#endif

            *exitcode = WTERMSIG(exit_int);
        }
        else {
            /* unexpected condition */
            return APR_EGENERAL;
        }

        return APR_CHILD_DONE;
    }
    else if (pstatus == 0) {
#ifdef __VMS
        if ((pstatus = apr$asl_waitpid (proc->pid, &exit_int, WNOHANG)) > 0)
            goto asl_wait_return;
        else
#endif
        return APR_CHILD_NOTDONE;
    }

    return errno;
}

#if APR_HAVE_STRUCT_RLIMIT
APR_DECLARE(apr_status_t) apr_procattr_limit_set(apr_procattr_t *attr,
                                                 apr_int32_t what,
                                                 struct rlimit *limit)
{
    switch(what) {
        case APR_LIMIT_CPU:
#ifdef RLIMIT_CPU
            attr->limit_cpu = limit;
            break;
#else
            return APR_ENOTIMPL;
#endif

        case APR_LIMIT_MEM:
#if defined(RLIMIT_DATA) || defined(RLIMIT_VMEM) || defined(RLIMIT_AS)
            attr->limit_mem = limit;
            break;
#else
            return APR_ENOTIMPL;
#endif

        case APR_LIMIT_NPROC:
#ifdef RLIMIT_NPROC
            attr->limit_nproc = limit;
            break;
#else
            return APR_ENOTIMPL;
#endif

        case APR_LIMIT_NOFILE:
#ifdef RLIMIT_NOFILE
            attr->limit_nofile = limit;
            break;
#else
            return APR_ENOTIMPL;
#endif

    }

    return APR_SUCCESS;
}
#endif /* APR_HAVE_STRUCT_RLIMIT */

APR_DECLARE(apr_status_t) apr_procattr_perms_set_register(apr_procattr_t *attr,
                                                 apr_perms_setfn_t *perms_set_fn,
                                                 void *data,
                                                 apr_fileperms_t perms)
{
    apr_procattr_pscb_t *c;

    c = apr_palloc(attr->pool, sizeof(apr_procattr_pscb_t));
    c->data = data;
    c->perms = perms;
    c->perms_set_fn = perms_set_fn;
    c->next = attr->perms_set_callbacks;
    attr->perms_set_callbacks = c;

    return APR_SUCCESS;
}


#ifdef __VMS
/******************************************************************************/
/*** apr$$dcl_exec_wrapper routine					    ***/
/******************************************************************************/
static int apr$$dcl_exec_wrapper (
    const char *progname,
    const char * const *arg,
    const char * const *env,
    apr_procattr_t *attr,
    apr_pool_t *pool
    )
{
static char *exec_wrapper = "APACHE$ROOT:[000000]APACHE$DCL.COM";
static char *shell_var = "APACHE$DCL_CMD";
char **arg_array = NULL;
char **env_array = NULL;
char *shell_cmd = NULL;
char dcl_proc_ctx_buf[32];
int dcl_proc_ctx = -1;
int argc = 0,
    envc = 0,
    i;

/*
** Count the argument & environment parameters
*/
while (arg && arg[argc]) {
    argc++;
}

while (env && env[envc]) {
    envc++;
}

/*
** Set arg_array to be the passed arg structure.
*/
arg_array = (char **) arg;

/*
** Set env_array to be the passed env structure.
*/
if (attr->cmdtype == APR_SHELLCMD)
    {
    /*
    ** Allocate a new environment array to be the passed
    */
    env_array = malloc ((envc + 2) * sizeof (char *));
    memset (env_array, 0, (envc + 2) * sizeof (char *));

    /*
    ** Create the shell command
    */
    shell_cmd = malloc (strlen (shell_var) + strlen (progname) + 2);
    sprintf (shell_cmd, "%s=%s", shell_var, progname);

    /*
    ** Add the shell command to the beginning of the new environment array
    */
    env_array[0] = shell_cmd;
    for (i = 0; i < envc; i++)
	env_array[i + 1] = (char *) env[i];

    }
else
    env_array = (char **) env;

/*
** Write the argument & environment data to be passed.
*/
apr$dcl_pass_arg (arg_array, &dcl_proc_ctx, attr->PrcRequestType);
apr$dcl_pass_env (env_array, &dcl_proc_ctx, attr->PrcRequestType);

/*
** Generate a process context to pass to the exec wrapper
*/
sprintf (dcl_proc_ctx_buf, "%04X", dcl_proc_ctx);

/*
** Free any allocated memory
*/
if (arg_array != (char **) arg)
    free (arg_array);
if (env_array != (char **) env)
    free (env_array);
if (shell_cmd)
    free (shell_cmd);

/*
** Execute the exec wrapper
*/
execl (exec_wrapper, attr->PrcRequestType, dcl_proc_ctx_buf, NULL);

/*
** Return
*/
return (SS$_NORMAL);

}

/*
**
**  apr$$dcl_sync_run - Synchronize with spawned child process
**
**  Functional Description:
**
**      This routine synchronizes with a spawned child process which is
**	running the APACHE$DCL.COM procedure.  It will wait until the
**	child process has either created the synchronizing logical name
**	or has terminated.
**
**  Usage:
**
**      apr$$dcl_sync_run pid
**
**  Formal parameters:
**
**      pid 		- (IN) pid value of the spawned child process.
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      None
**
**  Side Effects:
**
**      None
**
*/
static int
apr$$dcl_sync_run (
    unsigned int	pid
    )
{
struct dsc$descriptor SyncLnmDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
struct dsc$descriptor SyncTblDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
unsigned int SyncLnmAttr = LNM$M_CASE_BLIND;
char SyncLnmBuff[LNM$C_NAMLENGTH];
char *SyncTblBuff = {"LNM$JOB"};
ILE3 JpiItems[2],
     *Ile3Ptr;
int CpuTime,
    status;
IOSB iosb;

int usec; 					/* BRC 03-Feb-2018 (BZ 1125) */

/*
** Setup the synchronizing logical name table descriptor
*/
SyncTblDesc.dsc$w_length = strlen (SyncTblBuff);
SyncTblDesc.dsc$a_pointer = SyncTblBuff;

/*
** Setup the synchronizing logical name value descriptor
*/
sprintf (SyncLnmBuff, "APACHE$DCL_SYNC_%08X", pid);
SyncLnmDesc.dsc$w_length = strlen (SyncLnmBuff);
SyncLnmDesc.dsc$a_pointer = SyncLnmBuff;

/*
** Setup the JPI item list
*/
ILE3_INIT (JpiItems);
ILE3_ADD  (JPI$_CPUTIM, sizeof (CpuTime), &CpuTime, 0);
ILE3_TERM;

/*
** Loop here until the child creates the synchronizing logical name or until
** the child process has exited.
*/
usec = 10000; 					/* BRC 03-Feb-2018 (BZ 1125) */

while (TRUE)
    {
    /*
    ** Check to see if the child process has created the synchronizing logical
    ** name; If it has, then exit the loop.
    */
    status = SYS$TRNLNM (&SyncLnmAttr,
			 &SyncTblDesc,
			 &SyncLnmDesc,
			 0, 0);
    if (status & 1)
	break;

    /*
    ** Check to see if the child process has terminated; If it has, then exit the loop.
    */
    status = sys$getjpiw (EFN$C_ENF,
			  &pid,
			  0,
			  &JpiItems,
			  &iosb,
			  0, 0);
    if (status & 1)
	status = iosb.iosb$w_status;
    if (! (status & 1))
	break;

     /* Sleep for the specified delay period; BRC 03-Feb-2018 (BZ 1125) */
     apr_sleep(apr_time_make(0, usec));

     /*
     ** Increase the delay by 10 ms for each iteration of the loop
     ** until we reach 1000ms. After that keep a constant delay period
     ** of 1000ms.
     */
     if (usec < 1000000)
         usec+=10000;

    }

/*
** Delete the synchronizing logical  name
*/
status = SYS$DELLNM (&SyncTblDesc,
	    	     &SyncLnmDesc,
		     0);
if (! (status & 1) && status != SS$_NOLOGNAM)
    {
    fprintf (stderr, "Error deleting logical name '%s' (%08X)\n", SyncLnmBuff, status);
    fsync (fileno (stderr));
    }

/*
** Translate the status
*/
decc$$translate (SS$_NORMAL);

/*
** Return status
*/
return (SS$_NORMAL);

}

/*
**
**  apr$dcl_pass_env - Pass environment data to a spawned child process
**
**  Functional Description:
**
**      This routine creates a file of environment variables to be read
**	by a spawned child process as of yet not created.
**
**  Usage:
**
**      apr$dcl_pass_run env [dcl_proc_ctx] [dcl_proc_hdl]
**
**  Formal parameters:
**
**      env 		- (IN) address of environment variable array
**      dcl_proc_ctx	- (OUT) address of environment context integer (OPTIONAL)
**      dcl_proc_hdl	- (IN) address of environment context handle string (OPTIONAL)
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      0		- Success
**
**  Side Effects:
**
**      None
**
*/
int
apr$dcl_pass_env (
    char	*env[],
    ...
    )
{
char *dcl_proc_hdl = NULL;
int *dcl_proc_ctx = NULL;
SHM_CTX *env_ctx = NULL;
char *env_ptr = NULL;
int DclPrcIdx = -1;
char env_fn[256];
FILE *env_fd;
va_list argp;
int env_len,
    argc,i;

/*
** Process the optional parameters
*/
va_start (argp, env);
va_count (argc);
if (argc > 1)
    dcl_proc_ctx = va_arg (argp, int *);
if (argc > 2)
    dcl_proc_hdl = va_arg (argp, char *);
va_end (argp);

/*
** If the caller has passed a context pointer and a handle string, then let's
** try to locate it.  If we can't then we'll store it and pass back the next
** context.
*/
if (dcl_proc_ctx && dcl_proc_hdl)
    {
    int DclPrcHdlFnd = 0;

    /*
    ** Let's see if we need to allocate the environment shared memory list
    */
    if (DclPrcDataMax == 0)
	{
	DclPrcDataMax++;
	apr$$dcl_proc_ctx++;
	DclPrcDataCtx = malloc (sizeof (DCL_PRC_DATA_CTX) * DclPrcDataMax);
	DclPrcDataCtx[0].DclPrcHdl = strdup (dcl_proc_hdl);
	DclPrcDataCtx[0].DclPrcCtx = apr$$dcl_proc_ctx;
	DclPrcDataCtx[0].DclArgSiz = 0;
	DclPrcDataCtx[0].DclArgBuf = NULL;
	DclPrcDataCtx[0].DclArgCtx = NULL;
	DclPrcDataCtx[0].DclEnvSiz = 0;
	DclPrcDataCtx[0].DclEnvBuf = NULL;
	DclPrcDataCtx[0].DclEnvCtx = NULL;
	}

    /*
    ** Look throught the shared memory environment list for the process handle
    */
    for (DclPrcIdx = 0; DclPrcIdx < DclPrcDataMax; DclPrcIdx++)
	{
	if (strcmp (DclPrcDataCtx[DclPrcIdx].DclPrcHdl, dcl_proc_hdl) == 0)
	    {
	    DclPrcHdlFnd = 1;
	    break;
	    }
	}

    /*
    ** If the process context was not found, then lets expand the environment shared memory list
    */
    if (! DclPrcHdlFnd)
	{
	DclPrcDataMax++;
	apr$$dcl_proc_ctx++;
	DclPrcDataCtx = realloc (DclPrcDataCtx, sizeof (DCL_PRC_DATA_CTX) * DclPrcDataMax);
	if (! DclPrcDataCtx)
	    {
	    fprintf (stderr, "Error allocating memory for DCL process context structure (%d bytes)\n",
		     sizeof (DCL_PRC_DATA_CTX) * DclPrcDataMax);
	    fsync (fileno (stderr));
	    exit (vaxc$errno);
	    }
	DclPrcDataCtx[DclPrcIdx].DclPrcHdl = strdup (dcl_proc_hdl);
	DclPrcDataCtx[DclPrcIdx].DclPrcCtx = apr$$dcl_proc_ctx;
	DclPrcDataCtx[DclPrcIdx].DclArgSiz = 0;
	DclPrcDataCtx[DclPrcIdx].DclArgBuf = NULL;
	DclPrcDataCtx[DclPrcIdx].DclArgCtx = NULL;
	DclPrcDataCtx[DclPrcIdx].DclEnvSiz = 0;
	DclPrcDataCtx[DclPrcIdx].DclEnvBuf = NULL;
	DclPrcDataCtx[DclPrcIdx].DclEnvCtx = NULL;
	}

    /*
    ** Return the process context value
    */
    *dcl_proc_ctx = DclPrcDataCtx[DclPrcIdx].DclPrcCtx;
    }
else
    if (dcl_proc_ctx)
	{
	/*
	** Increment the process context
	*/
	apr$$dcl_proc_ctx++;

	/*
	** Return the next process context value
	*/
	*dcl_proc_ctx = apr$$dcl_proc_ctx;
	}

/*
** Get the required size of the environment data
*/
env_len = 0;
for (i = 0; env && env[i]; i++)
    env_len = env_len + strlen (env[i]) + sizeof (int);
env_len = env_len + 4;

/*
** Define the environment global section name
*/
if (dcl_proc_ctx)
    sprintf (env_fn, "APR$SHM_DCL_ENV_%08X%04X", getpid (), *dcl_proc_ctx);
else
    sprintf (env_fn, "APR$SHM_DCL_ENV_%08X", getpid ());


/*
** Process a saved context it we found one
*/
if (DclPrcIdx >= 0)
    {
    /*
    ** If we have a saved context that's not large enought, then delete it
    */
    if (DclPrcDataCtx[DclPrcIdx].DclEnvCtx && DclPrcDataCtx[DclPrcIdx].DclEnvSiz < env_len)
        {
        apr$$dcl_shm_delete (&DclPrcDataCtx[DclPrcIdx].DclEnvCtx);
        DclPrcDataCtx[DclPrcIdx].DclEnvSiz = 0;
        DclPrcDataCtx[DclPrcIdx].DclEnvBuf = NULL;
        DclPrcDataCtx[DclPrcIdx].DclEnvCtx = NULL;
        }

    /*
    ** If we have a saved context, then reuse it
    */
    if (DclPrcDataCtx[DclPrcIdx].DclEnvCtx)
        {
        env_len = DclPrcDataCtx[DclPrcIdx].DclEnvSiz;
        env_ptr = DclPrcDataCtx[DclPrcIdx].DclEnvBuf;
        env_ctx = DclPrcDataCtx[DclPrcIdx].DclEnvCtx;
        }
    }

/*
** If we don't have a valid context, then create one
*/
if (! env_ctx)
    {
    /*
    ** Create the argument shared memory section
    */
    env_ctx = apr$$dcl_shm_create (env_fn, &env_ptr, &env_len);
    if (! env_ctx)
        {
        fprintf (stderr, "Error creating DCL environment shared memory section '%s' (%08X)\n", env_fn, vaxc$errno);
        fsync (fileno (stderr));
        exit (vaxc$errno);
        }
    }

/*
** If we have a saved context, then update it
*/
if (DclPrcIdx >= 0)
    {
    DclPrcDataCtx[DclPrcIdx].DclEnvSiz = env_len;
    DclPrcDataCtx[DclPrcIdx].DclEnvBuf = env_ptr;
    DclPrcDataCtx[DclPrcIdx].DclEnvCtx = env_ctx;
    }

/*
** Load the environment file
*/
for (i = 0; env && env[i]; i++)
    {
    env_len = strlen (env[i]);
    memcpy (env_ptr, &env_len, sizeof (env_len));
    env_ptr = env_ptr + sizeof (env_len);
    memcpy (env_ptr, env[i], env_len);
    env_ptr = env_ptr + env_len;
    }
env_len = -1;
memcpy (env_ptr, &env_len, sizeof (env_len));

/*
** Return success
*/
return (0);

}

/*
**
**  apr$dcl_pass_arg - Pass argument data to a spawned child process
**
**  Functional Description:
**
**      This routine creates a file of argument variables to be read
**	by a spawned child process as of yet not created.
**
**  Usage:
**
**      apr$dcl_pass_run arg [dcl_proc_ctx] [dcl_proc_hdl]
**
**  Formal parameters:
**
**      arg 		- (IN) address of argument variable array
**      dcl_proc_ctx	- (OUT) address of argument context integer (OPTIONAL)
**      dcl_proc_hdl	- (IN) address of argument context handle string (OPTIONAL)
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      0		- Success
**
**  Side Effects:
**
**      None
**
*/
int
apr$dcl_pass_arg (
    char	*arg[],
    ...
    )
{
char *dcl_proc_hdl = NULL;
int *dcl_proc_ctx = NULL;
SHM_CTX *arg_ctx = NULL;
char *arg_ptr = NULL;
int DclPrcIdx = -1;
char arg_fn[256];
FILE *arg_fd;
va_list argp;
int arg_len,
    argc,i;

/*
** Process the optional parameters
*/
va_start (argp, arg);
va_count (argc);
if (argc > 1)
    dcl_proc_ctx = va_arg (argp, int *);
if (argc > 2)
    dcl_proc_hdl = va_arg (argp, char *);
va_end (argp);

/*
** If the caller has passed a context pointer and a handle string, then let's
** try to locate it.  If we can't then we'll store it and pass back the next
** context.
*/
if (dcl_proc_ctx && dcl_proc_hdl)
    {
    int DclPrcHdlFnd = 0;

    /*
    ** Let's see if we need to allocate the environment shared memory list
    */
    if (DclPrcDataMax == 0)
	{
	DclPrcDataMax++;
	apr$$dcl_proc_ctx++;
	DclPrcDataCtx = malloc (sizeof (DCL_PRC_DATA_CTX) * DclPrcDataMax);
	DclPrcDataCtx[0].DclPrcHdl = strdup (dcl_proc_hdl);
	DclPrcDataCtx[0].DclPrcCtx = apr$$dcl_proc_ctx;
	DclPrcDataCtx[0].DclArgSiz = 0;
	DclPrcDataCtx[0].DclArgBuf = NULL;
	DclPrcDataCtx[0].DclArgCtx = NULL;
	DclPrcDataCtx[0].DclEnvSiz = 0;
	DclPrcDataCtx[0].DclEnvBuf = NULL;
	DclPrcDataCtx[0].DclEnvCtx = NULL;
	}

    /*
    ** Look throught the shared memory argument list for the process handle
    */
    for (DclPrcIdx = 0; DclPrcIdx < DclPrcDataMax; DclPrcIdx++)
	{
	if (strcmp (DclPrcDataCtx[DclPrcIdx].DclPrcHdl, dcl_proc_hdl) == 0)
	    {
	    DclPrcHdlFnd = 1;
	    break;
	    }
	}

    /*
    ** If the process context was not found, then lets expand the argument shared memory list
    */
    if (! DclPrcHdlFnd)
	{
	DclPrcDataMax++;
	apr$$dcl_proc_ctx++;
	DclPrcDataCtx = realloc (DclPrcDataCtx, sizeof (DCL_PRC_DATA_CTX) * DclPrcDataMax);
	if (! DclPrcDataCtx)
	    {
	    fprintf (stderr, "Error allocating memory for DCL process context structure (%d bytes)\n",
		     sizeof (DCL_PRC_DATA_CTX) * DclPrcDataMax);
	    fsync (fileno (stderr));
	    exit (vaxc$errno);
	    }
	DclPrcDataCtx[DclPrcIdx].DclPrcHdl = strdup (dcl_proc_hdl);
	DclPrcDataCtx[DclPrcIdx].DclPrcCtx = apr$$dcl_proc_ctx;
	DclPrcDataCtx[DclPrcIdx].DclArgSiz = 0;
	DclPrcDataCtx[DclPrcIdx].DclArgBuf = NULL;
	DclPrcDataCtx[DclPrcIdx].DclArgCtx = NULL;
	DclPrcDataCtx[DclPrcIdx].DclEnvSiz = 0;
	DclPrcDataCtx[DclPrcIdx].DclEnvBuf = NULL;
	DclPrcDataCtx[DclPrcIdx].DclEnvCtx = NULL;
	}

    /*
    ** Return the process context value
    */
    *dcl_proc_ctx = DclPrcDataCtx[DclPrcIdx].DclPrcCtx;
    }
else
    if (dcl_proc_ctx)
	{
	/*
	** Increment the process context
	*/
	apr$$dcl_proc_ctx++;

	/*
	** Return the next process context value
	*/
	*dcl_proc_ctx = apr$$dcl_proc_ctx;
	}

/*
** Get the required size of the argument data
*/
arg_len = 0;
for (i = 0; arg && arg[i]; i++)
    arg_len = arg_len + strlen (arg[i]) + sizeof (int);
arg_len = arg_len + 4;

/*
** Define the argument global section name
*/
if (dcl_proc_ctx)
    sprintf (arg_fn, "APR$SHM_DCL_ARG_%08X%04X", getpid (), *dcl_proc_ctx);
else
    sprintf (arg_fn, "APR$SHM_DCL_ARG_%08X", getpid ());


/*
** Process a saved context it we found one
*/
if (DclPrcIdx >= 0)
    {
    /*
    ** If we have a saved context that's not large enought, then delete it
    */
    if (DclPrcDataCtx[DclPrcIdx].DclArgCtx && DclPrcDataCtx[DclPrcIdx].DclArgSiz < arg_len)
        {
        apr$$dcl_shm_delete (&DclPrcDataCtx[DclPrcIdx].DclArgCtx);
        DclPrcDataCtx[DclPrcIdx].DclArgSiz = 0;
        DclPrcDataCtx[DclPrcIdx].DclArgBuf = NULL;
        DclPrcDataCtx[DclPrcIdx].DclArgCtx = NULL;
        }

    /*
    ** If we have a saved context, then reuse it
    */
    if (DclPrcDataCtx[DclPrcIdx].DclArgCtx)
        {
        arg_len = DclPrcDataCtx[DclPrcIdx].DclArgSiz;
        arg_ptr = DclPrcDataCtx[DclPrcIdx].DclArgBuf;
        arg_ctx = DclPrcDataCtx[DclPrcIdx].DclArgCtx;
        }
    }

/*
** If we don't have a valid context, then create one
*/
if (! arg_ctx)
    {
    /*
    ** Create the argument shared memory section
    */
    arg_ctx = apr$$dcl_shm_create (arg_fn, &arg_ptr, &arg_len);
    if (! arg_ctx)
        {
        fprintf (stderr, "Error creating DCL argument shared memory section '%s' (%08X)\n", arg_fn, vaxc$errno);
        fsync (fileno (stderr));
        exit (vaxc$errno);
        }
    }

/*
** If we have a saved context, then update it
*/
if (DclPrcIdx >= 0)
    {
    DclPrcDataCtx[DclPrcIdx].DclArgSiz = arg_len;
    DclPrcDataCtx[DclPrcIdx].DclArgBuf = arg_ptr;
    DclPrcDataCtx[DclPrcIdx].DclArgCtx = arg_ctx;
    }

/*
** Load the argument file
*/
for (i = 0; arg && arg[i]; i++)
    {
    arg_len = strlen (arg[i]);
    memcpy (arg_ptr, &arg_len, sizeof (arg_len));
    arg_ptr = arg_ptr + sizeof (arg_len);
    memcpy (arg_ptr, arg[i], arg_len);
    arg_ptr = arg_ptr + arg_len;
    }
arg_len = -1;
memcpy (arg_ptr, &arg_len, sizeof (arg_len));

/*
** Return success
*/
return (0);

}

/*
**
**  apr$$dcl_shm_create - Create a DCL shared memory section
**
**  Functional Description:
**
**      This routine creates a global section for DCL processing.
**
**  Usage:
**
**      apr$$dcl_shm_create ShmName, ShmAddr, ShmSize
**
**  Formal parameters:
**
**      ShmName		- (IN) address of shared memory section name
**      ShmAddr		- (OUT) address of shared memory
**      ShmSize		- (IN/OUT) address of shared memory size
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      Non NULL	- Success
**      NULL		- Failure
**
**  Side Effects:
**
**      None
**
*/
static SHM_CTX *apr$$dcl_shm_create (
    char	*ShmName,
    char	**ShmAddr,
    int		*ShmSize
    )
{
SHM_CTX *shm_ctx = NULL;
int GalaxySection = FALSE,
    MaxSectionSize = TRUE;

/*
** Initialize vaxc$errno
*/
vaxc$errno = SS$_NORMAL;

/*
** Create the shared memory section
*/
shm_ctx = apr$shm_create (ShmName, *ShmSize, SHM_M_NOLOCKS, GalaxySection, MaxSectionSize);
if (shm_ctx)
    {
    /*
    ** Update the shared memory address and size
    */
    *ShmAddr = NULL;
    *ShmSize = apr$shm_available (shm_ctx);
    if (*ShmSize > 0)
	*ShmAddr = apr$shm_calloc (shm_ctx, 1, *ShmSize);
    if (*ShmAddr != NULL)
	apr$shm_set_common_ctx (shm_ctx, *ShmAddr);
    if (*ShmSize <= 0 || *ShmAddr == NULL)
	{
	apr$shm_destroy (shm_ctx);
	shm_ctx = NULL;
	}
    }

/*
** Return the shared memory context
*/
return (shm_ctx);

}

/*
**
**  apr$$dcl_shm_delete - Delete a DCL shared memory section
**
**  Functional Description:
**
**      This routine deletes a global section for DCL processing.
**
**  Usage:
**
**      apr$$dcl_shm_delete ShmCtx
**
**  Formal parameters:
**
**      ShmCtx		- (IN) address of shared memory section context
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      NULL		- Failure
**
**  Side Effects:
**
**      None
**
*/
static int apr$$dcl_shm_delete (
    SHM_CTX	**ShmCtx
    )
{

/*
** Initialize vaxc$errno
*/
vaxc$errno = SS$_NORMAL;

/*
** Delete the global section
*/
apr$shm_destroy (*ShmCtx);

/*
** Initialize the context if the destroy was successful
*/
if (vaxc$errno & 1)
    *ShmCtx = NULL;

/*
** Return
*/
return (vaxc$errno);

}

/*
**
**  apr$dcl_shm_open - open a DCL shared memory section
**
**  Functional Description:
**
**      This routine opens a global section for DCL processing.
**
**  Usage:
**
**      apr$dcl_shm_open shm_spec
**
**  Formal parameters:
**
**      shm_spec	- (IN) address of shared memory section name
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      NULL		- Indicates an error
**
**  Side Effects:
**
**      None
**
*/
void *apr$dcl_shm_open (
    char	*shm_spec
    )
{
DCL_SHM_DATA_CTX *CgiDataCtx = NULL;
SHM_CTX *shm_ctx = NULL;
int shm_size = 1024;
int *LenPtr;

/*
** Initialize the vaxc$errno
*/
vaxc$errno = SS$_NORMAL;

/*
** Create the shared memory section
*/
shm_ctx = apr$shm_create (shm_spec, shm_size, SHM_M_NOLOCKS);
if (shm_ctx)
    {
    /*
    ** Delete the global section if we created it
    */
    if (shm_ctx->ctx_status == SS$_CREATED)
	{
	apr$shm_destroy (shm_ctx);
	decc$$translate (SS$_NOSUCHSEC);
	}
    else
	{
	CgiDataCtx = malloc (sizeof (DCL_SHM_DATA_CTX));
	if (CgiDataCtx)
	    {
	    CgiDataCtx->shm_ctx = shm_ctx;
	    CgiDataCtx->shm_ptr = apr$shm_get_common_ctx (CgiDataCtx->shm_ctx);
	    if (! CgiDataCtx->shm_ptr)
		{
		apr$shm_destroy (CgiDataCtx->shm_ctx);
		free (CgiDataCtx);
		CgiDataCtx = NULL;
		decc$$translate (SS$_NOSUCHSEC);
		}
	    CgiDataCtx->shm_len = 0;
	    LenPtr = (int *) CgiDataCtx->shm_ptr;
	    while (*LenPtr > 0)
		{
		CgiDataCtx->shm_len = CgiDataCtx->shm_len + sizeof (*LenPtr) + *LenPtr;
		LenPtr = (int *) ((int) CgiDataCtx->shm_ptr + CgiDataCtx->shm_len);
		}
	    }
	}
    }

/*
** Return the status
*/
return ((void *) CgiDataCtx);

}

/*
**
**  apr$dcl_shm_read - Read a DCL shared memory section
**
**  Functional Description:
**
**      This routine reads a global section for DCL processing.
**
**  Usage:
**
**      apr$dcl_shm_read ptr, size_of_item, number_items, shm_ctx, ...
**
**  Formal parameters:
**
**      ptr		- (OUT) address of where to put read memory
**      size_of_item	- (IN) size of the items being read, in bytes
**      number_items	- (IN) number of items to be read
**      shm_ctx 	- (IN) Address of the shared memory context
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      n		- Indicates the number of items read
**      0		- Indicates the end-of-data or an error
**
**  Side Effects:
**
**      None
**
*/
int apr$dcl_shm_read (
    void 	*ptr,
    size_t	size_of_item,
    size_t	number_items,
    void 	*shm_ctx
    )
{
DCL_SHM_DATA_CTX *CgiDataCtx = shm_ctx;
int len = size_of_item * number_items;

/*
** If the return address is NULL, the context address is null, or
** there is no more data for this context to be read then return 0
*/
if (! ptr || len == 0 || ! CgiDataCtx || CgiDataCtx->shm_len == 0)
    return (0);

/*
** If the length requested is greater than what is requested then
** adjust the requested length to what is available.
*/
if (len > CgiDataCtx->shm_len)
    len = CgiDataCtx->shm_len;

/*
** If we're reading by reference then copy address of the memory
** to the callers buffer, otherwise return the contents of the
** current pointer to the user buffer.
*/
memcpy (ptr, CgiDataCtx->shm_ptr, len);
CgiDataCtx->shm_ptr = (void *) ((int) CgiDataCtx->shm_ptr + len);
CgiDataCtx->shm_len = CgiDataCtx->shm_len - len;

/*
** Return the length of the copied data
*/
return (len / size_of_item);

}

/*
**
**  apr$dcl_shm_rewind - Rewind a DCL shared memory section
**
**  Functional Description:
**
**      This routine rewinds a global section for DCL processing.
**
**  Usage:
**
**      apr$dcl_shm_rewind shm_ctx
**
**  Formal parameters:
**
**      shm_ctx 	- (IN) Address of the shared memory context
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      0		- Indicates success
**      EOF		- Indicates success
**
**  Side Effects:
**
**      None
**
*/
int apr$dcl_shm_rewind (
    void	*shm_ctx
    )
{
DCL_SHM_DATA_CTX *CgiDataCtx = shm_ctx;
int *LenPtr;
int status;

/*
** Rewind the shared memory context
*/
if (CgiDataCtx && CgiDataCtx->shm_ctx)
    {
    CgiDataCtx->shm_ptr = apr$shm_get_common_ctx (CgiDataCtx->shm_ctx);
    CgiDataCtx->shm_len = 0;
    LenPtr = (int *) CgiDataCtx->shm_ptr;
    while (*LenPtr > 0)
	{
	CgiDataCtx->shm_len = CgiDataCtx->shm_len + sizeof (*LenPtr) + *LenPtr;
	LenPtr = (int *) ((int) CgiDataCtx->shm_ptr + CgiDataCtx->shm_len);
	}
    status = SS$_NORMAL;
    }
else
    status = SS$_NOSUCHSEC;

/*
** Translate the status
*/
decc$$translate (status);

/*
** Return the status
*/
return (status);

}

/*
**
**  apr$dcl_shm_close - Close a DCL shared memory section
**
**  Functional Description:
**
**      This routine closes a global section for DCL processing.
**
**  Usage:
**
**      apr$dcl_shm_close shm_ctx
**
**  Formal parameters:
**
**      shm_ctx 	- (IN) Address of the shared memory context
**
**  Implicit Parameters:
**
**      None
**
**  Routine Value:
**
**      0		- Indicates success
**      EOF		- Indicates failure
**
**  Side Effects:
**
**      None
**
*/
int apr$dcl_shm_close (
    void	*shm_ctx
    )
{
DCL_SHM_DATA_CTX *CgiDataCtx = shm_ctx;
int status = 0;

/*
** Initialize the vaxc$errno
*/
vaxc$errno = SS$_NOSUCHSEC;

/*
** Destroy the shared memory context
*/
if (CgiDataCtx && CgiDataCtx->shm_ctx)
    apr$shm_destroy (CgiDataCtx->shm_ctx);

if (! (vaxc$errno & 1))
    status = EOF;

/*
** Return the status
*/
return (status);

}

/******************************************************************************/
/*** apr$asl_create 							    ***/
/******************************************************************************/
int apr$asl_create (
    int server_limit
    )
{
int AstInProgress = LIB$AST_IN_PROG ();
int status = APR_SUCCESS;

/*
** Disable AST delivery (if necessary)
*/
if (! AstInProgress)
    SYS$SETAST (0);

/*
** Allocate the active server list
*/
if (ActiveServerList)
    ActiveServerList = realloc (ActiveServerList, server_limit * sizeof (ACTIVE_SVR_DATA));
else
    ActiveServerList = calloc (server_limit, sizeof (ACTIVE_SVR_DATA));
if (ActiveServerList)
    ActiveServerSize = server_limit;
else
    status = APR_ENOPOOL;

/*
** Enable AST delivery (if necessary)
*/
if (! AstInProgress)
    SYS$SETAST (1);

/*
** Return status
*/
return (status);

}

/******************************************************************************/
/*** apr$asl_insert 							    ***/
/******************************************************************************/
int apr$asl_insert (
    pid_t pid
    )
{
int AstInProgress = LIB$AST_IN_PROG ();
int i;

/*
** Disable AST delivery (if necessary)
*/
if (! AstInProgress)
    SYS$SETAST (0);

/*
** Locate and insert a server in the active server list
*/
for (i = 0; ActiveServerList && i < ActiveServerSize; i++)
    if (ActiveServerList[i].pid == 0)
	{
	ActiveServerList[i].pid = pid;
	ActiveServerList[i].terminated = FALSE;
	ActiveServerList[i].terminated_status = 0;
	break;
	}

/*
** Enable AST delivery (if necessary)
*/
if (! AstInProgress)
    SYS$SETAST (1);

/*
** Return status
*/
if (i >= ActiveServerSize)
    return (APR_EOF);
else
    return (APR_SUCCESS);

}

/******************************************************************************/
/*** apr$asl_update 							    ***/
/******************************************************************************/
int apr$asl_update (
    pid_t pid,
    int status
    )
{
int AstInProgress = LIB$AST_IN_PROG ();
int i;

/*
** Disable AST delivery (if necessary)
*/
if (! AstInProgress)
    SYS$SETAST (0);

/*
** Locate and update a server in the active server list
*/
for (i = 0; ActiveServerList && i < ActiveServerSize; i++)
    if (ActiveServerList[i].pid == pid)
	{
	ActiveServerList[i].pid = pid;
	ActiveServerList[i].terminated = TRUE;
	ActiveServerList[i].terminated_status = status;
	break;
	}

/*
** Enable AST delivery (if necessary)
*/
if (! AstInProgress)
    SYS$SETAST (1);

/*
** If the active server list was updated, then wake up this process
*/
if (i < ActiveServerSize)
    SYS$WAKE (0, 0);

/*
** Return status
*/
if (i >= ActiveServerSize)
    return (APR_EOF);
else
    return (APR_SUCCESS);

}

/******************************************************************************/
/*** apr$asl_delete 							    ***/
/******************************************************************************/
int apr$asl_delete (
    void
    )
{
int AstInProgress = LIB$AST_IN_PROG ();

/*
** Disable AST delivery (if necessary)
*/
if (! AstInProgress)
    SYS$SETAST (0);

/*
** Free the active server list
*/
if (ActiveServerList)
    free (ActiveServerList);
ActiveServerList = NULL;
ActiveServerSize = 0;

/*
** Enable AST delivery (if necessary)
*/
if (! AstInProgress)
    SYS$SETAST (1);

/*
** Return status
*/
return (APR_SUCCESS);

}

/******************************************************************************/
/*** apr$asl_waitpid 						    ***/
/******************************************************************************/
pid_t apr$asl_waitpid (
    pid_t process_id,
    int *status_location,
    int options
    )
{
int AstInProgress = LIB$AST_IN_PROG ();
pid_t Pid = 0;
int i;

/*
** Wait for an exited server
*/
while (TRUE)
    {
    /*
    ** Disable AST delivery (if necessary)
    */
    if (! AstInProgress)
	SYS$SETAST (0);

    /*
    ** Locate and update a server in the active server list
    */
    for (i = 0; ActiveServerList && i < ActiveServerSize; i++)
	if (ActiveServerList[i].pid && ActiveServerList[i].terminated)
	    if (process_id == -1 || process_id == ActiveServerList[i].pid)
		{
		Pid = ActiveServerList[i].pid;
		*status_location = ActiveServerList[i].terminated_status;
		ActiveServerList[i].pid = 0;
		ActiveServerList[i].terminated = FALSE;
		ActiveServerList[i].terminated_status = 0;
		break;
		}

    /*
    ** Enable AST delivery (if necessary)
    */
    if (! AstInProgress)
	SYS$SETAST (1);

    /*
    ** If we haven't found a pid and we're not in an AST and we've been told not
    ** to wait for process termination then wait for a process to terminate,
    ** otherwise let's exit this loop
    */
    if (! Pid && ! AstInProgress && options != WNOHANG)
	SYS$HIBER ();
    else
	break;
    }

/*
** Return the process of the first terminated process
*/
return (Pid);

}
#endif
