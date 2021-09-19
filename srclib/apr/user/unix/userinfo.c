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

#include "apr_strings.h"
#include "apr_portable.h"
#include "apr_user.h"
#include "apr_private.h"
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#if APR_HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if APR_HAVE_UNISTD_H
#include <unistd.h> /* for _POSIX_THREAD_SAFE_FUNCTIONS */
#endif
#define APR_WANT_MEMFUNC
#include "apr_want.h"

#define PWBUF_SIZE 2048

#ifdef __VMS
#include <descrip.h>
#include <stdlib.h>
#include <ssdef.h>
#include "cvtfnm.h"
#include "protshr.h"
static char *UnixDirName = NULL;
#endif

static apr_status_t getpwnam_safe(const char *username,
                                  struct passwd *pw,
#ifdef __VMS
                                  char pwbuf[PWBUF_SIZE], apr_pool_t *p)
#else
                                  char pwbuf[PWBUF_SIZE])
#endif
{
#ifdef __VMS
    int status;
#endif
    struct passwd *pwptr;
#if APR_HAS_THREADS && defined(_POSIX_THREAD_SAFE_FUNCTIONS) && defined(HAVE_GETPWNAM_R)
    apr_status_t rv;

    /* POSIX defines getpwnam_r() et al to return the error number
     * rather than set errno, and requires pwptr to be set to NULL if
     * the entry is not found, imply that "not found" is not an error
     * condition; some implementations do return 0 with pwptr set to
     * NULL. */
    rv = getpwnam_r(username, pw, pwbuf, PWBUF_SIZE, &pwptr);
    if (rv) {
        return rv;
    }
    if (pwptr == NULL) {
        return APR_ENOENT;
    }
#else
    /* Some platforms (e.g. FreeBSD 4.x) do not set errno on NULL "not
     * found" return values for the non-threadsafe function either. */
    errno = 0;
#ifdef __VMS
    if ((pwptr = getpwnam(username, 0)) != NULL) {
#else
    if ((pwptr = getpwnam(username)) != NULL) {
#endif
        memcpy(pw, pwptr, sizeof *pw);
    }
    else {
#ifdef __VMS
    /*
    ** If we could not get the username because of privilege, then let's try the
    ** APR privileged shared image getpwnam routine.
    */
    if ((errno == EACCES || errno == EVMSERR) && (vaxc$errno == SS$_NOGRPPRV || vaxc$errno == SS$_NOSYSPRV))
    {
        struct dsc$descriptor UserDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
        int pw_struct_max = sizeof (struct passwd),
            pw_name_max = strlen (username) + 1,
            pw_shell_max = (31 + 1),
            pw_dir_max = (31 + 63 + 1),
            pw_buf_max = 0;

        /*
        ** See if the pwbuf has enough space to hold the password structure
        ** and it's elements.  If not, then allocate from heap.
        */
        if ((pw_struct_max + pw_name_max + pw_shell_max + pw_dir_max) > PWBUF_SIZE)
        {
            pw_buf_max = pw_struct_max + pw_name_max + pw_shell_max + pw_dir_max;
            if (! p)
                return (APR_ENOPOOL);
            pwptr = (void *) apr_pcalloc (p, pw_buf_max);
            if (! pwptr)
                return (APR_ENOPOOL);
        }
        else
        {
            pw_buf_max = PWBUF_SIZE;
            pwptr = (void *) pwbuf;
        }

        /*
        ** Establish the username descriptor
        */
        UserDesc.dsc$a_pointer = (void *) username;
        UserDesc.dsc$w_length = strlen (username);

        /*
        ** Establish the password structure elements in the password buffer
        */
        pwptr->pw_name  = (void *) ((int) pwptr + pw_struct_max);
        pwptr->pw_shell = (void *) ((int) pwptr + pw_struct_max + pw_name_max);
        pwptr->pw_dir   = (void *) ((int) pwptr + pw_struct_max + pw_name_max + pw_dir_max);

        /*
        ** Call the APR privileged getpwnam routine
        */
        status = apr$$getpwnam (&UserDesc, pwptr);
        if (! (status & 1))
            return (EACCES);

        /*
        ** Free any previous directory name
        */
        if (UnixDirName)
        {
            free (UnixDirName);
            UnixDirName = NULL;
        }

        /*
        ** Convert the directory filename to Unix
        */
        if (apr$cvt_fnm (CVT_FNM_VMS_TO_UNIX, pwptr->pw_dir, &UnixDirName))
        {
            if (pw_buf_max < (pw_buf_max - pw_dir_max) + strlen (UnixDirName))
                pwptr->pw_dir = apr_pcalloc (p, strlen (UnixDirName) + 1);
            strcpy (pwptr->pw_dir, UnixDirName);
        }

        /*
        ** Return the password structure
        */
        memcpy (pw, pwptr, sizeof(*pw));
    }
    else
#endif
        return errno ? errno : APR_ENOENT;
    }
#endif
    return APR_SUCCESS;
}

APR_DECLARE(apr_status_t) apr_uid_homepath_get(char **dirname,
                                               const char *username,
                                               apr_pool_t *p)
{
    struct passwd pw;
    char pwbuf[PWBUF_SIZE];
    apr_status_t rv;

#ifdef __VMS
    if ((rv = getpwnam_safe(username, &pw, pwbuf, p)) != APR_SUCCESS)
#else
    if ((rv = getpwnam_safe(username, &pw, pwbuf)) != APR_SUCCESS)
#endif
        return rv;

#ifdef OS2
    /* Need to manually add user name for OS/2 */
    *dirname = apr_pstrcat(p, pw.pw_dir, pw.pw_name, NULL);
#else
    *dirname = apr_pstrdup(p, pw.pw_dir);
#endif
    return APR_SUCCESS;
}



APR_DECLARE(apr_status_t) apr_uid_current(apr_uid_t *uid,
                                          apr_gid_t *gid,
                                          apr_pool_t *p)
{
    *uid = getuid();
    *gid = getgid();

    return APR_SUCCESS;
}




APR_DECLARE(apr_status_t) apr_uid_get(apr_uid_t *uid, apr_gid_t *gid,
                                      const char *username, apr_pool_t *p)
{
    struct passwd pw;
    char pwbuf[PWBUF_SIZE];
    apr_status_t rv;

#ifdef __VMS
    if ((rv = getpwnam_safe(username, &pw, pwbuf, p)) != APR_SUCCESS)
#else
    if ((rv = getpwnam_safe(username, &pw, pwbuf)) != APR_SUCCESS)
#endif

    *uid = pw.pw_uid;
    *gid = pw.pw_gid;

    return APR_SUCCESS;
}

APR_DECLARE(apr_status_t) apr_uid_name_get(char **username, apr_uid_t userid,
                                           apr_pool_t *p)
{
    struct passwd *pw;
#if APR_HAS_THREADS && defined(_POSIX_THREAD_SAFE_FUNCTIONS) && defined(HAVE_GETPWUID_R)
    struct passwd pwd;
    char pwbuf[PWBUF_SIZE];
    apr_status_t rv;

    rv = getpwuid_r(userid, &pwd, pwbuf, sizeof(pwbuf), &pw);
    if (rv) {
        return rv;
    }

    if (pw == NULL) {
        return APR_ENOENT;
    }

#else
    errno = 0;
    if ((pw = getpwuid(userid)) == NULL) {
        return errno ? errno : APR_ENOENT;
    }
#endif
    *username = apr_pstrdup(p, pw->pw_name);
    return APR_SUCCESS;
}
