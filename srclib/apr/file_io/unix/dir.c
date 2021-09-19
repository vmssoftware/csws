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

#include "apr_arch_file_io.h"
#include "apr_strings.h"
#include "apr_portable.h"
#if APR_HAVE_SYS_SYSLIMITS_H
#include <sys/syslimits.h>
#endif
#if APR_HAVE_LIMITS_H
#include <limits.h>
#endif
#ifdef __VMS
#include "cvtfnm.h"
#endif

#ifndef NAME_MAX
#define NAME_MAX 255
#endif

static apr_status_t dir_cleanup(void *thedir)
{
    apr_dir_t *dir = thedir;
    if (closedir(dir->dirstruct) == 0) {
        return APR_SUCCESS;
    }
    else {
        return errno;
    }
}

#define PATH_SEPARATOR '/'

/* Remove trailing separators that don't affect the meaning of PATH. */
static const char *path_canonicalize (const char *path, apr_pool_t *pool)
{
    /* At some point this could eliminate redundant components.  For
     * now, it just makes sure there is no trailing slash. */
    apr_size_t len = strlen (path);
    apr_size_t orig_len = len;

    while ((len > 0) && (path[len - 1] == PATH_SEPARATOR))
        len--;

    if (len != orig_len)
        return apr_pstrndup (pool, path, len);
    else
        return path;
}

/* Remove one component off the end of PATH. */
static char *path_remove_last_component (const char *path, apr_pool_t *pool)
{
    const char *newpath = path_canonicalize (path, pool);
    int i;

    for (i = (strlen(newpath) - 1); i >= 0; i--) {
        if (path[i] == PATH_SEPARATOR)
            break;
    }

    return apr_pstrndup (pool, path, (i < 0) ? 0 : i);
}

#ifdef __VMS
apr_status_t apr_dir_open(apr_dir_t ** new, const char *dirname,
                          apr_pool_t * pool)
{
    static int ReaddirNullType = -1;
    static int ReaddirDotFiles = -1;
    static int ReaddirUnixOpen = -1;
    apr_size_t dirent_size;
    char *UnixDirName = NULL;

    /*
     ** Determine whether we should turn off either null type support
     ** or dot files support.
     */
    if (ReaddirNullType < 0) {
        if (getenv("APR$READDIR_NO_NULL_TYPE"))
            ReaddirNullType = 0;
        else
            ReaddirNullType = 1;
    }
    if (ReaddirDotFiles < 0) {
        if (getenv("APR$READDIR_NO_DOT_FILES"))
            ReaddirDotFiles = 0;
        else
            ReaddirDotFiles = 1;
    }
    if (ReaddirUnixOpen < 0) {
        if (getenv("APR$READDIR_NO_UNIX_OPEN"))
            ReaddirUnixOpen = 0;
        else
            ReaddirUnixOpen = 1;
    }

    /* On some platforms (e.g., Linux+GNU libc), d_name[] in struct
     * dirent is declared with enough storage for the name.  On other
     * platforms (e.g., Solaris 8 for Intel), d_name is declared as a
     * one-byte array.  Note: gcc evaluates this at compile time.
     */
    dirent_size = (sizeof((*new)->entry->d_name) > 1 ?
                   sizeof(struct dirent) : sizeof(struct dirent) + 255);

    (*new) = (apr_dir_t *) apr_palloc(pool, sizeof(apr_dir_t));
    (*new)->pool = pool;
    /*
     ** Convert the directory name to Unix if we're forcing Unix filespecs for
     ** directory processing.
     */
    if (ReaddirUnixOpen
        && apr$cvt_fnm(CVT_FNM_VMS_TO_UNIX, (char *) dirname,
                          &UnixDirName)) {
        (*new)->dirname = apr_pstrdup(pool, UnixDirName);
        (*new)->dirstruct = opendir(UnixDirName);
    } else {
        (*new)->dirname = apr_pstrdup(pool, dirname);
        (*new)->dirstruct = opendir(dirname);
    }

    (*new)->entry = (*new)->pool_entry = apr_pcalloc(pool, dirent_size);
    (*new)->rdf.read_null_type = ReaddirNullType;
    (*new)->rdf.read_dot_files = ReaddirDotFiles;
    (*new)->rdf.read_1dot_file = FALSE;
    (*new)->rdf.read_2dot_file = FALSE;

    if ((*new)->dirstruct == NULL) {
        return errno;
    } else {
        apr_pool_cleanup_register((*new)->pool, (void *) (*new),
                                  dir_cleanup, apr_pool_cleanup_null);
        return APR_SUCCESS;
    }
}

#else
apr_status_t apr_dir_open(apr_dir_t **new, const char *dirname,
                          apr_pool_t *pool)
{
    DIR *dir = opendir(dirname);

    if (!dir) {
        return errno;
    }

    (*new) = (apr_dir_t *)apr_palloc(pool, sizeof(apr_dir_t));

    (*new)->pool = pool;
    (*new)->dirname = apr_pstrdup(pool, dirname);
    (*new)->dirstruct = dir;

#if APR_HAS_THREADS && defined(_POSIX_THREAD_SAFE_FUNCTIONS) \
                    && !defined(READDIR_IS_THREAD_SAFE)
    /* On some platforms (e.g., Linux+GNU libc), d_name[] in struct
     * dirent is declared with enough storage for the name.  On other
     * platforms (e.g., Solaris 8 for Intel), d_name is declared as a
     * one-byte array.  Note: gcc evaluates this at compile time.
     */
    (*new)->entry = apr_pcalloc(pool, sizeof(*(*new)->entry) +
                                      (sizeof((*new)->entry->d_name) > 1
                                       ? 0 : NAME_MAX));
#else
    (*new)->entry = NULL;
#endif

    apr_pool_cleanup_register((*new)->pool, *new, dir_cleanup,
                              apr_pool_cleanup_null);
    return APR_SUCCESS;
}
#endif

apr_status_t apr_dir_close(apr_dir_t *thedir)
{
    return apr_pool_cleanup_run(thedir->pool, thedir, dir_cleanup);
}

#ifdef DIRENT_TYPE
static apr_filetype_e filetype_from_dirent_type(int type)
{
    switch (type) {
    case DT_REG:
        return APR_REG;
    case DT_DIR:
        return APR_DIR;
    case DT_LNK:
        return APR_LNK;
    case DT_CHR:
        return APR_CHR;
    case DT_BLK:
        return APR_BLK;
#if defined(DT_FIFO)
    case DT_FIFO:
        return APR_PIPE;
#endif
#if !defined(BEOS) && defined(DT_SOCK)
    case DT_SOCK:
        return APR_SOCK;
#endif
    default:
        return APR_UNKFILE;
    }
}
#endif

apr_status_t apr_dir_read(apr_finfo_t *finfo, apr_int32_t wanted,
                          apr_dir_t *thedir)
{
    apr_status_t ret = 0;
#ifdef DIRENT_TYPE
    apr_filetype_e type;
#endif
#if APR_HAS_THREADS && defined(_POSIX_THREAD_SAFE_FUNCTIONS) \
                    && !defined(READDIR_IS_THREAD_SAFE)
#ifdef APR_USE_READDIR64_R
    struct dirent64 *retent;

    /* If LFS is enabled and readdir64_r is available, readdir64_r is
     * used in preference to readdir_r.  This allows directories to be
     * read which contain a (64-bit) inode number which doesn't fit
     * into the 32-bit apr_ino_t, iff the caller doesn't actually care
     * about the inode number (i.e. wanted & APR_FINFO_INODE == 0).
     * (such inodes may be seen in some wonky NFS environments)
     *
     * Similarly, if the d_off field cannot be reprented in a 32-bit
     * offset, the libc readdir_r() would barf; using readdir64_r
     * bypasses that case entirely since APR does not care about
     * d_off. */

    ret = readdir64_r(thedir->dirstruct, thedir->entry, &retent);
#else

    struct dirent *retent;

    ret = readdir_r(thedir->dirstruct, thedir->entry, &retent);
#endif

    /* POSIX treats "end of directory" as a non-error case, so ret
     * will be zero and retent will be set to NULL in that case. */
    if (!ret && retent == NULL) {
        ret = APR_ENOENT;
    }

    /* Solaris is a bit strange, if there are no more entries in the
     * directory, it returns EINVAL.  Since this is against POSIX, we
     * hack around the problem here.  EINVAL is possible from other
     * readdir implementations, but only if the result buffer is too small.
     * since we control the size of that buffer, we should never have
     * that problem.
     */
    if (ret == EINVAL) {
        ret = APR_ENOENT;
    }
#else
    /* We're about to call a non-thread-safe readdir() that may
       possibly set `errno', and the logic below actually cares about
       errno after the call.  Therefore we need to clear errno first. */
    errno = 0;
#ifndef __VMS
    thedir->entry = readdir(thedir->dirstruct);
#else
    if (thedir->rdf.read_dot_files)
    {
        if (! thedir->rdf.read_1dot_file) {
            thedir->rdf.read_1dot_file = TRUE;
            strcpy (thedir->entry->d_name, ".");
        }
        else {
            if (! thedir->rdf.read_2dot_file) {
                thedir->rdf.read_2dot_file = TRUE;
                strcpy (thedir->entry->d_name, "..");
            }
            else {
                thedir->entry = readdir (thedir->dirstruct);
            }
        }
    }
    else {
        thedir->entry = readdir (thedir->dirstruct);
    }
#endif

    if (thedir->entry == NULL) {
        /* If NULL was returned, this can NEVER be a success. Can it?! */
        if (errno == APR_SUCCESS) {
            ret = APR_ENOENT;
        }
        else
            ret = errno;
    }
#endif

    /* No valid bit flag to test here - do we want one? */
    finfo->fname = NULL;

    if (ret) {
        finfo->valid = 0;
        return ret;
    }

#ifdef DIRENT_TYPE
    type = filetype_from_dirent_type(thedir->entry->DIRENT_TYPE);
    if (type != APR_UNKFILE) {
        wanted &= ~APR_FINFO_TYPE;
    }
#endif
#ifdef DIRENT_INODE
    if (thedir->entry->DIRENT_INODE && thedir->entry->DIRENT_INODE != -1) {
#ifdef APR_USE_READDIR64_R
        /* If readdir64_r is used, check for the overflow case of trying
         * to fit a 64-bit integer into a 32-bit integer. */
        if (sizeof(apr_ino_t) >= sizeof(retent->DIRENT_INODE)
            || (apr_ino_t)retent->DIRENT_INODE == retent->DIRENT_INODE) {
            wanted &= ~APR_FINFO_INODE;
        } else {
            /* Prevent the fallback code below from filling in the
             * inode if the stat call fails. */
            retent->DIRENT_INODE = 0;
        }
#else
        wanted &= ~APR_FINFO_INODE;
#endif /* APR_USE_READDIR64_R */
    }
#endif /* DIRENT_INODE */

    wanted &= ~APR_FINFO_NAME;

    if (wanted)
    {
        char fspec[APR_PATH_MAX];
        char *end;

        end = apr_cpystrn(fspec, thedir->dirname, sizeof fspec);

        if (end > fspec && end[-1] != '/' && (end < fspec + APR_PATH_MAX))
            *end++ = '/';

        apr_cpystrn(end, thedir->entry->d_name,
                    sizeof fspec - (end - fspec));

        ret = apr_stat(finfo, fspec, APR_FINFO_LINK | wanted, thedir->pool);
        /* We passed a stack name that will disappear */
        finfo->fname = NULL;
    }

#ifdef __VMS
    /*
    ** Strip off any null type (i.e. '.')
    */
    if (thedir->rdf.read_null_type)
        {
        char FileSpec[APR_PATH_MAX];
        apr_finfo_t FileInfo;
        int FileOffset;

        /*
        ** If he caller did request the file type, then let's get it now, otherwise
        ** just copy their file info block.
        */
        if (! (wanted & APR_FINFO_TYPE))
            {
            apr_cpystrn (FileSpec, thedir->dirname, sizeof (FileSpec));
            FileOffset = strlen (FileSpec);
            if (FileSpec[FileOffset - 1] != '/')
                FileSpec[FileOffset++] = '/';
            apr_cpystrn (FileSpec + FileOffset, thedir->entry->d_name, sizeof (FileSpec) - FileOffset);
            memset (&FileInfo, 0, sizeof (FileInfo));
            apr_stat (&FileInfo, FileSpec, APR_FINFO_TYPE, thedir->pool);
            }
        else
            memcpy (&FileInfo, finfo, sizeof (FileInfo));

        /*
        ** Strip any empty type for non-Directory files
        */
        if (FileInfo.filetype != APR_DIR &&
            strcmp (thedir->entry->d_name, ".") != 0 &&
            strcmp (thedir->entry->d_name, "..") != 0 &&
            thedir->entry->d_name[strlen(thedir->entry->d_name)-1] == '.')
            thedir->entry->d_name[strlen(thedir->entry->d_name)-1] = '\0';
        }
#endif

    if (wanted && (ret == APR_SUCCESS || ret == APR_INCOMPLETE)) {
        wanted &= ~finfo->valid;
    }
    else {
        /* We don't bail because we fail to stat, when we are only -required-
         * to readdir... but the result will be APR_INCOMPLETE
         */
        finfo->pool = thedir->pool;
        finfo->valid = 0;
#ifdef DIRENT_TYPE
        if (type != APR_UNKFILE) {
            finfo->filetype = type;
            finfo->valid |= APR_FINFO_TYPE;
        }
#endif
#ifdef DIRENT_INODE
        if (thedir->entry->DIRENT_INODE && thedir->entry->DIRENT_INODE != -1) {
            finfo->inode = thedir->entry->DIRENT_INODE;
            finfo->valid |= APR_FINFO_INODE;
        }
#endif
    }

    finfo->name = apr_pstrdup(thedir->pool, thedir->entry->d_name);
    finfo->valid |= APR_FINFO_NAME;

    if (wanted)
        return APR_INCOMPLETE;

    return APR_SUCCESS;
}

apr_status_t apr_dir_rewind(apr_dir_t *thedir)
{
#ifdef __VMS
    thedir->entry = thedir->pool_entry;
    thedir->rdf.read_1dot_file = FALSE;
    thedir->rdf.read_2dot_file = FALSE;
#endif
    rewinddir(thedir->dirstruct);
    return APR_SUCCESS;
}

apr_status_t apr_dir_make(const char *path, apr_fileperms_t perm,
                          apr_pool_t *pool)
{
    mode_t mode = apr_unix_perms2mode(perm);

    if (mkdir(path, mode) == 0) {
        return APR_SUCCESS;
    }
    else {
        return errno;
    }
}

apr_status_t apr_dir_make_recursive(const char *path, apr_fileperms_t perm,
                                           apr_pool_t *pool)
{
    apr_status_t apr_err = 0;

    apr_err = apr_dir_make (path, perm, pool); /* Try to make PATH right out */

    if (apr_err == ENOENT) { /* Missing an intermediate dir */
        char *dir;

        dir = path_remove_last_component(path, pool);
        /* If there is no path left, give up. */
        if (dir[0] == '\0') {
            return apr_err;
        }

        apr_err = apr_dir_make_recursive(dir, perm, pool);

        if (!apr_err)
            apr_err = apr_dir_make (path, perm, pool);
    }

    /*
     * It's OK if PATH exists. Timing issues can lead to the second
     * apr_dir_make being called on existing dir, therefore this check
     * has to come last.
     */
    if (APR_STATUS_IS_EEXIST(apr_err))
        return APR_SUCCESS;

    return apr_err;
}

apr_status_t apr_dir_remove(const char *path, apr_pool_t *pool)
{
    if (rmdir(path) == 0) {
        return APR_SUCCESS;
    }
    else {
        return errno;
    }
}

apr_status_t apr_os_dir_get(apr_os_dir_t **thedir, apr_dir_t *dir)
{
    if (dir == NULL) {
        return APR_ENODIR;
    }
    *thedir = dir->dirstruct;
    return APR_SUCCESS;
}

apr_status_t apr_os_dir_put(apr_dir_t **dir, apr_os_dir_t *thedir,
                          apr_pool_t *pool)
{
    if ((*dir) == NULL) {
        (*dir) = (apr_dir_t *)apr_pcalloc(pool, sizeof(apr_dir_t));
        (*dir)->pool = pool;
    }
    (*dir)->dirstruct = thedir;
    return APR_SUCCESS;
}


