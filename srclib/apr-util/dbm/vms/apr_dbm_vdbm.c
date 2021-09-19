/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2000-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution,
 *    if any, must include the following acknowledgment:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgment may appear in the software itself,
 *    if and wherever such third-party acknowledgments normally appear.
 *
 * 4. The names "Apache" and "Apache Software Foundation" must
 *    not be used to endorse or promote products derived from this
 *    software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache",
 *    nor may "Apache" appear in their name, without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */

#include "apr_strings.h"
#define APR_WANT_MEMFUNC
#define APR_WANT_STRFUNC
#include "apr_want.h"

#include "apu.h"

#if APU_HAVE_VDBM

#include "apr_dbm_private.h"

#include "apr_vdbm.h"
#if APR_HAVE_STDLIB_H
#include <stdlib.h>  /* For abort() */
#endif
#include <file.h>

/* this is used in a few places to define a noop "function". it is needed
   to stop "no effect" warnings from GCC. */
#define NOOP_FUNCTION if (0) ; else

/* ### define defaults for now; these will go away in a while */
#define REGISTER_CLEANUP(dbm, pdatum) NOOP_FUNCTION
#define SET_FILE(pdb, f) ((pdb)->file = (f))

typedef apr_vdbm_t *real_file_t;

typedef apr_vdbm_datum_t *cvt_datum_t;
#define CONVERT_DATUM(cvt, pinput) ((cvt) = (apr_vdbm_datum_t *)(pinput))

typedef apr_vdbm_datum_t result_datum_t;
#define RETURN_DATUM(poutput, rd) (*(poutput) = *(apr_datum_t *)&(rd))

#define APR_DBM_CLOSE(f)        apr_vdbm_close(f)
#define APR_DBM_FETCH(f, k, v)  apr_vdbm_fetch(f, &(v), *(k))
#define APR_DBM_STORE(f, k, v)  apr_vdbm_store(f, *(k), *(v), APR_VDBM_REPLACE)
#define APR_DBM_DELETE(f, k)    apr_vdbm_delete(f, *(k))
#define APR_DBM_FIRSTKEY(f, k)  apr_vdbm_firstkey(f, &(k))
#define APR_DBM_NEXTKEY(f, k, nk) apr_vdbm_nextkey(f, &(nk))
#define APR_DBM_FREEDPTR(dptr)  NOOP_FUNCTION

#define APR_DBM_DBMODE_RO       APR_READ
#define APR_DBM_DBMODE_RW       (APR_READ | APR_WRITE)
#define APR_DBM_DBMODE_RWCREATE (APR_READ | APR_WRITE | APR_CREATE)
#define APR_DBM_DBMODE_RWTRUNC  (APR_READ | APR_WRITE | APR_CREATE | \
                                 APR_TRUNCATE)

static apr_status_t set_error(apr_dbm_t *dbm, apr_status_t dbm_said)
{
    apr_status_t rv = APR_SUCCESS;

    /* ### ignore whatever the DBM said (dbm_said); ask it explicitly */

    if ((dbm->errcode = dbm_said) == APR_SUCCESS) {
        dbm->errmsg = NULL;
    }
    else {
        dbm->errmsg = "I/O error occurred.";
        rv = APR_EGENERAL;        /* ### need something better */
    }

    return rv;
}

/* --------------------------------------------------------------------------
**
** DEFINE THE VTABLE FUNCTIONS FOR VDBM
*/

static apr_status_t vt_vdbm_open(apr_dbm_t **pdb, const char *pathname,
                                 apr_int32_t mode, apr_fileperms_t perm,
                                 apr_pool_t *pool)
{
    real_file_t file;
    int dbmode;

    *pdb = NULL;

    switch (mode) {
    case APR_DBM_READONLY:
        dbmode = APR_DBM_DBMODE_RO;
        break;
    case APR_DBM_READWRITE:
        dbmode = APR_DBM_DBMODE_RW;
        break;
    case APR_DBM_RWCREATE:
        dbmode = APR_DBM_DBMODE_RWCREATE;
        break;
    case APR_DBM_RWTRUNC:
        dbmode = APR_DBM_DBMODE_RWTRUNC;
        break;
    default:
        return APR_EINVAL;
    }

    {
        apr_status_t rv;

        rv = apr_vdbm_open(&file, pathname, dbmode,
                           apr_posix_perms2mode(perm), pool);
        if (rv != APR_SUCCESS)
            return rv;
    }

    /* we have an open database... return it */
    *pdb = apr_pcalloc(pool, sizeof(**pdb));
    (*pdb)->pool = pool;
    (*pdb)->type = &apr_dbm_type_vdbm;
    SET_FILE(*pdb, file);

    /* ### register a cleanup to close the DBM? */

    return APR_SUCCESS;
}

static void vt_vdbm_close(apr_dbm_t *dbm)
{
    APR_DBM_CLOSE(dbm->file);
}

static apr_status_t vt_vdbm_fetch(apr_dbm_t *dbm, apr_datum_t key,
                                  apr_datum_t * pvalue)
{
    apr_status_t rv;
    cvt_datum_t ckey;
    result_datum_t rd;

    CONVERT_DATUM(ckey, &key);
    rv = APR_DBM_FETCH(dbm->file, ckey, rd);
    RETURN_DATUM(pvalue, rd);

    REGISTER_CLEANUP(dbm, pvalue);

    /* store the error info into DBM, and return a status code. Also, note
       that *pvalue should have been cleared on error. */
    return set_error(dbm, rv);
}

static apr_status_t vt_vdbm_store(apr_dbm_t *dbm, apr_datum_t key,
                                  apr_datum_t value)
{
    apr_status_t rv;
    cvt_datum_t ckey;
    cvt_datum_t cvalue;

    CONVERT_DATUM(ckey, &key);
    CONVERT_DATUM(cvalue, &value);
    rv = APR_DBM_STORE(dbm->file, ckey, cvalue);

    /* store any error info into DBM, and return a status code. */
    return set_error(dbm, rv);
}

static apr_status_t vt_vdbm_del(apr_dbm_t *dbm, apr_datum_t key)
{
    apr_status_t rv;
    cvt_datum_t ckey;

    CONVERT_DATUM(ckey, &key);
    rv = APR_DBM_DELETE(dbm->file, ckey);

    /* store any error info into DBM, and return a status code. */
    return set_error(dbm, rv);
}

static int vt_vdbm_exists(apr_dbm_t *dbm, apr_datum_t key)
{
    int exists;
    apr_vdbm_datum_t *ckey = (apr_vdbm_datum_t *)&key;

    {
        apr_vdbm_datum_t value;
        if (apr_vdbm_fetch(dbm->file, &value, *ckey) != APR_SUCCESS) {
            exists = 0;
        }
        else
            exists = value.dptr != NULL;
    }

    return exists;
}

static apr_status_t vt_vdbm_firstkey(apr_dbm_t *dbm, apr_datum_t * pkey)
{
    apr_status_t rv;
    result_datum_t rd;

    rv = APR_DBM_FIRSTKEY(dbm->file, rd);
    RETURN_DATUM(pkey, rd);

    REGISTER_CLEANUP(dbm, pkey);

    /* store any error info into DBM, and return a status code. */
    return set_error(dbm, rv);
}

static apr_status_t vt_vdbm_nextkey(apr_dbm_t *dbm, apr_datum_t * pkey)
{
    apr_status_t rv;
    cvt_datum_t ckey;
    result_datum_t rd;

    CONVERT_DATUM(ckey, pkey);
    rv = APR_DBM_NEXTKEY(dbm->file, ckey, rd);
    RETURN_DATUM(pkey, rd);

    REGISTER_CLEANUP(dbm, pkey);

    /* store any error info into DBM, and return a status code. */
    return set_error(dbm, APR_SUCCESS);
}

static void vt_vdbm_freedatum(apr_dbm_t *dbm, apr_datum_t data)
{
    APR_DBM_FREEDPTR(data.dptr);
}

static void vt_vdbm_usednames(apr_pool_t *pool, const char *pathname,
                              const char **used1, const char **used2)
{
    char *work;

    /* ### this could be optimized by computing strlen() once and using
       ### memcpy and pmemdup instead. but why bother? */

    *used1 = apr_pstrcat(pool, pathname, APR_VDBM_DIRFEXT, NULL);
    *used2 = work = apr_pstrdup(pool, *used1);

    /* we know the extension is 4 characters */
    memcpy(&work[strlen(work) - 4], APR_VDBM_PAGFEXT, 4);
}

APU_DECLARE(apr_status_t) apr_vdbm_open (apr_vdbm_t **db,
					 const char *name,
                                         apr_int32_t mode,
                                         apr_fileperms_t perms,
					 apr_pool_t *p)
{
VDBM *vdbm;
int flags;

/*
** Set the appropriate flags
*/
switch (mode)
    {
    case APR_DBM_DBMODE_RO:
	flags = O_RDONLY;
	break;
    case APR_DBM_DBMODE_RW:
	flags = O_RDWR;
	break;
    case APR_DBM_DBMODE_RWCREATE:
	flags = O_RDWR | O_CREAT;
	break;
    case APR_DBM_DBMODE_RWTRUNC:
	flags = O_RDWR | O_CREAT | O_TRUNC;
	break;
    default:
        return APR_EINVAL;
    }

/*
** Open the file
*/
vdbm = vdbm_open ((char *) name, flags, perms);
if (vdbm == NULL)
    return (APR_ENOMEM);

#if 0
/*
** Register the pool cleanup
*/
apr_pool_cleanup_register (p,
			   vdbm,
			   vdbm_cleanup_callback,
			   apr_pool_cleanup_null);
#endif

/*
** Save the pool reference and update the database pointer
*/
vdbm->pool = p;
*db = vdbm;

/*
** Return success
*/
return (APR_SUCCESS);

}

APU_DECLARE(apr_status_t) apr_vdbm_close (apr_vdbm_t *db)
{

/*
** Close the file
*/
vdbm_close (db);

#if 0
/*
** Run the pool cleanup
*/
apr_pool_cleanup_run (db->pool,
		      db,
		      vdbm_cleanup_callback);
#endif

/*
** Return success
*/
return (APR_SUCCESS);

}

APU_DECLARE(apr_status_t) apr_vdbm_lock (apr_vdbm_t *db,
					 int type)
{
#if 0
apr_status_t status;

if (! (type == APR_FLOCK_SHARED || type == APR_FLOCK_EXCLUSIVE))
    return APR_EINVAL;

if (db->flags & VDBM_EXCLUSIVE_LOCK)
    {
    ++db->lckcnt;
    return APR_SUCCESS;
    }
else
    if (db->flags & VDBM_SHARED_LOCK)
	{
        /*
        ** Cannot promote a shared lock to an exlusive lock
        ** in a cross-platform compatibile manner.
        */
        if (type == APR_FLOCK_EXCLUSIVE)
            return APR_EINVAL;
        ++db->lckcnt;
        return APR_SUCCESS;
	}

/*
** zero size: either a fresh database, or one with a single,
** unsplit data page: dirpage is all zeros.
*/
status = vdbm_lock (db, type);
if (status == 0)
    {
    ++db->lckcnt;
    if (type == APR_FLOCK_SHARED)
	db->flags |= VDBM_SHARED_LOCK;
    else
	if (type == APR_FLOCK_EXCLUSIVE)
	    db->flags |= VDBM_EXCLUSIVE_LOCK;
    status = APR_SUCCESS;
    }
else
    status = errno;

return status;
#else
return (APR_ENOTIMPL);
#endif
}

APU_DECLARE(apr_status_t) apr_vdbm_unlock (apr_vdbm_t *db)
{
#if 0
apr_status_t status;

if (! (db->flags & (VDBM_SHARED_LOCK | VDBM_EXCLUSIVE_LOCK)))
    return APR_EINVAL;

if (--db->lckcnt > 0)
    return APR_SUCCESS;
db->flags &= ~(VDBM_SHARED_LOCK | VDBM_EXCLUSIVE_LOCK);

status = vdbm_unlock (db);
if (status == 0)
    status = APR_SUCCESS;
else
    status = errno;

return status;
#else
return (APR_ENOTIMPL);
#endif
}

APU_DECLARE(apr_status_t) apr_vdbm_fetch (apr_vdbm_t *db,
                                          apr_vdbm_datum_t *value,
                                          apr_vdbm_datum_t key)
{
apr_vdbm_datum_t retval = nullitem;
apr_status_t status;

retval = vdbm_fetch (db, key);
if (retval.dsize)
    status = APR_SUCCESS;
else
   status = errno;

*value = retval;

return (status);

}

APU_DECLARE(apr_status_t) apr_vdbm_store (apr_vdbm_t *db,
					  apr_vdbm_datum_t key,
                                          apr_vdbm_datum_t value,
					  int opt)
{
apr_status_t status;
int flags;

status = vdbm_store (db, key, value, opt);
if (status == 0)
    status = APR_SUCCESS;
else
    status = errno;

return (status);

}

APU_DECLARE(apr_status_t) apr_vdbm_delete (apr_vdbm_t *db,
                                           const apr_vdbm_datum_t key)
{
apr_status_t status;

status = vdbm_delete (db, key);
if (status == 0)
    status = APR_SUCCESS;
else
    status = errno;

return (status);

}

APU_DECLARE(apr_status_t) apr_vdbm_firstkey (apr_vdbm_t *db,
					     apr_vdbm_datum_t *key)
{
apr_vdbm_datum_t retval = nullitem;
apr_status_t status;

retval = vdbm_firstkey (db);
if (retval.dsize)
    status = APR_SUCCESS;
else
    status = errno;

*key = retval;

return (status);

}

APU_DECLARE(apr_status_t) apr_vdbm_nextkey (apr_vdbm_t *db,
					    apr_vdbm_datum_t *key)
{
apr_vdbm_datum_t retval = nullitem;
apr_status_t status;

retval = vdbm_nextkey (db);
if (retval.dsize)
    status = APR_SUCCESS;
else
    status = errno;

*key = retval;

return (status);

}

APU_DECLARE(int) apr_vdbm_rdonly (apr_vdbm_t *db)
{

return (APR_ENOTIMPL);

}

APU_DECLARE_DATA const apr_dbm_type_t apr_dbm_type_vdbm = {
    "vdbm",

    vt_vdbm_open,
    vt_vdbm_close,
    vt_vdbm_fetch,
    vt_vdbm_store,
    vt_vdbm_del,
    vt_vdbm_exists,
    vt_vdbm_firstkey,
    vt_vdbm_nextkey,
    vt_vdbm_freedatum,
    vt_vdbm_usednames
};

#endif /* APU_HAVE_VDBM */
