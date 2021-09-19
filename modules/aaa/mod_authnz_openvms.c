/* ====================================================================
 * Copyright (c) 1995 The Apache Group.  All rights reserved.
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
 * 3. All advertising materials mentioning features or use of this
 *    software must display the following acknowledgment:
 *    "This product includes software developed by the Apache Group
 *    for use in the Apache HTTP server project (http://www.apache.org/)."
 *
 * 4. The names "Apache Server" and "Apache Group" must not be used to
 *    endorse or promote products derived from this software without
 *    prior written permission.
 *
 * 5. Redistributions of any form whatsoever must retain the following
 *    acknowledgment:
 *    "This product includes software developed by the Apache Group
 *    for use in the Apache HTTP server project (http://www.apache.org/)."
 *
 * THIS SOFTWARE IS PROVIDED BY THE APACHE GROUP ``AS IS'' AND ANY
 * EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE APACHE GROUP OR
 * IT'S CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Group and was originally based
 * on public domain software written at the National Center for
 * Supercomputing Applications, University of Illinois, Urbana-Champaign.
 * For more information on the Apache Group and the Apache HTTP server
 * project, please see <http://www.apache.org/>.
 *
 */

/*
**  FACILITY:
**	Compaq Secure Web Server for OpenVMS Alpha
**
**  ABSTRACT:
**	This module provides a basic routine for authenticating a user based
**	on a username and password contained in the SYSUAF records.
**
**      NOTE: This module is based on MOD_AUTH_NIS.C.  Credit goes to
**      Dirk.vanGulik@jrc.it, the author of that module.
**
**  RESTRICTION:
**	There is no support for secondary passwords or external authentication
**	at this time.  (The MOD_AUTH_xxx services only provide one password.)
**
**  CREATION DATE:  November 29, 2000
**
**  AUTHORS:
**	Kevin D. O'Kelley
**
**  MODIFICATION HISTORY:
**
**	000	KDO/29-Nov-2000
**		Created.
**
** 	001	RJB/02-Feb-2001
**		vms_auth_princ_sysuaf syntax has changed to require
**		the caller to supply the client's host-name string.
**
**	002	KDO/02-Nov-2001
**		Add authoritative/non-authoritative support.  Add support
**		for case-insensitive username comparison.  Add support for
**		the "require group" directive.
**
**	003	MPD/15-Jan-2003
**		Added Apache 2.0 API changes
**
*/


/*
**  Include files
*/
#include <ctype.h>
#include <types.h>
#include <limits.h>
#include <string.h>

#include <ssdef.h>
#include <kgbdef.h>
#include <lgidef.h>
#include <stsdef.h>
#include <descrip.h>
#include <starlet.h>
#include <builtins.h>

#ifdef SHADOW
#undef SHADOW
#endif
#ifdef MULTITHREADING
#undef MULTITHREADING
#endif

#include "apr_strings.h"

#include "ap_config.h"
#include "ap_provider.h"
#include "httpd.h"
#include "http_config.h"
#include "http_core.h"
#include "http_log.h"
#include "http_protocol.h"
#include "http_request.h"

#include "mod_auth.h"
#include "protshr.h"

/*
**  Conditional asssembly macros
*/
#ifndef __VMS_AUTH_SYSUAF_DEBUG
#define __VMS_AUTH_SYSUAF_DEBUG 0		/* 1= debugging */
#endif
#if     __VMS_AUTH_SYSUAF_DEBUG
#define DEBUG_PRINTF(ARGS) printf ARGS
#define DEBUG_OFF (0)
#define DEBUG_ON (1)
#include <stdio.h>
#else
#define DEBUG_PRINTF(ARGS)
#define DEBUG_OFF (1)
#define DEBUG_ON (0)
#endif


/*
**  Definitions
*/
#ifndef INTERNAL
#define INTERNAL static
#endif
#ifndef NULL
#define NULL (void *) 0
#endif
#ifndef alloca
#define alloca __ALLOCA
#endif

#define BAD_STATUS(I) (((I) & STS$M_SUCCESS) == 0)
#define GOOD_STATUS(I) (((I) & STS$M_SUCCESS) == 1)


/*
**  Data structures
*/
typedef unsigned int VMS_STATUS;
typedef struct dsc$descriptor_s DSC_S;

typedef struct {
        int dummy;  			/* just here to stop compiler warnings for now. */
} authz_openvms_config_rec;


typedef struct				/* temp structure for identifiers */
{
    int     iUsed;			/* number of identifiers */
    int     iAllocated;			/* number of slots allocated */
    int     aiIds[10];			/* initial allocation of space */
}
IDENT_LIST;


/*
**  Function prototypes
*/
INTERNAL int save_identifier(const char *pcszName, IDENT_LIST **ppident);

module AP_MODULE_DECLARE_DATA authnz_openvms_module;


void *create_authnz_openvms_context (apr_pool_t *p, char *d)
{
    authz_openvms_config_rec *conf = (authz_openvms_config_rec *) apr_pcalloc (p, sizeof(authz_openvms_config_rec));
    return (void *) conf;
}


static const command_rec authnz_openvms_cmds[] =
{
    {NULL}
};


static authn_status authn_openvms_check_password(request_rec *r, const char *user, const char *password)
{

    unsigned int st;
    conn_rec *c = r->connection;
    const char *pcszHostName;
    char errstr[MAX_STRING_LEN];
    int res;

    pcszHostName = ap_get_remote_host ( r->connection, r->per_dir_config,
			REMOTE_NOLOOKUP, 0 );

    st = apr$$auth_princ_sysuaf (
            (const char *) user, strlen(user),
            password, strlen(password),
            pcszHostName, strlen(pcszHostName) );
    if (st == LGI$_NOSUCHUSER)
    {
        ap_log_rerror(APLOG_MARK, APLOG_NOERRNO|APLOG_ERR, 0, r,
               "OpenVMS user %s not found", user);
        return AUTH_USER_NOT_FOUND;
    }
    if (BAD_STATUS(st))
    {
        apr_snprintf(errstr, sizeof(errstr),
                "OpenVMS authorization failure %s status=0x%08x",
                user, st);
	ap_log_error(APLOG_MARK, APLOG_ERR, 0, r->server,
                "access to %s failed for %s, reason: %s",
		r->uri,
                ap_get_remote_host(r->connection, r->per_dir_config, REMOTE_NAME, 0),
                errstr);
       return AUTH_GENERAL_ERROR;
    }
    return AUTH_GRANTED;
}


static int check_access(request_rec *r, int fAuthorized, IDENT_LIST *pident)
{
    unsigned int st;

    if ((fAuthorized == 0) && (pident != NULL))
    {
	/* No "require valid-user" or a matching "require user" directive but we did find one or more
           "require group" directives. See if the user is a member of the specified group UICs or if
           the account has any of the specified identifiers granted to it. This also tests whether the
	   account is valid */

        st = apr$$check_rights_list(r->user, strlen(r->user), (const unsigned int *) &pident->aiIds[0], pident->iUsed);

        if (GOOD_STATUS(st))
        {
            return (AUTHZ_GRANTED);
        }
    }
    else
    {
	/* Just test validity of the account */

        st = apr$$check_rights_list(r->user, strlen(r->user), (const unsigned int *) NULL, 0);

        if (GOOD_STATUS(st))
        {
            return (AUTHZ_GRANTED);
        }
    }

    if (st == LGI$_NOSUCHUSER)
    {
        ap_log_rerror(APLOG_MARK, APLOG_NOERRNO|APLOG_ERR, 0, r, "access to %s failed, reason: OpenVMS user %s not found", r->uri, r->user);
        return (AUTHZ_DENIED);
    }

    if (st == SS$_NOCALLPRIV)
    {
        ap_log_rerror(APLOG_MARK, APLOG_NOERRNO|APLOG_ERR, 0, r, "access to %s failed, reason: user %s is not allowed", r->uri, r->user);
        return (AUTHZ_DENIED);
    }

    ap_log_rerror(APLOG_MARK, APLOG_NOERRNO|APLOG_ERR, 0, r, "access to %s failed, reason: unexpected error 0x%x (user %s)", r->uri, st, r->user);
    return (AUTHZ_DENIED);
}


/*
**  s a v e _ i d e n t i f i e r
**
**  Add an identifier to the internal list.  If necesssary, expand the list.
**  Return with zero for success or minus one for failure.  However, if the
**  "require group" value is not an identifier, ignore it (not an error).
**
**  NOTE: For simplicity, we use the realloc() function to expand an existing
**  array.  However, this function is not available to the low level pool
**  allocation routines.  Therefore, we must ensure that the structure is
**  deallocated in all cases.
*/
INTERNAL int save_identifier(const char *pcszName, IDENT_LIST **ppident)
{
    int i;
    VMS_STATUS st;
    unsigned int uiId;
    unsigned int uiAttrib;
    IDENT_LIST *pidentNew;
    DSC_S dscsName;


    /*
    **  Convert the name to an identifier.
    */
    dscsName.dsc$b_dtype = DSC$K_DTYPE_T;
    dscsName.dsc$b_class = DSC$K_CLASS_S;
    dscsName.dsc$a_pointer = (char *) pcszName;
    dscsName.dsc$w_length = strlen(dscsName.dsc$a_pointer);
    if (dscsName.dsc$w_length == 0)
        return(0);
    if (dscsName.dsc$w_length > CHAR_MAX)
    {
        errno = EINVAL;
        return(-1);
    }
    st = sys$asctoid(&dscsName, &uiId, &uiAttrib);
    if (BAD_STATUS(st))
    {
        DEBUG_PRINTF(("(save_identifier) sys$asctoid(\"%.*s\") status=0x%x\n",
                dscsName.dsc$w_length, dscsName.dsc$a_pointer, st));
        return(0);
    }


    /*
    **  Only keep plain identifiers (i.e. no attributes), resource identifiers,
    **  and group ids.  User names are also accepted here, but they are not
    **  relevant: user names are immediately handled by a string comparison.
    */
    if (uiAttrib != (uiAttrib & (KGB$M_RESOURCE | KGB$M_DYNAMIC)))
        return(0);
    DEBUG_PRINTF(("(save_identifier) \"%.*s\" = 0x%08x (attrib=0x%08x)\n",
            dscsName.dsc$w_length, dscsName.dsc$a_pointer, uiId, uiAttrib));


    /*
    **  Save the value in the IDENT_LIST structure.
    */
    if ((*ppident) == NULL)
    {
        (*ppident) = malloc(sizeof(**ppident));
        if ((*ppident) == NULL)
            return(-1);
        (*ppident)->iUsed = 0;
        (*ppident)->iAllocated = sizeof((*ppident)->aiIds) /
                sizeof((*ppident)->aiIds[0]);
    }
    else
    {
        if ((*ppident)->iUsed >= (*ppident)->iAllocated)
        {
            pidentNew = (IDENT_LIST *) realloc((*ppident), sizeof(**ppident) +
                    ((*ppident)->iAllocated * sizeof((*ppident)->aiIds[0])));
            if (pidentNew == NULL)
                return(-1);
            (*ppident) = pidentNew;
            (*ppident)->iAllocated += sizeof((*ppident)->aiIds) /
                    sizeof((*ppident)->aiIds[0]);
        }
    }
    (*ppident)->aiIds[(*ppident)->iUsed++] = uiId;
    return(0);
}


static const char *parse_config(cmd_parms *cmd, const char *require_line,
                                     const void **parsed_require_line)
{
    const char *expr_err = NULL;
    ap_expr_info_t *expr;

    expr = ap_expr_parse_cmd(cmd, require_line, AP_EXPR_FLAG_STRING_RESULT,
            &expr_err, NULL);

    if (expr_err)
        return apr_pstrcat(cmd->temp_pool,
                           "Cannot parse expression in require line: ",
                           expr_err, NULL);

    *parsed_require_line = expr;

    return NULL;
}

static authz_status group_check_authorization(request_rec *r,
                                              const char *require_args,
                                              const void *parsed_require_args)
{
    const char *err = NULL;
    const ap_expr_info_t *expr = parsed_require_args;
    const char *require;
    char *user = r->user;
    const char *t, *w;
    int rv;
    IDENT_LIST *pident = NULL;

    if (!user) {
        return (AUTHZ_DENIED_NO_USER);
    }

    require = ap_expr_str_exec(r, expr, &err);
    if (err) {
        ap_log_rerror(APLOG_MARK, APLOG_ERR, 0, r, APLOGNO(02592)
                      "authz_groupfile authorize: require group: Can't "
                      "evaluate require expression: %s", err);
        return AUTHZ_DENIED;
    }

    t = require;

    while ((w = ap_getword_conf(r->pool, &t)) && w[0]) {
        if (save_identifier(w, &pident)) {
            if (pident != NULL) {
                free(pident);
            }

            ap_log_rerror(APLOG_MARK, APLOG_NOERRNO|APLOG_ERR, 0, r, "internal error: cannot save groups, errno=%d (user %s)", errno, r->user);

            return (AUTHZ_DENIED);
        }
    }

    rv = check_access(r, 0, pident);

    if (pident != NULL) {
       free(pident);
    }

    return (rv);
}


static authz_status user_check_authorization(request_rec *r,
                                             const char *require_args,
                                             const void *parsed_require_args)
{
    const char *err = NULL;
    const ap_expr_info_t *expr = parsed_require_args;
    const char *require;

    const char *t, *w;

    if (!r->user) {
        return (AUTHZ_DENIED_NO_USER);
    }

    require = ap_expr_str_exec(r, expr, &err);
    if (err) {
        ap_log_rerror(APLOG_MARK, APLOG_ERR, 0, r, APLOGNO(02594)
                      "authz_user authorize: require user: Can't "
                      "evaluate require expression: %s", err);
        return AUTHZ_DENIED;
    }

    t = require;
    while ((w = ap_getword_conf(r->pool, &t)) && w[0]) {
        if (!strcasecmp(r->user, w)) {			/* BRC 16-Aug-2018; changed from strcmp() */
           /* Usernames match, but is the user valid? */
           return (check_access(r, 1, NULL));
        }
    }

    ap_log_rerror(APLOG_MARK, APLOG_DEBUG, 0, r, APLOGNO(01663)
                  "access to %s failed, reason: user '%s' does not meet "
                  "'require'ments for user to be allowed access",
                  r->uri, r->user);

    return (AUTHZ_DENIED);
}

static authz_status validuser_check_authorization(request_rec *r,
                                                  const char *require_line,
                                                  const void *parsed_require_line)
{
    if (!r->user) {
        return AUTHZ_DENIED_NO_USER;
    }

    /* Verify that this is a valid user */
    return (check_access(r, 1, NULL));
}

static const authn_provider authn_openvms_provider =
{
    &authn_openvms_check_password,
};

static const authz_provider authz_group_provider =
{
    &group_check_authorization,
    &parse_config,
};

static const authz_provider authz_user_provider =
{
    &user_check_authorization,
    &parse_config,
};

static const authz_provider authz_validuser_provider =
{
    &validuser_check_authorization,
    NULL,
};


static void register_hooks(apr_pool_t *p)
{
    /* Register authn provider */
    ap_register_auth_provider(p, AUTHN_PROVIDER_GROUP, "OpenVMS",
                              AUTHN_PROVIDER_VERSION,
                              &authn_openvms_provider, AP_AUTH_INTERNAL_PER_CONF);

    /* Register authz providers */
    ap_register_auth_provider(p, AUTHZ_PROVIDER_GROUP, "group",
                              AUTHZ_PROVIDER_VERSION,
                              &authz_group_provider,
                              AP_AUTH_INTERNAL_PER_CONF);

    ap_register_auth_provider(p, AUTHZ_PROVIDER_GROUP, "user",
                              AUTHZ_PROVIDER_VERSION,
                              &authz_user_provider, AP_AUTH_INTERNAL_PER_CONF);

    ap_register_auth_provider(p, AUTHZ_PROVIDER_GROUP, "valid-user",
                              AUTHZ_PROVIDER_VERSION,
                              &authz_validuser_provider,
                              AP_AUTH_INTERNAL_PER_CONF);

}

module AP_MODULE_DECLARE_DATA authnz_openvms_module =
{
    STANDARD20_MODULE_STUFF,
    create_authnz_openvms_context,	/* dir config creater */
    NULL,				/* dir merger --- default is to override */
    NULL,				/* server config */
    NULL,				/* merge server config */
    authnz_openvms_cmds,		/* command apr_table_t */
    register_hooks			/* register hooks */
};
