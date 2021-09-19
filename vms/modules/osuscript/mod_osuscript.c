/* ====================================================================
 * Copyright (c) 1995-1999 The Apache Group.  All rights reserved.
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
 *    prior written permission. For written permission, please contact
 *    apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
 *
 * 6. Redistributions of any form whatsoever must retain the following
 *    acknowledgment:
 *    "This product includes software developed by the Apache Group
 *    for use in the Apache HTTP server project (http://www.apache.org/)."
 *
 * THIS SOFTWARE IS PROVIDED BY THE APACHE GROUP ``AS IS'' AND ANY
 * EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE APACHE GROUP OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
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
 * Apache Module to provide the OSU scriptserver environment for running
 * existing OSU-based scripts under apache.
 *
 * To install this modue:
 *    1. Include an AddModule line in the src/configuration file (e.g.
 *	 "AddModule os/mod_osuscript.o") near the location of the
 *	 CGI addmodule line.  Then rebuild the server to include the module.
 *
 *    2. Edit the server's runtime configuration (conf/httpd.conf) to
 *	 declare a location handled by the module, as follows:
 *	    <Location /htbin>
 *		SetHandler osuscript-handler
 *		OSUscript 0::"WWWEXEC" www_root:[bin]
 *		Order allow,deny
 *		Allow from all
 *	    </Location>
 *
 *    3. Place the OSU server's WWWEXEC.COM in the apache server's
 *	 login directory and make www_root:[bin] (which contains the
 *	 OSU scripts) accessible from the apache server.
 *
 *-----------------------------------------------------------------------------
 * Modification History:
 *
 *	22-May-2001	Scott LePage
 *	Add fix by Dave Jones to handle URL parameters properly.  Perl scripts
 *	using the 'import_names' function would have the first character of
 *	the imported variable name stripped off.
 *
 *	21-March-2002	Steve Fesler, Helder Mendes
 *	NET_XXX processes hang around after redirects with LOCATION HEADER
 *	parameter.  Add dnet_disconnect call before returns.
 *
 *	25-March-2003	Matthew Doremus
 *	Apache 2.0 Port.
 *
 *	12-April-2004	Richard Barry
 *	Remove OSUscript_merge_dir_config from osuscript_module structure.
 *	We want the server to default the directory merge operation (take the
 *	proposed "new" config record) like we did in Apache 1.3.
 *
 */

#include "httpd.h"
#include "http_config.h"
#include "http_core.h"
#include "http_log.h"
#include "http_main.h"
#include "http_request.h"
#include "http_protocol.h"
#include "util_script.h"
#include "http_connection.h"
#include "apr_strings.h"
#include "apr_lib.h"
#include <stdio.h>

#include "decnet_io.h"

int vms_child_id;
#define child_id vms_child_id
/*--------------------------------------------------------------------------*/
/*                                                                          */
/* Data declarations.                                                       */
/*                                                                          */
/* Here are the static cells and structure declarations private to our      */
/* module.                                                                  */
/*                                                                          */
/*--------------------------------------------------------------------------*/

/*
 * Sample configuration record.  Used for both per-directory and per-server
 * configuration data.
 *
 * It's perfectly reasonable to have two different structures for the two
 * different environments.  The same command handlers will be called for
 * both, though, so the handlers need to be able to tell them apart.  One
 * possibility is for both structures to start with an int which is zero for
 * one and 1 for the other.
 *
 * Note that while the per-directory and per-server configuration records are
 * available to most of the module handlers, they should be treated as
 * READ-ONLY by all except the command and merge handlers.  Sometimes handlers
 * are handed a record that applies to the current location by implication or
 * inheritance, and modifying it will change the rules for other locations.
 */
typedef struct excfg {
    int cmode;                  /* Environment to which record applies (directory,
                                 * server, or combination).
                                 */
#define CONFIG_MODE_SERVER 1
#define CONFIG_MODE_DIRECTORY 2
#define CONFIG_MODE_COMBO 3     /* Shouldn't ever happen. */
    int local;                  /* Boolean: "Example" directive declared here? */
    int congenital;             /* Boolean: did we inherit an "Example"? */
    char *dir_template;		/* name of 'exec' directory (e.g. /htbin) */
    char *taskspec;		/* Decnet task specification */
    char *bindir;		/* argument to fee to task */
} excfg;

/*
 * Let's set up a module-local static cell to point to the accreting callback
 * trace.  As each API callback is made to us, we'll tack on the particulars
 * to whatever we've already recorded.  To avoid massive memory bloat as
 * directories are walked again and again, we record the routine/environment
 * the first time (non-request context only), and ignore subsequent calls for
 * the same routine/environment.
 */
static const char *trace = NULL;
static apr_table_t *static_calls_made = NULL;

/*
 * To avoid leaking memory from pools other than the per-request one, we
 * allocate a module-private pool, and then use a sub-pool of that which gets
 * freed each time we modify the trace.  That way previous layers of trace
 * data don't get lost.
 */
static apr_pool_t *example_pool = NULL;
static apr_pool_t *example_subpool = NULL;

/*
 * Declare ourselves so the configuration routines can find and know us.
 * We'll fill it in at the end of the module.
 */
module osuscript_module;

/*--------------------------------------------------------------------------*/
/*                                                                          */
/* These routines are strictly internal to this module, and support its     */
/* operation.  They are not referenced by any external portion of the       */
/* server.                                                                  */
/*                                                                          */
/*--------------------------------------------------------------------------*/

/*
 * Locate our directory configuration record for the current request.
 */
static excfg *our_dconfig(request_rec *r)
{
    return (excfg *) ap_get_module_config(r->per_dir_config, &osuscript_module);
}

/*
 * Locate our server configuration record for the specified server.
 */
static excfg *our_sconfig(server_rec *s)
{

    return (excfg *) ap_get_module_config(s->module_config, &osuscript_module);
}

/*
 * Likewise for our configuration record for the specified request.
 */
static excfg *our_rconfig(request_rec *r)
{

    return (excfg *) ap_get_module_config(r->request_config, &osuscript_module);
}

/*
 * This routine sets up some module-wide cells if they haven't been already.
 */
static void setup_module_cells()
{
    /*
     * If we haven't already allocated our module-private pool, do so now.
     */
    if (example_pool == NULL) {
	dnet_initialize();
        apr_pool_create_ex(&example_pool, NULL, NULL, NULL);
    }
    /*
     * Likewise for the table of routine/environment pairs we visit outside of
     * request context.
     */
    if (static_calls_made == NULL) {
        static_calls_made = apr_table_make(example_pool, 16);
    }
}


/*--------------------------------------------------------------------------*/
/* We prototyped the various syntax for command handlers (routines that     */
/* are called when the configuration parser detects a directive declared    */
/* by our module) earlier.  Now we actually declare a "real" routine that   */
/* will be invoked by the parser when our "real" directive is               */
/* encountered.                                                             */
/*                                                                          */
/* If a command handler encounters a problem processing the directive, it   */
/* signals this fact by returning a non-NULL pointer to a string            */
/* describing the problem.                                                  */
/*                                                                          */
/* The magic return value DECLINE_CMD is used to deal with directives       */
/* that might be declared by multiple modules.  If the command handler      */
/* returns NULL, the directive was processed; if it returns DECLINE_CMD,    */
/* the next module (if any) that declares the directive is given a chance   */
/* at it.  If it returns any other value, it's treated as the text of an    */
/* error message.                                                           */
/*--------------------------------------------------------------------------*/
/*
 * Command handler for the OSUscript  directive.  All we do is mark the
 * call in the trace log, and flag the applicability of the directive to the
 * current location in that location's configuration record.
 */
static const char *cmd_OSUscript(cmd_parms *cmd, void *mconfig,
	const char *taskspec, const char *bindir )
{
    excfg *cfg = (excfg *) mconfig;

    /*
     * Save the arguments in the configuration record, taking defaults.
     */
#ifdef DEBUG
printf("cmd_example called with arg '%s', '%s', cfg: %x pid: %x\n",
	taskspec ? taskspec : "<NULL>", bindir ? bindir : "<NULL>", cfg,getpid() );
printf("   arguments apply to path '%s'\n", cfg->dir_template ?
cfg->dir_template : "<NULL>" );
#endif

    if ( NULL == taskspec ) taskspec = "SYS$NODE::\"0=WWWEXEC\"";
    if ( NULL == bindir ) bindir = "WWW_ROOT:[BIN]";

    cfg->taskspec = apr_palloc(cmd->pool, strlen(taskspec) + 1 );
    if ( cfg->taskspec) strcpy ( cfg->taskspec, taskspec );

    cfg->bindir = apr_palloc(cmd->pool, strlen(bindir) + 1 );
    if ( cfg->bindir) strcpy ( cfg->bindir, bindir );
    /*
     * Parse the taskspec and generate an expanded list if node is a
     * multi-valued logical name.
     */

    cfg->local = 1;
    return NULL;
}
/* Obtain the Request-URI from the original request-line, returning
 * a new string from the request pool containing the URI or "".
 */
static char *original_uri(request_rec *r)
{
    char *first, *last;

    if (r->the_request == NULL) {
        return (char *) apr_pcalloc(r->pool, 1);
    }

    first = r->the_request;     /* use the request-line */

    while (*first && !apr_isspace(*first)) {
        ++first;                /* skip over the method */
    }
    while (apr_isspace(*first)) {
        ++first;                /*   and the space(s)   */
    }

    last = first;
    while (*last && !apr_isspace(*last)) {
        ++last;                 /* end at next whitespace */
    }

    return apr_pstrndup(r->pool, first, last - first);
}
/*
 * Table scanning routine.
 */
static int xfer_header ( void *link, const char *key, const char *value )
{
    char iobuf[4096];
    int key_len, val_len, status, xferred;

    key_len = strlen ( key );
    val_len = strlen ( value );
    if ( (key_len + val_len) < (sizeof(iobuf)-4) ) {
	sprintf ( iobuf, "%s: %s", key, value );
	status = dnet_write ( link, iobuf, (key_len + val_len + 2), &xferred );
	if ( (status&1) == 0 ) return 0;	/* abort on error */
    }
    return 1;		/* success */
}
/*--------------------------------------------------------------------------*/
/* Handle 'dialog' phase of the scriptserver protocol.  The remote client
 * sends special tags.  Return value is the terminal opcode (DNET_TEXT,
 * DNET_RAW, DNET_CGI) or -1 if error.  On error r->status and r->status_line
 * are set.
 */
static enum opcodes { DNET_HDR, DNET_ARG, DNET_ARG2, DNET_INPUT,
	DNET_TEXT, DNET_RAW, DNET_RQURL, DNET_CGI, DNET_HOST, DNET_ID,
	DNET_BINDIR, DNET_PATH, DNET_XLATE, DNET_XLATEV, DNET_SENTINEL,
	DNET_INVCACHE, DNET_RECMODE, DNET_RECMODE2, DNET_ID2, DNET_MANAGE,
	DNET_REUSE, DNET_FORCEKA } dummy;

static struct { enum opcodes opc; 	/* opcode */
		int l; 			/* Length of tag name (s) */
		char *s; 		/* Tag name */
		int terminal; }	 	/* If true, ends session */
    tag_list[] = {
	{ DNET_HDR, 9, "<DNETHDR>", 0 }, 	/* Send header */
	{ DNET_ARG, 9, "<DNETARG>", 0 }, 	/* Send search arg */
	{ DNET_ARG2, 10, "<DNETARG2>", 0 },	/* Send trunc. search arg */
	{ DNET_INPUT, 11, "<DNETINPUT>", 0 },	/* Send client data */
	{ DNET_TEXT, 10, "<DNETTEXT>", 1 }, 	/* Read text response */
	{ DNET_RAW, 9, "<DNETRAW>", 1 }, 	/* Read HTTP response */
	{ DNET_RQURL, 11, "<DNETRQURL>", 0 },	/* Send original URL */
	{ DNET_CGI, 9, "<DNETCGI>", 1 }, 	/* Read 'CGI' response */
	{ DNET_HOST, 10,"<DNETHOST>", 0 },	/* Send server host */
	{ DNET_ID, 8,"<DNETID>", 0 },		/* Send connection info */
	{ DNET_BINDIR, 12,"<DNETBINDIR>", 0 },	/* Send htbin/exec directory */
	{ DNET_PATH, 10, "<DNETPATH>", 0 },	/* Send htbin/exec prefix */
        { DNET_XLATE, 11, "<DNETXLATE>", 0 },	/* Translate URL by rules file*/
        { DNET_XLATEV, 12, "<DNETXLATEV>", 0 },	/* Translate URL by rules file*/
	{ DNET_INVCACHE, 14, "<DNETINVCACHE>", 0 },
        { DNET_RECMODE, 13, "<DNETRECMODE>", 0 },
        { DNET_RECMODE2, 14, "<DNETRECMODE2>", 0 },
	{ DNET_ID2, 9, "<DNETID2>", 0 },	/* extended ID2 */
	{ DNET_MANAGE, 12, "<DNETMANAGE>", 0 }, /* Managment command */
	{ DNET_REUSE, 11, "<DNETREUSE>", 0 },   /* keep decnet link open */
	{ DNET_FORCEKA, 13, "<DNETFORCEKA>", 0 }, /* explicit KA */
	{ DNET_SENTINEL, -1, "Sentinel", 1 }
    };
static int dialog_phase (
	request_rec *r, 		/* Apache request record */
	void *link, 			/* DECnet I/O context */
	excfg *dcfg, 			/* Module configuration record */
	int *rec_mode )			/* I/O mode: stream or record */
{
    int status, xferred, i, opcode, terminal, length, content_status;
    char *sts_line, *buf, iobuf[4096];
    /*
     * Content_status indicates whether we've call should_client_block()
     * yet:
     *    0 	Initial state, should_client_block not called (defer
     *		the call until first <DNETINPUT> tag received.
     *    1     There is client input.
     *    2	This is no client input.
     */
    content_status = 0;		/* unknown */
    /*
     * main loop.
     */
    for ( terminal=0; terminal==0; ) {
	/*
	 * Read input from script, all tags must start with <DNET
	 */
	status = dnet_read ( link, iobuf, sizeof(iobuf)-1, &xferred );
	if ( (status&1) == 0 ) break;
	iobuf[xferred] = '\0';
#ifdef DEBUG
	printf("Dialog message from DECnet task: '%s', %d\n", iobuf, xferred );
#endif
	if ( (xferred < 7) || (0 != strncmp(iobuf,"<DNET",5)) ) {
	    r->status = 500;
	    sts_line = apr_palloc ( r->pool, 260 );
	    sprintf ( sts_line, "500 protocol error in DECnet task" );
	    r->status_line = sts_line;
	    return -1;
	}
	/*
	 * Match tag against table.
	 */
	length = xferred-5;
	for ( i = 0; tag_list[i].l > 0; i++ ) {
	    if ( (xferred == tag_list[i].l) &&
		(0==strncmp(&iobuf[5],&tag_list[i].s[5], length)) ) break;
	}
	terminal = tag_list[i].terminal;
	opcode = tag_list[i].opc;
	/*
	 * Dispatch on the opcode found.
	 */
	switch ( opcode ) {
	  case DNET_HDR:
		/*
		 * return raw headers from request.
		 */
		apr_table_do ( xfer_header, link, r->headers_in, (char *) 0 );
		status = dnet_write ( link, "", 0, &xferred );
	   break;
	  case DNET_ARG:
	  case DNET_ARG2:
           if ( r->args ) {
		/*
		 * Prepend r->args with '?' to match scriptserver protocol,
		 * taking care not to overflow scratch buffer.
		 */
                iobuf[0] = '?';
                strncpy ( &iobuf[1], r->args, sizeof(iobuf)-1 );
                iobuf[sizeof(iobuf)-1] = '\0';
                buf = iobuf;
           } else {
                buf = "";                       /* no args present */
           }
	   i = strlen ( buf );
	   if ( opcode == DNET_ARG ) {
		if ( i > 4095 ) i = 4095;
	    } else {
		if ( i > 255 ) i = 255;
	    }
	    status = dnet_write ( link, buf, i, &xferred );
	   break;
	  case DNET_TEXT:
	  case DNET_RAW:
	   break;
	  case DNET_RQURL:
	   buf = original_uri(r);
	   i = strlen ( buf );
	   if ( i > 4095 ) i = 4095;
	    status = dnet_write ( link, buf, i, &xferred );
	   break;
	  case DNET_CGI:
	   break;
	  case DNET_HOST:
	   {
	      const char *host;
	      host = ap_get_server_name(r);
	      status = dnet_write ( link, host, strlen(host), &xferred );
	   }
	   break;
	  case DNET_ID:
	  case DNET_ID2:
	    /*
	     * Return "version srvhost srvport remport remaddr remuser remost"
	     * to script, (last item only for id2).
	     */
	    i = 0;			/* current length */
	    strncpy ( iobuf, ap_get_server_banner(), 200 );	// BRC
	    for ( i=0; i<199 && iobuf[i]; i++ ) {
		if ( iobuf[i] == ' ' ) break;
	    }
	    sprintf ( &iobuf[i], " %s %u %u %d %s",
		ap_get_server_name(r), ap_get_server_port(r),
		 ntohs(r->connection->client_addr->sa.sin.sin_port),
		(int)(r->connection->client_addr->sa.sin.sin_addr.s_addr),
		/* r->connection->remote_ip, */
		r->user ? r->user : "" );
	    i = strlen ( iobuf );
	    if ( opcode == DNET_ID2 ) {
		sprintf ( &iobuf[i]," %s", r->connection->remote_host ? r->connection->remote_host : "" );
		i += strlen(&iobuf[i]);
	    }
	    status = dnet_write ( link, iobuf, i, &xferred );
	    break;
	  case DNET_BINDIR:	/* <DNETBINDIR> */
	    status = dnet_write ( link, dcfg->bindir,
		strlen(dcfg->bindir), &xferred );
	    break;
	  case DNET_PATH:	/* <DNETPATH> */
	    status = dnet_write ( link, dcfg->dir_template,
		strlen(dcfg->dir_template), &xferred );
	    break;
	  case DNET_INPUT:
	    /*
	     * Read raw bytes from server and relay to scriptserver,
	     * Max to transfer is 254 bytes.
	     */
	    if ( content_status == 0 ) {
		content_status = 2;
		if ( ap_should_client_block(r) ) content_status = 1;
	    }
	    if ( content_status == 1 ) {
	        i = ap_get_client_block(r, iobuf, 254);
	        if ( i > 0 ) {
		    status = dnet_write ( link, iobuf, i, &xferred );
		} else content_status = 3;
	    }
	    if ( (content_status != 1) ) {
		/*
		 * Script asking for content not there.
		 */
		return -1;
	    }

	    break;
	  case DNET_XLATE:
	  case DNET_XLATEV:	/* <DNETXLATEV> */
	    /*
	     * read string to translate.
	     */
	    status = dnet_read ( link, iobuf, sizeof(iobuf)-1, &xferred );
	    if ( (status&1) == 1 ) {
		request_rec *pa_req;
		char *pt;
		iobuf[xferred] = '\0';
		pa_req = ap_sub_req_lookup_uri(ap_escape_uri(r->pool, iobuf), r, NULL);
		if ( pa_req->filename ) {	/* tack on filename */
		    pt = apr_pstrcat(r->pool, pa_req->filename,
			pa_req->path_info, NULL );
		} else {
		    pt = "";
		}
		status = dnet_write ( link, pt, strlen(pt), &xferred );
	    }
	    if ( (status&1) == 0 ) {
		r->status = 500;
		sts_line = apr_palloc ( r->pool, 260 );
		sprintf ( sts_line, "500 protocol error in DECnet task" );
		r->status_line = sts_line;
		return -1;
	    }
	    break;
	  case DNET_INVCACHE:	/* <DNETINVCACHE> */
	    break;		/* ignore */

	  case DNET_RECMODE:	/* <DNETRECMODE> */
	  case DNET_RECMODE2:	/* <DNETRECMODE2> */
	    *rec_mode = (opcode==DNET_RECMODE) ? 1 : 2;
	    break;
	  case DNET_MANAGE:
	    /*
	     * read argument from script and respond with error.
	     */
	    status = dnet_read ( link, iobuf, sizeof(iobuf)-1, &xferred );
	    if ( (status&1) == 1 ) {
		status = dnet_write ( link, "402 operation unsupported",
			25, &xferred );
	    }
	    break;
	  case DNET_REUSE:			/* Ignore */
	  case DNET_FORCEKA:
	    break;
	  default:
	    r->status = 500;
	    sts_line = apr_palloc ( r->pool, 260 );
	    sprintf ( sts_line, "500 protocol error in DECnet task" );
	    r->status_line = sts_line;
	    return -1;
	}
    }
    return opcode;
}
/*--------------------------------------------------------------------------*/
/*
 * Copy data from Decnet link to client connection.  Headers have
 * already been sent.
 */
static void transfer_output ( request_rec *r, excfg *dcfg,
	void *link, int rec_mode, char *end_tag )
{
    char buffer[4100];
    int status, iosize, n, length, xferred, i, j, et_len;

    et_len = end_tag ? strlen(end_tag) : -5;

    iosize = sizeof(buffer) - 4;
    for ( ; ; ) {
	/*
	 * get input data.
	 */
	if ( rec_mode ) {
	    status = dnet_read ( link, buffer, iosize, &xferred );
	} else {
	    status = dnet_read_streamed ( link, buffer, iosize,
		&xferred );
	}
	length = xferred;
	if ( length == et_len ) {
	    /* Check for end-of-data marker */
	    if ( strncmp ( buffer, end_tag, et_len ) == 0 ) break;
	}
	if ( rec_mode && ((length+2) <= sizeof(buffer)) ) {
	    buffer[length++] = '\r';
	    buffer[length++] = '\n';
	}
	/*
	 * Transfer data to client, may take multiple writes.
	 */
	for ( i = 0; i < length; i += n ) {
	    n = ap_rwrite ( buffer, length-i, r );
	    if ( n <= 0 ) {
		break;
	    }
	}
	if ( i < length ) break;
    }
    /*
     * Cleanup.
     */
    ap_rflush ( r );
    dnet_disconnect ( link );
}
/*--------------------------------------------------------------------------*/
static void process_cgi ( request_rec *r, excfg *dcfg,
	void *link, int rec_mode )
{
    const char *location;
    char sbuf[MAX_STRING_LEN];
    int ret;
    typedef int (*hoser)(char *, int, void *);
    struct dnet_streambuf stream;
    /*
     * Read CGI headers.
     */
    stream.link = link;
    stream.endtag = "</DNETCGI>";
    stream.recmode = rec_mode;
    stream.buflen = stream.bufpos = 0;

    if ( (ret = ap_scan_script_header_err_core(r, sbuf,
		(hoser) dnet_gets,  &stream )) ) {
	return;
    }
    /*
     * Check for "Location" header redirect.
     */
    location = apr_table_get(r->headers_out, "Location");

    if (location && location[0] == '/' && r->status == 200) {

        /* This redirect needs to be a GET no matter what the original
         * method was.
         */
        r->method = apr_pstrdup(r->pool, "GET");
        r->method_number = M_GET;

        /* We already read the message body (if any), so don't allow
         * the redirected request to think it has one.  We can ignore
         * Transfer-Encoding, since we used REQUEST_CHUNKED_ERROR.
         */
        apr_table_unset(r->headers_in, "Content-Length");

        ap_internal_redirect_handler(location, r);
	dnet_disconnect ( link );	/* PTR 75-62-305 */
        return;
        }
    else if (location && r->status == 200) {
        /* XX Note that if a script wants to produce its own Redirect
         * body, it now has to explicitly *say* "Status: 302"
         */
	r->status = HTTP_MOVED_TEMPORARILY;
    }

/*    ap_send_http_header(r); */

    if (r->status == HTTP_MOVED_TEMPORARILY)
	{				/* PTR 75-62-305 */
	dnet_disconnect ( link );       /* PTR 75-62-305 */
	return;
	}				/* PTR 75-62-305 */

    while ( stream.bufpos < stream.buflen ) {
	int n;
	n = ap_rwrite ( &stream.buffer[stream.bufpos],
		stream.buflen - stream.bufpos, r );
	if ( n <= 0 ) {
		break;
	}
	stream.bufpos += n;
    }
    transfer_output ( r, dcfg, link, rec_mode, stream.endtag );
}
/*--------------------------------------------------------------------------*/
/*                                                                          */
/* Now we declare our content handlers, which are invoked when the server   */
/* encounters a document which our module is supposed to have a chance to   */
/* see.  (See mod_mime's SetHandler and AddHandler directives, and the      */
/* mod_info and mod_status examples, for more details.)                     */
/*                                                                          */
/* Since content handlers are dumping data directly into the connexion      */
/* (using the r*() routines, such as rputs() and rprintf()) without         */
/* intervention by other parts of the server, they need to make             */
/* sure any accumulated HTTP headers are sent first.  This is done by       */
/* calling send_http_header().  Otherwise, no header will be sent at all,   */
/* and the output sent to the client will actually be HTTP-uncompliant.     */
/*--------------------------------------------------------------------------*/
/*
 * Sample content handler.  All this does is display the call list that has
 * been built up so far.
 *
 * The return value instructs the caller concerning what happened and what to
 * do next:
 *  OK ("we did our thing")
 *  DECLINED ("this isn't something with which we want to get involved")
 *  HTTP_mumble ("an error status should be reported")
 */
static int OSUscript_handler(request_rec *r)
{

    excfg *dcfg;
    void *link;			/* handle to DECnet link */
    int status, xferred, rec_mode, out_phase, content_setup;
    char *subfunc, *ident, *sts_line, errmsg[256], buffer[1000];

    if (strcmp(r->handler, "osuscript-handler")) {
        return DECLINED;
    }

    dcfg = our_dconfig(r);
#ifdef DEBUG
printf("osu script handler called, r=%x pid %x\n", r, getpid() );
printf ( "  dconf: template = '%s', task = '%s', bindir = '%s'\n",
    dcfg->dir_template?dcfg->dir_template:"<NULL>",
    dcfg->taskspec?dcfg->taskspec:"<NULL>",
    dcfg->bindir?dcfg->bindir:"<NULL>" );
#endif
    if ( !dcfg ) return DECLINED;
    /*
     * Check header for content, don't allow chunked transfer.
     */
    content_setup = ap_setup_client_block ( r, REQUEST_CHUNKED_ERROR );
    if ( content_setup != 0 ) return content_setup;
    /*
     * Attempt to make a DECnet connection.
     */
    status = dnet_connect ( dcfg->taskspec, &link );
    if ( (status&1) == 0 ) {
	dnet_format_error ( status, errmsg, sizeof(errmsg)-1 );
        sts_line = apr_palloc ( r->pool, 260 );
	sprintf ( sts_line, "500 %s", errmsg );
	r->status = 500;
	r->status_line = sts_line;
	return 500;
    } else errmsg[0] = '\0';
    /*
     * Send prolog: subfunc method protocol translated-url-path
     */
    subfunc = "HTBIN";		/* 'exec' scripts */
    if ( strcmp ( r->handler, "osuscript-handler" ) != 0 ) subfunc = "CONVERT";
    status = dnet_write ( link, subfunc, strlen(subfunc), &xferred );
    if (status&1) status = dnet_write ( link, r->method, strlen(r->method),
	&xferred );
    if (status&1) status = dnet_write ( link, r->protocol,
	strlen(r->protocol), &xferred );
    ident = r->uri;	 		/* translated URI? */
    if ( !ident ) ident = "????";
    if (status&1) status = dnet_write ( link, ident, strlen(ident), &xferred );
    if ( (status&1) == 0 ) {
	/*
	 * Error writing prologue, abort with error.
	 */
	dnet_format_error ( status, errmsg, sizeof(errmsg)-1 );
	sts_line = apr_palloc ( r->pool, 260 );
	sprintf ( sts_line, "500 %s", errmsg );
        r->status_line = sts_line;
	r->status = 500;
	return 500;
    }
    /*
     * We are now in dialog phase, process tags until error or output
     * phase entered.
     */
    rec_mode = 0;
    out_phase = dialog_phase ( r, link, dcfg, &rec_mode );
#ifdef DEBUG
    printf("Dialog phased returned code %d\n", out_phase );
#endif
    if ( out_phase < 0 ) return 500;
    /*
     * The DECnet link is now in the output phase, how to intrepret the
     * the  output depends upon the final dialog phase tag.
     */
    switch ( out_phase ) {
      case DNET_TEXT:
	/*
	 * Text mode, DNETRECMODE always implied.
	 */
	if ( !rec_mode ) rec_mode = 1;
	r->content_type = "text/plain";
/*	ap_soft_timeout("OSUscript text response", r); */
	status = dnet_read ( link, buffer, sizeof(buffer)-1, &xferred );
	if ( (status&1) == 0 ) break;
	buffer[xferred] = '\0';
	sts_line = apr_palloc ( r->pool, xferred+1 );
	strncpy ( sts_line, buffer, xferred );
	r->status_line = sts_line;
	r->status = atoi ( buffer );

/*	ap_send_http_header(r); */
	transfer_output ( r, dcfg, link, rec_mode, "</DNETTEXT>" );
/*        ap_kill_timeout(r); */
	status = OK;
	break;
      case DNET_RAW:
	/*
	 * Equivalent of -nph script.
	 */
	transfer_output ( r, dcfg, link, rec_mode, "</DNETRAW>" );
	status = OK;
	break;
      case DNET_CGI:
	/*
	 * Output is CGI mode.
	 */
	process_cgi ( r, dcfg, link, rec_mode );
	status = OK;
	break;
      default:
	r->status_line = "500 Bugcheck, unexpected dialog phase result";
	r->status = 500;
	status = 500;
	break;
    }
    /*
     * We're all done, so cancel the timeout we set.  Since this is probably
     * the end of the request we *could* assume this would be done during
     * post-processing - but it's possible that another handler might be
     * called and inherit our outstanding timer.  Not good; to each its own.
     */
/*    ap_kill_timeout(r); */
    /*
     * We did what we wanted to do, so tell the rest of the server we
     * succeeded.
     */
    return status;
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/* Now let's declare routines for each of the callback phase in order.      */
/* (That's the order in which they're listed in the callback list, *not     */
/* the order in which the server calls them!  See the command_rec           */
/* declaration near the bottom of this file.)  Note that these may be       */
/* called for situations that don't relate primarily to our function - in   */
/* other words, the fixup handler shouldn't assume that the request has     */
/* to do with "example" stuff.                                              */
/*                                                                          */
/* With the exception of the content handler, all of our routines will be   */
/* called for each request, unless an earlier handler from another module   */
/* aborted the sequence.                                                    */
/*                                                                          */
/* Handlers that are declared as "int" can return the following:            */
/*                                                                          */
/*  OK          Handler accepted the request and did its thing with it.     */
/*  DECLINED    Handler took no action.                                     */
/*  HTTP_mumble Handler looked at request and found it wanting.             */
/*                                                                          */
/* What the server does after calling a module handler depends upon the     */
/* handler's return value.  In all cases, if the handler returns            */
/* DECLINED, the server will continue to the next module with an handler    */
/* for the current phase.  However, if the handler return a non-OK,         */
/* non-DECLINED status, the server aborts the request right there.  If      */
/* the handler returns OK, the server's next action is phase-specific;      */
/* see the individual handler comments below for details.                   */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*
 * This function is called during server initialisation.  Any information
 * that needs to be recorded must be in static cells, since there's no
 * configuration record.
 *
 * There is no return value.
 */

/*
 * All our module-initialiser does is add its trace to the log.
 */
static void example_init(server_rec *s, apr_pool_t *p)
{

    char *sname = s->server_hostname;

    /*
     * Set up any module cells that ought to be initialised.
     */
#ifdef DEBUG
    printf("Process %x (child %d) called osuscript init function\n",
	getpid(), child_id );
#endif
    setup_module_cells();
    /*
     * The arbitrary text we add to our trace entry indicates for which server
     * we're being called.
     */
    sname = (sname != NULL) ? sname : "";
}

/*
 * This function is called during server initialisation when an heavy-weight
 * process (such as a child) is being initialised.  As with the
 * module-initialisation function, any information that needs to be recorded
 * must be in static cells, since there's no configuration record.
 *
 * There is no return value.
 */

/*
 * All our process-initialiser does is add its trace to the log.
 */
static void OSUscript_child_init(apr_pool_t *p, server_rec *s)
{
    char *note;
    char *sname = s->server_hostname;

    /*
     * Set up any module cells that ought to be initialised.
     */
    setup_module_cells();
    /*
     * The arbitrary text we add to our trace entry indicates for which server
     * we're being called.
     */
    sname = (sname != NULL) ? sname : "";
    note = apr_pstrcat(p, "OSUscript_child_init(", sname, ")", NULL);
}

/*
 * This function gets called to create up a per-directory configuration
 * record.  This will be called for the "default" server environment, and for
 * each directory for which the parser finds any of our directives applicable.
 * If a directory doesn't have any of our directives involved (i.e., they
 * aren't in the .htaccess file, or a <Location>, <Directory>, or related
 * block), this routine will *not* be called - the configuration for the
 * closest ancestor is used.
 *
 * The return value is a pointer to the created module-specific
 * structure.
 */
static void *OSUscript_create_dir_config(apr_pool_t *p, char *dirspec)
{

    excfg *cfg;
    char *dname = dirspec;
    int length;

#ifdef DEBUG
printf("create_dir_config entered, dir: '%s', pid %x\n",
dname ? dname : "<NULL>", getpid() );
#endif
    /*
     * Allocate the space for our record from the pool supplied.
     */
    cfg = (excfg *) apr_pcalloc(p, sizeof(excfg));
    /*
     * Now fill in the defaults.  If there are any `parent' configuration
     * records, they'll get merged as part of a separate callback.
     */
    cfg->local = 0;
    cfg->congenital = 0;
    cfg->cmode = CONFIG_MODE_DIRECTORY;
    /*
     * Save the directory, make sure it ends in a slash.
     */
    dname = (dname != NULL) ? dname : "";
    length = strlen(dname);
    cfg->dir_template = apr_palloc(p, length + 2 );
    if ( cfg->dir_template) {
	strcpy ( cfg->dir_template, dname );
	if ( length > 0 ) if ( dname[length-1] != '/' ) {
	    strcpy ( &cfg->dir_template[length], "/" );
	}
    }
    cfg->taskspec = (char *) 0;
    cfg->bindir = (char *) 0;

    return (void *) cfg;
}

/*
 * This function gets called to merge two per-directory configuration
 * records.  This is typically done to cope with things like .htaccess files
 * or <Location> directives for directories that are beneath one for which a
 * configuration record was already created.  The routine has the
 * responsibility of creating a new record and merging the contents of the
 * other two into it appropriately.  If the module doesn't declare a merge
 * routine, the record for the closest ancestor location (that has one) is
 * used exclusively.
 *
 * The routine MUST NOT modify any of its arguments!
 *
 * The return value is a pointer to the created module-specific structure
 * containing the merged values.
 */
static void *OSUscript_merge_dir_config(apr_pool_t *p, void *parent_conf,
                                      void *newloc_conf)
{

    excfg *merged_config = (excfg *) apr_pcalloc(p, sizeof(excfg));
    excfg *pconf = (excfg *) parent_conf;
    excfg *nconf = (excfg *) newloc_conf;
    char *note;

    /*
     * Some things get copied directly from the more-specific record, rather
     * than getting merged.
     */
    merged_config->local = nconf->local;
    /*
     * Others, like the setting of the `congenital' flag, get ORed in.  The
     * setting of that particular flag, for instance, is TRUE if it was ever
     * true anywhere in the upstream configuration.
     */
    merged_config->congenital = (pconf->congenital | pconf->local);
    /*
     * If we're merging records for two different types of environment (server
     * and directory), mark the new record appropriately.  Otherwise, inherit
     * the current value.
     */
    merged_config->cmode =
        (pconf->cmode == nconf->cmode) ? pconf->cmode : CONFIG_MODE_COMBO;

    return (void *) merged_config;
}

/*
 * This function gets called to create a per-server configuration
 * record.  It will always be called for the "default" server.
 *
 * The return value is a pointer to the created module-specific
 * structure.
 */
static void *OSUscript_create_server_config(apr_pool_t *p, server_rec *s)
{

    excfg *cfg;
    char *sname = s->server_hostname;
    /*
     * As with the create_dir_config() routine, we allocate and fill
     * in an empty record.
     */
    cfg = (excfg *) apr_pcalloc(p, sizeof(excfg));
    cfg->local = 0;
    cfg->congenital = 0;
    cfg->cmode = CONFIG_MODE_SERVER;
    /*
     * Note that we were called in the trace list.
     */
    sname = (sname != NULL) ? sname : "";
    /*
     * Zero the per-module stuff
     */
    cfg->dir_template = (char *) 0;
    cfg->taskspec = (char *) 0;
    cfg->bindir = (char *) 0;

    return (void *) cfg;
}

static void *OSUscript_merge_server_config(apr_pool_t *p, void *server1_conf,
                                         void *server2_conf)
{

    excfg *merged_config = (excfg *) apr_pcalloc(p, sizeof(excfg));
    excfg *s1conf = (excfg *) server1_conf;
    excfg *s2conf = (excfg *) server2_conf;

    /*
     * Our inheritance rules are our own, and part of our module's semantics.
     * Basically, just note whence we came.
     */
    merged_config->cmode =
        (s1conf->cmode == s2conf->cmode) ? s1conf->cmode : CONFIG_MODE_COMBO;
    merged_config->local = s2conf->local;
    merged_config->congenital = (s1conf->congenital | s1conf->local);

    return (void *) merged_config;
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/* Which functions are responsible for which hooks in the server.           */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*
 * Each function our module provides to handle a particular hook is
 * specified here.  The functions are registered using
 * ap_hook_foo(name, predecessors, successors, position)
 * where foo is the name of the hook.
 *
 * The args are as follows:
 * name         -> the name of the function to call.
 * predecessors -> a list of modules whose calls to this hook must be
 *                 invoked before this module.
 * successors   -> a list of modules whose calls to this hook must be
 *                 invoked after this module.
 * position     -> The relative position of this module.  One of
 *                 APR_HOOK_FIRST, APR_HOOK_MIDDLE, or APR_HOOK_LAST.
 *                 Most modules will use APR_HOOK_MIDDLE.  If multiple
 *                 modules use the same relative position, Apache will
 *                 determine which to call first.
 *                 If your module relies on another module to run first,
 *                 or another module running after yours, use the
 *                 predecessors and/or successors.
 *
 * The number in brackets indicates the order in which the routine is called
 * during request processing.  Note that not all routines are necessarily
 * called (such as if a resource doesn't have access restrictions).
 * The actual delivery of content to the browser [9] is not handled by
 * a hook; see the handler declarations below.
 */
static void OSUscript_register_hooks(apr_pool_t *p)
{
    ap_hook_child_init(OSUscript_child_init, NULL, NULL, APR_HOOK_MIDDLE);
    ap_hook_handler(OSUscript_handler, NULL, NULL, APR_HOOK_MIDDLE);
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/* All of the routines have been declared now.  Here's the list of          */
/* directives specific to our module, and information about where they      */
/* may appear and how the command parser should pass them to us for         */
/* processing.  Note that care must be taken to ensure that there are NO    */
/* collisions of directive names between modules.                           */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*
 * List of directives specific to our module.  The OSUscript directive
 * specifies the taskname/bindir information normally defined in
 * an 'exec' rule in the OSU server.  This directive is only valid
 * inside a <directory> or <location> block.
 */
static const command_rec OSUscript_cmds[] =
{
    AP_INIT_TAKE2("OSUscript", cmd_OSUscript, NULL, ACCESS_CONF,
		  "OSU scripteserver taskname and bin directory"),
    {NULL}
};

/*--------------------------------------------------------------------------*/
/*                                                                          */
/* Finally, the list of callback routines and data structures that provide  */
/* the static hooks into our module from the other parts of the server.     */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*
 * Module definition for configuration.  If a particular callback is not
 * needed, replace its routine name below with the word NULL.
 */
module AP_MODULE_DECLARE_DATA osuscript_module =
{
    STANDARD20_MODULE_STUFF,
    OSUscript_create_dir_config,      /* per-directory config creator */
    NULL,                             /* dir config merger */
    OSUscript_create_server_config,   /* server config creator */
    OSUscript_merge_server_config,    /* server config merger */
    OSUscript_cmds,                   /* command table */
    OSUscript_register_hooks,         /* set up other request processing hooks */
};
