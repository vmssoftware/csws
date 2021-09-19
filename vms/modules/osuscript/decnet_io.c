/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2000 The Apache Software Foundation.  All rights
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
 *
 * Portions of this software are based upon public domain software
 * originally written at the National Center for Supercomputing Applications,
 * University of Illinois, Urbana-Champaign.
 */

/*
 * This module provides support functions for the OSUscript module, providing
 * I/O routines for DECnet non-transparent I/O.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <descrip.h>
#include <dvidef.h>
#include <iodef.h>
#include <jpidef.h>
#include <cmbdef.h>
#include <ssdef.h>

#include "decnet_io.h"	/* validate prototypes against actual */
int SYS$ASSIGN(), SYS$DASSGN(), SYS$QIOW(), SYS$CREMBX(), SYS$CANCEL(),
	SYS$DELPRC();
/*
 * Global (module-wide) variables, Initialized by dnet_initialize.
 */
static $DESCRIPTOR(net_device,"_NET:");
static $DESCRIPTOR(mbxnet_device, "WWW_MBXNET_REQUEST");
static short mbxnet_request, mbxnet_response;
static long parent_pid;

struct dnet_iosb { short status; short length; long pid; };

struct connect_context {
    struct connect_context *flink, *blink;	/* Open connection list */
    int status;
    struct dnet_iosb iosb;
    short chan, mbxnet_state;
    int time_limit;			/* 0-none, 1-pending, 2-expired */
    long partner_pid;			/* server process */
};
typedef struct connect_context cnx_struct, *connection;

static int dnet_ef;
static connection free_connections;		/* Cache of free contexts. */
static connection active_connections;

int dnet_initialize ( )
{
    static int initialized = 0;
    if ( initialized ) return 3;
    dnet_ef = 12;
    free_connections = (connection) 0;
    active_connections = (connection) 0;
    initialized = 1;
    return 1;
}
/***************************************************************************/
int dnet_connect ( char *taskname, void **dptr )
{
    connection ctx;
    int i, status, SYS$ASSIGN(), length;
    struct { int length; char *data; } ncb_desc;
    char *ncb;
    char message[128];
    /*
     * Allocate a connection context block.
     */
    *dptr = (void *) NULL;
    ncb = message;

    ctx = free_connections;
    if ( ctx ) free_connections = ctx->flink;

    if ( ctx == (connection) NULL ) {
	ctx = (connection) malloc ( sizeof(cnx_struct) );
	if ( !ctx ) return 0;

	ctx->flink = ctx->blink = (connection) NULL;
	ctx->status = 2;
	ctx->chan = 0;
    }
    ctx->time_limit = 0;
    /*
     * Build NCB and descriptor.  Replace final quote with
     * ncb connect/opt-data structure.
     */
    strncpy ( ncb, taskname, 80 );
    ncb[80] = '\0';
    length = strlen ( ncb );
    if ( length > 0 ) ncb[length-1] = '/';
    ncb[length++] = 0;
    ncb[length++] = 0;			/* zero word (new connect */
    for ( i = 0; i < 17; i++ ) ncb[length++] = 0; 	/* null opt. data */
    ncb[length++] = '"';		/* closing quote */

    ncb_desc.length = length;
    ncb_desc.data = ncb;
	/*
	 * Attempt connection to decnet object.
         */
        status = SYS$ASSIGN ( &net_device, &ctx->chan, 0, 0 );
        if ( (status&1) == 0 ) {
	    ctx->flink = free_connections; free_connections = ctx;
	    return 0;
        }

        ctx->status = 1;
        status = SYS$QIOW ( dnet_ef, ctx->chan, IO$_ACCESS, &ctx->iosb,
		0, 0,
		0, &ncb_desc, 0, 0, 0, 0 );
	if ( (status&1) == 1 ) status = ctx->iosb.status;

    if ( (status&1) == 1 ) {
	/*
	 * Add to context list for this thread.
	 */
    } else {
	/* Failure */
	ctx->status = 0;
	ctx->flink = free_connections; free_connections = ctx;
	ctx = (connection) NULL;
    }
    /*
     * return status and context poitner to caller.
     */
    *dptr = (void *) ctx;
    return status;
}
/***************************************************************************/
int dnet_write ( void *dptr, const char *buffer, int bufsize, int *written )
{
    int status;
    connection ctx;

    ctx = (connection) dptr;
    status = SYS$QIOW ( dnet_ef, ctx->chan, IO$_WRITEVBLK, &ctx->iosb,
		0, 0,
		buffer, bufsize, 0, 0, 0, 0 );
    if ( (status&1) == 1 ) status = ctx->iosb.status;

    if ( (status&1) == 1 ) *written = (unsigned) ctx->iosb.length;
    else *written = 0;

    return status;
}
/***************************************************************************/
int dnet_read ( void *dptr, char *buffer, int bufsize, int *read )
{
    int status;
    connection ctx;

    ctx = (connection) dptr;
    status = SYS$QIOW ( dnet_ef, ctx->chan, IO$_READVBLK, &ctx->iosb,
		0, 0,
		buffer, bufsize, 0, 0, 0, 0 );
    if ( (status&1) == 1 ) status = ctx->iosb.status;

    if ( (status&1) == 1 ) *read = (unsigned) ctx->iosb.length;
    else *read = 0;

    return status;
}
/***************************************************************************/
int dnet_read_streamed ( void *dptr, char *buffer, int bufsize, int *read )
{
    int status;
    connection ctx;

    ctx = (connection) dptr;
    status = SYS$QIOW ( dnet_ef, ctx->chan, IO$_READVBLK|IO$M_MULTIPLE, 
		&ctx->iosb, 0, 0,
		buffer, bufsize, 0, 0, 0, 0 );
    if ( (status&1) == 1 ) {
	/*
	 * Ignore bufferovf errors, the io$m_multiple option means
	 * what didn't fit in this buffer is still in the driver's buffer.
	 */
	status = ctx->iosb.status;
	if ( (status == SS$_BUFFEROVF) ) status = SS$_NORMAL;
    }

    if ( (status&1) == 1 ) *read = (unsigned) ctx->iosb.length;
    else *read = 0;

    return status;
}
/***************************************************************************/
int dnet_disconnect ( void *dptr )
{
    int status;
    connection ctx, first, cur;
    /*
     * Perform the disconnect.
     */
    ctx = (connection) dptr;
    if ( ctx->status == 0 ) return 20;
	ctx->time_limit = 0;	/* disable time limit */
        status = SYS$QIOW ( dnet_ef, ctx->chan, IO$_DEACCESS|IO$M_SYNCH, 
		&ctx->iosb, 0, 0,
		0, 0, 0, 0, 0, 0 );
	if ((status&1) == 1 ) status = ctx->iosb.status;

    ctx->status = 0;
    SYS$DASSGN ( ctx->chan );
    /*
     * Verify the connection belongs to thread.
     */
    /*
     * Remove from thread's connection list.
     */
    /*
     * Place control block onto free list.
     */
    ctx->flink = free_connections; free_connections = ctx;

    return status;
}
/***************************************************************************/
int dnet_format_error ( int code, char *buffer, int bufsize )
{
    int flags, status, SYS$GETMSG(), msglen, info;
    struct dsc$descriptor buf;

    buf.dsc$b_dtype = DSC$K_DTYPE_T;		/* text data */
    buf.dsc$b_class = DSC$K_CLASS_S;		/* fixed (Static) */
    buf.dsc$w_length = bufsize - 1;
    buf.dsc$a_pointer = buffer;
    flags = 0;

    msglen = 0;
    status = SYS$GETMSG ( code, &msglen, &buf, flags, &info );
    if ( (status&1) == 1 ) buffer[msglen] = '\0';
    else buffer[0] = '\0';
    return status;
}
/***************************************************************************/
int dnet_set_time_limit ( void *dptr, int limit )
{
    return 0;
}
/***************************************************************************/
/* Emulate the apache ap_bgets function.
 *
 * Reads from the stream into the array pointed to by buff, until
 * a (CR)LF sequence is read, or end-of-file condition is encountered
 * or until n-1 bytes have been stored in buff. If a CRLF sequence is
 * read, it is replaced by a newline character.  The string is then
 * terminated with a null character.
 *
 * Returns the number of bytes stored in buff (not including the terminating
 * NULL), or zero on end of transmission, or -1 on an error.
 *
 * Notes:
 *  If null characters are expected in the data stream, then
 * buff should not be treated as a null terminated C string; instead
 * the returned count should be used to determine the length of the
 * string.
 *  CR characters in the byte stream not immediately followed by a LF
 * will be preserved.
 */
int dnet_gets ( char *buf, int bufsize, struct dnet_streambuf *in )
{
    int status, count, i, maxmsg;
    struct { unsigned short status, count; long pid; } iosb;
    char c;

    if ( !in->link ) return 0;		/* not a valid stream */
    maxmsg = sizeof(in->buffer)-1;
    if ( in->recmode ) maxmsg = maxmsg - 1;
    if ( bufsize > maxmsg ) bufsize = maxmsg+1;
    for ( count = 0; ; ) {
	if ( in->bufpos >= in->buflen ) {
	    /*
	     * read more data.  Check for no writer only after first read.
	     */
	    if ( in->recmode ) {
	       status = dnet_read_streamed ( in->link, in->buffer, maxmsg,
	           	&in->buflen );
	    } else {
		status = dnet_read ( in->link, in->buffer, maxmsg,
			&in->buflen );
	    }
	    if ( (status&1) == 0 ) {
	        errno = EVMSERR;
	        vaxc$errno = status;
		return -1;
	    }
	    in->bufpos = 0;
	    if ( in->endtag && (in->buffer[0] == '<') ) {
		if ( strlen(in->endtag) == in->buflen ) {
		    if ( strncmp ( in->endtag, in->buffer, in->buflen ) == 0 ){
			in->link = (char *) 0;
			break;
		    }
		}
	    }

	    if ( in->recmode ) {
		in->buffer[in->buflen++] = '\n';
	    }
	    in->buffer[in->buflen] = '\0';
	}
	/*
	 * Move characters into buffer.
	 */
	c = '\0';
	for ( i = in->bufpos; (c!='\n') && (i < in->buflen); i++ ) {

	    if ( count >= (bufsize-1) ) break;
	    c = in->buffer[i];
	    if ( c == '\r' ) {
		/* since LF was appended we know buffer[i+1] exists. */
		if ( in->buffer[i+1] == '\n' ) buf[count++] = c;
	    } else {
		buf[count++] = c;
	    }
	}
	in->bufpos = i;
	if ( c == '\n' ) break;
    }
    /*
     * Always terminate the string (differs from fgets() function).
     */
    buf[count] = '\0';
    return count;
}
