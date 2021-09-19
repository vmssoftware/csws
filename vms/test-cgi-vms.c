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

/* Test-CGI-VMS.C -- Sample program to access & display C environment and
 *		     Apache CGI variables.
 *
 * Lee Tibbert
 * 1999-06-28
 *
 * Modified:	2000-07-27	Rick Barry: Change REMOTE_ADDRESS to REMOTE_ADDR
 *
 * Compile:	$ cc/prefix=All test-cgi-vms
 * Link:	$ link test-cgi-vms
 * 			! Simple printout of HOME, PATH, etc. No CGI available.
 * Run: 	$ run  test-cgi-vms.exe
 *
 * Copy .exe to [APACHE.CGI-BIN]. If one is running in a cluster with
 * both Alpha and VAX machines, one may want to give the output file a
 * .exe_alpha or .exe_vax extension. Set file ownership and protection for
 * Apache. Make sure that [APACHE.CONF] allows access to the CGI-BIN
 * (the default .conf does).
 * Access this script from a browser as (replace host.domain by your system
 * name):
 * http://host.domain/cgi-bin/test-cgi-vms.exe
 * (or .exe_alpha or .exe_vax if you have used either of those file extensions).
 *
 *
 * CGI variables are passed as global DCL symbols.
 *
 * Modification History
 * 1999-06-30	Lee Tibbert	    V1.0-1
 *		Removed the 1.0 from the CGI string.  I believe
 *		that Apache is now using CGI 1.1.
 *
 *		Added printout of default (current) directory to help
 *		me get it right.
 *
 * 1999-11-23	Jim Tyree          V1.0-2
 *              Remove calls to fflush() -- no longer needed.
 *              Display more environmental variables.
 *              Change display headings slightly.
 *
 * 1999-12-14	Lee Tibbert	   V1.0-3
 *		Fix typo for QUERY_STRING.
 *
 * 2001-12-18	Steve Fesler	   V1.0-4
 *		Take out spaces in "REMOTE_ADDR   "
 */

#include <netdb.h>
#include <namdef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

const char * const vars[] = {"USER", "PATH", "HOME", "TERM" , NULL};
const char * const more[] =
	    {"AUTH_TYPE"        ,"CONTENT_TYPE"     ,"CONTENT_LENGTH"
            ,"DOCUMENT_ROOT"    ,"GATEWAY_INTERFACE","HTTP_ACCEPT"
	    ,"HTTP_COOKIE"      ,"HTTP_FROM"        ,"HTTP_REFERER"
	    ,"HTTP_USER_AGENT"  ,"PATH_INFO"        ,"PATH_TRANSLATED"
            ,"REMOTE_HOST"      ,"REMOTE_ADDR"      ,"REMOTE_USER"
            ,"REMOTE_IDENT"     ,"QUERY_STRING"     ,"REQUEST_METHOD"
            ,"SERVER_SOFTWARE"  ,"SERVER_ADMIN"     ,"SERVER_NAME"
	    ,"SERVER_PORT"      ,"SERVER_PROTOCOL"  ,"SCRIPT_NAME"
            ,"SCRIPT_FILENAME"  , NULL };

int main(int argc, char **argv, char **envp)
{
    int     j;
    FILE * fp;
    char   *cp;
    char hostName[1024];
    char * cwd;
    time_t now;
    char * dp;


    fp = fdopen (STDOUT_FILENO, "w");

    fprintf (fp, "Content-type: text/plain\n\n");

    fprintf (fp, "CGI test script (test-cgi-vms.c) report:\n\n");


    if (gethostname(hostName, sizeof(hostName)) != 0) {
    	strcpy(hostName, "<Unavailable>");
    }
    else {
    	hostName[sizeof(hostName) - 1] = '\0';
    }

    now = time(0);
    cwd = getcwd(NULL, NAM$C_MAXRSS);

    dp =  asctime(localtime(&now));
    dp[strlen(dp) - 1] = '\0';	/* Trim off newline trailing */

    fprintf (fp, "Generated on host %s at %s.\n", hostName, dp);


    if (cwd != NULL) {
    	fprintf (fp, "Default directory is %s.\n\n", cwd);
	free(cwd);
    }


    for (j = 0; j < argc; j++) {
	fprintf (fp, "Argv[%d] is |%s|\n", j, argv[j]);
    }


    if (envp == NULL) {
	fprintf (fp, "Envp is |%s|\n", "NULL");
    }
    else {

	fprintf (fp, "\n\nFetch all the CGI environment variables.\n\n");

	for (j = 0; envp[j] != NULL; j++) {
	    fprintf (fp, "Envp[%2d] is |%s|\n", j, envp[j]);
	}
    }

/* Show the values of USER, HOME, PATH, and TERM in
 * in CGI script process.  This helps education & debugging.
 */

    fprintf (fp, "\n\nShow the C standard environment variables.\n\n");

    for (j = 0; vars[j] != NULL; j++) {
	fprintf (fp, "getenv(%s) is |%s|\n", vars[j],
		(cp = getenv(vars[j])) ? cp : "(NULL)");
    }

/* Show the values of other variables that CGI scripts mightuse
 */

    fprintf (fp, "\n\nFetch other CGI environment variables.\n\n");

    for (j = 0; more[j] != NULL; j++) {
        char temp[256];
        sprintf (temp,"getenv(%s)",more[j]);
	fprintf (fp, "%-25s is |%s|\n", temp,
		(cp = getenv(more[j])) ? cp : "(NULL)");
    }

    fprintf (fp, "\n\t\t\t\t- The End -\n");

    exit (EXIT_SUCCESS);

}	/* Main() */
/* End of file */
