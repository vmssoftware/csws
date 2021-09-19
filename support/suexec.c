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

/*
 * suexec.c -- "Wrapper" support program for suEXEC behaviour for Apache
 *
 ***********************************************************************
 *
 * NOTE! : DO NOT edit this code!!!  Unless you know what you are doing,
 *         editing this code might open up your system in unexpected
 *         ways to would-be crackers.  Every precaution has been taken
 *         to make this code as safe as possible; alter it at your own
 *         risk.
 *
 ***********************************************************************
 *
 *
 */
#ifdef __VMS
#ifdef _USE_STD_STAT    /* Can't do this here */
#undef _USE_STD_STAT
#endif
#endif

#include "apr.h"
#include "ap_config.h"
#include "suexec.h"

#ifdef __VMS
#ifndef __NEW_STARLET
#define __NEW_STARLET
#define __NEW_STARLET_SET
#endif
#include <lib$routines.h>
#include <starlet.h>
#include <descrip.h>
#include <iosbdef.h>
#include <unixio.h>
#include <file.h>
#include <fibdef.h>
#include <acldef.h>
#include <acedef.h>
#include <atrdef.h>
#include <uicdef.h>
#include <impdef.h>
#include <issdef.h>
#include <iledef.h>
#include <psldef.h>
#include <stsdef.h>
#include <fiddef.h>
#include <fh2def.h>
#include <ctype.h>
#include <ssdef.h>
#include <iodef.h>
#include "ilemac.h"
#ifdef  __NEW_STARLET_SET
#undef  __NEW_STARLET_SET
#undef  __NEW_STARLET
#endif
#else
#include <sys/param.h>
#endif
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include <time.h>
#if APR_HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#if APR_HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#ifdef HAVE_GRP_H
#include <grp.h>
#endif

#ifdef AP_LOG_SYSLOG
#include <syslog.h>
#endif

#if defined(PATH_MAX)
#define AP_MAXPATH PATH_MAX
#elif defined(MAXPATHLEN)
#define AP_MAXPATH MAXPATHLEN
#else
#define AP_MAXPATH 8192
#endif

#define AP_ENVBUF 256

#ifdef __VMS
#define ARG_DATA 1
#define ENV_DATA 2
#define SYM_VAL_MAX 970
#define MAX(a,b) ((a > b) ? a : b)

#define SCRIPT_FN "SCRIPT_FILENAME="

#define lstat stat
#define getcwd(buf,siz) getcwd(buf,siz,0)
#define exit(stat) exit(stat|STS$M_INHIB_MSG)

static char **arg_data;
static char **env_data;

static int has_identifier (struct dsc$descriptor *, struct dsc$descriptor *);
static int sub_directory (char *, char *, struct stat *);
static int acl_access (struct stat *, int);
static int load_data (int);
static void clean_arg ();

extern int decc$$translate ();
extern void *apr$dcl_shm_open ();
extern int apr$dcl_shm_read ();
extern int apr$dcl_shm_rewind ();
extern int apr$dcl_shm_close ();
extern int apr$dcl_pass_arg ();
extern int apr$dcl_pass_env ();
#endif

extern char **environ;

#ifdef AP_LOG_SYSLOG
/* Syslog support. */
#if !defined(AP_LOG_FACILITY) && defined(LOG_AUTHPRIV)
#define AP_LOG_FACILITY LOG_AUTHPRIV
#elif !defined(AP_LOG_FACILITY)
#define AP_LOG_FACILITY LOG_AUTH
#endif

static int log_open;
#else
/* Non-syslog support. */
static FILE *log = NULL;
#endif

static const char *const safe_env_lst[] =
{
    /* variable name starts with */
    "HTTP_",
    "SSL_",

    /* variable name is */
    "AUTH_TYPE=",
    "CONTENT_LENGTH=",
    "CONTENT_TYPE=",
    "CONTEXT_DOCUMENT_ROOT=",
    "CONTEXT_PREFIX=",
    "DATE_GMT=",
    "DATE_LOCAL=",
    "DOCUMENT_ARGS=",
    "DOCUMENT_NAME=",
    "DOCUMENT_PATH_INFO=",
    "DOCUMENT_ROOT=",
    "DOCUMENT_URI=",
    "GATEWAY_INTERFACE=",
    "HTTPS=",
    "LAST_MODIFIED=",
    "PATH_INFO=",
    "PATH_TRANSLATED=",
    "QUERY_STRING=",
    "QUERY_STRING_UNESCAPED=",
    "REMOTE_ADDR=",
    "REMOTE_HOST=",
    "REMOTE_IDENT=",
    "REMOTE_PORT=",
    "REMOTE_USER=",
    "REDIRECT_ERROR_NOTES=",
    "REDIRECT_HANDLER=",
    "REDIRECT_QUERY_STRING=",
    "REDIRECT_REMOTE_USER=",
    "REDIRECT_SCRIPT_FILENAME=",
    "REDIRECT_STATUS=",
    "REDIRECT_URL=",
    "REQUEST_METHOD=",
    "REQUEST_URI=",
    "REQUEST_SCHEME=",
    "SCRIPT_FILENAME=",
    "SCRIPT_NAME=",
    "SCRIPT_URI=",
    "SCRIPT_URL=",
    "SERVER_ADMIN=",
    "SERVER_NAME=",
    "SERVER_ADDR=",
    "SERVER_PORT=",
    "SERVER_PROTOCOL=",
    "SERVER_SIGNATURE=",
    "SERVER_SOFTWARE=",
    "UNIQUE_ID=",
    "USER_NAME=",
    "TZ=",
    NULL
};

static void log_err(const char *fmt,...)
    __attribute__((format(printf,1,2)));
static void log_no_err(const char *fmt,...)
    __attribute__((format(printf,1,2)));
static void err_output(int is_error, const char *fmt, va_list ap)
    __attribute__((format(printf,2,0)));

static void err_output(int is_error, const char *fmt, va_list ap)
{
#if defined(AP_LOG_SYSLOG)
    if (!log_open) {
        openlog("suexec", LOG_PID, AP_LOG_FACILITY);
        log_open = 1;
    }

    vsyslog(is_error ? LOG_ERR : LOG_INFO, fmt, ap);
#elif defined(AP_LOG_EXEC)
    time_t timevar;
    struct tm *lt;

#ifdef __VMS
    if (!log) {
        int fd = open(AP_LOG_EXEC, O_RDWR | O_APPEND | O_CREAT, 0777, "shr=get,put", "ctx=stm");
        if (fd < 0) {
            fprintf(stderr, "failed to open log file\n");
            perror("open");
            exit(1);
        }
        log = fdopen(fd, "a");
        if (log == NULL) {
            fprintf(stderr, "failed to fdopen log file\n");
            perror("fdopen");
            exit(1);
        }
    }
#else
    if (!log) {
#if defined(_LARGEFILE64_SOURCE) && HAVE_FOPEN64
        if ((log = fopen64(AP_LOG_EXEC, "a")) == NULL) {
#else
        if ((log = fopen(AP_LOG_EXEC, "a")) == NULL) {
#endif
            fprintf(stderr, "suexec failure: could not open log file\n");
            perror("fopen");
            exit(1);
        }
    }
#endif

    if (is_error) {
        fprintf(stderr, "suexec policy violation: see suexec log for more "
                        "details\n");
    }

    time(&timevar);
    lt = localtime(&timevar);

    fprintf(log, "[%d-%.2d-%.2d %.2d:%.2d:%.2d]: ",
            lt->tm_year + 1900, lt->tm_mon + 1, lt->tm_mday,
            lt->tm_hour, lt->tm_min, lt->tm_sec);

    vfprintf(log, fmt, ap);

    fflush(log);
#endif /* AP_LOG_EXEC */
    return;
}

static void log_err(const char *fmt,...)
{
#ifdef AP_LOG_EXEC
    va_list ap;

    va_start(ap, fmt);
    err_output(1, fmt, ap); /* 1 == is_error */
    va_end(ap);
#endif /* AP_LOG_EXEC */
    return;
}

static void log_no_err(const char *fmt,...)
{
#ifdef AP_LOG_EXEC
    va_list ap;

    va_start(ap, fmt);
    err_output(0, fmt, ap); /* 0 == !is_error */
    va_end(ap);
#endif /* AP_LOG_EXEC */
    return;
}

static void clean_env(void)
{
    char **cleanenv;
    char **ep;
    int cidx = 0;
    int idx;

    /* While cleaning the environment, the environment should be clean.
     * (e.g. malloc() may get the name of a file for writing debugging info.
     * Bad news if MALLOC_DEBUG_FILE is set to /etc/passwd.  Sprintf() may be
     * susceptible to bad locale settings....)
     * (from PR 2790)
     */
    char **envp = environ;
    char *empty_ptr = NULL;
#ifdef __VMS
    int cmax;
#endif

    environ = &empty_ptr; /* VERY safe environment */

#ifdef __VMS
    for (cmax = 0; env_data && env_data[cmax]; cmax++);
    if ((cleanenv = (char **) calloc((cmax + 1), sizeof(char *))) == NULL) {
#else
    if ((cleanenv = (char **) calloc(AP_ENVBUF, sizeof(char *))) == NULL) {
#endif
        log_err("failed to malloc memory for environment\n");
        exit(123);
    }

    cleanenv[cidx] = strdup("PATH=" AP_SAFE_PATH);
    if (cleanenv[cidx] == NULL) {
        log_err("failed to malloc memory for environment\n");
        exit(124);
    }
    cidx++;

#ifdef __VMS
    for (ep = env_data; ep && *ep && cidx < cmax; ep++) {
        if (!strncmp(*ep, "#", 1))
            strcpy (*ep, *ep + 1);
#else
    for (ep = envp; *ep && cidx < AP_ENVBUF-1; ep++) {
#endif
        for (idx = 0; safe_env_lst[idx]; idx++) {
            if (!strncmp(*ep, safe_env_lst[idx],
                         strlen(safe_env_lst[idx]))) {
                cleanenv[cidx] = *ep;
                cidx++;
                break;
            }
        }
    }

    cleanenv[cidx] = NULL;
#ifdef __VMS
    free (env_data);
    env_data = cleanenv;
#else
    environ = cleanenv;
#endif
}

int main(int argc, char *argv[])
{
#ifdef __VMS
    struct dsc$descriptor TargetUserDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, NULL};
    struct dsc$descriptor LogNamDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, NULL};
    struct dsc$descriptor LogValDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, NULL};
    struct dsc$descriptor IdentDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, NULL};
    struct dsc$descriptor UserDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, NULL};
    static char *script_name = "APACHE$ROOT:[000000]APACHE$DCL.COM";
    unsigned int PersonaMode = PSL$C_USER,
                 PersonaFlags = 0;
    char *userdir_suffix = NULL;
    char SuExecCgiArg1[256];
    char *doc_root = NULL;
    ILE3 PersonaItems[2],
         *Ile3Ptr;
    unsigned int PersonaId = 0;
    int status,
        i;
    int sz;
    int dbgflg = 0;
#endif
    int userdir = 0;        /* ~userdir flag             */
    uid_t uid;              /* user information          */
    gid_t gid;              /* target group placeholder  */
    char *target_uname;     /* target user name          */
    char *target_gname;     /* target group name         */
    char *target_homedir;   /* target home directory     */
    char *actual_uname;     /* actual user name          */
    char *actual_gname;     /* actual group name         */
    char *cmd;              /* command to be executed    */
    char cwd[AP_MAXPATH];   /* current working directory */
    char dwd[AP_MAXPATH];   /* docroot working directory */
    struct passwd *pw;      /* password entry holder     */
    struct group *gr;       /* group entry holder        */
    struct stat dir_info;   /* directory info holder     */
    struct stat prg_info;   /* program info holder       */

#ifndef __VMS
    /*
     * Start with a "clean" environment
     */
    clean_env();
#else
    if (getenv("APACHE$DBG_SUEXEC")) {
        dbgflg = 1;
        log_err("debug: Debugging enabled for suexec\n");
    }
#endif

    /*
     * Check existence/validity of the UID of the user
     * running this program.  Error out if invalid.
     */
    uid = getuid();
    if ((pw = getpwuid(uid)) == NULL) {
        log_err("crit: invalid uid: (%lu)\n", (unsigned long)uid);
        exit(102);
    }
#ifdef __VMS
    if (dbgflg) log_err("debug: UID: %d\n", uid);
    /*
    ** Load the user descriptor
    */
    UserDesc.dsc$a_pointer = pw->pw_name;
    UserDesc.dsc$w_length = strlen (pw->pw_name);

    /*
    ** Load the ident descriptor
    */
    IdentDesc.dsc$a_pointer = SUEXEC_SRVR;
    IdentDesc.dsc$w_length = strlen (SUEXEC_SRVR);

    /*
    ** Check to see if the calling user has the SUEXEC_SRVR right
    */
    status = has_identifier (&UserDesc, &IdentDesc);
    if (status <= 0)
        {
        log_err ("user mismatch (%s), (0x%08X, %s right was not detected)\n",
                 pw->pw_name, vaxc$errno, SUEXEC_SRVR);
        exit (103);
        }

    if (dbgflg) log_err("debug: Rights Id %s is ok\n", SUEXEC_SRVR);
    /*
    ** Determine whether there exists a document root
    */
    doc_root = getenv ("APACHE$DOCUMENT_ROOT");
    if (! doc_root)
        doc_root = getenv ("WWW_APACHE$DOCUMENT_ROOT");
    if (! doc_root)
        doc_root = AP_DOC_ROOT;

    if (dbgflg) log_err("debug: Document Root is %s\n", doc_root);
    /*
    ** Determine whether there exists a userdir suffix
    */
    userdir_suffix = getenv ("APACHE$USERDIR");
    if (! userdir_suffix)
        userdir_suffix = getenv ("WWW_APACHE$USERDIR");
    if (! userdir_suffix)
        userdir_suffix = AP_USERDIR_SUFFIX;

    if (userdir_suffix) {
        if (dbgflg) log_err("debug: UserDir Suffix: %s\n", userdir_suffix);
    }
    else {
        if (dbgflg) log_err("debug: No UserDir Suffix found\n");
    }
#endif
    /*
     * See if this is a 'how were you compiled' request, and
     * comply if so.
     */
    if ((argc > 1)
#ifndef __VMS
        && (! strcmp(argv[1], "-V"))
        && ((uid == 0)
#ifdef _OSD_POSIX
        /* User name comparisons are case insensitive on BS2000/OSD */
            || (! strcasecmp(AP_HTTPD_USER, pw->pw_name)))
#else  /* _OSD_POSIX */
            || (! strcmp(AP_HTTPD_USER, pw->pw_name)))
#endif /* _OSD_POSIX */
#else
        && (argc < 4)
#endif
        ) {
#ifdef AP_DOC_ROOT
#ifdef __VMS
        fprintf(stderr, " -D DOC_ROOT=\"%s\"\n", doc_root);
#else
        fprintf(stderr, " -D AP_DOC_ROOT=\"%s\"\n", AP_DOC_ROOT);
#endif
#endif
#ifdef AP_GID_MIN
        fprintf(stderr, " -D AP_GID_MIN=%d\n", AP_GID_MIN);
#endif
#ifdef AP_HTTPD_USER
        fprintf(stderr, " -D AP_HTTPD_USER=\"%s\"\n", AP_HTTPD_USER);
#endif
#if defined(AP_LOG_SYSLOG)
        fprintf(stderr, " -D AP_LOG_SYSLOG\n");
#elif defined(AP_LOG_EXEC)
        fprintf(stderr, " -D AP_LOG_EXEC=\"%s\"\n", AP_LOG_EXEC);
#endif
#ifdef AP_SAFE_PATH
        fprintf(stderr, " -D AP_SAFE_PATH=\"%s\"\n", AP_SAFE_PATH);
#endif
#ifdef AP_SUEXEC_UMASK
        fprintf(stderr, " -D AP_SUEXEC_UMASK=%03o\n", AP_SUEXEC_UMASK);
#endif
#ifdef AP_UID_MIN
        fprintf(stderr, " -D AP_UID_MIN=%d\n", AP_UID_MIN);
#endif
#ifdef AP_USERDIR_SUFFIX
#ifdef __VMS
        fprintf(stderr, " -D USERDIR_SUFFIX=\"%s\"\n", userdir_suffix);
#else
        fprintf(stderr, " -D AP_USERDIR_SUFFIX=\"%s\"\n", AP_USERDIR_SUFFIX);
#endif
#endif
        exit(0);
    }
    /*
     * If there are a proper number of arguments, set
     * all of them to variables.  Otherwise, error out.
     */
    if (argc < 4) {
        log_err("too few arguments\n");
        exit(101);
    }
    target_uname = argv[1];
    target_gname = argv[2];
    cmd = argv[3];

    /*
     * Check to see if the user running this program
     * is the user allowed to do so as defined in
     * suexec.h.  If not the allowed user, error out.
     */
#ifdef __VMS
     /*
     ** For OpenVMS this test is done above
     */
#elif _OSD_POSIX
    /* User name comparisons are case insensitive on BS2000/OSD */
    if (strcasecmp(AP_HTTPD_USER, pw->pw_name)) {
        log_err("user mismatch (%s instead of %s)\n", pw->pw_name, AP_HTTPD_USER);
        exit(103);
    }
#else  /*_OSD_POSIX*/
    if (strcmp(AP_HTTPD_USER, pw->pw_name)) {
        log_err("user mismatch (%s instead of %s)\n", pw->pw_name, AP_HTTPD_USER);
        exit(103);
    }
#endif /*_OSD_POSIX*/

    /*
     * Check for a leading '/' (absolute path) in the command to be executed,
     * or attempts to back up out of the current directory,
     * to protect against attacks.  If any are
     * found, error out.  Naughty naughty crackers.
     */
    if ((cmd[0] == '/') || (!strncmp(cmd, "../", 3))
        || (strstr(cmd, "/../") != NULL)) {
        log_err("invalid command (%s)\n", cmd);
        exit(104);
    }

    /*
     * Check to see if this is a ~userdir request.  If
     * so, set the flag, and remove the '~' from the
     * target username.
     */
    if (!strncmp("~", target_uname, 1)) {
        target_uname++;
        userdir = 1;
#ifdef __VMS
        if (dbgflg) log_err("debug: userdir request\n");
#endif
    }

    /*
     * Error out if the target username is invalid.
     */
    if (strspn(target_uname, "1234567890") != strlen(target_uname)) {
        if ((pw = getpwnam(target_uname)) == NULL) {
            log_err("invalid target user name: (%s)\n", target_uname);
            exit(105);
        }
    }
    else {
        if ((pw = getpwuid(atoi(target_uname))) == NULL) {
            log_err("invalid target user id: (%s)\n", target_uname);
            exit(121);
        }
    }

#ifdef __VMS
    /*
    ** Load the user descriptor
    */
    UserDesc.dsc$a_pointer = pw->pw_name;
    UserDesc.dsc$w_length = strlen (pw->pw_name);

    /*
    ** Load the ident descriptor
    */
    IdentDesc.dsc$a_pointer = SUEXEC_USER;
    IdentDesc.dsc$w_length = strlen (SUEXEC_USER);

    /*
    ** Check to see if the target user has the SUEXEC_USER right
    */
    status = has_identifier (&UserDesc, &IdentDesc);
    if (status <= 0)
        {
        log_err ("invalid target user name: (%s), (0x%08X, %s right was not detected)\n",
                 pw->pw_name, vaxc$errno, SUEXEC_USER);
        exit(105);
        }

    if (dbgflg) log_err("debug: User %s has identifier %s\n", pw->pw_name, SUEXEC_USER);
#endif

    /*
     * Error out if the target group name is invalid.
     */
    if (strspn(target_gname, "1234567890") != strlen(target_gname)) {
        if ((gr = getgrnam(target_gname)) == NULL) {
            log_err("invalid target group name: (%s)\n", target_gname);
            exit(106);
        }
    }
    else {
        if ((gr = getgrgid(atoi(target_gname))) == NULL) {
            log_err("invalid target group id: (%s)\n", target_gname);
            exit(106);
        }
    }
    gid = gr->gr_gid;
    if ((actual_gname = strdup(gr->gr_name)) == NULL) {
        log_err("failed to alloc memory\n");
        exit(125);
    }

#ifdef _OSD_POSIX
    /*
     * Initialize BS2000 user environment
     */
    {
        pid_t pid;
        int status;

        switch (pid = ufork(target_uname)) {
        case -1:    /* Error */
            log_err("failed to setup bs2000 environment for user %s: %s\n",
                    target_uname, strerror(errno));
            exit(150);
        case 0:     /* Child */
            break;
        default:    /* Father */
            while (pid != waitpid(pid, &status, 0))
                ;
            /* @@@ FIXME: should we deal with STOP signals as well? */
            if (WIFSIGNALED(status)) {
                kill (getpid(), WTERMSIG(status));
            }
            exit(WEXITSTATUS(status));
        }
    }
#endif /*_OSD_POSIX*/

    /*
     * Save these for later since initgroups will hose the struct
     */
    uid = pw->pw_uid;
    actual_uname = strdup(pw->pw_name);
    target_homedir = strdup(pw->pw_dir);
    if (actual_uname == NULL || target_homedir == NULL) {
        log_err("failed to alloc memory\n");
        exit(126);
    }

    /*
     * Log the transaction here to be sure we have an open log
     * before we setuid().
     */
    log_no_err("uid: (%s/%s) gid: (%s/%s) cmd: %s\n",
               target_uname, actual_uname,
               target_gname, actual_gname,
               cmd);

#ifdef __VMS
    /*
    ** Load the argument & environment data
    */
    load_data (ARG_DATA);
    load_data (ENV_DATA);

    if (dbgflg) log_err("debug: arg data and env data loaded\n");
    /*
    ** Uppercase the target user name
    */
    for (i = 0; i < strlen (actual_uname); i++)
        actual_uname[i] = toupper (actual_uname[i]);

    /*
    ** Create the target username descriptor
    */
    TargetUserDesc.dsc$w_length = strlen (actual_uname);
    TargetUserDesc.dsc$a_pointer = actual_uname;

    /*
    ** Setup the persona item list
    */
    ILE3_INIT (PersonaItems);
    ILE3_ADD  (ISS$_MODE, sizeof (PersonaMode), &PersonaMode, 0);
    ILE3_TERM;

    /*
    ** Create the persona
    */
    status = SYS$PERSONA_CREATE (&PersonaId, &TargetUserDesc, PersonaFlags, 0, &PersonaItems);
    if (! (status & 1))
        {
        log_err ("failed to create persona 0x%08X (%s: %s)\n", status, actual_uname, cmd);
        exit (109);
        }

    if (dbgflg) log_err("debug: Personae created for %s\n", actual_uname);
    status = SYS$PERSONA_ASSUME (&PersonaId, 0);
    if (! (status & 1))
        {
        log_err ("failed to assume persona 0x%08X (%s: %s)\n", status, actual_uname, cmd);
        exit (110);
        }

    if (dbgflg) log_err("debug: Personae assumed for %s\n", actual_uname);
#else
    /*
     * Error out if attempt is made to execute as root or as
     * a UID less than AP_UID_MIN.  Tsk tsk.
     */
    if ((uid == 0) || (uid < AP_UID_MIN)) {
        log_err("cannot run as forbidden uid (%lu/%s)\n", (unsigned long)uid, cmd);
        exit(107);
    }

    /*
     * Error out if attempt is made to execute as root group
     * or as a GID less than AP_GID_MIN.  Tsk tsk.
     */
    if ((gid == 0) || (gid < AP_GID_MIN)) {
        log_err("cannot run as forbidden gid (%lu/%s)\n", (unsigned long)gid, cmd);
        exit(108);
    }

    /*
     * Change UID/GID here so that the following tests work over NFS.
     *
     * Initialize the group access list for the target user,
     * and setgid() to the target group. If unsuccessful, error out.
     */
    if (((setgid(gid)) != 0) || (initgroups(actual_uname, gid) != 0)) {
        log_err("failed to setgid/initgroups (%lu: %s): %s\n",
                (unsigned long)gid, cmd, strerror(errno));
        exit(109);
    }

    /*
     * setuid() to the target user.  Error out on fail.
     */
    if ((setuid(uid)) != 0) {
        log_err("failed to setuid (%lu: %s): %s\n",
                (unsigned long)uid, cmd, strerror(errno));
        exit(110);
    }
#endif			// __VMS

    /*
     * Get the current working directory, as well as the proper
     * document root (dependent upon whether or not it is a
     * ~userdir request).  Error out if we cannot get either one,
     * or if the current working directory is not in the docroot.
     * Use chdir()s and getcwd()s to avoid problems with symlinked
     * directories.  Yuck.
     */
    if (getcwd(cwd, AP_MAXPATH) == NULL) {
        log_err("cannot get current working directory\n");
        exit(111);
    }

    if (userdir) {
        if (((chdir(target_homedir)) != 0) ||
#ifdef __VMS
            ((chdir(userdir_suffix)) != 0) ||
#else
            ((chdir(AP_USERDIR_SUFFIX)) != 0) ||
#endif
            ((getcwd(dwd, AP_MAXPATH)) == NULL) ||
            ((chdir(cwd)) != 0)) {
            log_err("cannot get docroot information (%s)\n", target_homedir);
            exit(112);
        }
    }
    else {
#ifdef __VMS
        if (((chdir(doc_root)) != 0) ||
#else
        if (((chdir(AP_DOC_ROOT)) != 0) ||
#endif
            ((getcwd(dwd, AP_MAXPATH)) == NULL) ||
            ((chdir(cwd)) != 0)) {
#ifdef __VMS
            log_err("cannot get docroot information (%s)\n", doc_root);
#else
            log_err("cannot get docroot information (%s)\n", AP_DOC_ROOT);
#endif
            exit(113);
        }
    }

#ifdef __VMS
    /*
    ** Make sure that the current working directory is located under
    ** the specified document directory.
    */
    status = sub_directory (dwd, cwd, &dir_info);
    if (status == -1)
        {
        log_err ("error: cannot determine if command is within docroot (%s/%s), (%d, %08X)\n",
                 cwd, cmd, errno, vaxc$errno);
        exit (114);
        }
    if (status == 0)
        {
        log_err ("error: command not in docroot (%s/%s)\n", cwd, cmd);
        exit (114);
        }

    if (dbgflg) log_err("debug: command in correct directory (%s)\n", cwd);
#else
    if ((strncmp(cwd, dwd, strlen(dwd))) != 0) {
        log_err("command not in docroot (%s/%s)\n", cwd, cmd);
        exit(114);
    }

    /*
     * Stat the cwd and verify it is a directory, or error out.
     */
    if (((lstat(cwd, &dir_info)) != 0) || !(S_ISDIR(dir_info.st_mode))) {
        log_err("cannot stat directory: (%s)\n", cwd);
        exit(115);
    }
#endif		// __VMS

    /*
     * Error out if cwd is writable by others.
     */
#ifdef __VMS
    status = acl_access (&dir_info, S_IWOTH | S_IWGRP);
    if (status == -1)
        {
        log_err ("directory ACL access check: (%s), (%d, %08X)\n", cwd, errno, vaxc$errno);
        exit (115);
        }
    if (status == 1)
        {
        log_err ("directory is writable by others: (%s)\n", cwd);
        exit (116);
        }

    if (dbgflg) log_err("debug: Access to directory via ACL is ok\n");
#else
    if ((dir_info.st_mode & S_IWOTH) || (dir_info.st_mode & S_IWGRP)) {
        log_err("directory is writable by others: (%s)\n", cwd);
        exit(116);
    }
#endif

    /*
     * Error out if we cannot stat the program.
     */
    if (((lstat(cmd, &prg_info)) != 0) || (S_ISLNK(prg_info.st_mode))) {
        log_err("cannot stat program: (%s)\n", cmd);
        exit(117);
    }

    /*
     * Error out if the program is writable by others.
     */
#ifdef __VMS
    if (dbgflg) log_err("debug: STAT on command %s was good\n", cmd);

    status = acl_access (&prg_info, S_IWOTH | S_IWGRP);
    if (status == -1)
        {
        log_err ("program ACL access check: (%s), (%d, %08X)\n", cmd, errno, vaxc$errno);
        exit (117);
        }
    if (status == 1)
        {
        log_err ("file is writable by others: (%s/%s)\n", cwd, cmd);
        exit (118);
        }
    if (dbgflg) log_err("debug: Access to command is ok\n");
#endif
    if ((prg_info.st_mode & S_IWOTH) || (prg_info.st_mode & S_IWGRP)) {
        log_err("file is writable by others: (%s/%s)\n", cwd, cmd);
        exit(118);
    }

    /*
     * Error out if the file is setuid or setgid.
     */
    if ((prg_info.st_mode & S_ISUID) || (prg_info.st_mode & S_ISGID)) {
        log_err("file is either setuid or setgid: (%s/%s)\n", cwd, cmd);
        exit(119);
    }

    /*
     * Error out if the target name/group is different from
     * the name/group of the cwd or the program.
     */
    if ((uid != dir_info.st_uid) ||
        (gid != dir_info.st_gid) ||
        (uid != prg_info.st_uid) ||
        (gid != prg_info.st_gid)) {
        log_err("target uid/gid (%lu/%lu) mismatch "
                "with directory (%lu/%lu) or program (%lu/%lu)\n",
                (unsigned long)uid, (unsigned long)gid,
                (unsigned long)dir_info.st_uid, (unsigned long)dir_info.st_gid,
                (unsigned long)prg_info.st_uid, (unsigned long)prg_info.st_gid);
        exit(120);
    }
    /*
     * Error out if the program is not executable for the user.
     * Otherwise, she won't find any error in the logs except for
     * "[error] Premature end of script headers: ..."
     */
    if (!(prg_info.st_mode & S_IXUSR)) {
        log_err("file has no execute permission: (%s/%s)\n", cwd, cmd);
        exit(121);
    }

#ifdef AP_SUEXEC_UMASK
    /*
     * umask() uses inverse logic; bits are CLEAR for allowed access.
     */
    if ((~AP_SUEXEC_UMASK) & 0022) {
        log_err("notice: AP_SUEXEC_UMASK of %03o allows "
                "write permission to group and/or other\n", AP_SUEXEC_UMASK);
    }
    umask(AP_SUEXEC_UMASK);
#endif /* AP_SUEXEC_UMASK */

#ifdef __VMS
    /*
    ** Clean the environment variables
    */
    clean_arg();
    clean_env();

    /*
    ** Pass the CGI data
    */
    apr$dcl_pass_arg (arg_data);
    apr$dcl_pass_env (env_data);
    if (dbgflg) log_err("debug: ENV & ARG setup to run cmd\n");
#endif

    /* Be sure to close the log file so the CGI can't mess with it. */
#ifdef AP_LOG_SYSLOG
    if (log_open) {
        closelog();
        log_open = 0;
    }
#else
    if (log != NULL) {
#if APR_HAVE_FCNTL_H
        /*
         * ask fcntl(2) to set the FD_CLOEXEC flag on the log file,
         * so it'll be automagically closed if the exec() call succeeds.
         */
        fflush(log);
        setbuf(log, NULL);
        if ((fcntl(fileno(log), F_SETFD, FD_CLOEXEC) == -1)) {
            log_err("error: can't set close-on-exec flag");
            exit(122);
        }
#else
        /*
         * In this case, exec() errors won't be logged because we have already
         * dropped privileges and won't be able to reopen the log file.
         */
        fclose(log);
        log = NULL;
#endif
    }
#endif

    /*
     * Execute the command, replacing our image with its own.
     */
#ifdef __VMS
    if (dbgflg) log_err("debug: Executing: %s\n", script_name);
    sprintf (SuExecCgiArg1, "SUEXEC:%s", getenv ("P1"));
    execl (script_name, SuExecCgiArg1, NULL);
#else
#ifdef NEED_HASHBANG_EMUL
    /* We need the #! emulation when we want to execute scripts */
    {
        extern char **environ;

        ap_execve(cmd, &argv[3], environ);
    }
#else /*NEED_HASHBANG_EMUL*/
    execv(cmd, &argv[3]);
#endif /*NEED_HASHBANG_EMUL*/
#endif

    /*
     * (I can't help myself...sorry.)
     *
     * Uh oh.  Still here.  Where's the kaboom?  There was supposed to be an
     * EARTH-shattering kaboom!
     *
     * Oh well, log the failure and error out.
     */
    log_err("(%d)%s: exec failed (%s)\n", errno, strerror(errno), cmd);
    exit(255);
}

#ifdef __VMS
static int has_identifier (struct dsc$descriptor *UserDesc, struct dsc$descriptor *IdentDesc)
{
unsigned int Holder[2] = {0, 0};
unsigned int RdbCtx = 0;
unsigned int RdbIdent;
unsigned int Ident;
int HasIdent = FALSE;
ILE3 JpiItems[2],
     *Ile3Ptr;
int status;

/*
** Convert the username string to a value
*/
status = SYS$ASCTOID (UserDesc,
                      &Holder[0],
       		      0);
if (! (status & 1))
    {
    decc$$translate (status);
    return (-1);
    }

/*
** Convert the identifier string to a value
*/
status = SYS$ASCTOID (IdentDesc,
                      &Ident,
     		      0);
if (! (status & 1))
    {
    decc$$translate (status);
    return (-1);
    }

/*
** Loop through all the rights this user is authorized to hold
*/
while (TRUE)
    {
    /*
    ** Find the rights identifiers held by this user and see if this they have
    ** requested rights identifier
    */
    status = SYS$FIND_HELD ((struct _generic_64 *) Holder,
			    &RdbIdent,
			    0,
			    &RdbCtx);
    if (! (status & 1))
	{
	decc$$translate (status);
	if (status != SS$_NOSUCHID)
	    {
	    SYS$FINISH_RDB (&RdbCtx);
	    return (-1);
	    }
	break;
	}
    if (Ident == RdbIdent)
	{
	HasIdent = TRUE;
	break;
	}
    }

/*
** Cleanup the rights identifier search context
*/
status = SYS$FINISH_RDB (&RdbCtx);
if (! (status & 1))
    {
    decc$$translate (status);
    return (-1);
    }

/*
** Return HasIdent indicator
*/
return (HasIdent);

}

static int sub_directory (char *TopDirSpec, char *SubDirSpec, struct stat *SubDirStat)
{
struct dsc$descriptor DevDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
struct dsc$descriptor FibDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
struct stat TopDirStat;
int SubDirectory = FALSE;
unsigned short DevChan;
ILE2 AtrItems[2],
     *Ile2Ptr;
char *TopDirDev;
FH2 FileHdr;
FIBDEF Fib;
int status;
IOSB iosb;

/*
** Stat the top directory spec
*/
if ((stat (TopDirSpec, &TopDirStat) != 0) || !S_ISDIR(TopDirStat.st_mode))
    {
    log_err ("error: cannot stat directory: (%s)\n", TopDirSpec);
    return(-1);
    }

/*
** Save the top directory device to be compared to the sub directory device because
** the stat function reuses the device address.
*/
TopDirDev = strdup (TopDirStat.st_dev);

/*
** Stat the sub directory spec
*/
if ((stat (SubDirSpec, SubDirStat) != 0) || !S_ISDIR(SubDirStat->st_mode))
    {
    log_err ("error: cannot stat directory: (%s)\n", SubDirSpec);
    return(-1);
    }

/*
** Check to see that the device names are the same
*/
if (strcmp (TopDirDev, SubDirStat->st_dev) != 0)
    {
    free (TopDirDev);
    return (-1);
    }

/*
** Free the top directory device since the two devices are the same
*/
free (TopDirDev);

/*
** Check to see if the two directories are the same
*/
if (TopDirStat.st_ino[0] == SubDirStat->st_ino[0] &&
    TopDirStat.st_ino[1] == SubDirStat->st_ino[1] &&
    TopDirStat.st_ino[2] == SubDirStat->st_ino[2])
    return (TRUE);

/*
** Setup the device descriptor
*/
DevDesc.dsc$a_pointer = SubDirStat->st_dev;
DevDesc.dsc$w_length = strlen (SubDirStat->st_dev);

/*
** Assign a Channel to the Root Device
*/
status = SYS$ASSIGN (&DevDesc, &DevChan, 0, 0);
if (! (status & 1))
    {
    decc$$translate (status);
    return (-1);
    }

/*
** Initialize the FIB
*/
memset (&Fib, 0, sizeof (Fib));
Fib.fib$w_fid_num = SubDirStat->st_ino[0];
Fib.fib$w_fid_seq = SubDirStat->st_ino[1];
Fib.fib$w_fid_rvn = SubDirStat->st_ino[2];

/*
** Setup the device descriptor
*/
FibDesc.dsc$a_pointer = (void *) &Fib;
FibDesc.dsc$w_length = sizeof (Fib);

/*
** Setup Attribute Item List
*/
ILE2_INIT (AtrItems);
ILE2_ADD  (ATR$C_HEADER, ATR$S_HEADER, &FileHdr);
ILE2_TERM;

while (TRUE)
    {
    /*
    ** Get the file header
    */
    status = SYS$QIOW (0, DevChan, IO$_ACCESS, &iosb, 0, 0, &FibDesc, 0, 0, 0, (__int64) &AtrItems, 0);
    if (status & 1)
	status = iosb.iosb$w_status;
    if (! (status & 1))
	{
	decc$$translate (status);
	SYS$DASSGN (DevChan);
	return (-1);
	}

    /*
    ** Check to see if the back-link of this file is the top directory
    */
    if (FileHdr.fh2$w_bk_fidnum == TopDirStat.st_ino[0] &&
        FileHdr.fh2$w_bk_fidseq == TopDirStat.st_ino[1] &&
        FileHdr.fh2$w_bk_fidrvn == TopDirStat.st_ino[2])
	{
	SubDirectory = TRUE;
	break;
	}

    /*
    ** Check to see if the back-link of this file is the master file directory
    */
    if (FileHdr.fh2$w_bk_fidnum == FID$C_MFD &&
        FileHdr.fh2$w_bk_fidseq == FID$C_MFD &&
        FileHdr.fh2$w_bk_fidrvn == 0)
	break;

    /*
    ** Change the lookup file to the back-link file
    */
    Fib.fib$w_fid_num = FileHdr.fh2$w_bk_fidnum;
    Fib.fib$w_fid_seq = FileHdr.fh2$w_bk_fidseq;
    Fib.fib$w_fid_rvn = FileHdr.fh2$w_bk_fidrvn;
    }

/*
** Deassign our channel to the device
*/
SYS$DASSGN (DevChan);

/*
** Return status
*/
return (SubDirectory);

}

static int acl_access (struct stat *FileStat, int FileAccess)
{
struct dsc$descriptor DevDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
struct dsc$descriptor FibDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
unsigned short DevChan;
int AclAccess = FALSE;
UICDEF *OwnerUicPtr,
       *AceUicPtr;
ILE2 AtrItems[2],
     *Ile2Ptr;
ACEDEF *AcePtr;
char *AclBuf;
int AclLen;
FIBDEF Fib;
int status;
IOSB iosb;
int i;

/*
** Setup the device descriptor
*/
DevDesc.dsc$a_pointer = FileStat->st_dev;
DevDesc.dsc$w_length = strlen (FileStat->st_dev);

/*
** Assign a Channel to the Root Device
*/
status = SYS$ASSIGN (&DevDesc, &DevChan, 0, 0);
if (! (status & 1))
    {
    decc$$translate (status);
    return (-1);
    }

/*
** Initialize the FIB
*/
memset (&Fib, 0, sizeof (Fib));
Fib.fib$w_fid_num = FileStat->st_ino[0];
Fib.fib$w_fid_seq = FileStat->st_ino[1];
Fib.fib$w_fid_rvn = FileStat->st_ino[2];

/*
** Setup the device descriptor
*/
FibDesc.dsc$a_pointer = (void *) &Fib;
FibDesc.dsc$w_length = sizeof (Fib);

/*
** Setup Attribute Item List
*/
ILE2_INIT (AtrItems);
ILE2_ADD  (ATR$C_ACLLENGTH, ACL$S_ACLLENGTH, &AclLen);
ILE2_TERM;

/*
** Get the File Spec
*/
status = SYS$QIOW (0, DevChan, IO$_ACCESS, &iosb, 0, 0, &FibDesc, 0, 0, 0, (__int64) &AtrItems, 0);
if (status & 1)
    status = iosb.iosb$w_status;
if (! (status & 1))
    {
    decc$$translate (status);
    SYS$DASSGN (DevChan);
    return (-1);
    }

/*
** If there are no ACE for this file then return false
*/
if (! AclLen)
    {
    SYS$DASSGN (DevChan);
    return (FALSE);
    }

/*
** Establish a pointer to the file owner's UIC
*/
OwnerUicPtr = (UICDEF *) &FileStat->st_uid;

/*
** Allocate the ACL segment buffer
*/
AclBuf = malloc (ACL$C_MAX_SEGMENT_SIZE);
if (! AclBuf)
    return (-1);

/*
** Loop through all the ACLs for this file until we have
*/
while (! AclAccess)
    {
    /*
    ** Initialize the ACL buffer
    */
    memset (AclBuf, 0, ACL$C_MAX_SEGMENT_SIZE);

    /*
    ** Setup Attribute Item List
    */
    ILE2_INIT (AtrItems);
    ILE2_ADD  (ATR$C_READACL, ACL$C_MAX_SEGMENT_SIZE, AclBuf);
    ILE2_TERM;

    /*
    ** Get the File ACL Segment
    */
    status = SYS$QIOW (0, DevChan, IO$_ACCESS, &iosb, 0, 0, &FibDesc, 0, 0, 0, (__int64) &AtrItems, 0);
    if (status & 1)
	status = iosb.iosb$w_status;
    if (status & 1)
	status = Fib.fib$l_acl_status;
    if (! (status & 1))
	{
	if (status != SS$_NOMOREACE)
	    {
	    decc$$translate (status);
	    SYS$DASSGN (DevChan);
	    free (AclBuf);
	    return (-1);
	    }
	break;
	}

    /*
    ** Let's walk throught the ACEs in this segment
    */
    AcePtr = (ACEDEF *) AclBuf;
    while (! AclAccess && (char *) AcePtr < AclBuf + ACL$C_MAX_SEGMENT_SIZE && AcePtr->ace$b_size > 0)
	{
	if (AcePtr->ace$b_type == ACE$C_KEYID)
	    {
	    /*
	    ** Establish a pointer to the ACE UIC
	    */
	    AceUicPtr = (UICDEF *) &AcePtr->ace$l_key;

	    /*
	    ** Check to see if the ACE specifies Control access
	    */
	    if (AcePtr->ace$l_access & ACE$M_CONTROL && FileAccess)
		{
		/*
		** If Control access is granted to anyone other than the owner of the file
		** then indicate ACL access is allowed.
		*/
	        for (i = AcePtr->ace$v_reserved; (int) &AceUicPtr[i] < ((int) AcePtr + AcePtr->ace$b_size); i++)
		    if (AceUicPtr[i].uic$w_grp != OwnerUicPtr->uic$w_grp && AceUicPtr[i].uic$w_mem != OwnerUicPtr->uic$w_mem)
			AclAccess = TRUE;
		}

	    /*
	    ** Check to see if the ACE specifies Read access
	    */
	    if (AcePtr->ace$l_access & ACE$M_READ && (FileAccess & S_IRUSR || FileAccess & S_IRGRP || FileAccess & S_IROTH))
		{
		/*
		** If Read access is granted to anyone other than the owner of the file
		** then indicate ACL access is allowed.
		*/
	        for (i = AcePtr->ace$v_reserved; (int) &AceUicPtr[i] < ((int) AcePtr + AcePtr->ace$b_size); i++)
		    if (AceUicPtr[i].uic$w_grp != OwnerUicPtr->uic$w_grp && AceUicPtr[i].uic$w_mem != OwnerUicPtr->uic$w_mem)
			AclAccess = TRUE;
		}

	    /*
	    ** Check to see if the ACE specifies Write access
	    */
	    if (AcePtr->ace$l_access & ACE$M_WRITE && (FileAccess & S_IWUSR || FileAccess & S_IWGRP || FileAccess & S_IWOTH))
		{
		/*
		** If Write access is granted to anyone other than the owner of the file
		** then indicate ACL access is allowed.
		*/
	        for (i = AcePtr->ace$v_reserved; (int) &AceUicPtr[i] < ((int) AcePtr + AcePtr->ace$b_size); i++)
		    if (AceUicPtr[i].uic$w_grp != OwnerUicPtr->uic$w_grp && AceUicPtr[i].uic$w_mem != OwnerUicPtr->uic$w_mem)
			AclAccess = TRUE;
		}

	    /*
	    ** Check to see if the ACE specifies Execute access
	    */
	    if (AcePtr->ace$l_access & ACE$M_EXECUTE && (FileAccess & S_IXUSR || FileAccess & S_IXGRP || FileAccess & S_IXOTH))
		{
		/*
		** If Execute access is granted to anyone other than the owner of the file
		** then indicate ACL access is allowed.
		*/
	        for (i = AcePtr->ace$v_reserved; (int) &AceUicPtr[i] < ((int) AcePtr + AcePtr->ace$b_size); i++)
		    if (AceUicPtr[i].uic$w_grp != OwnerUicPtr->uic$w_grp && AceUicPtr[i].uic$w_mem != OwnerUicPtr->uic$w_mem)
			AclAccess = TRUE;
		}
	    }

	/*
	** Increment the ACE pointer in this buffer
	*/
	AcePtr = (ACEDEF *) ((int) AcePtr + AcePtr->ace$b_size);
	}
    }

/*
** Deassign our channel to the device
*/
SYS$DASSGN (DevChan);

/*
** Free the ACL segment buffer
*/
free (AclBuf);

/*
** Return status
*/
return (AclAccess);

}

static int load_data (int data_type)
{
char *dcl_proc_ctx = getenv ("APACHE$DCL_PROC_CTX");
int data_buf_max = 0,
    data_len = 0,
    data_ctr = 0,
    eval_data = 0,
    save_data = 1,
    data_pass;
char data_name[256];
void *data_gs = NULL;
FILE *data_fd = NULL;
char *data_buf = NULL,
     *eql_sign,
     *data_nam,
     *data_val;

/*
** Open the data global section
*/
sprintf (data_name, "APACHE$SHM_DCL_%s_%08X%s",
        (data_type == ARG_DATA ? "ARG" : "ENV"), (getppid () ? getppid () : getppid ()),
        (dcl_proc_ctx ? dcl_proc_ctx : ""));
data_gs = apr$dcl_shm_open (data_name);
if (! data_gs)
    {
    log_err ("error: Opening %s global section (%08X)\n   Global Section: (%s)\n",
	    (data_type == ARG_DATA ? "argument" : "environment"), vaxc$errno, data_name);
    return (FALSE);
    }

/*
** Do two passes through the data first to evaluate and then to save
*/
for (data_pass = eval_data; data_pass <= save_data; data_pass++)
    {
    /*
    ** Process the data
    */
    while (1)
	{
	/*
	** Read the data length from the global section
	*/
	if (apr$dcl_shm_read (&data_len, sizeof (data_len), 1, data_gs) == 0)
	    break;
	if (data_len < 0)
	    break;

	/*
	** Determine whether the data read buffer is large enough
	*/
	if (data_len > data_buf_max)
	    {
	    /*
	    ** Free the previous data buffer (if necessary)
	    */
	    if (data_buf)
		free (data_buf);

	    /*
	    ** If this is our first time through then let's allocate a buffer
	    ** large enough for most data variables.
	    */
	    data_buf_max = MAX (data_len, SYM_VAL_MAX);

	    /*
	    ** Allocate the data read buffer
	    */
	    data_buf = malloc (data_buf_max + 1);
	    if (! data_buf)
		{
		log_err ("error: Allocating data read buffer (%d bytes)\n", data_buf_max);
		apr$dcl_shm_close (data_gs);
		return (FALSE);
		}
	    }

	/*
	** Read the data value from the global section
	*/
	if (apr$dcl_shm_read (data_buf, sizeof (char), data_len, data_gs) != data_len)
	    {
	    fprintf (stderr, "Error: end of data global section reached\n");
	    apr$dcl_shm_close (data_gs);
	    free (data_buf);
	    exit (1);
	    }

	/*
	** If this is not the first time thru the environment data then load the
	** environment array
	*/
	if (data_pass == save_data)
	    {
	    /*
	    ** Null terminate the environment record
	    */
	    data_buf[data_len] = '\0';
	    if (data_type == ARG_DATA)
		arg_data[data_ctr] = strdup (data_buf);
	    else
		env_data[data_ctr] = strdup (data_buf);
	    }
	data_ctr++;
	}

    /*
    ** If this is the first pass thru the environment data then let's rewind
    */
    if (data_pass == eval_data)
        {
	/*
	** Rewind the data global section
	*/
	apr$dcl_shm_rewind (data_gs);

	/*
	** Allocate an environment array
	*/
	if (data_type == ARG_DATA)
	    {
	    arg_data = malloc ((data_ctr + 1) * sizeof (char *));
	    memset (arg_data, 0, (data_ctr + 1) * sizeof (char *));
	    }
	else
	    {
	    env_data = malloc ((data_ctr + 1) * sizeof (char *));
	    memset (env_data, 0, (data_ctr + 1) * sizeof (char *));
	    }
	data_ctr = 0;
	}
    }

/*
** Close the data global section
*/
apr$dcl_shm_close (data_gs);

/*
** Return success
*/
return (TRUE);

}

static void clean_arg (void)
{
char **cleanarg;
char **ap;
int cidx = 0;
int cmax;

for (cmax = 0; arg_data && arg_data[cmax]; cmax++);
if ((cleanarg = (char **) calloc ((cmax + 1), sizeof (char *))) == NULL)
    {
    log_err ("emerg: failed to malloc memory for arguments\n");
    exit (120);
    }

for (ap = arg_data; ap && *ap && cidx < cmax; ap++)
    {
    if (! strncmp (*ap, "#", 1))
	{
	cleanarg[cidx] = *ap + 1;
	cidx++;
	}
    }

cleanarg[cidx] = NULL;

free (arg_data);
arg_data = cleanarg;

}

#endif
