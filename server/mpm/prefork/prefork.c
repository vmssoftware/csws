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

#include "apr.h"
#include "apr_portable.h"
#include "apr_strings.h"
#include "apr_thread_proc.h"
#include "apr_signal.h"

#define APR_WANT_STDIO
#define APR_WANT_STRFUNC
#include "apr_want.h"

#if APR_HAVE_UNISTD_H
#include <unistd.h>
#endif
#if APR_HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "ap_config.h"
#include "httpd.h"
#include "mpm_default.h"
#include "http_main.h"
#include "http_log.h"
#include "http_config.h"
#include "http_core.h"          /* for get_remote_host */
#include "http_connection.h"
#include "scoreboard.h"
#include "ap_mpm.h"
#include "util_mutex.h"
#include "unixd.h"
#include "mpm_common.h"
#include "ap_listen.h"
#include "ap_mmn.h"
#include "apr_poll.h"

#include <stdlib.h>

#ifdef HAVE_TIME_H
#include <time.h>
#endif
#ifdef HAVE_SYS_PROCESSOR_H
#include <sys/processor.h> /* for bindprocessor() */
#endif

#include <signal.h>
#include <sys/times.h>

#ifdef __VMS
#include "apr_arch_networkio.h"
#include "apr_getopt.h"
#include <ctype.h>
#include <descrip.h>

#ifndef __NEW_STARLET
#define __NEW_STARLET
#define __NEW_STARLET_SET
#endif
#include <lib$routines.h>
#include <fscndef.h>
#include <starlet.h>
#include <lksbdef.h>
#include <iosbdef.h>
#include <efndef.h>
#include <jpidef.h>
#include <uaidef.h>
#include <syidef.h>
#include <lgidef.h>
#include <lckdef.h>
#include <psldef.h>
#include <iledef.h>
#include <dvidef.h>
#include <accdef.h>
#include <stsdef.h>
#include <prcdef.h>
#include <lnmdef.h>
#include <iodef.h>
#include <ssdef.h>
#include <rms.h>

extern int apr$mbx_create (unsigned short int *MbxChan, ...);
extern int apr$mbx_delete (unsigned short int MbxChan);

#include "ilemac.h"
#include "cvtfnm.h"
#include "spl.h"
#include "sps.h"

#ifdef  __NEW_STARLET_SET
#undef  __NEW_STARLET_SET
#undef  __NEW_STARLET
#endif

#ifdef  NAML$C_BID
#define ODS5_LONG_NAME_SUPPORT
#else
#ifdef  ODS5_LONG_NAME_SUPPORT
#undef  ODS5_LONG_NAME_SUPPORT
#endif
#endif

/* Limit on the total --- clients will be locked out if more servers than
 * this are needed.  It is intended solely to keep the server from crashing
 * when things get out of hand.
 *
 * We keep a hard maximum number of servers, for two reasons --- first off,
 * in case something goes seriously wrong, we want to stop the fork bomb
 * short of actually crashing the machine we're running on by filling some
 * kernel table.  Secondly, it keeps the size of the scoreboard file small
 * enough that we can read the whole thing without worrying too much about
 * the overhead.
 */
#ifndef DEFAULT_SERVER_LIMIT
#define DEFAULT_SERVER_LIMIT 1024
#endif

#ifndef FAILURE
#define FAILURE	1
#endif
#ifndef SUCCESS
#define SUCCESS !FAILURE
#endif

/*
** Admin can't tune ServerLimit beyond MAX_SERVER_LIMIT.  We want
** some sort of compile-time limit to help catch typos.
*/
#ifndef MAX_SERVER_LIMIT
#define MAX_SERVER_LIMIT 20000
#endif

#ifndef HARD_THREAD_LIMIT
#define HARD_THREAD_LIMIT 1
#endif

#ifndef PROCESS_NAME_MAX
#define PROCESS_NAME_MAX 15
#endif

/*
** Define the mailbox AST data structure
*/
typedef struct _MbxAstData
    {
    unsigned short chan;
    IOSB iosb;
    } MBX_AST_DATA;

/*
** Define the control MBX data structure
*/
typedef struct _CtrlMsgData
    {
    unsigned short type;
    unsigned short length;
    char value[];
    } CNTL_MSG_DATA;

#define CTRL_MSG_SIZE_MIN   	4
#define CTRL_MSG_SIZE_MAX   	CTRL_MSG_SIZE_MIN + 256

/*
** Define the control message types
*/
#define APACHE$CTRL_RESTART		1
#define APACHE$CTRL_GRACEFUL		2
#define APACHE$CTRL_STOP		3
#define APACHE$CTRL_FLUSH		4
#define APACHE$CTRL_NEW			5

/*
** Define the maximum command line buffer size
*/
#define MAX_CMDLINE_SIZE  256
#define UCASE_CHARS	"ABCDEFGHIJKLMNOPQRSTUVWXYZ"

static int remaining_children_to_start;
static char *ServerSpecificDirectory = NULL;
static char *ServerShutdown = NULL;
static char *ServerStartup = NULL;
static char *ServerTag = "SWS";
static int ProcessType = 0;
static int ControlState = 0;
static int ParentPid = 0;
static int ChildPid = 0;
static int ChildSlot = 0;

static char *lock_fname = NULL; 		// BRC

static int requests_this_child = 0;
static int num_listensocks = 0;
static ap_listen_rec *listensocks;

/* volatile just in case */
static int hold_off_on_exponential_spawning;
static int volatile shutdown_pending = FALSE;
static int volatile restart_pending = FALSE;
static int volatile is_graceful = 0;

static void accept_mutex_on (void);
static void accept_mutex_off (void);
static void clean_child_exit(int code);
static void set_signals (void);

/*
** Define control routine prototypes
*/
static int ControlMain (apr_pool_t *, server_rec *);
static int CreateParent (apr_pool_t *, server_rec *);

/*
** Define parent routine prototypes
*/
static void ParentControlMbxAst (MBX_AST_DATA *);
static int ParentMain (apr_pool_t *, server_rec *);
static int ParentInit (apr_pool_t *, server_rec *);
static int ParentCore (apr_pool_t *, server_rec *);
static int ParentTerm (apr_pool_t *, server_rec *);
static int CreateChildren (int);
static int CreateChild (server_rec *, int);

/*
** Define child routine prototypes
*/
static void ChildControlMbxAst (MBX_AST_DATA *);
static int ChildMain (apr_pool_t *, server_rec *);
static int ChildInit (apr_pool_t *, server_rec *);
static int ChildCore (apr_pool_t *, server_rec *);
static int ChildTerm (apr_pool_t *, server_rec *);
static void TerminationMbxAst (MBX_AST_DATA *);

/*
** Define common routine prototypes
*/
static pid_t CreateDetached (server_rec *, char *, char *, int);
static int SendControlMbxMsg (CNTL_MSG_DATA *, char *);
static void TranslateServerSpecific (void);
static void CreateServerSpecificLnm (void);
static void DeleteServerSpecificLnm (void);
static void VerifyGracefulExits (void);

/*
** Define apache kill
*/
int apache$kill (int, int);

/*
** Define external routine prototypes
*/
extern int decc$$translate ();

#ifdef SINGLE_LISTEN_UNSERIALIZED_ACCEPT
#define SAFE_ACCEPT(stmt) do {if (ap_listeners->next) {stmt;}} while(0)
#else
#define SAFE_ACCEPT(stmt) do {stmt;} while(0)
#endif

#ifndef MIN
#define MIN(a,b) (a < b ? a : b)
#endif
#ifndef MAX
#define MAX(a,b) (a > b ? a : b)
#endif

static int idle_spawn_rate = 1;

/*
** Define the termination mailbox channel & unit
*/
static unsigned short TerminationMbxUnit = 0;

/*
** idle_spawn_rate is the number of children that will be spawned on the
** next maintenance cycle if there aren't enough idle servers.  It is
** doubled up to MAX_SPAWN_RATE, and reset only when a cycle goes by
** without the need to spawn.
*/
#ifndef MAX_SPAWN_RATE
#define MAX_SPAWN_RATE	(32)
#endif

struct extended_scoreboard {
    time_t restart_time;		/* time of last server restart */
    ap_generation_t generation;		/* Number of restarts? */
    int spare[29];
    char scoreboard[1];			/* overlapps with scoreboard */
};

static struct extended_scoreboard *scoreboard_plus = (struct extended_scoreboard *) 0;

static void ParentDied (LKSB *LckNameLKSB);
static void perform_idle_server_maintenance (apr_pool_t *p);

/*
** config globals
*/
static apr_proc_mutex_t *accept_mutex;
static int ap_daemons_to_start = 0;
static int ap_daemons_min_free = 0;
static int ap_daemons_max_free = 0;
static int ap_daemons_limit = 0;      /* MaxClients */
static int server_limit = DEFAULT_SERVER_LIMIT;
static int first_server_limit;
static int changed_limit_at_restart;

static int mpm_state = AP_MPMQ_STARTING;


/*
 * The max child slot ever assigned, preserved across restarts.  Necessary
 * to deal with MaxClients changes across AP_SIG_GRACEFUL restarts.  We
 * use this value to optimize routines that have to scan the entire scoreboard.
 */
int ap_max_daemons_limit = -1;
server_rec *ap_server_conf;

/* one_process --- debugging mode variable; can be set from the command line
 * with the -X flag.  If set, this gets you the child_main loop running
 * in the process which originally started up (no detach, no make_child),
 * which is a pretty nice debugging environment.  (You'll get a SIGHUP
 * early in standalone_main; just continue through.  This is the server
 * trying to kill off any child processes which it might have lying
 * around --- Apache doesn't keep track of their pids, it just sends
 * SIGHUP to the process group, ignoring it in the root process.
 * Continue through and you'll be fine.).
 */

static int one_process = 0;

static apr_pool_t *pconf;		/* Pool for config stuff */
static apr_pool_t *pchild;		/* Pool for httpd child stuff */
static apr_pool_t *ptrans;

static pid_t ap_my_pid;	/* it seems silly to call getpid all the time */
ap_generation_t volatile ap_my_generation = 0;

static void prefork_note_child_killed(int childnum, pid_t pid,
                                      ap_generation_t gen)
{
	/* Empty for now */
}


#ifdef GPROF
/*
 * change directory for gprof to plop the gmon.out file
 * configure in httpd.conf:
 * GprofDir $RuntimeDir/   -> $ServerRoot/$RuntimeDir/gmon.out
 * GprofDir $RuntimeDir/%  -> $ServerRoot/$RuntimeDir/gprof.$pid/gmon.out
 */
static void chdir_for_gprof(void)
{
    core_server_config *sconf =
	ap_get_module_config(ap_server_conf->module_config, &core_module);
    char *dir = sconf->gprof_dir;
    const char *use_dir;

    if(dir) {
        apr_status_t res;
	char buf[512];
	int len = strlen(sconf->gprof_dir) - 1;
	if(*(dir + len) == '%') {
	    dir[len] = '\0';
	    apr_snprintf(buf, sizeof(buf), "%sgprof.%d", dir, (int)getpid());
	}
	use_dir = ap_server_root_relative(pconf, buf[0] ? buf : dir);
	res = apr_dir_make(use_dir, 0755, pconf);
	if(res != APR_SUCCESS && !APR_STATUS_IS_EEXIST(res)) {
	    ap_log_error(APLOG_MARK, APLOG_ERR, errno, ap_server_conf,
			 "gprof: error creating directory %s", dir);
	}
    }
    else {
	use_dir = ap_server_root_relative(pconf, DEFAULT_REL_RUNTIMEDIR);
    }

    chdir(use_dir);
}
#else
#define chdir_for_gprof()
#endif

/* XXX - I don't know if TPF will ever use this module or not, so leave
 * the ap_check_signals calls in but disable them - manoj */
#define ap_check_signals()

/******************************************************************************/
/*** ControlMain () routine.						    ***/
/******************************************************************************/
static int ControlMain (apr_pool_t *p, server_rec *s)
{
struct dsc$descriptor UserNameDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
char ControlMsgBuf[CTRL_MSG_SIZE_MAX];
CNTL_MSG_DATA *ControlMsgPtr;
unsigned int UserNamePersona,
             OrigUserPersona;
char MbxName[32+1];
int AssumePersona,
    status;

/*
** Determine whether we need to assume a persona to create this server
*/
if (strcasecmp (getlogin (), (char *) ap_unixd_config.user_name) == 0)
    AssumePersona = FALSE;
else
    AssumePersona = TRUE;

/*
** If we need a new persona, then create it now
*/
if (AssumePersona)
    {
    /*
    ** Create the user name descriptor
    */
    UserNameDesc.dsc$a_pointer = (char *) ap_unixd_config.user_name;
    UserNameDesc.dsc$w_length = strlen (ap_unixd_config.user_name);

    /*
    ** Create the user name persona (requires user have IMPERSONATE privilege)
    */
    status = SYS$PERSONA_CREATE (&UserNamePersona,
			         &UserNameDesc,
                                 0, 0, 0);
    if (! (status & 1))
	{
	decc$$translate (status);
	ap_log_error (APLOG_MARK, APLOG_ERR, errno, s,
		      "error creating persona for %s", ap_unixd_config.user_name);
	return (FAILURE);
	}

    /*
    ** Assume the user name persona
    */
    status = SYS$PERSONA_ASSUME (&UserNamePersona,
			         0,
                                 &OrigUserPersona,
			         0);
    if (! (status & 1))
	{
	decc$$translate (status);
	ap_log_error (APLOG_MARK, APLOG_ERR, errno, s,
                      "error assuming persona for %s", ap_unixd_config.user_name);
	SYS$PERSONA_DELETE (&UserNamePersona);
	return (FAILURE);
	}
    }

/*
** Determine what control state we should process
*/
switch (ControlState)
    {
    case AP_MPMQ_CONTROL_START:
	status = CreateParent (p, s);
	break;

    case AP_MPMQ_CONTROL_RESTART:
    case AP_MPMQ_CONTROL_GRACEFUL:
    case AP_MPMQ_CONTROL_STOP:
    case AP_MPMQ_CONTROL_FLUSH:
    case AP_MPMQ_CONTROL_NEW:
        /*
	** Establish the server control mailbox name
	*/
        sprintf (MbxName, "APACHE$%s_CONTROL_MBX", ServerTag);

	/*
	** Establish the control message pointer
	*/
	ControlMsgPtr = (CNTL_MSG_DATA *) ControlMsgBuf;

        /*
	** Establish the server control mailbox name
	*/
	if (ControlState == AP_MPMQ_CONTROL_RESTART)
	    ControlMsgPtr->type = APACHE$CTRL_RESTART;
	if (ControlState == AP_MPMQ_CONTROL_GRACEFUL)
	    ControlMsgPtr->type = APACHE$CTRL_GRACEFUL;
	if (ControlState == AP_MPMQ_CONTROL_STOP)
	    ControlMsgPtr->type = APACHE$CTRL_STOP;
	if (ControlState == AP_MPMQ_CONTROL_FLUSH)
	    ControlMsgPtr->type = APACHE$CTRL_FLUSH;
	if (ControlState == AP_MPMQ_CONTROL_NEW)
	    ControlMsgPtr->type = APACHE$CTRL_NEW;
        ControlMsgPtr->length = 0;

        /*
	** Send the control mailbox message
	*/
        status = SendControlMbxMsg (ControlMsgPtr, MbxName);
	break;

    default:
        ap_log_error (APLOG_MARK, APLOG_ERR, 0, s,
                      "unknown control state = %d", ControlState);
	break;
    }

/*
** If we've assumed a new persona, then revert to our original identity
*/
if (AssumePersona)
    {
    /*
    ** Revert to original identity
    */
    status = SYS$PERSONA_ASSUME (&OrigUserPersona,
			     0, 0, 0);
    if (! (status & 1))
	{
	decc$$translate (status);
	ap_log_error (APLOG_MARK, APLOG_ERR, errno, s,
		      "error reverting to original identity (persona assume)");
	}

    /*
    ** Delete the user name persona
    */
    status = SYS$PERSONA_DELETE (&UserNamePersona);
    }

/*
** Exit with the appropriate status
*/
return (status);

}
/******************************************************************************/
/*** a clean exit from a child with proper cleanup                          ***/
/******************************************************************************/
static void clean_child_exit(int code)
{
    if (pchild) {
	apr_pool_destroy(pchild);
    }
    chdir_for_gprof();
    exit(code);
}

/******************************************************************************/
/*** accept_mutex_on routine.                                               ***/
/******************************************************************************/
static void accept_mutex_on (void)
{
apr_status_t rv = apr_proc_mutex_lock (accept_mutex);
if (rv != APR_SUCCESS)
    {
    const char *msg = "couldn't grab the accept mutex";

    if (restart_pending || shutdown_pending)
	return;

    if (ap_my_generation != ap_scoreboard_image->global->running_generation)
	{
	ap_log_error (APLOG_MARK, APLOG_DEBUG, rv, NULL, msg);
	clean_child_exit (0);
	}
    else
	{
	ap_log_error (APLOG_MARK, APLOG_EMERG, rv, NULL, msg);
	exit (APEXIT_CHILDFATAL);
	}
    }

}

/******************************************************************************/
/*** accept_mutex_off routine.                                              ***/
/******************************************************************************/
static void accept_mutex_off (void)
{
apr_status_t rv = apr_proc_mutex_unlock (accept_mutex);
if (rv != APR_SUCCESS)
    {
    const char *msg = "couldn't release the accept mutex";

    if (restart_pending || shutdown_pending)
	return;

    if (ap_my_generation != ap_scoreboard_image->global->running_generation)
	{
	ap_log_error (APLOG_MARK, APLOG_DEBUG, rv, NULL, msg);
	/*
	** don't exit here... we have a connection to
	** process, after which point we'll see that the
	** generation changed and we'll exit cleanly
	*/
	}
    else
	{
	ap_log_error (APLOG_MARK, APLOG_EMERG, rv, NULL, msg);
	exit (APEXIT_CHILDFATAL);
	}
    }

}
/*
** On some architectures it's safe to do unserialized accept()s in the single
** Listen case.  But it's never safe to do it in the case where there's
** multiple Listen statements.  Define SINGLE_LISTEN_UNSERIALIZED_ACCEPT
** when it's safe in the single Listen case.
*/


/******************************************************************************/
/******************************************************************************/
/* See definition of AP_PLATFORM_REWRITE_ARGS_HOOK in os.h */
void ap_mpm_vms_rewrite_args (process_rec *process)
{
apr_array_header_t *mpm_new_argv;
const char *optarg;
apr_getopt_t *opt;
int argcnt = 0;
char optbuf[3];

/*
** Create a new argument array.
*/
mpm_new_argv = apr_array_make (process->pool, process->argc,
			       sizeof (const char *));

/*
** Push the first argument which should be the program name.
*/
*(const char **) apr_array_push(mpm_new_argv) = process->argv[0];
argcnt += 1;

/*
** Initialize the get options context
*/
apr_getopt_init (&opt, process->pool, process->argc, process->argv);
opt->errfn = NULL;

/*
** Initialize the option buffer used to pushed the nonconsumed/error options
** back onto the stack.
*/
optbuf[0] = '-';
optbuf[1] = '\0';
optbuf[2] = '\0';

/*
** Process each argument provided on the command line
*/
while (apr_getopt (opt, "k:p:w:" AP_SERVER_BASEARGS, &optbuf[1], &optarg)
	== APR_SUCCESS)
    {
    /*
    ** Parse the command line options
    */
    switch (optbuf[1])
	{
	/*
	** Parse the control option
	*/
        case 'k':
	    if (ProcessType)
		{
		*(const char **) apr_array_push (mpm_new_argv) =
		    apr_pstrdup (process->pool, optbuf);
		argcnt += 1;
		if (optarg)
		    {
		    *(const char **) apr_array_push (mpm_new_argv) = optarg;
		    argcnt += 1;
		    }
		}
	    else
		ProcessType = AP_MPMQ_IS_CONTROL_PROCESS;

	    /*
	    ** Parse the control option
	    */
	    if (strcasecmp (optarg, "start") == 0)
		ControlState = AP_MPMQ_CONTROL_START;
	    else
	    if (strcasecmp (optarg, "restart") == 0)
	    	ControlState = AP_MPMQ_CONTROL_RESTART;
	    else
	    if (strcasecmp (optarg, "graceful") == 0)
	        ControlState = AP_MPMQ_CONTROL_GRACEFUL;
	    else
	    if (strcasecmp (optarg, "stop") == 0)
	        ControlState = AP_MPMQ_CONTROL_STOP;
	    else
	    if (strcasecmp (optarg, "flush") == 0)
	        ControlState = AP_MPMQ_CONTROL_FLUSH;
	    else
	    if (strcasecmp (optarg, "new") == 0)
	        ControlState = AP_MPMQ_CONTROL_NEW;
	    else
	        {
	        *(const char **) apr_array_push (mpm_new_argv) =
		    apr_pstrdup (process->pool, optbuf);
		argcnt += 1;
		if (optarg)
		    {
		    *(const char **) apr_array_push (mpm_new_argv) = optarg;
		    argcnt += 1;
		    }
	        }
            break;

	/*
	** Parse the parent option
	*/
        case 'p':
	    if (ProcessType != 0 &&
	        ProcessType != AP_MPMQ_IS_CHILD_PROCESS)
		{
		*(const char **) apr_array_push (mpm_new_argv) =
		    apr_pstrdup (process->pool, optbuf);
		argcnt += 1;
		if (optarg)
		    {
		    *(const char **) apr_array_push (mpm_new_argv) = optarg;
		    argcnt += 1;
		    }
		}
	    else
	        if (ProcessType != AP_MPMQ_IS_CHILD_PROCESS)
		    ProcessType = AP_MPMQ_IS_PARENT_PROCESS;

	    /*
	    ** Parse the parent process id
	    */
	    ParentPid = atoi (optarg);
            break;

	/*
	** Parse the child slot option
	*/
        case 'w':
	    if (ProcessType != 0 &&
	        ProcessType != AP_MPMQ_IS_PARENT_PROCESS)
		{
		*(const char **) apr_array_push (mpm_new_argv) =
		    apr_pstrdup (process->pool, optbuf);
		argcnt += 1;
		if (optarg)
		    {
		    *(const char **) apr_array_push (mpm_new_argv) = optarg;
		    argcnt += 1;
		    }
		}
	    else
		ProcessType = AP_MPMQ_IS_CHILD_PROCESS;

	    /*
	    ** Parse the child slot number
	    */
	    ChildSlot = atoi (optarg);
            break;

	/*
	** Push any non-VMS specific options back onto the stack
	*/
        default :
            *(const char **) apr_array_push (mpm_new_argv) =
		apr_pstrdup (process->pool, optbuf);
            argcnt += 1;
            if (optarg)
		{
                *(const char **) apr_array_push (mpm_new_argv) = optarg;
		argcnt += 1;
		}
            break;
	}
    }

/*
** Reset the count of the new argument array if necessary
*/
if (argcnt != process->argc)
    {
    process->argc = argcnt;
    process->argv = (const char * const *) mpm_new_argv->elts;
    }

}

/******************************************************************************/
/*** sig_coredump routine.                                                  ***/
/******************************************************************************/
static void sig_coredump(int sig)
{

chdir (ap_coredump_dir);
apr_signal (sig, SIG_DFL);
if (ap_my_pid == ParentPid)
	{
	ap_log_error (APLOG_MARK, APLOG_NOTICE, 0, ap_server_conf,
				  "seg fault or similar nasty error detected "
				  "in the parent process");
	}

kill (getpid(), sig);

/*
** At this point we've got sig blocked, because we're still inside
** the signal handler.  When we leave the signal handler it will
** be unblocked, and we'll take the signal... and coredump or whatever
** is appropriate for this particular Unix.  In addition the parent
** will see the real signal we received -- whereas if we called
** abort() here, the parent would only see SIGABRT.
*/

}

/******************************************************************************/
/*** ParentDied routine.                                                    ***/
/******************************************************************************/
static void ParentDied (LKSB *LckNameLKSB)
{

fprintf (stdout, "INFO: Parent process has died, child process will now exit ...\n");
clean_child_exit (0);

}

/******************************************************************************/
/*** just_die routine.                                                      ***/
/******************************************************************************/
static void just_die (int sig)
{

clean_child_exit (0);

}


/******************************************************************************/
/*** sig_term routine.                                                      ***/
/******************************************************************************/
static void sig_term (int sig)
{

if (shutdown_pending == 1)
    {
    /*
    ** Um, is this _probably_ not an error, if the user has
    ** tried to do a shutdown twice quickly, so we won't
    ** worry about reporting it.
    */
    return;
    }

shutdown_pending = 1;

}

/******************************************************************************/
/*** restart routine.                                                       ***/
/******************************************************************************/
/*
** restart() is the signal handler for SIGHUP and AP_SIG_GRACEFUL
** in the parent process, unless running in ONE_PROCESS mode
*/
static void restart (int sig)
{

if (restart_pending == 1)
    {
    /*
    ** Probably not an error - don't bother reporting it
    */
    return;
    }

restart_pending = 1;

is_graceful = (sig == AP_SIG_GRACEFUL);

}

/******************************************************************************/
/*** set_signals routine.                                                   ***/
/******************************************************************************/
static void set_signals (void)
{
#ifndef NO_USE_SIGACTION
struct sigaction sa;

sigemptyset (&sa.sa_mask);
sa.sa_flags = 0;

if (! one_process)
    {
    sa.sa_handler = sig_coredump;
#if defined(SA_ONESHOT)
    sa.sa_flags = SA_ONESHOT;
#elif defined(SA_RESETHAND)
    sa.sa_flags = SA_RESETHAND;
#endif

    if (sigaction (SIGSEGV, &sa, NULL) < 0)
        ap_log_error (APLOG_MARK, APLOG_WARNING, errno, ap_server_conf,
                      "sigaction(SIGSEGV)");
#ifdef SIGBUS
    if (sigaction (SIGBUS, &sa, NULL) < 0)
        ap_log_error (APLOG_MARK, APLOG_WARNING, errno, ap_server_conf,
                      "sigaction(SIGBUS)");
#endif
#ifdef SIGABORT
    if (sigaction (SIGABORT, &sa, NULL) < 0)
        ap_log_error (APLOG_MARK, APLOG_WARNING, errno, ap_server_conf,
                      "sigaction(SIGABORT)");
#endif
#ifdef SIGABRT
    if (sigaction (SIGABRT, &sa, NULL) < 0)
        ap_log_error (APLOG_MARK, APLOG_WARNING, errno, ap_server_conf,
                      "sigaction(SIGABRT)");
#endif
#ifdef SIGILL
    if (sigaction (SIGILL, &sa, NULL) < 0)
        ap_log_error (APLOG_MARK, APLOG_WARNING, errno, ap_server_conf,
                      "sigaction(SIGILL)");
#endif
    sa.sa_flags = 0;
    }

sa.sa_handler = sig_term;
if (sigaction (SIGTERM, &sa, NULL) < 0)
    ap_log_error (APLOG_MARK, APLOG_WARNING, errno, ap_server_conf,
                  "sigaction(SIGTERM)");
#ifdef SIGINT
if (sigaction (SIGINT, &sa, NULL) < 0)
    ap_log_error (APLOG_MARK, APLOG_WARNING, errno, ap_server_conf,
                  "sigaction(SIGINT)");
#endif
#ifdef SIGXCPU
sa.sa_handler = SIG_DFL;
if (sigaction (SIGXCPU, &sa, NULL) < 0)
    ap_log_error (APLOG_MARK, APLOG_WARNING, errno, ap_server_conf,
                  "sigaction(SIGXCPU)");
#endif
#ifdef SIGXFSZ
sa.sa_handler = SIG_DFL;
if (sigaction (SIGXFSZ, &sa, NULL) < 0)
    ap_log_error (APLOG_MARK, APLOG_WARNING, errno, ap_server_conf,
                  "sigaction(SIGXFSZ)");
#endif
#ifdef SIGPIPE
sa.sa_handler = SIG_IGN;
if (sigaction (SIGPIPE, &sa, NULL) < 0)
    ap_log_error (APLOG_MARK, APLOG_WARNING, errno, ap_server_conf,
                  "sigaction(SIGPIPE)");
#endif

/*
** we want to ignore HUPs and AP_SIG_GRACEFUL while we're busy
** processing one
*/
sigaddset (&sa.sa_mask, SIGHUP);
sigaddset (&sa.sa_mask, AP_SIG_GRACEFUL);
sa.sa_handler = restart;
if (sigaction (SIGHUP, &sa, NULL) < 0)
    ap_log_error (APLOG_MARK, APLOG_WARNING, errno, ap_server_conf,
                  "sigaction(SIGHUP)");
if (sigaction (AP_SIG_GRACEFUL, &sa, NULL) < 0)
    ap_log_error (APLOG_MARK, APLOG_WARNING, errno, ap_server_conf,
                  "sigaction(" AP_SIG_GRACEFUL_STRING ")");
#else
if (!one_process)
    {
    apr_signal (SIGSEGV, sig_coredump);
#ifdef SIGBUS
    apr_signal (SIGBUS, sig_coredump);
#endif /* SIGBUS */
#ifdef SIGABORT
    apr_signal (SIGABORT, sig_coredump);
#endif /* SIGABORT */
#ifdef SIGABRT
    apr_signal (SIGABRT, sig_coredump);
#endif /* SIGABRT */
#ifdef SIGILL
    apr_signal (SIGILL, sig_coredump);
#endif /* SIGILL */
#ifdef SIGXCPU
    apr_signal (SIGXCPU, SIG_DFL);
#endif /* SIGXCPU */
#ifdef SIGXFSZ
    apr_signal (SIGXFSZ, SIG_DFL);
#endif /* SIGXFSZ */
	}

apr_signal (SIGTERM, sig_term);
#ifdef SIGHUP
apr_signal (SIGHUP, restart);
#endif /* SIGHUP */
#ifdef AP_SIG_GRACEFUL
apr_signal (AP_SIG_GRACEFUL, restart);
#endif /* AP_SIG_GRACEFUL */
#ifdef SIGPIPE
apr_signal (SIGPIPE, SIG_IGN);
#endif /* SIGPIPE */
#endif

}


/******************************************************************************/
/*** ap_graceful_stop_signalled routine.                                    ***/
/******************************************************************************/
int ap_graceful_stop_signalled (void)
{

/*
** not ever called anymore...
*/
return 0;

}

/******************************************************************************/
/*** perform_idle_server_maintenance routine.                               ***/
/******************************************************************************/
static void perform_idle_server_maintenance (apr_pool_t *p)
{
char ControlMsgBuf[CTRL_MSG_SIZE_MIN];
CNTL_MSG_DATA *ControlMsgPtr;
char MbxName[32+1];
int i;
int to_kill;
int idle_count;
worker_score *ws;
int free_length;
int free_slots[MAX_SPAWN_RATE];
int last_non_dead;
int total_non_dead;

/*
** Verify any graceful child process exits
*/
if (ap_my_generation)
    VerifyGracefulExits ();

/*
** initialize the free_list
*/
free_length = 0;

to_kill = -1;
idle_count = 0;
last_non_dead = -1;
total_non_dead = 0;

for (i = 0; i < ap_daemons_limit; ++i)
    {
    int status;

    if (i >= ap_max_daemons_limit && free_length == idle_spawn_rate)
        break;
    ws = &ap_scoreboard_image->servers[i][0];
    status = ws->status;
    if (status == SERVER_DEAD)
        {
        /*
        ** try to keep children numbers as low as possible
        */
        if (free_length < idle_spawn_rate)
            {
            free_slots[free_length] = i;
            ++free_length;
            }
        }
    else
        {
        /*
        ** We consider a starting server as idle because we started it
        ** at least a cycle ago, and if it still hasn't finished starting
        ** then we're just going to swamp things worse by forking more.
        ** So we hopefully won't need to fork more if we count it.
        ** This depends on the ordering of SERVER_READY and SERVER_STARTING.
        */
        if (status <= SERVER_READY)
            {
            ++ idle_count;
            /*
            ** always kill the highest numbered child if we have to...
            ** no really well thought out reason ... other than observing
            ** the server behaviour under linux where lower numbered children
            ** tend to service more hits (and hence are more likely to have
            ** their data in cpu caches).
            */
            to_kill = i;
            }

        ++total_non_dead;
        last_non_dead = i;
        }
    }

ap_max_daemons_limit = last_non_dead + 1;
if (idle_count > ap_daemons_max_free)
    {
    /*
    ** kill off one child... we use the pod because that'll cause it to
    ** shut down gracefully, in case it happened to pick up a request
    ** while we were counting
    */

    /*
    ** Establish the server control mailbox name
    */
    sprintf (MbxName, "APACHE$%s_CONTROL_MBX_%d", ServerTag, to_kill);

    /*
    ** Establish the control message
    */
    ControlMsgPtr = (CNTL_MSG_DATA *) ControlMsgBuf;
    ControlMsgPtr->type = APACHE$CTRL_RESTART;
    ControlMsgPtr->length = 0;

    /*
    ** Send the control message
    */
    SendControlMbxMsg (ControlMsgPtr, MbxName);

    idle_spawn_rate = 1;
    }
else
    if (idle_count < ap_daemons_min_free)
        {
        /*
        ** terminate the free list
        */
        if (free_length == 0)
            {
            /*
            ** only report this condition once
            */
            static int reported = 0;

            if (! reported)
                {
                ap_log_error (APLOG_MARK, APLOG_ERR, 0, ap_server_conf,
                              "server reached MaxClients setting, consider"
                              " raising the MaxClients setting");
                reported = 1;
                }
            idle_spawn_rate = 1;
            }
        else
            {
            if (idle_spawn_rate >= 8)
                {
                ap_log_error (APLOG_MARK, APLOG_INFO, 0, ap_server_conf,
                              "server seems busy, (you may need "
                              "to increase StartServers, "
                              "or Min/MaxSpareServers), "
                              "spawning %d children, there are %d idle, and "
                              "%d total children", idle_spawn_rate,
                              idle_count, total_non_dead);
                }

            for (i = 0; i < free_length; ++i) {
                CreateChild (ap_server_conf, free_slots[i]);
	    }

            /*
            ** the next time around we want to spawn twice as many if this
            ** wasn't good enough, but not if we've just done a graceful
            */
            if (hold_off_on_exponential_spawning)
                {
                --hold_off_on_exponential_spawning;
                }
            else
                if (idle_spawn_rate < MAX_SPAWN_RATE)
                    {
                    idle_spawn_rate *= 2;
                    }
            }
        }
    else
        {
        idle_spawn_rate = 1;
        }

}

static int prefork_query(int query_code, int *result, apr_status_t *rv)
{

    *rv = APR_SUCCESS;
switch (query_code)
    {
    case AP_MPMQ_MAX_DAEMON_USED:
        *result = ap_daemons_limit;
        break;
    case AP_MPMQ_IS_THREADED:
        *result = AP_MPMQ_NOT_SUPPORTED;
        break;
    case AP_MPMQ_IS_FORKED:
        *result = AP_MPMQ_DYNAMIC;
        break;
    case AP_MPMQ_HARD_LIMIT_DAEMONS:
        *result = server_limit;
        break;
    case AP_MPMQ_HARD_LIMIT_THREADS:
        *result = HARD_THREAD_LIMIT;
        break;
    case AP_MPMQ_MAX_THREADS:
        *result = 0;
        break;
    case AP_MPMQ_MIN_SPARE_DAEMONS:
        *result = ap_daemons_min_free;
        break;
    case AP_MPMQ_MIN_SPARE_THREADS:
        *result = 0;
        break;
    case AP_MPMQ_MAX_SPARE_DAEMONS:
        *result = ap_daemons_max_free;
        break;
    case AP_MPMQ_MAX_SPARE_THREADS:
        *result = 0;
        break;
    case AP_MPMQ_MAX_REQUESTS_DAEMON:
        *result = ap_max_requests_per_child;
        break;
    case AP_MPMQ_MAX_DAEMONS:
        *result = ap_daemons_limit;
        break;
    case AP_MPMQ_IS_CONTROL_PROCESS:
        *result = (ProcessType == AP_MPMQ_IS_CONTROL_PROCESS ? TRUE : FALSE);
        break;
    case AP_MPMQ_IS_PARENT_PROCESS:
        *result = (ProcessType == AP_MPMQ_IS_PARENT_PROCESS ? TRUE : FALSE);
        break;
    case AP_MPMQ_IS_CHILD_PROCESS:
        *result = (ProcessType == AP_MPMQ_IS_CHILD_PROCESS ? TRUE : FALSE);
        break;
    case AP_MPMQ_CONTROL_STATE:
        *result = ControlState;
        break;
    case AP_MPMQ_PARENT_PID:
        *result = ParentPid;
        break;
    case AP_MPMQ_CHILD_SLOT:
        *result = ChildSlot;
        break;
    case AP_MPMQ_SERVER_TAG:
        *result = (int) ServerTag;
        break;
    case AP_MPMQ_MPM_STATE:
        *result = mpm_state;						// >>> BRC 21-May-2019
        break;
    case AP_MPMQ_GENERATION:
        *result = ap_scoreboard_image->global->running_generation;	// >>> BRC 21-May-2019
        break;
    default:
        *rv = APR_ENOTIMPL;
        break;
    }
    return OK;
}

static int prefork_run (apr_pool_t *p, apr_pool_t *plog, server_rec *s)
{
int status = 0;

/*
** Set the appropriate signals
*/
set_signals ();

/*
** Determine the appropriate process type
*/
switch (ProcessType)
    {
    case AP_MPMQ_IS_CONTROL_PROCESS:
        status = ControlMain (p, s);
	break;

    case AP_MPMQ_IS_PARENT_PROCESS:
        status = ParentMain (p, s);
	break;

    case AP_MPMQ_IS_CHILD_PROCESS:
        status = ChildMain (p, s);
	break;

    default:
	break;
    }

/*
** Return status
*/
return (status);

}

/******************************************************************************/
/*** prefork_open_logs routine.                                             ***/
/******************************************************************************/
/*
** This really should be a post_config hook, but the error log is already
** redirected by that point, so we need to do this in the open_logs phase.
*/
static int prefork_open_logs (apr_pool_t *p, apr_pool_t *plog,
			      apr_pool_t *ptemp, server_rec *s)
{
ap_server_conf = s;
apr_status_t rv;
pconf = p;

/*
** If we're in the parent process, then create the socket listeners
*/
if ((num_listensocks = ap_setup_listeners (ap_server_conf)) < 1)
    {
    ap_log_error (APLOG_MARK, APLOG_ALERT|APLOG_STARTUP, 0, NULL,
		  "no listening sockets available, shutting down");
    return DONE;
    }

return OK;

}

/******************************************************************************/
/*** prefork_pre_config routine.                                            ***/
/******************************************************************************/
static int prefork_pre_config (apr_pool_t *p, apr_pool_t *plog,
			       apr_pool_t *ptemp)
{
int no_detach, debug, foreground;
static int restart_num = 0;
apr_status_t rv;

debug = ap_exists_config_define ("DEBUG");
if (debug)
    {
    foreground = one_process = 1;
    no_detach = 0;
    }
else
    {
    no_detach = ap_exists_config_define ("NO_DETACH");
    one_process = ap_exists_config_define ("ONE_PROCESS");
    foreground = ap_exists_config_define ("FOREGROUND");
    }

/*
** sigh, want this only the second time around
*/
if (restart_num++ == 1)
    {
    is_graceful = 0;
    ap_my_pid = getpid ();
    }

// unixd_pre_config (ptemp);  	/* No longer required? */
ap_listen_pre_config ();
ap_daemons_to_start = DEFAULT_START_DAEMON;
ap_daemons_min_free = DEFAULT_MIN_FREE_DAEMON;
ap_daemons_max_free = DEFAULT_MAX_FREE_DAEMON;
ap_daemons_limit = server_limit;
ap_pid_fname = DEFAULT_PIDLOG;
// ap_lock_fname = DEFAULT_LOCKFILE;
ap_max_requests_per_child = DEFAULT_MAX_REQUESTS_PER_CHILD;
ap_extended_status = 0;

apr_cpystrn (ap_coredump_dir, ap_server_root, sizeof (ap_coredump_dir));

return OK;

}


static const char *prefork_get_name(void)
{
    return "prefork";
}


/******************************************************************************/
/*** prefork_hooks routine.                                                 ***/
/******************************************************************************/
static void prefork_hooks (apr_pool_t *p)
{
/*
** The prefork open_logs phase must run before the core's, or stderr
** will be redirected to a file, and the messages won't print to the
** console.
*/
static const char *const aszSucc[] = {"core.c", NULL};

    ap_hook_open_logs(prefork_open_logs, NULL, aszSucc, APR_HOOK_REALLY_FIRST);
    ap_hook_pre_config(prefork_pre_config, NULL, NULL, APR_HOOK_REALLY_FIRST);
//  ap_hook_check_config(prefork_check_config, NULL, NULL, APR_HOOK_MIDDLE);
    ap_hook_mpm(prefork_run, NULL, NULL, APR_HOOK_MIDDLE);
    ap_hook_mpm_query(prefork_query, NULL, NULL, APR_HOOK_MIDDLE);
    ap_hook_mpm_get_name(prefork_get_name, NULL, NULL, APR_HOOK_MIDDLE);
}

/******************************************************************************/
/*** set_daemons_to_start routine.                                          ***/
/******************************************************************************/
static const char *set_daemons_to_start (cmd_parms *cmd, void *dummy,
					 const char *arg)
{
const char *err = ap_check_cmd_context (cmd, GLOBAL_ONLY);

if (err != NULL)
    return err;

ap_daemons_to_start = atoi (arg);

return NULL;

}

/******************************************************************************/
/*** set_min_free_servers routine.                                          ***/
/******************************************************************************/
static const char *set_min_free_servers (cmd_parms *cmd, void *dummy,
					 const char *arg)
{
const char *err = ap_check_cmd_context (cmd, GLOBAL_ONLY);

if (err != NULL)
    return err;

ap_daemons_min_free = atoi (arg);

if (ap_daemons_min_free <= 0)
    {
    ap_log_error (APLOG_MARK, APLOG_STARTUP, 0, NULL,
    		  "WARNING: detected MinSpareServers set to non-positive.");
    ap_log_error (APLOG_MARK, APLOG_STARTUP, 0, NULL,
    		  "Resetting to 1 to avoid almost certain Apache failure.");
    ap_log_error (APLOG_MARK, APLOG_STARTUP, 0, NULL,
    		  "Please read the documentation.");
    ap_daemons_min_free = 1;
    }

return NULL;

}

/******************************************************************************/
/*** set_max_free_servers routine.                                          ***/
/******************************************************************************/
static const char *set_max_free_servers (cmd_parms *cmd, void *dummy,
					 const char *arg)
{
const char *err = ap_check_cmd_context (cmd, GLOBAL_ONLY);

if (err != NULL)
    return err;

ap_daemons_max_free = atoi (arg);

return NULL;

}

/******************************************************************************/
/*** set_max_clients routine.                                               ***/
/******************************************************************************/
static const char *set_max_clients (cmd_parms *cmd, void *dummy,
 				    const char *arg)
{
const char *err = ap_check_cmd_context (cmd, GLOBAL_ONLY);

if (err != NULL)
    return err;

ap_daemons_limit = atoi (arg);

if (ap_daemons_limit > server_limit)
    {
    ap_log_error (APLOG_MARK, APLOG_STARTUP, 0, NULL,
    		  "WARNING: MaxClients of %d exceeds ServerLimit value "
    		  "of %d servers,", ap_daemons_limit, server_limit);
    ap_log_error (APLOG_MARK, APLOG_STARTUP, 0, NULL,
    		  " lowering MaxClients to %d.  To increase, please "
    		  "see the ServerLimit", server_limit);
    ap_log_error (APLOG_MARK, APLOG_STARTUP, 0, NULL,
    		  " directive.");
    ap_daemons_limit = server_limit;
    }
else
    if (ap_daemons_limit < 1)
    	{
    	ap_log_error (APLOG_MARK, APLOG_STARTUP, 0, NULL,
      		      "WARNING: Require MaxClients > 0, setting to 1");
	ap_daemons_limit = 1;
	}

return NULL;

}

/******************************************************************************/
/*** set_server_limit routine.                                              ***/
/******************************************************************************/
static const char *set_server_limit (cmd_parms *cmd, void *dummy,
                                     const char *arg)
{
const char *err = ap_check_cmd_context (cmd, GLOBAL_ONLY);
int tmp_server_limit;

if (err != NULL)
    return err;

tmp_server_limit = atoi (arg);

/*
** you cannot change ServerLimit across a restart; ignore any such attempts
*/
if (first_server_limit && tmp_server_limit != server_limit)
    {
    /*
    ** how do we log a message?  the error log is a bit bucket at this
    ** point; we'll just have to set a flag so that ap_run_mpm()
    ** logs a warning later
    */
    changed_limit_at_restart = 1;
    return NULL;
    }

server_limit = tmp_server_limit;

if (server_limit > MAX_SERVER_LIMIT)
    {
    ap_log_error (APLOG_MARK, APLOG_STARTUP, 0, NULL,
                  "WARNING: ServerLimit of %d exceeds compile time limit "
                  "of %d servers,", server_limit, MAX_SERVER_LIMIT);
    ap_log_error (APLOG_MARK, APLOG_STARTUP, 0, NULL,
                  " lowering ServerLimit to %d.", MAX_SERVER_LIMIT);
    server_limit = MAX_SERVER_LIMIT;
    }
else
    if (server_limit < 1)
	{
	ap_log_error (APLOG_MARK, APLOG_STARTUP, 0, NULL,
                      "WARNING: Require ServerLimit > 0, setting to 1");
	server_limit = 1;
	}

return NULL;

}

/******************************************************************************/
/*** set_server_tag routine.                                                ***/
/******************************************************************************/
static const char *set_server_tag (cmd_parms *cmd, void *dummy,
                                   const char *arg)
{
const char *err = ap_check_cmd_context (cmd, GLOBAL_ONLY);

if (err != NULL)
    return err;

if (strlen (arg) > 4)
    {
    ap_log_error (APLOG_MARK, APLOG_STARTUP, 0, NULL,
                  "VmsServerTag must be 1 - 4 characters");
    exit (1);
    }
else
    ServerTag = apr_pstrdup (cmd->pool, arg);

return NULL;

}

/******************************************************************************/
/*** set_server_startup routine.                                            ***/
/******************************************************************************/
static const char *set_server_startup (cmd_parms *cmd, void *dummy,
                                       const char *arg)
{
const char *err = ap_check_cmd_context (cmd, GLOBAL_ONLY);
char *VmsFname = NULL;
struct stat stat_buf;

if (err != NULL)
    return err;

if (stat (arg, &stat_buf) != 0)
    {
    ap_log_error (APLOG_MARK, APLOG_STARTUP, errno, NULL,
                  "Unable to stat VmsServerStartup file");
    exit (1);
    }

/*
** Convert the startup filename to VMS format if necessary
*/
if (apr$cvt_fnm (CVT_FNM_UNIX_TO_VMS, (char *) arg, (char **) &VmsFname))
    ServerStartup = VmsFname;
else
    ServerStartup = strdup (arg);

return NULL;

}

/******************************************************************************/
/*** set_server_shutdown routine.                                           ***/
/******************************************************************************/
static const char *set_server_shutdown (cmd_parms *cmd, void *dummy,
                                        const char *arg)
{
const char *err = ap_check_cmd_context (cmd, GLOBAL_ONLY);
char *VmsFname = NULL;
struct stat stat_buf;

if (err != NULL)
    return err;

if (stat (arg, &stat_buf) != 0)
    {
    ap_log_error (APLOG_MARK, APLOG_STARTUP, errno, NULL,
                  "Unable to stat VmsServerShutdown file");
    exit (1);
    }

/*
** Convert the shutdown filename to VMS format if necessary
*/
if (apr$cvt_fnm (CVT_FNM_UNIX_TO_VMS, (char *) arg, (char **) &VmsFname))
    ServerShutdown = VmsFname;
else
    ServerShutdown = strdup (arg);

return NULL;

}

/******************************************************************************/
/*** prefork_cmds routine.                                                  ***/
/******************************************************************************/
static const command_rec prefork_cmds[] =
    {
    LISTEN_COMMANDS,
    AP_INIT_TAKE1("StartServers", set_daemons_to_start, NULL, RSRC_CONF,
                  "Number of child processes launched at server startup"),
    AP_INIT_TAKE1("MinSpareServers", set_min_free_servers, NULL, RSRC_CONF,
                  "Minimum number of idle children, to handle request spikes"),
    AP_INIT_TAKE1("MaxSpareServers", set_max_free_servers, NULL, RSRC_CONF,
                  "Maximum number of idle children"),
    AP_INIT_TAKE1("MaxClients", set_max_clients, NULL, RSRC_CONF,
                  "Deprecated name of MaxRequestWorkers"),
    AP_INIT_TAKE1("MaxRequestWorkers", set_max_clients, NULL, RSRC_CONF,
                   "Maximum number of children alive at the same time"),
    AP_INIT_TAKE1("ServerLimit", set_server_limit, NULL, RSRC_CONF,
                  "Maximum value of MaxRequestWorkers for this run of Apache"),
    AP_INIT_TAKE1("VmsServerTag", set_server_tag, NULL, RSRC_CONF,
                  "Unique string identifying an Apache instance (1-4 chars)"),
    AP_INIT_TAKE1("VmsServerStartup", set_server_startup, NULL, RSRC_CONF,
                  "User-defined server startup procedure"),
    AP_INIT_TAKE1("VmsServerShutdown", set_server_shutdown, NULL, RSRC_CONF,
                  "User-defined server shutdown procedure"),
    AP_GRACEFUL_SHUTDOWN_TIMEOUT_COMMAND,
    { NULL }
    };

/******************************************************************************/
/*** mpm_prefork_module routine.                                            ***/
/******************************************************************************/

AP_DECLARE_MODULE(mpm_prefork) = {
    MPM20_MODULE_STUFF,
    NULL, 			/* hook to run before apache parses args */
    NULL,			/* create per-directory config structure */
    NULL,			/* merge per-directory config structures */
    NULL,			/* create per-server config structure */
    NULL,			/* merge per-server config structures */
    prefork_cmds,		/* command apr_table_t */
    prefork_hooks,		/* register hooks */
    };

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static void TerminationMbxAst (MBX_AST_DATA *TerminationMbxData)
{
ACCDEF TerminationMbxBuff;
IOSB iosb;
int status,
    i;

/*
** Initialize the termination mailbox buffer
*/
memset (&TerminationMbxBuff, 0, sizeof (TerminationMbxBuff));

/*
** Read the mailbox
*/
status = SYS$QIOW (EFN$C_ENF,
		   TerminationMbxData->chan,
		   IO$_READVBLK | IO$M_NOW,
		   &iosb,
		   0, 0,
		   &TerminationMbxBuff,
		   sizeof (TerminationMbxBuff),
		   0, 0, 0, 0);
if (status & 1)
    status = iosb.iosb$w_status;
if (! (status & 1))
    fprintf (stderr, "ERROR (0x%08X): Unable to read the termination mailbox\n", status);

/*
** ReEstablish the write attention AST
*/
status = SYS$QIO (EFN$C_ENF,
		  TerminationMbxData->chan,
		  IO$_SETMODE | IO$M_WRTATTN,
		  &TerminationMbxData->iosb,
		  0, 0,
		  (void *) &TerminationMbxAst,
		  (int) TerminationMbxData,
		  0, 0, 0, 0);
if (status & 1)
    status = iosb.iosb$w_status;
if (! (status & 1))
    fprintf (stderr, "ERROR (0x%08X): Unable to issue async write of termination mailbox\n", status);

/*
** Update this server in the active server list as having terminated
*/
status = apr$asl_update (TerminationMbxBuff.acc$l_pid,
			    TerminationMbxBuff.acc$l_finalsts);
if (status != APR_SUCCESS)
    fprintf (stderr, "ERROR: Unable to update active server record for terminating child process 0x%08X\n",
	     TerminationMbxBuff.acc$l_pid);

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static void ParentControlMbxAst (MBX_AST_DATA *ControlMbxData)
{
char ControlMbxBuf[CTRL_MSG_SIZE_MAX];
CNTL_MSG_DATA *ControlMsgPtr;
char MbxName[32+1];
int status,
    i;
IOSB iosb;

if (! ControlMbxData || ControlMbxData->iosb.iosb$w_status == SS$_ABORT)
    return;

/*
** Read the message from the mailbox
*/
ControlMsgPtr = (CNTL_MSG_DATA *) ControlMbxBuf;

status = SYS$QIOW (EFN$C_ENF,
		   ControlMbxData->chan,
		   IO$_READVBLK | IO$M_NOW,
		   &iosb,
		   0, 0,
                   ControlMsgPtr,
		   CTRL_MSG_SIZE_MAX,
                   0, 0, 0, 0);
if (status & 1)
    status = iosb.iosb$w_status;
if (! (status & 1))
    {
    decc$$translate (status);
    ap_log_error (APLOG_MARK, APLOG_ERR, errno, ap_server_conf,
                  "error reading from parent control mailbox");
    }

/*
** Dispatch based on the input message
*/
if (status & 1)
    {
    switch (ControlMsgPtr->type)
	{
        case APACHE$CTRL_GRACEFUL:
        case APACHE$CTRL_RESTART:
        case APACHE$CTRL_STOP:
	    /*
	    ** If there's a current pending request, then abort
	    */
	    if ((restart_pending || shutdown_pending) && ControlMsgPtr->type == APACHE$CTRL_STOP)
	    	{
		ap_log_error (APLOG_MARK, APLOG_ERR, 0, ap_server_conf,
                              "Parent received control message to shutdown while a %s was in progess, Aborting",
			      shutdown_pending ? "shutdown" : is_graceful ? "graceful restart" : "restart");
	        apache$spl_flush ();
	        apache$spl_write_messages ();
		exit (SS$_ABORT);
		}

	    /*
	    ** Write log message and set switches appropriately
	    */
	    if (ControlMsgPtr->type == APACHE$CTRL_GRACEFUL)
		{
		ap_log_error (APLOG_MARK, APLOG_INFO, 0, ap_server_conf,
                              "Parent received control message to restart gracefully");
		restart_pending = 1;
		is_graceful = 1;
		}
	    if (ControlMsgPtr->type == APACHE$CTRL_RESTART)
		{
		ap_log_error (APLOG_MARK, APLOG_INFO, 0, ap_server_conf,
                              "Parent received control message to restart");
		restart_pending = 1;
		}
	    if (ControlMsgPtr->type == APACHE$CTRL_STOP)
		{
		ap_log_error (APLOG_MARK, APLOG_INFO, 0, ap_server_conf,
                              "Parent received control message to shutdown");
		shutdown_pending = 1;
		}

	    /*
	    ** Send the message along to the the children
	    */
	    for (i = 0; i < ap_max_daemons_limit; i++)
		if (ap_scoreboard_image->servers[i][0].status != SERVER_DEAD)
		    {
                    sprintf (MbxName, "APACHE$%s_CONTROL_MBX_%d", ServerTag, i);
                    SendControlMbxMsg (ControlMsgPtr, MbxName);
		    }
            break;

        case APACHE$CTRL_FLUSH:
	    /*
	    ** Log the flush message received
	    */
	    ap_log_error (APLOG_MARK, APLOG_INFO, 0, ap_server_conf,
                          "Parent received control message to flush logs");

	    /*
	    ** Issue a shared process log flush request
	    */
	    apache$spl_flush ();
            break;

        case APACHE$CTRL_NEW:
	    /*
	    ** Log the new message received
	    */
	    ap_log_error (APLOG_MARK, APLOG_INFO, 0, ap_server_conf,
                          "Parent received control message to create new logs");

	    /*
	    ** Issue a shared process log new request
	    */
	    apache$spl_new ();
            break;

        default:
            ap_log_error (APLOG_MARK, APLOG_ERR, 0, ap_server_conf,
			  "unexpected control message type in parent process - %d",
			  ControlMsgPtr->type);
	    printf ("Parent received invalid message type %d ...\n", ControlMsgPtr->type);
            break;
        }
    }

/*
** Re-arm the AST
*/
status = SYS$QIO (EFN$C_ENF,
		  ControlMbxData->chan,
		  IO$_SETMODE | IO$M_WRTATTN,
                  &ControlMbxData->iosb,
		  0, 0,
                  (void *) &ParentControlMbxAst,
		  (int) ControlMbxData,
		  0, 0, 0, 0);
if (! (status & 1))
    {
    decc$$translate (status);
    ap_log_error (APLOG_MARK, APLOG_ERR, errno, ap_server_conf,
    	          "error re-arming write attention AST on parent control mailbox");
    }

return;

}

/******************************************************************************/
/***                                                                        ***/
/******************************************************************************/
static void ChildControlMbxAst (MBX_AST_DATA *ControlMbxData)
{
char ControlMbxBuf[CTRL_MSG_SIZE_MAX];
CNTL_MSG_DATA *ControlMsgPtr;
int status, i;
IOSB iosb;

if (! ControlMbxData || ControlMbxData->iosb.iosb$w_status == SS$_ABORT)
    return;

/*
** Read the message from the mailbox
*/
ControlMsgPtr = (CNTL_MSG_DATA *) ControlMbxBuf;

status = SYS$QIOW (EFN$C_ENF,
		   ControlMbxData->chan,
		   IO$_READVBLK | IO$M_NOW,
		   &iosb,
		   0, 0,
                   ControlMsgPtr,
		   CTRL_MSG_SIZE_MAX,
                   0, 0, 0, 0);
if (status & 1)
    status = iosb.iosb$w_status;
if (! (status & 1))
    {
    decc$$translate (status);
    ap_log_error (APLOG_MARK, APLOG_ERR, errno, ap_server_conf,
                  "error reading from child %d control mailbox", ChildSlot);
    }

/*
** Dispatch based on the input message
*/
if (status & 1)
    {
    switch (ControlMsgPtr->type)
	{
        case APACHE$CTRL_GRACEFUL:
        case APACHE$CTRL_RESTART:
        case APACHE$CTRL_STOP:
	    /*
	    ** If there's a current pending request, then abort
	    */
	    if ((restart_pending || shutdown_pending) && ControlMsgPtr->type == APACHE$CTRL_STOP)
	    	{
		ap_log_error (APLOG_MARK, APLOG_ERR, 0, ap_server_conf,
                              "Child %d received control message to shutdown while a %s was in progess, Aborting",
			      ChildSlot, shutdown_pending ? "shutdown" : is_graceful ? "graceful restart" : "restart");
		clean_child_exit (SS$_ABORT);
		}

	    /*
	    ** Write log message and set switches appropriately
	    */
	    if (ControlMsgPtr->type == APACHE$CTRL_GRACEFUL)
		{
		ap_log_error (APLOG_MARK, APLOG_INFO, 0, ap_server_conf,
                              "Child %d received control message to restart gracefully", ChildSlot);
		restart_pending = 1;
		is_graceful = 1;
		}
	    if (ControlMsgPtr->type == APACHE$CTRL_RESTART)
		{
		ap_log_error (APLOG_MARK, APLOG_INFO, 0, ap_server_conf,
                              "Child %d received control message to restart", ChildSlot);
		restart_pending = 1;
		}
	    if (ControlMsgPtr->type == APACHE$CTRL_STOP)
		{
		ap_log_error (APLOG_MARK, APLOG_INFO, 0, ap_server_conf,
                              "Child %d received control message to shutdown", ChildSlot);
		shutdown_pending = 1;
		}

	    /*
	    ** Cleanup the accept mutex
	    */
	    if (accept_mutex)
		apr_proc_mutex_cleanup (accept_mutex);

 	    /*
	    ** Issue cancel & deassign on accept/select in progress.
	    */
	    for (i = 0; listensocks && i < num_listensocks; i++)
		if (listensocks[i].sd && vaxc$get_sdc (listensocks[i].sd->socketdes))
		    {
		    SYS$CANCEL (vaxc$get_sdc (listensocks[i].sd->socketdes));
		    SYS$DASSGN (vaxc$get_sdc (listensocks[i].sd->socketdes));
		    }
	    break;

        default:
	    ap_log_error (APLOG_MARK, APLOG_ERR, 0, ap_server_conf,
                          "Child %d received unexpected control message type - %d",
			  ChildSlot, ControlMsgPtr->type);
            break;
        }
    }

/*
** Re-arm the AST (unless it's a shutdown request)
*/
status = SYS$QIO (EFN$C_ENF,
		  ControlMbxData->chan,
		  IO$_SETMODE | IO$M_WRTATTN,
                  &ControlMbxData->iosb,
		  0, 0,
                  (void *) &ChildControlMbxAst,
		  (int) ControlMbxData,
		  0, 0, 0, 0);
if (! (status & 1))
    {
    decc$$translate (status);
    ap_log_error (APLOG_MARK, APLOG_ERR, errno, ap_server_conf,
    		  "error re-arming write attention AST on child control mailbox");
    }

}

/******************************************************************************/
/*** CreateParent routine.                                                  ***/
/******************************************************************************/
static int CreateParent (apr_pool_t *p, server_rec *s)
{
char CmdLineExt[MAX_CMDLINE_SIZE+1];
char PrcName[32+1];
pid_t Pid;
int status,
    i;

/*
** Create the active server list
*/
status = apr$asl_create (server_limit);
if (status != APR_SUCCESS)
    fprintf (stderr, "ERROR: Unable to create active server list for %d child processes\n",
	     server_limit);

/*
** Create the process name
*/
sprintf (PrcName, "APACHE$%s", ServerTag);

/*
** Create the appropriate command line extensions
*/
strcpy (CmdLineExt, " -p 0");

/*
** Create this detached process
*/
Pid = CreateDetached (s, PrcName, CmdLineExt, TerminationMbxUnit);
if (Pid == -1)
    return (FAILURE);

/*
** Insert this server into the active server list
*/
status = apr$asl_insert (Pid);
if (status != APR_SUCCESS)
    fprintf (stderr, "ERROR: Unable to insert server %08X into the active server list\n",
	     Pid);

/*
** Return success
*/
return (SUCCESS);

}

/******************************************************************************/
/*** ParentMain () routine.						    ***/
/******************************************************************************/
static int ParentMain (apr_pool_t *p, server_rec *s)
{
int status;

/*
** Initialize the parent processing
*/
status = ParentInit (p, s);
if (status != SUCCESS)
    return (FAILURE);

/*
** Core parent processing
*/
status = ParentCore (p, s);
if (status != SUCCESS)
    return (FAILURE);

/*
** Terminate the parent processing
*/
status = ParentTerm (p, s);
if (status != SUCCESS)
    return (FAILURE);

/*
** Return success
*/
return (SUCCESS);

}

/******************************************************************************/
/*** ParentInit () routine.						    ***/
/******************************************************************************/
static int ParentInit (apr_pool_t *p, server_rec *s)
{
struct dsc$descriptor LckNameDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
static MBX_AST_DATA TerminationMbxData = {0};
static MBX_AST_DATA ControlMbxData = {0};
static unsigned int ParentLckId = 0;
unsigned int DevUnit = 0;
char MbxName[32+1];
char LckName[32+1];
LKSB LckNameLKSB;
ILE3 DviItems[2],
     *Ile3Ptr;
apr_status_t rv;
int status;
IOSB iosb;

/*
** Set the parent pid to ourselves
*/
ParentPid = ap_my_pid;

/*
** Create the parent control mailbox (if necessary)
*/
if (! ControlMbxData.chan)
    {
    /*
    ** Create the control mailbox
    */
    sprintf (MbxName, "APACHE$%s_CONTROL_MBX", ServerTag);
    status = apr$mbx_create (&ControlMbxData.chan,
				CTRL_MSG_SIZE_MAX,
				10 * CTRL_MSG_SIZE_MIN,
				MbxName, "LNM$SYSTEM");
    if (! (status & 1))
	{
	ap_log_error (APLOG_MARK, APLOG_EMERG, 0, s,
         	      "Couldn't create parent control mailbox (0x%08X)",
		      status);
	return (FAILURE);
	}

    /*
    ** Establish the control mailbox AST
    */
    status = SYS$QIO (EFN$C_ENF,
		      ControlMbxData.chan,
		      IO$_SETMODE | IO$M_WRTATTN,
		      &ControlMbxData.iosb,
		      0, 0,
		      (void *) &ParentControlMbxAst,
		      (int) &ControlMbxData,
		      0, 0, 0, 0);
    if (! (status & 1))
	{
	ap_log_error (APLOG_MARK, APLOG_EMERG, 0, s,
         	      "Couldn't establish parent control mailbox AST (0x%08X)",
		      status);
	return (FAILURE);
	}
    }

/*
** Create the parent lock (if necessary)
*/
if (! ParentLckId)
    {
    /*
    ** Create the lock resouce name descriptor
    */
    sprintf (LckName, "APACHE$%08X_PARENT_LCK", ParentPid);
    LckNameDesc.dsc$w_length = strlen (LckName);
    LckNameDesc.dsc$a_pointer = LckName;

    /*
    ** Clear the lock status block
    */
    memset (&LckNameLKSB, 0, sizeof (LckNameLKSB));

    /*
    ** Create an Exclusive parent lock
    */
    status = SYS$ENQW (EFN$C_ENF,		/* No event flag		*/
    		       LCK$K_EXMODE,		/* Exclusive mode		*/
		       &LckNameLKSB,		/* Lock Status Block		*/
		       0,			/* No Flags			*/
		       &LckNameDesc,		/* resource name descriptor	*/
		       0,			/* No parent			*/
		       0, 			/* No AST routine		*/
		       0,			/* No AST parameter		*/
		       0,			/* No blocking AST		*/
		       PSL$C_USER,		/* Access mode = user		*/
		       0,			/* Resource domain ID		*/
		       0,			/* Range			*/
		       0);			/* Highest priority		*/
    if (status & 1)
	status = LckNameLKSB.lksb$w_status;
    if (! (status & 1))
	{
	ap_log_error (APLOG_MARK, APLOG_EMERG, 0, s,
         	      "Couldn't create parent lock (0x%08X)",
		      status);
	return (FAILURE);
	}

    /*
    ** Save the Parent lock ID
    */
    ParentLckId = LckNameLKSB.lksb$l_lkid;
    }

/*
** Create the termination mailbox (if necessary)
*/
if (! TerminationMbxData.chan)
    {
    /*
    ** Create the termination mailbox
    */
    status = apr$mbx_create (&TerminationMbxData.chan,
				sizeof (ACCDEF),
				ap_daemons_limit * sizeof(ACCDEF));
    if (! (status & 1))
	{
	ap_log_error (APLOG_MARK, APLOG_EMERG, 0, s,
         	      "Couldn't create termination mailbox (0x%08X)",
		      status);
	return (FAILURE);
	}

    /*
    ** Establish the termination mailbox AST
    */
    status = SYS$QIO (EFN$C_ENF,
		      TerminationMbxData.chan,
		      IO$_SETMODE | IO$M_WRTATTN,
		      &TerminationMbxData.iosb,
		      0, 0,
		      (void *) &TerminationMbxAst,
		      (int) &TerminationMbxData,
		      0, 0, 0, 0);
    if (! (status & 1))
	{
	ap_log_error (APLOG_MARK, APLOG_EMERG, 0, s,
         	      "Couldn't establish termination mailbox AST (0x%08X)",
		      status);
	return (FAILURE);
	}

    /*
    ** Get the Mailbox Unit Number
    */
    ILE3_INIT (DviItems);
    ILE3_ADD (DVI$_UNIT, sizeof (DevUnit), &DevUnit, 0);
    ILE3_TERM;

    /*
    ** Get the Mailbox Unit Number
    */
    status = SYS$GETDVIW (EFN$C_ENF,
		          TerminationMbxData.chan,
		          0,
		          &DviItems,
		          &iosb,
		          0, 0, 0);
    if (status & 1)
	status = iosb.iosb$w_status;
    if (! (status & 1))
	{
	ap_log_error (APLOG_MARK, APLOG_EMERG, 0, s,
         	      "Couldn't retrieve termination mailbox unit (0x%08X)",
		      status);
	return (FAILURE);
	}

    /*
    ** Save the termination mailbox unit number
    */
    TerminationMbxUnit = (unsigned short) DevUnit;
    }

/*
** Create the pid file
*/
ap_log_pid (p, ap_pid_fname);

/*
** Create the active server list
*/
status = apr$asl_create (server_limit);
if (status != APR_SUCCESS)
    fprintf (stderr, "ERROR: Unable to create active server list for %d child processes\n",
	     server_limit);

/*
** Initialize cross-process accept lock
*/
    lock_fname = apr_psprintf (p, "APACHE$%08X_ACCEPT_LCK", ParentPid);
    rv = apr_proc_mutex_create (&accept_mutex, lock_fname, APR_LOCK_VMSDLM, p);

if (rv != APR_SUCCESS)
    {
    ap_log_error (APLOG_MARK, APLOG_EMERG, rv, s,
                  "Couldn't create cross-process accept lock in parent");
    return (FAILURE);
    }

/*
** If we're not restarting gracefully then do some initialization
*/
if (! is_graceful)
    {
    /*
    ** Initialize the scoreboard in pool
    */
    if (ap_run_pre_mpm (s->process->pool, SB_SHARED) != OK)
	return (FAILURE);

    /*
    ** fix the generation number in the global score; we just got a new,
    ** cleared scoreboard
    */
    ap_scoreboard_image->global->running_generation = ap_my_generation;
    }

/*
** Validate the server limit
*/
first_server_limit = server_limit;
if (changed_limit_at_restart)
    {
    ap_log_error (APLOG_MARK, APLOG_WARNING, 0, s,
		  "WARNING: Attempt to change ServerLimit "
		  "ignored during restart");
    changed_limit_at_restart = 0;
    }

/*
** If necessary, adjust the max free daemons so we don't thrash...
*/
if (ap_daemons_max_free < ap_daemons_min_free + 1)
    ap_daemons_max_free = ap_daemons_min_free + 1;

/*
** If we're doing a graceful_restart then we're going to see a lot
** of children exiting immediately when we get into the main loop
** below (because we just sent them AP_SIG_GRACEFUL).  This happens pretty
** rapidly... and for each one that exits we'll start a new one until
** we reach at least daemons_min_free.  But we may be permitted to
** start more than that, so we'll just keep track of how many we're
** supposed to start up without the 1 second penalty between each fork.
*/
remaining_children_to_start = ap_daemons_to_start;
if (remaining_children_to_start > ap_daemons_limit)
    remaining_children_to_start = ap_daemons_limit;

if (! is_graceful)
    {
    CreateChildren (remaining_children_to_start);
    remaining_children_to_start = 0;
    }
else
    {
    /*
    ** give the system some time to recover before kicking into
    ** exponential mode
    */
    hold_off_on_exponential_spawning = 10;
    }

ap_log_error (APLOG_MARK, APLOG_NOTICE, 0, ap_server_conf,
	      "%s configured -- resuming normal operations",
	      ap_get_server_description ());
ap_log_error (APLOG_MARK, APLOG_INFO, 0, ap_server_conf,
	      "Server built: %s", ap_get_server_built ());
#ifdef AP_MPM_WANT_SET_ACCEPT_LOCK_MECH
ap_log_error (APLOG_MARK, APLOG_DEBUG, 0, ap_server_conf,
	      "AcceptMutex: %s (default: %s)",
	      apr_proc_mutex_name (accept_mutex),
	      apr_proc_mutex_defname ());
#endif

/*
** Initialize the Shutdown & Restart indicators
*/
shutdown_pending = FALSE;
restart_pending = FALSE;

/*
** Write the share process logs
*/
apache$spl_write_messages ();

/*
** Return success
*/
return (SUCCESS);

}

/******************************************************************************/
/*** ParentCore () routine.						    ***/
/******************************************************************************/
static int ParentCore (apr_pool_t *p, server_rec *s)
{
int status, processed_status;
apr_exit_why_e exitwhy;
int child_slot;
apr_proc_t pid;

/*
** Process while not Restarting or Shutting down
*/
while (! restart_pending && ! shutdown_pending)
    {
    /*
    ** If we're running as one process, then process the child in line
    */
    if (one_process)
	ChildMain (p, s);

    /*
    ** Wait for any exiting children or timeout
    */
    ap_wait_or_timeout (&exitwhy, &status, &pid, pconf, ap_server_conf);
    /*
    ** XXX: if it takes longer than 1 second for all our children
    ** to start up and get into IDLE state then we may spawn an
    ** extra child
    */
    if (pid.pid != -1)
	{
        processed_status = ap_process_child_status (&pid, exitwhy, status);
	if (processed_status == APEXIT_CHILDFATAL)
	    {
            return 1;
            }

	/*
	** non-fatal death... note that it's gone in the scoreboard.
	*/
	child_slot = ap_find_child_by_pid (&pid);
	if (child_slot >= 0)
	    {
	    (void) ap_update_child_status_from_indexes (child_slot, 0, SERVER_DEAD,
                                                        (request_rec *) NULL);
	    if (processed_status == APEXIT_CHILDSICK)
		{
                /*
		** child detected a resource shortage (E[NM]FILE, ENOBUFS, etc)
		** cut the fork rate to the minimum
		*/
                idle_spawn_rate = 1;
                }
	    else
		if (remaining_children_to_start && child_slot < ap_daemons_limit)
		    {
		    /*
		    ** we're still doing a 1-for-1 replacement of dead
		    ** children with new children
		    */
		    CreateChild (ap_server_conf, child_slot);
		    --remaining_children_to_start;
		    }
	    }
	else
	    if (is_graceful)
		{
		/*
		** Great, we've probably just lost a slot in the
		** scoreboard.  Somehow we don't know about this
		** child.
		*/
		ap_log_error (APLOG_MARK, APLOG_WARNING, 0, ap_server_conf,
			      "long lost child came home! (pid %ld)", (long)pid.pid);
		}

	/*
	** Don't perform idle maintenance when a child dies,
	** only do it when there's a timeout.  Remember only a
	** finite number of children can die, and it's pretty
	** pathological for a lot to die suddenly.
	*/
	continue;
	}
    else
	if (remaining_children_to_start)
	    {
	    /*
	    ** we hit a 1 second timeout in which none of the previous
	    ** generation of children needed to be reaped... so assume
	    ** they're all done, and pick up the slack if any is left.
	    */
	    CreateChildren (remaining_children_to_start);
	    remaining_children_to_start = 0;
	    /*
	    ** In any event we really shouldn't do the code below because
	    ** few of the servers we just started are in the IDLE state
	    ** yet, so we'd mistakenly create an extra server.
	    */
	    continue;
	    }

    /*
    ** Perform the idle server maintenance
    */
    perform_idle_server_maintenance (pconf);

    /*
    ** Write the share process logs
    */
    apache$spl_write_messages ();
    }

/*
** Return success
*/
return (SUCCESS);

}

/******************************************************************************/
/*** ParentTerm () routine.						    ***/
/******************************************************************************/
static int ParentTerm (apr_pool_t *p, server_rec *s)
{
const char *pidfile = NULL;
int i;

/*
** Was there a shutdown requested ?
*/
if (shutdown_pending)
    {
    ap_log_error (APLOG_MARK, APLOG_NOTICE, 0, ap_server_conf,
		  "caught SIGTERM, shutting down");

    /*
    ** Reclaim any processes created via APR (i.e. spawned children)
    */
    ap_reclaim_child_processes (1, prefork_note_child_killed);

    /*
    ** cleanup pid file on normal shutdown
    */
    pidfile = ap_server_root_relative (pconf, ap_pid_fname);
    if (pidfile != NULL && unlink (pidfile) == 0)
	ap_log_error (APLOG_MARK, APLOG_INFO, 0, ap_server_conf,
		      "removed PID file %s (pid=%ld)", pidfile, (long) getpid ());

    /*
    ** Write the share process logs
    */
    apache$spl_flush ();
    apache$spl_write_messages ();

    /*
    ** Return FAILURE to prevent restart
    */
    return (FAILURE);
    }

/*
** Advance to the next generation and save it in the scoreboard
*/
++ap_my_generation;
ap_scoreboard_image->global->running_generation = ap_my_generation;

/*
** Was there a graceful restart requested ?
*/
if (is_graceful)
    {
    /*
    ** Log the graceful restart message
    */
    ap_log_error (APLOG_MARK, APLOG_NOTICE, 0, ap_server_conf,
		  "Graceful restart requested, doing restart");

    /*
    ** This is mostly for debugging... so that we know what is still
    ** gracefully dealing with existing request.  This will break
    ** in a very nasty way if we ever have the scoreboard totally
    ** file-based (no shared memory)
    */
    for (i = 0; i < ap_daemons_limit; i++)
	{
	if (ap_scoreboard_image->servers[i][0].status != SERVER_DEAD)
	    ap_scoreboard_image->servers[i][0].status = SERVER_GRACEFUL;
	}
    }
else
    {
    /*
    ** Log the restart message
    */
    ap_log_error (APLOG_MARK, APLOG_NOTICE, 0, ap_server_conf,
		  "SIGHUP received.  Attempting to restart");

    /*
    ** Reclaim any processes created via APR (i.e. spawned children)
    */
    ap_reclaim_child_processes (0, prefork_note_child_killed);
    }

/*
** Write the share process logs
*/
apache$spl_flush ();
apache$spl_write_messages ();

/*
** If we're doing a graceful restart, then let's verify the graceful children
** that have exited.
*/
if (is_graceful)
    VerifyGracefulExits ();

/*
** Return success
*/
return (SUCCESS);

}

/******************************************************************************/
/*** CreateChildren routine.                                                ***/
/******************************************************************************/
static int CreateChildren (int ChildrenToStart)
{
int status,
    i;

/*
** Loop through the server list looking for "dead" slots
*/
for (i = 0; i < ap_daemons_limit && ChildrenToStart; ++i)
    {
    /*
    ** Skip any non-dead server slots
    */
    if (ap_scoreboard_image->servers[i][0].status != SERVER_DEAD)
        continue;

    /*
    ** Create a child process in this slot
    */
    status = CreateChild (ap_server_conf, i);
    if (status != SUCCESS)
	return (status);

    /*
    ** Decrement the number of server to start
    */
    --ChildrenToStart;
    }

/*
** Return success
*/
return (SUCCESS);

}

/******************************************************************************/
/*** CreateChild routine.                                                   ***/
/******************************************************************************/
static int CreateChild (server_rec *s, int slot)
{
char CmdLineExt[MAX_CMDLINE_SIZE+1];
char PrcName[32+1];
pid_t Pid;
int status,
    i;


/*
** Make room for this slot number
*/
if (slot + 1 > ap_max_daemons_limit)
    ap_max_daemons_limit = slot + 1;

/*
** Indicate that we're starting this child
*/
ap_update_child_status_from_indexes (slot, 0, SERVER_STARTING,
                                     (request_rec *) NULL);

/*
** Create the process name
*/
sprintf (PrcName, "APACHE$%s%04X", ServerTag, slot);

/*
** Create the appropriate command line extensions
*/
sprintf (CmdLineExt, " -p %d -w %d", ParentPid, slot);

/*
** Create this detached process
*/
Pid = CreateDetached (s, PrcName, CmdLineExt, TerminationMbxUnit);
if (Pid == -1)
    {
    /*
    ** fork didn't succeed. Fix the scoreboard or else
    ** it will say SERVER_STARTING forever and ever
    */
    ap_update_child_status_from_indexes (slot, 0, SERVER_DEAD, (request_rec *) NULL);

    /*
    ** In case system resources are maxxed out, we don't want
    ** Apache running away with the CPU trying to fork over and
    ** over and over again.
    */
    sleep (10);

    return (FAILURE);
    }

/*
** Update the scoreboard
*/
ap_scoreboard_image->parent[slot].pid = Pid;

/*
** Insert this server into the active server list
*/
status = apr$asl_insert (Pid);
if (status != APR_SUCCESS)
    fprintf (stderr, "ERROR: Unable to insert server %08X into the active server list\n",
	     Pid);

/*
** Return success
*/
return (SUCCESS);

}

/******************************************************************************/
/*** ChildMain routine.							    ***/
/******************************************************************************/
static int ChildMain (apr_pool_t *p, server_rec *s)
{
int status;

/*
** Initialize the child processing
*/
status = ChildInit (p, s);
if (status != SUCCESS)
    return (FAILURE);

/*
** Process the core of the child processing
*/
status = ChildCore (p, s);
if (status != SUCCESS)
    return (FAILURE);

/*
** Terminate the child processing
*/
status = ChildTerm (p, s);
if (status != SUCCESS)
    return (FAILURE);

/*
** Return success
*/
return (SUCCESS);

}

/******************************************************************************/
/*** ChildInit routine.							    ***/
/******************************************************************************/
static int ChildInit (apr_pool_t *p, server_rec *s)
{
struct dsc$descriptor LckNameDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
static MBX_AST_DATA ControlMbxData = {0};
apr_allocator_t *allocator;
static LKSB LckNameLKSB;
char MbxName[32+1];
char LckName[32+1];
apr_status_t rv;
int status;

/*
** Set the child pid to ourselves
*/
ChildPid = ap_my_pid;

/*
** Create the control mailbox
*/
sprintf (MbxName, "APACHE$%s_CONTROL_MBX_%d", ServerTag, ChildSlot);
status = apr$mbx_create (&ControlMbxData.chan,
			    CTRL_MSG_SIZE_MAX,
			    10 * CTRL_MSG_SIZE_MAX,
			    MbxName, "LNM$GROUP");
if (! (status & 1))
    {
    ap_log_error (APLOG_MARK, APLOG_EMERG, 0, s,
                  "Couldn't create child control mailbox (0x%08X)",
		  status);
    return (FAILURE);
    }

/*
** Establish the control mailbox AST
*/
status = SYS$QIO (EFN$C_ENF,
		  ControlMbxData.chan,
		  IO$_SETMODE | IO$M_WRTATTN,
		  &ControlMbxData.iosb,
		  0, 0,
		  (void *) &ChildControlMbxAst,
		  (int) &ControlMbxData,
		  0, 0, 0, 0);
if (! (status & 1))
    {
    ap_log_error (APLOG_MARK, APLOG_EMERG, 0, s,
         	  "Couldn't establish child control mailbox AST (0x%08X)",
		  status);
    return (FAILURE);
    }

/*
** Create the lock resouce name descriptor
*/
sprintf (LckName, "APACHE$%08X_PARENT_LCK", ParentPid);
LckNameDesc.dsc$w_length = strlen (LckName);
LckNameDesc.dsc$a_pointer = LckName;

/*
** Clear the lock status block
*/
memset (&LckNameLKSB, 0, sizeof (LckNameLKSB));

/*
** Queue a Concurrent Read to the parent lock
*/
status = SYS$ENQ (EFN$C_ENF,		/* No event flag		*/
    		  LCK$K_CRMODE,		/* Concurrent Read mode		*/
		  &LckNameLKSB,		/* Lock Status Block		*/
		  LCK$M_NODLCKWT,	/* Flags			*/
		  &LckNameDesc,		/* resource name descriptor	*/
		  0,			/* No parent			*/
		  ParentDied, 		/* AST routine			*/
		  (int) &LckNameLKSB,	/* AST parameter		*/
		  0,			/* No blocking AST		*/
		  PSL$C_USER,		/* Access mode = user		*/
		  0,			/* Resource domain ID		*/
		  0,			/* Range			*/
		  0);			/* Highest priority		*/
if (! (status & 1))
    {
    ap_log_error (APLOG_MARK, APLOG_EMERG, 0, s,
       	          "Couldn't queue to the parent lock (0x%08X)", status);
    return (FAILURE);
    }

/*
** Get a sub context for global allocations in this child, so that
** we can have cleanups occur when the child exits.
*/
apr_allocator_create (&allocator);
apr_allocator_max_free_set (allocator, ap_max_mem_free);
apr_pool_create_ex (&pchild, pconf, NULL, allocator);
apr_allocator_owner_set (allocator, pchild);

apr_pool_create (&ptrans, p);
apr_pool_tag (ptrans, "transaction");

/*
** Reopen the scoreboard in pool
*/
ap_reopen_scoreboard (pchild, NULL, 1);

/*
** Initialize cross-process accept lock
*/
#if 0
if (! one_process)
    {
    if (ap_accept_lock_mech == APR_LOCK_VMSDLM)
	ap_lock_fname = apr_psprintf (pchild, "APACHE$%08X_ACCEPT_LCK", ParentPid);
    else
	ap_lock_fname = apr_psprintf (pchild, "%s_%" APR_PID_T_FMT,
                                      ap_server_root_relative (pchild, ap_lock_fname),
				      ParentPid);
    }
rv = apr_proc_mutex_create (&accept_mutex, ap_lock_fname,
                            ap_accept_lock_mech, pchild);
#else
if (! one_process)
    {
	lock_fname = apr_psprintf (pchild, "APACHE$%08X_ACCEPT_LCK", ParentPid);
    }
rv = apr_proc_mutex_create (&accept_mutex, lock_fname, APR_LOCK_VMSDLM, pchild);
#endif
if (rv != APR_SUCCESS)
    {
    ap_log_error (APLOG_MARK, APLOG_EMERG, rv, ap_server_conf,
                  "Couldn't initialize cross-process accept lock in child");
    return (FAILURE);
    }

/*
** Initialize this child's generation to the current running generation
*/
ap_my_generation = ap_scoreboard_image->global->running_generation;

/*
** Run the child initialization
*/
ap_run_child_init (pchild, ap_server_conf);

/*
** Return success
*/
return (SUCCESS);

}

/******************************************************************************/
/*** ChildCore routine.							    ***/
/******************************************************************************/
static int ChildCore (apr_pool_t *p, server_rec *s)
{
ap_sb_handle_t *ScoreboardHandle = NULL;
apr_bucket_alloc_t *bucket_alloc;
conn_rec *current_conn;
apr_pollfd_t *pollset;
ap_listen_rec *lr;
void *csd = NULL;
apr_status_t rv;
int last_pollfd = 0,
    curr_pollfd,
    status,
    offset,
    i;

/*
** Create the child scoreboard handle
*/
ap_create_sb_handle (&ScoreboardHandle, pchild, ChildSlot, 0);

/*
** Set up the pollfd array
*/
listensocks = apr_pcalloc (pchild, sizeof (*listensocks) * (num_listensocks));
for (lr = ap_listeners, i = 0; i < num_listensocks; lr = lr->next, i++)
    {
    listensocks[i].accept_func = ap_unixd_accept;
    listensocks[i].sd = lr->sd;
    }

pollset = apr_palloc (pchild, sizeof(*pollset) * num_listensocks);
pollset[0].p = pchild;
for (i = 0; i < num_listensocks; i++)
    {
    pollset[i].desc.s = listensocks[i].sd;
    pollset[i].desc_type = APR_POLL_SOCKET;
    pollset[i].reqevents = APR_POLLIN;
    }

bucket_alloc = apr_bucket_alloc_create (pchild);

while (! restart_pending && ! shutdown_pending)
    {
    /*
    ** (Re)initialize this child to a pre-connection state.
    */
    current_conn = NULL;

    /*
    ** Clear the transaction pool
    */
    apr_pool_clear (ptrans);

    /*
    ** If there was a maximum request number indicated, the determine whether
    ** this child has exceeded that value.  If so, then exit now.
    */
    if (ap_max_requests_per_child > 0 &&
	requests_this_child++ >= ap_max_requests_per_child)
        clean_child_exit (0);

    /*
    ** Update the current server status to READY
    */
    ap_update_child_status (ScoreboardHandle, SERVER_READY, (request_rec *) NULL);

    /*
    ** Lock around "accept", if necessary
    */
    SAFE_ACCEPT (accept_mutex_on ());
    if (restart_pending || shutdown_pending)
	return (FAILURE);

    if (num_listensocks == 1)
        {
        offset = 0;
        }
    else
        {
        /*
        ** multiple listening sockets - need to poll
        */
        for (;;)
            {
            apr_status_t ret;
            apr_int32_t n;

            ret = apr_poll (pollset, num_listensocks, &n, -1);
            if (ret != APR_SUCCESS)
                {
		if (restart_pending || shutdown_pending)
		    return (FAILURE);
                if (APR_STATUS_IS_EINTR (ret))
                    continue;

                /*
                ** Single Unix documents select as returning errnos
                ** EBADF, EINTR, and EINVAL... and in none of those
                ** cases does it make sense to continue.  In fact
                ** on Linux 2.0.x we seem to end up with EFAULT
                ** occasionally, and we'd loop forever due to it.
                */
                ap_log_error (APLOG_MARK, APLOG_ERR, ret, ap_server_conf,
                              "apr_poll: (listen)");
                clean_child_exit (1);
                }

            /*
            ** find a listener
            */
            curr_pollfd = last_pollfd;
            do  {
                curr_pollfd++;
                if (curr_pollfd >= num_listensocks)
                    {
                    curr_pollfd = 0;
                    }

                /*
                ** XXX: Should we check for POLLERR?
                */
                if (pollset[curr_pollfd].rtnevents & APR_POLLIN)
                    {
                    last_pollfd = curr_pollfd;
                    offset = curr_pollfd;
                    goto got_fd;
                    }
                } while (curr_pollfd != last_pollfd);

            continue;
            }
        }

got_fd:
    /*
    ** if we accept() something we don't want to die, so we have to
    ** defer the exit
    */
    status = listensocks[offset].accept_func (&csd, &listensocks[offset],
                                              ptrans);
    SAFE_ACCEPT (accept_mutex_off ());	/* unlock after "accept" */

    if (status == APR_EGENERAL)
	{
	/*
	** resource shortage or should-not-occur occured
	*/
	clean_child_exit (1);
	}
    else
	if (status != APR_SUCCESS)
	    {
	    continue;
	    }

    /*
    ** We now have a connection, so set it up with the appropriate
    ** socket options, file descriptors, and read/write buffers.
    */
    current_conn = ap_run_create_connection (ptrans, ap_server_conf, csd,
                                             ChildSlot, ScoreboardHandle, bucket_alloc);
    if (current_conn)
        {
        ap_process_connection (current_conn, csd);
        ap_lingering_close (current_conn);
        }

    /*
    ** Write the share process logs if we're in one process
    */
    if (one_process)
	apache$spl_write_messages ();
    }

/*
** Return success
*/
return (SUCCESS);

}

/******************************************************************************/
/*** ChildTerm routine.							    ***/
/******************************************************************************/
static int ChildTerm (apr_pool_t *p, server_rec *s)
{

/*
** Destroy created pools
*/
if (pchild)
    apr_pool_destroy (pchild);
if (ptrans)
    apr_pool_destroy (ptrans);

/*
**
*/
chdir_for_gprof();

/*
** Return success
*/
return (FAILURE);

}

/******************************************************************************/
/*** SendControlMbxMsg routine.						    ***/
/******************************************************************************/
static int SendControlMbxMsg (CNTL_MSG_DATA *ControlMsgPtr, char *MbxName)
{
struct dsc$descriptor MbxNameDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
unsigned short MbxChan;
int ControlMsgLen;
int status;
IOSB iosb;

/*
** Establish the control mailbox descriptor
*/
MbxNameDesc.dsc$a_pointer = MbxName;
MbxNameDesc.dsc$w_length = strlen (MbxName);

/*
** Assign a channel to the control mailbox
*/
status = SYS$ASSIGN (&MbxNameDesc,
		     &MbxChan,
		     0, 0, 0, 0);
if (! (status & 1))
    {
    decc$$translate (status);
    ap_log_error (APLOG_MARK, APLOG_ERR, errno, ap_server_conf,
                  "error assigning channel to control mailbox %s\n",
                  MbxName);
    return (status);
    }

/*
** Establish the control message size
*/
ControlMsgLen = ControlMsgPtr->length + CTRL_MSG_SIZE_MIN;

/*
** Write the control message to the mailbox
*/
status = SYS$QIOW (0,
		   MbxChan,
		   IO$_WRITEVBLK | IO$M_NOW,
		   &iosb,
		   0, 0,
		   ControlMsgPtr,
		   ControlMsgLen,
                   0, 0, 0, 0);
if (status & 1)
    status = iosb.iosb$w_status;
if (! (status & 1))
    {
    decc$$translate (status);
    ap_log_error (APLOG_MARK, APLOG_ERR, errno, ap_server_conf,
		  "error writing to control mailbox %s\n",
		  MbxName);
    return (status);
    }

/*
** Deassign the channel to the control mailbox
*/
status = SYS$DASSGN (MbxChan);
if (! (status & 1))
    {
    decc$$translate (status);
    ap_log_error (APLOG_MARK, APLOG_ERR, errno, ap_server_conf,
                  "error deassigning channel to control mailbox %s\n",
		  MbxName);
    return (status);
    }

/*
** Return status
*/
return (status);

}

/******************************************************************************/
/*** TranslateServerSpecific routine.					    ***/
/******************************************************************************/
static void TranslateServerSpecific ()
{
char VmsServerSpec[NAML$C_MAXRSS + 1];
char *VmsFname = NULL;
char *VmsServerRoot;
struct NAML naml;
struct FAB fab;
int status;

/*
** Convert the server root to VMS format
*/
if (apr$cvt_fnm (CVT_FNM_UNIX_TO_VMS, (char *) ap_server_specific, (char **) &VmsFname))
    VmsServerRoot = VmsFname;
else
    VmsServerRoot = (char *) ap_server_specific;

/*
** Setup the FAB structure
*/
fab = cc$rms_fab;
fab.fab$l_fna = (char *) -1;
fab.fab$l_naml = &naml;

/*
** Setup the NAML structure
*/
naml = cc$rms_naml;
naml.naml$b_nop = NAML$M_NOCONCEAL;
naml.naml$l_long_filename = VmsServerRoot;
naml.naml$l_long_filename_size = strlen (VmsServerRoot);
naml.naml$l_long_expand = VmsServerSpec;
naml.naml$l_long_expand_alloc = sizeof (VmsServerSpec) - 1;

/*
** Parse the file specification
*/
status = SYS$PARSE (&fab);
if (! (status & 1))
    {
    decc$$translate (status);
    ap_log_error (APLOG_MARK, APLOG_ERR, errno, ap_server_conf,
                  "Unable to parse the server root directory");
    if (VmsServerRoot != ap_server_specific)
	free (VmsServerRoot);
    exit (1);
    }

/*
** Free the server root if it was allocated
*/
if (VmsServerRoot != ap_server_specific)
    free (VmsServerRoot);

/*
** Null terminate the login directory
*/
VmsServerSpec[naml.naml$l_long_dev_size + naml.naml$l_long_dir_size] = '\0';

/*
** RMS allocates internal data structures, including a channel.
** To release these resources, do another SYS$PARSE call.
*/
naml.naml$b_nop |= NAM$M_SYNCHK;
naml.naml$l_long_result = NULL;
naml.naml$l_long_result_alloc = 0;
naml.naml$l_long_expand = NULL;
naml.naml$l_long_expand_alloc = 0;
naml.naml$l_long_filename = NULL;
naml.naml$l_long_filename_size = 0;
naml.naml$l_long_defname = NULL;
naml.naml$l_long_defname_size = 0;

/*
** Parse the file specification
*/
SYS$PARSE (&fab);

/*
** Parse the server root to be used in the child command procedure definition
** for the APACHE$SPECIFIC logical.
*/
if (! strcmp (&VmsServerSpec[strlen (VmsServerSpec) - 10], ".][000000]"))
    VmsServerSpec[strlen (VmsServerSpec) - 8] = '\0';
if (strcmp (&VmsServerSpec[strlen (VmsServerSpec) - 2], ".]"))
    strcpy (&VmsServerSpec[strlen (VmsServerSpec) - 1], ".]");

/*
** Allocate the server root directory specification
*/
if (ServerSpecificDirectory)
    free (ServerSpecificDirectory);
ServerSpecificDirectory = strdup (VmsServerSpec);

}

/******************************************************************************/
/*** CreateServerSpecificLnm routine.					    ***/
/******************************************************************************/
static void CreateServerSpecificLnm ()
{
struct dsc$descriptor_s log_nam_desc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
struct dsc$descriptor_s log_val_desc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
int log_nam_attr = LNM$M_CONCEALED | LNM$M_TERMINAL;
int status;

/*
** Setup the logical name descriptor.
*/
log_nam_desc.dsc$a_pointer = "APACHE$SPECIFIC";
log_nam_desc.dsc$w_length = strlen (log_nam_desc.dsc$a_pointer);

/*
** Setup the logical value descriptor.
*/
log_val_desc.dsc$a_pointer = ServerSpecificDirectory;
log_val_desc.dsc$w_length = strlen (log_val_desc.dsc$a_pointer);

/*
** Create the logical name in the process table
*/
status = lib$set_logical (&log_nam_desc, &log_val_desc, NULL, &log_nam_attr, NULL);
if (! (status & 1))
    {
    decc$$translate (status);
    ap_log_error (APLOG_MARK, APLOG_ERR, errno, ap_server_conf,
                  "Unable to define the server specific root directory");
    exit (1);
    }

}

/******************************************************************************/
/*** DeleteServerSpecificLnm routine.					    ***/
/******************************************************************************/
static void DeleteServerSpecificLnm ()
{
struct dsc$descriptor_s log_nam_desc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
int status;

/*
** Setup the logical name descriptor.
*/
log_nam_desc.dsc$a_pointer = "APACHE$SPECIFIC";
log_nam_desc.dsc$w_length = strlen (log_nam_desc.dsc$a_pointer);

/*
** Delete the logical name from the process table
*/
status = lib$delete_logical (&log_nam_desc, NULL);
if (! (status & 1))
    {
    decc$$translate (status);
    ap_log_error (APLOG_MARK, APLOG_ERR, errno, ap_server_conf,
                  "Unable to deassign the server specific root directory");
    exit (1);
    }

}

/******************************************************************************/
/*** CreateDetached routine.						    ***/
/******************************************************************************/
static pid_t CreateDetached (server_rec *s,
			     char *PrcName,
			     char *CmdLineExt,
			     int TerminationMbxUnit)
{
struct dsc$descriptor UsrNameDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
struct dsc$descriptor LgiFileDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
struct dsc$descriptor PgmFileDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
struct dsc$descriptor InpFileDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
struct dsc$descriptor OutFileDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
struct dsc$descriptor ErrFileDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
struct dsc$descriptor PrcNameDesc = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
FSCNDEF fscn_vlist[5] = {0, FSCN$_DEVICE, 0,
			 0, FSCN$_DIRECTORY, 0,
			 0, FSCN$_NAME, 0,
			 0, FSCN$_TYPE, 0,
			 0, 0, 0};
char CmdLine[MAX_CMDLINE_SIZE+1];
static int HaveUserData = FALSE;
static char *LgiFilePtr = NULL;
char LgiFile[NAML$C_MAXRSS+1];
char InpFile[NAML$C_MAXRSS+1];
char OutFile[NAML$C_MAXRSS+1];
char ErrFile[NAML$C_MAXRSS+1];
static int BasePriority = 0;
ap_listen_rec *ListenRecPtr;
FLDFLAGS fscn_flags;
char DefDevBuf[31],
     DefDirBuf[63],
     LgiCmdBuf[64],
     UsrName[12+1];
unsigned int Pid;
ILE3 UaiItems[4],
     JpiItems[3],
     *Ile3Ptr;
char *ArgPtr;
int QuoteArg,
    ArgLen,
    status,
    i;
FILE *fp;

/*
** Get the current server root directory (if necessary)
*/
if (! ServerSpecificDirectory)
    TranslateServerSpecific ();

/*
** Build the child command line
*/
strcpy (CmdLine, "$ httpd");
for (i = 1; i < s->process->argc; i++)
    {
    /*
    ** Reference the argument and check if there are any uppercase characters
    ** that will require quotes.
    */
    ArgPtr = (char *) s->process->argv[i];
    if (strpbrk (ArgPtr, UCASE_CHARS))
	QuoteArg = TRUE;
    else
	QuoteArg = FALSE;

    /*
    ** Calculate the length of the argument and check to see that is will fit
    ** on the current command line.
    */
    ArgLen = strlen (s->process->argv[i]) + (QuoteArg ? 2 : 0);
    if (strlen (CmdLine) + ArgLen + 1 > MAX_CMDLINE_SIZE)
	{
        ap_log_error (APLOG_MARK, APLOG_ERR, 0, ap_server_conf,
                      "command line exceeds %d bytes", MAX_CMDLINE_SIZE);
	return (-1);
	}

    /*
    ** Add a separating space and a '-' if this is an option argument.
    */
    strcat (CmdLine, " ");
    if (*ArgPtr == '-')
	{
	strcat (CmdLine, "-");
	ArgPtr++;
	}

    /*
    ** Add the argument with quotes if required
    */
    if (QuoteArg)
	strcat (CmdLine, "\"");
    strcat (CmdLine, ArgPtr);
    if (QuoteArg)
	strcat (CmdLine, "\"");
    }

/*
** Get the users data for the process creation
*/
if (! HaveUserData)
    {
    /*
    ** Create the JPI item list
    */
    ILE3_INIT (JpiItems);
    ILE3_ADD (JPI$_PRIB, sizeof (BasePriority), &BasePriority, 0);
    ILE3_ADD (JPI$_USERNAME, sizeof (UsrName) - 1, UsrName, 0);
    ILE3_TERM;

    /*
    ** Get the JPI items
    */
    status = SYS$GETJPIW (EFN$C_ENF,
                       	  0, 0,
                       	  &JpiItems,
                       	  0, 0, 0);
    if (! (status & 1))
	{
	decc$$translate (status);
	ap_log_error (APLOG_MARK, APLOG_WARNING, errno, ap_server_conf,
	              "Unable to get process base priority");
	}

    /*
    ** trim the username
    */
    for (i = 0; i < sizeof (UsrName) - 1 && UsrName[i] != ' '; i++);
    UsrName[i] = '\0';

    /*
    ** Setup the username descriptor
    */
    UsrNameDesc.dsc$a_pointer = UsrName;
    UsrNameDesc.dsc$w_length = strlen (UsrNameDesc.dsc$a_pointer);

    /*
    ** Create the UAI item list
    */
    ILE3_INIT (UaiItems);
    ILE3_ADD (UAI$_DEFDEV, sizeof (DefDevBuf), DefDevBuf, 0);
    ILE3_ADD (UAI$_DEFDIR, sizeof (DefDirBuf), DefDirBuf, 0);
    ILE3_ADD (UAI$_LGICMD, sizeof (LgiCmdBuf), LgiCmdBuf, 0);
    ILE3_TERM;

    /*
    ** Get the UAI items
    */
    status = SYS$GETUAI (0, 0,
                       	 &UsrNameDesc,
                       	 &UaiItems,
                       	 0, 0, 0);
    if (! (status & 1))
	{
	decc$$translate (status);
	ap_log_error (APLOG_MARK, APLOG_ERR, errno, ap_server_conf,
	              "Unable to get user login command procedure");
	return (-1);
	}

    /*
    ** Allocate memory for the users login file spec
    */
    LgiFilePtr = malloc ((int) DefDevBuf[0] + (int) DefDirBuf[0] + (int) LgiCmdBuf[0] + 1);
    if (! LgiFilePtr)
	{
	ap_log_error (APLOG_MARK, APLOG_ERR, errno, ap_server_conf,
	              "Unable to allocate memory for the user login command procedure");
	return (-1);
	}

    /*
    ** Construct the users login file spec
    */
    sprintf (LgiFilePtr, "%.*s%.*s%.*s",
	     (int) DefDevBuf[0], &DefDevBuf[1],
	     (int) DefDirBuf[0], &DefDirBuf[1],
	     (int) LgiCmdBuf[0], &LgiCmdBuf[1]);

    /*
    ** Setup the login file descriptor
    */
    LgiFileDesc.dsc$a_pointer = LgiFilePtr;
    LgiFileDesc.dsc$w_length = strlen (LgiFilePtr);

    /*
    ** Clear the file scan flags
    */
    memset ((void *) &fscn_flags, 0, sizeof (fscn_flags));

    /*
    ** Scan the login filename
    */
    status = SYS$FILESCAN (&LgiFileDesc, &fscn_vlist, (unsigned int *) &fscn_flags, 0, 0);
    if (! (status & 1))
	{
	decc$$translate (status);
	ap_log_error (APLOG_MARK, APLOG_ERR, errno, ap_server_conf,
	              "Unable to scan the user login command procedure");
	return (-1);
	}

    /*
    ** Fill in the login command file device & directory
    */
    if (fscn_flags.fscn$v_device && fscn_flags.fscn$v_directory)
	sprintf (LgiFile, "%.*s%.*s",
     	     	 fscn_vlist[0].fscn$w_length, (char *) fscn_vlist[0].fscn$l_addr,
	     	 fscn_vlist[1].fscn$w_length, (char *) fscn_vlist[1].fscn$l_addr);
    else
	strcpy (LgiFile, "SYS$LOGIN:");

    /*
    ** Fill in the login command file name
    */
    if (fscn_flags.fscn$v_name)
	sprintf (LgiFile + strlen (LgiFile), "%.*s",
	         fscn_vlist[2].fscn$w_length, (char *) fscn_vlist[2].fscn$l_addr);
    else
	strcat (LgiFile, "LOGIN");

    /*
    ** Fill in the login command file type
    */
    if (fscn_flags.fscn$v_type)
	sprintf (LgiFile + strlen (LgiFile), "%.*s",
	         fscn_vlist[3].fscn$w_length, (char *) fscn_vlist[3].fscn$l_addr);
    else
	strcat (LgiFile, ".COM");

    /*
    ** Free the Lgi pointer and dup the newly constructed buffer
    */
    free (LgiFilePtr);
    LgiFilePtr = strdup (LgiFile);

    /*
    ** Indicate that we Have the users data
    */
    HaveUserData = TRUE;
    }

/*
** Determine whether the VMS specific command line extensions will fit on the
** current command line.
*/
if (strlen (CmdLine) + strlen (CmdLineExt) + 1 > MAX_CMDLINE_SIZE)
    {
    ap_log_error (APLOG_MARK, APLOG_ERR, 0, ap_server_conf,
                  "command line exceeds %d bytes", MAX_CMDLINE_SIZE);
    return (-1);
    }
strcat (CmdLine, CmdLineExt);

/*
** Establish the program file name & descriptor
*/
PgmFileDesc.dsc$a_pointer = "SYS$SYSTEM:LOGINOUT.EXE";
PgmFileDesc.dsc$w_length = strlen (PgmFileDesc.dsc$a_pointer);

/*
** Establish the input file name & descriptor
*/
sprintf (InpFile, "%s[000000]%s.COM", ServerSpecificDirectory, PrcName);
InpFileDesc.dsc$a_pointer = InpFile;
InpFileDesc.dsc$w_length = strlen (InpFile);

/*
** Establish the output file name & descriptor
*/
sprintf (OutFile, "%s[000000]%s.LOG", ServerSpecificDirectory, PrcName);
OutFileDesc.dsc$a_pointer = OutFile;
OutFileDesc.dsc$w_length = strlen (OutFile);

/*
** Establish the error file name & descriptor
*/
sprintf (ErrFile, "%s[000000]%s.ERR", ServerSpecificDirectory, PrcName);
ErrFileDesc.dsc$a_pointer = ErrFile;
ErrFileDesc.dsc$w_length = strlen (ErrFile);

/*
** Establish the process name & descriptor
*/
PrcNameDesc.dsc$a_pointer = PrcName;
PrcNameDesc.dsc$w_length = strlen (PrcName);

/*
** Create the Child Run procedure
*/
fp = fopen (InpFile, "w");
if (! fp)
    {
    ap_log_error (APLOG_MARK, APLOG_ERR, errno, ap_server_conf,
                  "Unable to create input file %s", InpFile);
    return (-1);
    }

/*
** Fill the Child Run procedure
*/
fprintf (fp, "$ Set NoOn\n");
fprintf (fp, "$ Verify = F$VERIFY (0)\n");
fprintf (fp, "$ Define/NoLog/Process APACHE$SPECIFIC %s/Translation=(Concealed,Terminal)\n", ServerSpecificDirectory);
fprintf (fp, "$ Define/NoLog/Process APACHE$ROOT APACHE$SPECIFIC,APACHE$COMMON\n");
fprintf (fp, "$ Define/NoLog/Process APACHE$SERVER_TAG \"%s\"\n", ServerTag);
if (ProcessType != AP_MPMQ_IS_CONTROL_PROCESS)
    {
    fprintf (fp, "$ Define/NoLog/Process APACHE$PARENT_PID %d\n", ParentPid);
    apache$spl_write_resources (fp);
    apache$sps_write_resources (fp);
    }
fprintf (fp, "$ If F$SEARCH (\"APACHE$COMMON:[000000]APACHE$SETUP.COM\") .NES. \"\" -\n  Then @APACHE$COMMON:[000000]APACHE$SETUP.COM\n");
fprintf (fp, "$ If F$SEARCH (\"%s\") .NES. \"\" -\n  Then @%s\n", LgiFilePtr, LgiFilePtr);
if (ServerStartup) {
    fprintf (fp, "$ If F$SEARCH (\"%s\") .NES. \"\" Then @%s\n", ServerStartup, ServerStartup);
}
fprintf (fp, "%s\n", CmdLine);
fprintf (fp, "$ httpd_status = $STATUS\n");
if (ServerShutdown)
    fprintf (fp, "$ If F$SEARCH (\"%s\") .NES. \"\" Then @%s\n", ServerShutdown, ServerShutdown);
fprintf (fp, "$ If F$SEARCH (\"%s\") .NES. \"\" -\n  Then Delete /NoLog /NoConfirm %s;*\n", InpFile, InpFile);
fprintf (fp, "$ Verify = F$VERIFY (Verify)\n");
fprintf (fp, "$ Exit 'httpd_status'\n");
fclose (fp);

/*
** Create the server specific logical name if we're the control process
*/
if (ProcessType == AP_MPMQ_IS_CONTROL_PROCESS)
    CreateServerSpecificLnm ();

/*
** Create the server process
*/
status = SYS$CREPRC (&Pid,
		     &PgmFileDesc,
		     &InpFileDesc,
                     &OutFileDesc,
		     &ErrFileDesc,
		     NULL, NULL,
		     &PrcNameDesc,
		     BasePriority,
		     0,
		     TerminationMbxUnit,
		     PRC$M_DETACH,
		     NULL, NULL);

/*
** Delete the server specific logical name if we're the control process
*/
if (ProcessType == AP_MPMQ_IS_CONTROL_PROCESS)
    DeleteServerSpecificLnm ();

/*
** Determine whether the detach worked
*/
if (! (status & 1))
    {
    decc$$translate (status);
    ap_log_error (APLOG_MARK, APLOG_ERR, errno, ap_server_conf,
	         "Unable to create detached process %s", PrcName);
    return (-1);
    }

/*
** Return success
*/
return Pid;

}

/******************************************************************************/
/*** Apache kill routine.						    ***/
/******************************************************************************/
int apache$kill (int pid, int sig)
{
char ControlMsgBuf[CTRL_MSG_SIZE_MAX];
CNTL_MSG_DATA *ControlMsgPtr;
apr_proc_t ChildProc;
char MbxName[32+1];
int ChildSlot,
    status;

/*
** Setup the child process structure
*/
memset (&ChildProc, 0, sizeof (ChildProc));
ChildProc.pid = pid;

/*
** Locate the child slot
*/
ChildSlot = ap_find_child_by_pid (&ChildProc);
if (ChildSlot < 0 || sig == 0)
    return (kill (pid, sig));

/*
** Establish the server control mailbox name
*/
sprintf (MbxName, "APACHE$%s_CONTROL_MBX_%d", ServerTag, ChildSlot);

/*
** Establish the control message
*/
ControlMsgPtr = (CNTL_MSG_DATA *) ControlMsgBuf;
switch (sig)
    {
    case AP_SIG_GRACEFUL:
	ControlMsgPtr->type = APACHE$CTRL_GRACEFUL;
	break;

    case SIGHUP:
	ControlMsgPtr->type = APACHE$CTRL_RESTART;
	break;

    case SIGTERM:
    case SIGKILL:
	ControlMsgPtr->type = APACHE$CTRL_STOP;
	break;

    default:
	ControlMsgPtr->type = 0;
	break;
    }
ControlMsgPtr->length = 0;

/*
** Send the control message
*/
status = SendControlMbxMsg (ControlMsgPtr, MbxName);
if (! (status & 1))
    return (-1);
else
    return (0);

}

/******************************************************************************/
/*** Verify graceful child process exits				    ***/
/******************************************************************************/
static void VerifyGracefulExits ()
{
apr_proc_t ChildProc;
int status;
int i;

/*
** Loop through all the server slots
*/
for (i = 0; i < ap_max_daemons_limit; i++)
    {
    /*
    ** Only process the servers with a graceful restart status
    */
    if (ap_scoreboard_image->servers[i][0].status == SERVER_GRACEFUL)
	{
	/*
	** If the child pid is valid, then see if it's exited, otherwise
	** simply make the server as dead.
	*/
	if (ap_scoreboard_image->parent[i].pid != 0)
	    {
	    /*
	    ** Setup the child process structure
	    */
	    memset (&ChildProc, 0, sizeof (ChildProc));
            ChildProc.pid = ap_scoreboard_image->parent[i].pid;

	    /*
	    ** See if this server has exited
	    */
            status = apr_proc_wait (&ChildProc, NULL, NULL, APR_NOWAIT);
            if (status != APR_CHILD_NOTDONE)
		ap_update_child_status_from_indexes (i, 0, SERVER_DEAD, NULL);
            }
	else
	    ap_update_child_status_from_indexes (i, 0, SERVER_DEAD, NULL);
	}
    }

}
#else 		/* __VMS */

/* Limit on the total --- clients will be locked out if more servers than
 * this are needed.  It is intended solely to keep the server from crashing
 * when things get out of hand.
 *
 * We keep a hard maximum number of servers, for two reasons --- first off,
 * in case something goes seriously wrong, we want to stop the fork bomb
 * short of actually crashing the machine we're running on by filling some
 * kernel table.  Secondly, it keeps the size of the scoreboard file small
 * enough that we can read the whole thing without worrying too much about
 * the overhead.
 */
#ifndef DEFAULT_SERVER_LIMIT
#define DEFAULT_SERVER_LIMIT 256
#endif

/* Admin can't tune ServerLimit beyond MAX_SERVER_LIMIT.  We want
 * some sort of compile-time limit to help catch typos.
 */
#ifndef MAX_SERVER_LIMIT
#define MAX_SERVER_LIMIT 200000
#endif

#ifndef HARD_THREAD_LIMIT
#define HARD_THREAD_LIMIT 1
#endif

/* config globals */

static int ap_daemons_to_start=0;
static int ap_daemons_min_free=0;
static int ap_daemons_max_free=0;
static int ap_daemons_limit=0;      /* MaxRequestWorkers */
static int server_limit = 0;

/* data retained by prefork across load/unload of the module
 * allocated on first call to pre-config hook; located on
 * subsequent calls to pre-config hook
 */
typedef struct prefork_retained_data {
    ap_unixd_mpm_retained_data *mpm;

    int first_server_limit;
    int maxclients_reported;
    /*
     * The max child slot ever assigned, preserved across restarts.  Necessary
     * to deal with MaxRequestWorkers changes across AP_SIG_GRACEFUL restarts.  We
     * use this value to optimize routines that have to scan the entire scoreboard.
     */
    int max_daemons_limit;
    /*
     * idle_spawn_rate is the number of children that will be spawned on the
     * next maintenance cycle if there aren't enough idle servers.  It is
     * doubled up to MAX_SPAWN_RATE, and reset only when a cycle goes by
     * without the need to spawn.
     */
    int idle_spawn_rate;
#ifndef MAX_SPAWN_RATE
#define MAX_SPAWN_RATE  (32)
#endif
    int hold_off_on_exponential_spawning;
} prefork_retained_data;
static prefork_retained_data *retained;

typedef struct prefork_child_bucket {
    ap_pod_t *pod;
    ap_listen_rec *listeners;
    apr_proc_mutex_t *mutex;
} prefork_child_bucket;
static prefork_child_bucket *all_buckets, /* All listeners buckets */
                            *my_bucket;   /* Current child bucket */

#define MPM_CHILD_PID(i) (ap_scoreboard_image->parent[i].pid)

/* one_process --- debugging mode variable; can be set from the command line
 * with the -X flag.  If set, this gets you the child_main loop running
 * in the process which originally started up (no detach, no make_child),
 * which is a pretty nice debugging environment.  (You'll get a SIGHUP
 * early in standalone_main; just continue through.  This is the server
 * trying to kill off any child processes which it might have lying
 * around --- Apache doesn't keep track of their pids, it just sends
 * SIGHUP to the process group, ignoring it in the root process.
 * Continue through and you'll be fine.).
 */

static int one_process = 0;

static apr_pool_t *pconf;               /* Pool for config stuff */
static apr_pool_t *pchild;              /* Pool for httpd child stuff */

static pid_t ap_my_pid; /* it seems silly to call getpid all the time */
static pid_t parent_pid;
static int my_child_num;

#ifdef GPROF
/*
 * change directory for gprof to plop the gmon.out file
 * configure in httpd.conf:
 * GprofDir $RuntimeDir/   -> $ServerRoot/$RuntimeDir/gmon.out
 * GprofDir $RuntimeDir/%  -> $ServerRoot/$RuntimeDir/gprof.$pid/gmon.out
 */
static void chdir_for_gprof(void)
{
    core_server_config *sconf =
        ap_get_core_module_config(ap_server_conf->module_config);
    char *dir = sconf->gprof_dir;
    const char *use_dir;

    if(dir) {
        apr_status_t res;
        char *buf = NULL ;
        int len = strlen(sconf->gprof_dir) - 1;
        if(*(dir + len) == '%') {
            dir[len] = '\0';
            buf = ap_append_pid(pconf, dir, "gprof.");
        }
        use_dir = ap_server_root_relative(pconf, buf ? buf : dir);
        res = apr_dir_make(use_dir,
                           APR_UREAD | APR_UWRITE | APR_UEXECUTE |
                           APR_GREAD | APR_GEXECUTE |
                           APR_WREAD | APR_WEXECUTE, pconf);
        if(res != APR_SUCCESS && !APR_STATUS_IS_EEXIST(res)) {
            ap_log_error(APLOG_MARK, APLOG_ERR, res, ap_server_conf, APLOGNO(00142)
                         "gprof: error creating directory %s", dir);
        }
    }
    else {
        use_dir = ap_runtime_dir_relative(pconf, "");
    }

    chdir(use_dir);
}
#else
#define chdir_for_gprof()
#endif

static void prefork_note_child_killed(int childnum, pid_t pid,
                                      ap_generation_t gen)
{
    AP_DEBUG_ASSERT(childnum != -1); /* no scoreboard squatting with this MPM */
    ap_run_child_status(ap_server_conf,
                        ap_scoreboard_image->parent[childnum].pid,
                        ap_scoreboard_image->parent[childnum].generation,
                        childnum, MPM_CHILD_EXITED);
    ap_scoreboard_image->parent[childnum].pid = 0;
}

static void prefork_note_child_started(int slot, pid_t pid)
{
    ap_generation_t gen = retained->mpm->my_generation;
    ap_scoreboard_image->parent[slot].pid = pid;
    ap_scoreboard_image->parent[slot].generation = gen;
    ap_run_child_status(ap_server_conf, pid, gen, slot, MPM_CHILD_STARTED);
}

/* a clean exit from a child with proper cleanup */
static void clean_child_exit(int code) __attribute__ ((noreturn));
static void clean_child_exit(int code)
{
    retained->mpm->mpm_state = AP_MPMQ_STOPPING;

    apr_signal(SIGHUP, SIG_IGN);
    apr_signal(SIGTERM, SIG_IGN);

    if (pchild) {
        apr_pool_destroy(pchild);
    }

    if (one_process) {
        prefork_note_child_killed(/* slot */ 0, 0, 0);
    }

    ap_mpm_pod_close(my_bucket->pod);
    chdir_for_gprof();
    exit(code);
}

static apr_status_t accept_mutex_on(void)
{
    apr_status_t rv = apr_proc_mutex_lock(my_bucket->mutex);
    if (rv != APR_SUCCESS) {
        const char *msg = "couldn't grab the accept mutex";

        if (retained->mpm->my_generation !=
            ap_scoreboard_image->global->running_generation) {
            ap_log_error(APLOG_MARK, APLOG_DEBUG, rv, ap_server_conf, APLOGNO(00143) "%s", msg);
            clean_child_exit(0);
        }
        else {
            ap_log_error(APLOG_MARK, APLOG_EMERG, rv, ap_server_conf, APLOGNO(00144) "%s", msg);
            exit(APEXIT_CHILDFATAL);
        }
    }
    return APR_SUCCESS;
}

static apr_status_t accept_mutex_off(void)
{
    apr_status_t rv = apr_proc_mutex_unlock(my_bucket->mutex);
    if (rv != APR_SUCCESS) {
        const char *msg = "couldn't release the accept mutex";

        if (retained->mpm->my_generation !=
            ap_scoreboard_image->global->running_generation) {
            ap_log_error(APLOG_MARK, APLOG_DEBUG, rv, ap_server_conf, APLOGNO(00145) "%s", msg);
            /* don't exit here... we have a connection to
             * process, after which point we'll see that the
             * generation changed and we'll exit cleanly
             */
        }
        else {
            ap_log_error(APLOG_MARK, APLOG_EMERG, rv, ap_server_conf, APLOGNO(00146) "%s", msg);
            exit(APEXIT_CHILDFATAL);
        }
    }
    return APR_SUCCESS;
}

/* On some architectures it's safe to do unserialized accept()s in the single
 * Listen case.  But it's never safe to do it in the case where there's
 * multiple Listen statements.  Define SINGLE_LISTEN_UNSERIALIZED_ACCEPT
 * when it's safe in the single Listen case.
 */
#ifdef SINGLE_LISTEN_UNSERIALIZED_ACCEPT
#define SAFE_ACCEPT(stmt) (ap_listeners->next ? (stmt) : APR_SUCCESS)
#else
#define SAFE_ACCEPT(stmt) (stmt)
#endif

static int prefork_query(int query_code, int *result, apr_status_t *rv)
{
    *rv = APR_SUCCESS;
    switch(query_code){
    case AP_MPMQ_MAX_DAEMON_USED:
        *result = ap_daemons_limit;
        break;
    case AP_MPMQ_IS_THREADED:
        *result = AP_MPMQ_NOT_SUPPORTED;
        break;
    case AP_MPMQ_IS_FORKED:
        *result = AP_MPMQ_DYNAMIC;
        break;
    case AP_MPMQ_HARD_LIMIT_DAEMONS:
        *result = server_limit;
        break;
    case AP_MPMQ_HARD_LIMIT_THREADS:
        *result = HARD_THREAD_LIMIT;
        break;
    case AP_MPMQ_MAX_THREADS:
        *result = 1;
        break;
    case AP_MPMQ_MIN_SPARE_DAEMONS:
        *result = ap_daemons_min_free;
        break;
    case AP_MPMQ_MIN_SPARE_THREADS:
        *result = 0;
        break;
    case AP_MPMQ_MAX_SPARE_DAEMONS:
        *result = ap_daemons_max_free;
        break;
    case AP_MPMQ_MAX_SPARE_THREADS:
        *result = 0;
        break;
    case AP_MPMQ_MAX_REQUESTS_DAEMON:
        *result = ap_max_requests_per_child;
        break;
    case AP_MPMQ_MAX_DAEMONS:
        *result = ap_daemons_limit;
        break;
    case AP_MPMQ_MPM_STATE:
        *result = retained->mpm->mpm_state;
        break;
    case AP_MPMQ_GENERATION:
        *result = retained->mpm->my_generation;
        break;
    default:
        *rv = APR_ENOTIMPL;
        break;
    }
    return OK;
}

static const char *prefork_get_name(void)
{
    return "prefork";
}

/*****************************************************************
 * Connection structures and accounting...
 */

static void just_die(int sig)
{
    clean_child_exit(0);
}

/* volatile because it's updated from a signal handler */
static int volatile die_now = 0;

static void stop_listening(int sig)
{
    retained->mpm->mpm_state = AP_MPMQ_STOPPING;
    ap_close_listeners_ex(my_bucket->listeners);

    /* For a graceful stop, we want the child to exit when done */
    die_now = 1;
}

/*****************************************************************
 * Child process main loop.
 * The following vars are static to avoid getting clobbered by longjmp();
 * they are really private to child_main.
 */

static int requests_this_child;
static int num_listensocks = 0;

static void child_main(int child_num_arg, int child_bucket)
{
#if APR_HAS_THREADS
    apr_thread_t *thd = NULL;
    apr_os_thread_t osthd;
#endif
    apr_pool_t *ptrans;
    apr_allocator_t *allocator;
    apr_status_t status;
    int i;
    ap_listen_rec *lr;
    apr_pollset_t *pollset;
    ap_sb_handle_t *sbh;
    apr_bucket_alloc_t *bucket_alloc;
    int last_poll_idx = 0;
    const char *lockfile;

    /* for benefit of any hooks that run as this child initializes */
    retained->mpm->mpm_state = AP_MPMQ_STARTING;

    my_child_num = child_num_arg;
    ap_my_pid = getpid();
    requests_this_child = 0;

    ap_fatal_signal_child_setup(ap_server_conf);

    /* Get a sub context for global allocations in this child, so that
     * we can have cleanups occur when the child exits.
     */
    apr_allocator_create(&allocator);
    apr_allocator_max_free_set(allocator, ap_max_mem_free);
    apr_pool_create_ex(&pchild, pconf, NULL, allocator);
    apr_allocator_owner_set(allocator, pchild);
    apr_pool_tag(pchild, "pchild");

#if APR_HAS_THREADS
    osthd = apr_os_thread_current();
    apr_os_thread_put(&thd, &osthd, pchild);
#endif

    apr_pool_create(&ptrans, pchild);
    apr_pool_tag(ptrans, "transaction");

    /* close unused listeners and pods */
    for (i = 0; i < retained->mpm->num_buckets; i++) {
        if (i != child_bucket) {
            ap_close_listeners_ex(all_buckets[i].listeners);
            ap_mpm_pod_close(all_buckets[i].pod);
        }
    }

    /* needs to be done before we switch UIDs so we have permissions */
    ap_reopen_scoreboard(pchild, NULL, 0);
    status = SAFE_ACCEPT(apr_proc_mutex_child_init(&my_bucket->mutex,
                                    apr_proc_mutex_lockfile(my_bucket->mutex),
                                    pchild));
    if (status != APR_SUCCESS) {
        lockfile = apr_proc_mutex_lockfile(my_bucket->mutex);
        ap_log_error(APLOG_MARK, APLOG_EMERG, status, ap_server_conf, APLOGNO(00155)
                     "Couldn't initialize cross-process lock in child "
                     "(%s) (%s)",
                     lockfile ? lockfile : "none",
                     apr_proc_mutex_name(my_bucket->mutex));
        clean_child_exit(APEXIT_CHILDFATAL);
    }

    if (ap_run_drop_privileges(pchild, ap_server_conf)) {
        clean_child_exit(APEXIT_CHILDFATAL);
    }

    ap_run_child_init(pchild, ap_server_conf);

    ap_create_sb_handle(&sbh, pchild, my_child_num, 0);

    (void) ap_update_child_status(sbh, SERVER_READY, (request_rec *) NULL);

    /* Set up the pollfd array */
    status = apr_pollset_create(&pollset, num_listensocks, pchild,
                                APR_POLLSET_NOCOPY);
    if (status != APR_SUCCESS) {
        ap_log_error(APLOG_MARK, APLOG_EMERG, status, ap_server_conf, APLOGNO(00156)
                     "Couldn't create pollset in child; check system or user limits");
        clean_child_exit(APEXIT_CHILDSICK); /* assume temporary resource issue */
    }

    for (lr = my_bucket->listeners, i = num_listensocks; i--; lr = lr->next) {
        apr_pollfd_t *pfd = apr_pcalloc(pchild, sizeof *pfd);

        pfd->desc_type = APR_POLL_SOCKET;
        pfd->desc.s = lr->sd;
        pfd->reqevents = APR_POLLIN;
        pfd->client_data = lr;

        status = apr_pollset_add(pollset, pfd);
        if (status != APR_SUCCESS) {
            /* If the child processed a SIGWINCH before setting up the
             * pollset, this error path is expected and harmless,
             * since the listener fd was already closed; so don't
             * pollute the logs in that case. */
            if (!die_now) {
                ap_log_error(APLOG_MARK, APLOG_EMERG, status, ap_server_conf, APLOGNO(00157)
                             "Couldn't add listener to pollset; check system or user limits");
                clean_child_exit(APEXIT_CHILDSICK);
            }
            clean_child_exit(0);
        }

        lr->accept_func = ap_unixd_accept;
    }

    retained->mpm->mpm_state = AP_MPMQ_RUNNING;

    bucket_alloc = apr_bucket_alloc_create(pchild);

    /* die_now is set when AP_SIG_GRACEFUL is received in the child;
     * {shutdown,restart}_pending are set when a signal is received while
     * running in single process mode.
     */
    while (!die_now
           && !retained->mpm->shutdown_pending
           && !retained->mpm->restart_pending) {
        conn_rec *current_conn;
        void *csd;

        /*
         * (Re)initialize this child to a pre-connection state.
         */

        apr_pool_clear(ptrans);

        if ((ap_max_requests_per_child > 0
             && requests_this_child++ >= ap_max_requests_per_child)) {
            clean_child_exit(0);
        }

        (void) ap_update_child_status(sbh, SERVER_READY, (request_rec *) NULL);

        /*
         * Wait for an acceptable connection to arrive.
         */

        /* Lock around "accept", if necessary */
        SAFE_ACCEPT(accept_mutex_on());

        if (num_listensocks == 1) {
            /* There is only one listener record, so refer to that one. */
            lr = my_bucket->listeners;
        }
        else {
            /* multiple listening sockets - need to poll */
            for (;;) {
                apr_int32_t numdesc;
                const apr_pollfd_t *pdesc;

                /* check for termination first so we don't sleep for a while in
                 * poll if already signalled
                 */
                if (die_now         /* in graceful stop/restart */
                        || retained->mpm->shutdown_pending
                        || retained->mpm->restart_pending) {
                    SAFE_ACCEPT(accept_mutex_off());
                    clean_child_exit(0);
                }

                /* timeout == 10 seconds to avoid a hang at graceful restart/stop
                 * caused by the closing of sockets by the signal handler
                 */
                status = apr_pollset_poll(pollset, apr_time_from_sec(10),
                                          &numdesc, &pdesc);
                if (status != APR_SUCCESS) {
                    if (APR_STATUS_IS_TIMEUP(status) ||
                        APR_STATUS_IS_EINTR(status)) {
                        continue;
                    }
                    /* Single Unix documents select as returning errnos
                     * EBADF, EINTR, and EINVAL... and in none of those
                     * cases does it make sense to continue.  In fact
                     * on Linux 2.0.x we seem to end up with EFAULT
                     * occasionally, and we'd loop forever due to it.
                     */
                    ap_log_error(APLOG_MARK, APLOG_ERR, status,
                                 ap_server_conf, APLOGNO(00158) "apr_pollset_poll: (listen)");
                    SAFE_ACCEPT(accept_mutex_off());
                    clean_child_exit(APEXIT_CHILDSICK);
                }

                /* We can always use pdesc[0], but sockets at position N
                 * could end up completely starved of attention in a very
                 * busy server. Therefore, we round-robin across the
                 * returned set of descriptors. While it is possible that
                 * the returned set of descriptors might flip around and
                 * continue to starve some sockets, we happen to know the
                 * internal pollset implementation retains ordering
                 * stability of the sockets. Thus, the round-robin should
                 * ensure that a socket will eventually be serviced.
                 */
                if (last_poll_idx >= numdesc)
                    last_poll_idx = 0;

                /* Grab a listener record from the client_data of the poll
                 * descriptor, and advance our saved index to round-robin
                 * the next fetch.
                 *
                 * ### hmm... this descriptor might have POLLERR rather
                 * ### than POLLIN
                 */
                lr = pdesc[last_poll_idx++].client_data;
                goto got_fd;
            }
        }
    got_fd:
        /* if we accept() something we don't want to die, so we have to
         * defer the exit
         */
        status = lr->accept_func(&csd, lr, ptrans);

        SAFE_ACCEPT(accept_mutex_off());      /* unlock after "accept" */

        if (status == APR_EGENERAL) {
            /* resource shortage or should-not-occur occurred */
            clean_child_exit(APEXIT_CHILDSICK);
        }
        else if (status != APR_SUCCESS) {
            continue;
        }

        /*
         * We now have a connection, so set it up with the appropriate
         * socket options, file descriptors, and read/write buffers.
         */

        current_conn = ap_run_create_connection(ptrans, ap_server_conf, csd, my_child_num, sbh, bucket_alloc);
        if (current_conn) {
#if APR_HAS_THREADS
            current_conn->current_thread = thd;
#endif
            ap_process_connection(current_conn, csd);
            ap_lingering_close(current_conn);
        }

        /* Check the pod and the generation number after processing a
         * connection so that we'll go away if a graceful restart occurred
         * while we were processing the connection or we are the lucky
         * idle server process that gets to die.
         */
        if (ap_mpm_pod_check(my_bucket->pod) == APR_SUCCESS) { /* selected as idle? */
            die_now = 1;
        }
        else if (retained->mpm->my_generation !=
                 ap_scoreboard_image->global->running_generation) { /* restart? */
            /* yeah, this could be non-graceful restart, in which case the
             * parent will kill us soon enough, but why bother checking?
             */
            die_now = 1;
        }
    }
    apr_pool_clear(ptrans); /* kludge to avoid crash in APR reslist cleanup code */
    clean_child_exit(0);
}


static int make_child(server_rec *s, int slot)
{
    int bucket = slot % retained->mpm->num_buckets;
    int pid;

    if (slot + 1 > retained->max_daemons_limit) {
        retained->max_daemons_limit = slot + 1;
    }

    if (one_process) {
        my_bucket = &all_buckets[0];

        prefork_note_child_started(slot, getpid());
        child_main(slot, 0);
        /* NOTREACHED */
        ap_assert(0);
        return -1;
    }

    (void) ap_update_child_status_from_indexes(slot, 0, SERVER_STARTING,
                                               (request_rec *) NULL);

#ifdef _OSD_POSIX
    /* BS2000 requires a "special" version of fork() before a setuid() call */
    if ((pid = os_fork(ap_unixd_config.user_name)) == -1) {
#else
    if ((pid = fork()) == -1) {
#endif
        ap_log_error(APLOG_MARK, APLOG_ERR, errno, s, APLOGNO(00159) "fork: Unable to fork new process");

        /* fork didn't succeed. Fix the scoreboard or else
         * it will say SERVER_STARTING forever and ever
         */
        (void) ap_update_child_status_from_indexes(slot, 0, SERVER_DEAD,
                                                   (request_rec *) NULL);

        /* In case system resources are maxxed out, we don't want
         * Apache running away with the CPU trying to fork over and
         * over and over again.
         */
        sleep(10);

        return -1;
    }

    if (!pid) {
        my_bucket = &all_buckets[bucket];

#ifdef HAVE_BINDPROCESSOR
        /* by default AIX binds to a single processor
         * this bit unbinds children which will then bind to another cpu
         */
        int status = bindprocessor(BINDPROCESS, (int)getpid(),
                                   PROCESSOR_CLASS_ANY);
        if (status != OK) {
            ap_log_error(APLOG_MARK, APLOG_DEBUG, errno,
                         ap_server_conf, APLOGNO(00160) "processor unbind failed");
        }
#endif
        RAISE_SIGSTOP(MAKE_CHILD);
        AP_MONCONTROL(1);
        /* Disable the parent's signal handlers and set up proper handling in
         * the child.
         */
        apr_signal(SIGHUP, just_die);
        apr_signal(SIGTERM, just_die);
        /* Ignore SIGINT in child. This fixes race-conditions in signals
         * handling when httpd is running on foreground and user hits ctrl+c.
         * In this case, SIGINT is sent to all children followed by SIGTERM
         * from the main process, which interrupts the SIGINT handler and
         * leads to inconsistency.
         */
        apr_signal(SIGINT, SIG_IGN);
        /* The child process just closes listeners on AP_SIG_GRACEFUL.
         * The pod is used for signalling the graceful restart.
         */
        apr_signal(AP_SIG_GRACEFUL, stop_listening);
        child_main(slot, bucket);
    }

    prefork_note_child_started(slot, pid);

    return 0;
}


/* start up a bunch of children */
static void startup_children(int number_to_start)
{
    int i;

    for (i = 0; number_to_start && i < ap_daemons_limit; ++i) {
        if (ap_scoreboard_image->servers[i][0].status != SERVER_DEAD) {
            continue;
        }
        if (make_child(ap_server_conf, i) < 0) {
            break;
        }
        --number_to_start;
    }
}

static void perform_idle_server_maintenance(apr_pool_t *p)
{
    int i;
    int idle_count;
    worker_score *ws;
    int free_length;
    int free_slots[MAX_SPAWN_RATE];
    int last_non_dead;
    int total_non_dead;

    /* initialize the free_list */
    free_length = 0;

    idle_count = 0;
    last_non_dead = -1;
    total_non_dead = 0;

    for (i = 0; i < ap_daemons_limit; ++i) {
        int status;

        if (i >= retained->max_daemons_limit && free_length == retained->idle_spawn_rate)
            break;
        ws = &ap_scoreboard_image->servers[i][0];
        status = ws->status;
        if (status == SERVER_DEAD) {
            /* try to keep children numbers as low as possible */
            if (free_length < retained->idle_spawn_rate) {
                free_slots[free_length] = i;
                ++free_length;
            }
        }
        else {
            /* We consider a starting server as idle because we started it
             * at least a cycle ago, and if it still hasn't finished starting
             * then we're just going to swamp things worse by forking more.
             * So we hopefully won't need to fork more if we count it.
             * This depends on the ordering of SERVER_READY and SERVER_STARTING.
             */
            if (status <= SERVER_READY) {
                ++ idle_count;
            }

            ++total_non_dead;
            last_non_dead = i;
        }
    }
    retained->max_daemons_limit = last_non_dead + 1;
    if (idle_count > ap_daemons_max_free) {
        static int bucket_kill_child_record = -1;
        /* kill off one child... we use the pod because that'll cause it to
         * shut down gracefully, in case it happened to pick up a request
         * while we were counting
         */
        bucket_kill_child_record = (bucket_kill_child_record + 1) % retained->mpm->num_buckets;
        ap_mpm_pod_signal(all_buckets[bucket_kill_child_record].pod);
        retained->idle_spawn_rate = 1;
    }
    else if (idle_count < ap_daemons_min_free) {
        /* terminate the free list */
        if (free_length == 0) {
            /* only report this condition once */
            if (!retained->maxclients_reported) {
                ap_log_error(APLOG_MARK, APLOG_ERR, 0, ap_server_conf, APLOGNO(00161)
                            "server reached MaxRequestWorkers setting, consider"
                            " raising the MaxRequestWorkers setting");
                retained->maxclients_reported = 1;
            }
            retained->idle_spawn_rate = 1;
        }
        else {
            if (retained->idle_spawn_rate >= 8) {
                ap_log_error(APLOG_MARK, APLOG_INFO, 0, ap_server_conf, APLOGNO(00162)
                    "server seems busy, (you may need "
                    "to increase StartServers, or Min/MaxSpareServers), "
                    "spawning %d children, there are %d idle, and "
                    "%d total children", retained->idle_spawn_rate,
                    idle_count, total_non_dead);
            }
            for (i = 0; i < free_length; ++i) {
                make_child(ap_server_conf, free_slots[i]);
            }
            /* the next time around we want to spawn twice as many if this
             * wasn't good enough, but not if we've just done a graceful
             */
            if (retained->hold_off_on_exponential_spawning) {
                --retained->hold_off_on_exponential_spawning;
            }
            else if (retained->idle_spawn_rate < MAX_SPAWN_RATE) {
                retained->idle_spawn_rate *= 2;
            }
        }
    }
    else {
        retained->idle_spawn_rate = 1;
    }
}

/*****************************************************************
 * Executive routines.
 */

static int prefork_run(apr_pool_t *_pconf, apr_pool_t *plog, server_rec *s)
{
    int index;
    int remaining_children_to_start;
    int i;

    ap_log_pid(pconf, ap_pid_fname);

    if (!retained->mpm->was_graceful) {
        if (ap_run_pre_mpm(s->process->pool, SB_SHARED) != OK) {
            retained->mpm->mpm_state = AP_MPMQ_STOPPING;
            return !OK;
        }
        /* fix the generation number in the global score; we just got a new,
         * cleared scoreboard
         */
        ap_scoreboard_image->global->running_generation = retained->mpm->my_generation;
    }

    ap_unixd_mpm_set_signals(pconf, one_process);

    if (one_process) {
        AP_MONCONTROL(1);
        make_child(ap_server_conf, 0);
        /* NOTREACHED */
        ap_assert(0);
        return !OK;
    }

    /* Don't thrash since num_buckets depends on the
     * system and the number of online CPU cores...
     */
    if (ap_daemons_limit < retained->mpm->num_buckets)
        ap_daemons_limit = retained->mpm->num_buckets;
    if (ap_daemons_to_start < retained->mpm->num_buckets)
        ap_daemons_to_start = retained->mpm->num_buckets;
    if (ap_daemons_min_free < retained->mpm->num_buckets)
        ap_daemons_min_free = retained->mpm->num_buckets;
    if (ap_daemons_max_free < ap_daemons_min_free + retained->mpm->num_buckets)
        ap_daemons_max_free = ap_daemons_min_free + retained->mpm->num_buckets;

    /* If we're doing a graceful_restart then we're going to see a lot
     * of children exiting immediately when we get into the main loop
     * below (because we just sent them AP_SIG_GRACEFUL).  This happens pretty
     * rapidly... and for each one that exits we'll start a new one until
     * we reach at least daemons_min_free.  But we may be permitted to
     * start more than that, so we'll just keep track of how many we're
     * supposed to start up without the 1 second penalty between each fork.
     */
    remaining_children_to_start = ap_daemons_to_start;
    if (remaining_children_to_start > ap_daemons_limit) {
        remaining_children_to_start = ap_daemons_limit;
    }
    if (!retained->mpm->was_graceful) {
        startup_children(remaining_children_to_start);
        remaining_children_to_start = 0;
    }
    else {
        /* give the system some time to recover before kicking into
         * exponential mode
         */
        retained->hold_off_on_exponential_spawning = 10;
    }

    ap_log_error(APLOG_MARK, APLOG_NOTICE, 0, ap_server_conf, APLOGNO(00163)
                "%s configured -- resuming normal operations",
                ap_get_server_description());
    ap_log_error(APLOG_MARK, APLOG_INFO, 0, ap_server_conf, APLOGNO(00164)
                "Server built: %s", ap_get_server_built());
    ap_log_command_line(plog, s);
    ap_log_mpm_common(s);
    ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, ap_server_conf, APLOGNO(00165)
                "Accept mutex: %s (default: %s)",
                (all_buckets[0].mutex)
                    ? apr_proc_mutex_name(all_buckets[0].mutex)
                    : "none",
                apr_proc_mutex_defname());

    retained->mpm->mpm_state = AP_MPMQ_RUNNING;

    while (!retained->mpm->restart_pending && !retained->mpm->shutdown_pending) {
        int child_slot;
        apr_exit_why_e exitwhy;
        int status, processed_status;
        /* this is a memory leak, but I'll fix it later. */
        apr_proc_t pid;

        ap_wait_or_timeout(&exitwhy, &status, &pid, pconf, ap_server_conf);

        /* XXX: if it takes longer than 1 second for all our children
         * to start up and get into IDLE state then we may spawn an
         * extra child
         */
        if (pid.pid != -1) {
            processed_status = ap_process_child_status(&pid, exitwhy, status);
            child_slot = ap_find_child_by_pid(&pid);
            if (processed_status == APEXIT_CHILDFATAL) {
                /* fix race condition found in PR 39311
                 * A child created at the same time as a graceful happens
                 * can find the lock missing and create a fatal error.
                 * It is not fatal for the last generation to be in this state.
                 */
                if (child_slot < 0
                    || ap_get_scoreboard_process(child_slot)->generation
                       == retained->mpm->my_generation) {
                    retained->mpm->mpm_state = AP_MPMQ_STOPPING;
                    return !OK;
                }
                else {
                    ap_log_error(APLOG_MARK, APLOG_WARNING, 0, ap_server_conf, APLOGNO(00166)
                                 "Ignoring fatal error in child of previous "
                                 "generation (pid %ld).",
                                 (long)pid.pid);
                }
            }

            /* non-fatal death... note that it's gone in the scoreboard. */
            if (child_slot >= 0) {
                (void) ap_update_child_status_from_indexes(child_slot, 0, SERVER_DEAD,
                                                           (request_rec *) NULL);
                prefork_note_child_killed(child_slot, 0, 0);
                if (processed_status == APEXIT_CHILDSICK) {
                    /* child detected a resource shortage (E[NM]FILE, ENOBUFS, etc)
                     * cut the fork rate to the minimum
                     */
                    retained->idle_spawn_rate = 1;
                }
                else if (remaining_children_to_start
                    && child_slot < ap_daemons_limit) {
                    /* we're still doing a 1-for-1 replacement of dead
                     * children with new children
                     */
                    make_child(ap_server_conf, child_slot);
                    --remaining_children_to_start;
                }
#if APR_HAS_OTHER_CHILD
            }
            else if (apr_proc_other_child_alert(&pid, APR_OC_REASON_DEATH, status) == APR_SUCCESS) {
                /* handled */
#endif
            }
            else if (retained->mpm->was_graceful) {
                /* Great, we've probably just lost a slot in the
                 * scoreboard.  Somehow we don't know about this
                 * child.
                 */
                ap_log_error(APLOG_MARK, APLOG_WARNING,
                            0, ap_server_conf, APLOGNO(00167)
                            "long lost child came home! (pid %ld)", (long)pid.pid);
            }
            /* Don't perform idle maintenance when a child dies,
             * only do it when there's a timeout.  Remember only a
             * finite number of children can die, and it's pretty
             * pathological for a lot to die suddenly.
             */
            continue;
        }
        else if (remaining_children_to_start) {
            /* we hit a 1 second timeout in which none of the previous
             * generation of children needed to be reaped... so assume
             * they're all done, and pick up the slack if any is left.
             */
            startup_children(remaining_children_to_start);
            remaining_children_to_start = 0;
            /* In any event we really shouldn't do the code below because
             * few of the servers we just started are in the IDLE state
             * yet, so we'd mistakenly create an extra server.
             */
            continue;
        }

        perform_idle_server_maintenance(pconf);
    }

    retained->mpm->mpm_state = AP_MPMQ_STOPPING;

    if (retained->mpm->shutdown_pending && retained->mpm->is_ungraceful) {
        /* Time to shut down:
         * Kill child processes, tell them to call child_exit, etc...
         */
        if (ap_unixd_killpg(getpgrp(), SIGTERM) < 0) {
            ap_log_error(APLOG_MARK, APLOG_WARNING, errno, ap_server_conf, APLOGNO(00168) "killpg SIGTERM");
        }
        ap_reclaim_child_processes(1, /* Start with SIGTERM */
                                   prefork_note_child_killed);

        /* cleanup pid file on normal shutdown */
        ap_remove_pid(pconf, ap_pid_fname);
        ap_log_error(APLOG_MARK, APLOG_NOTICE, 0, ap_server_conf, APLOGNO(00169)
                    "caught SIGTERM, shutting down");

        return DONE;
    }

    if (retained->mpm->shutdown_pending) {
        /* Time to perform a graceful shut down:
         * Reap the inactive children, and ask the active ones
         * to close their listeners, then wait until they are
         * all done to exit.
         */
        int active_children;
        apr_time_t cutoff = 0;

        /* Stop listening */
        ap_close_listeners();

        /* kill off the idle ones */
        for (i = 0; i < retained->mpm->num_buckets; i++) {
            ap_mpm_pod_killpg(all_buckets[i].pod, retained->max_daemons_limit);
        }

        /* Send SIGUSR1 to the active children */
        active_children = 0;
        for (index = 0; index < ap_daemons_limit; ++index) {
            if (ap_scoreboard_image->servers[index][0].status != SERVER_DEAD) {
                /* Ask each child to close its listeners. */
                ap_mpm_safe_kill(MPM_CHILD_PID(index), AP_SIG_GRACEFUL);
                active_children++;
            }
        }

        /* Allow each child which actually finished to exit */
        ap_relieve_child_processes(prefork_note_child_killed);

        /* cleanup pid file */
        ap_remove_pid(pconf, ap_pid_fname);
        ap_log_error(APLOG_MARK, APLOG_NOTICE, 0, ap_server_conf, APLOGNO(00170)
           "caught " AP_SIG_GRACEFUL_STOP_STRING ", shutting down gracefully");

        if (ap_graceful_shutdown_timeout) {
            cutoff = apr_time_now() +
                     apr_time_from_sec(ap_graceful_shutdown_timeout);
        }

        /* Don't really exit until each child has finished */
        retained->mpm->shutdown_pending = 0;
        do {
            /* Pause for a second */
            sleep(1);

            /* Relieve any children which have now exited */
            ap_relieve_child_processes(prefork_note_child_killed);

            active_children = 0;
            for (index = 0; index < ap_daemons_limit; ++index) {
                if (ap_mpm_safe_kill(MPM_CHILD_PID(index), 0) == APR_SUCCESS) {
                    active_children = 1;
                    /* Having just one child is enough to stay around */
                    break;
                }
            }
        } while (!retained->mpm->shutdown_pending && active_children &&
                 (!ap_graceful_shutdown_timeout || apr_time_now() < cutoff));

        /* We might be here because we received SIGTERM, either
         * way, try and make sure that all of our processes are
         * really dead.
         */
        ap_unixd_killpg(getpgrp(), SIGTERM);

        return DONE;
    }

    /* we've been told to restart */
    if (one_process) {
        /* not worth thinking about */
        return DONE;
    }

    /* advance to the next generation */
    /* XXX: we really need to make sure this new generation number isn't in
     * use by any of the children.
     */
    ++retained->mpm->my_generation;
    ap_scoreboard_image->global->running_generation = retained->mpm->my_generation;

    if (!retained->mpm->is_ungraceful) {
        ap_log_error(APLOG_MARK, APLOG_NOTICE, 0, ap_server_conf, APLOGNO(00171)
                    "Graceful restart requested, doing restart");

        /* kill off the idle ones */
        for (i = 0; i < retained->mpm->num_buckets; i++) {
            ap_mpm_pod_killpg(all_buckets[i].pod, retained->max_daemons_limit);
        }

        /* This is mostly for debugging... so that we know what is still
         * gracefully dealing with existing request.  This will break
         * in a very nasty way if we ever have the scoreboard totally
         * file-based (no shared memory)
         */
        for (index = 0; index < ap_daemons_limit; ++index) {
            if (ap_scoreboard_image->servers[index][0].status != SERVER_DEAD) {
                ap_scoreboard_image->servers[index][0].status = SERVER_GRACEFUL;
                /* Ask each child to close its listeners.
                 *
                 * NOTE: we use the scoreboard, because if we send SIGUSR1
                 * to every process in the group, this may include CGI's,
                 * piped loggers, etc. They almost certainly won't handle
                 * it gracefully.
                 */
                ap_mpm_safe_kill(ap_scoreboard_image->parent[index].pid, AP_SIG_GRACEFUL);
            }
        }
    }
    else {
        /* Kill 'em off */
        if (ap_unixd_killpg(getpgrp(), SIGHUP) < 0) {
            ap_log_error(APLOG_MARK, APLOG_WARNING, errno, ap_server_conf, APLOGNO(00172) "killpg SIGHUP");
        }
        ap_reclaim_child_processes(0, /* Not when just starting up */
                                   prefork_note_child_killed);
        ap_log_error(APLOG_MARK, APLOG_NOTICE, 0, ap_server_conf, APLOGNO(00173)
                    "SIGHUP received.  Attempting to restart");
    }

    return OK;
}

/* This really should be a post_config hook, but the error log is already
 * redirected by that point, so we need to do this in the open_logs phase.
 */
static int prefork_open_logs(apr_pool_t *p, apr_pool_t *plog, apr_pool_t *ptemp, server_rec *s)
{
    int startup = 0;
    int level_flags = 0;
    ap_listen_rec **listen_buckets;
    apr_status_t rv;
    char id[16];
    int i;

    pconf = p;

    /* the reverse of pre_config, we want this only the first time around */
    if (retained->mpm->module_loads == 1) {
        startup = 1;
        level_flags |= APLOG_STARTUP;
    }

    if ((num_listensocks = ap_setup_listeners(ap_server_conf)) < 1) {
        ap_log_error(APLOG_MARK, APLOG_ALERT | level_flags, 0,
                     (startup ? NULL : s),
                     "no listening sockets available, shutting down");
        return !OK;
    }

    if (one_process) {
        retained->mpm->num_buckets = 1;
    }
    else if (!retained->mpm->was_graceful) {
        /* Preserve the number of buckets on graceful restarts. */
        retained->mpm->num_buckets = 0;
    }
    if ((rv = ap_duplicate_listeners(pconf, ap_server_conf,
                                     &listen_buckets, &retained->mpm->num_buckets))) {
        ap_log_error(APLOG_MARK, APLOG_CRIT | level_flags, rv,
                     (startup ? NULL : s),
                     "could not duplicate listeners");
        return !OK;
    }
    all_buckets = apr_pcalloc(pconf, retained->mpm->num_buckets *
                                     sizeof(prefork_child_bucket));
    for (i = 0; i < retained->mpm->num_buckets; i++) {
        if ((rv = ap_mpm_pod_open(pconf, &all_buckets[i].pod))) {
            ap_log_error(APLOG_MARK, APLOG_CRIT | level_flags, rv,
                         (startup ? NULL : s),
                         "could not open pipe-of-death");
            return !OK;
        }
        /* Initialize cross-process accept lock (safe accept needed only) */
        if ((rv = SAFE_ACCEPT((apr_snprintf(id, sizeof id, "%i", i),
                               ap_proc_mutex_create(&all_buckets[i].mutex,
                                                    NULL, AP_ACCEPT_MUTEX_TYPE,
                                                    id, s, pconf, 0))))) {
            ap_log_error(APLOG_MARK, APLOG_CRIT | level_flags, rv,
                         (startup ? NULL : s),
                         "could not create accept mutex");
            return !OK;
        }
        all_buckets[i].listeners = listen_buckets[i];
    }

    return OK;
}

static int prefork_pre_config(apr_pool_t *p, apr_pool_t *plog, apr_pool_t *ptemp)
{
    int no_detach, debug, foreground;
    apr_status_t rv;
    const char *userdata_key = "mpm_prefork_module";

    debug = ap_exists_config_define("DEBUG");

    if (debug) {
        foreground = one_process = 1;
        no_detach = 0;
    }
    else
    {
        no_detach = ap_exists_config_define("NO_DETACH");
        one_process = ap_exists_config_define("ONE_PROCESS");
        foreground = ap_exists_config_define("FOREGROUND");
    }

    ap_mutex_register(p, AP_ACCEPT_MUTEX_TYPE, NULL, APR_LOCK_DEFAULT, 0);

    retained = ap_retained_data_get(userdata_key);
    if (!retained) {
        retained = ap_retained_data_create(userdata_key, sizeof(*retained));
        retained->mpm = ap_unixd_mpm_get_retained_data();
        retained->max_daemons_limit = -1;
        retained->idle_spawn_rate = 1;
    }
    retained->mpm->mpm_state = AP_MPMQ_STARTING;
    if (retained->mpm->baton != retained) {
        retained->mpm->was_graceful = 0;
        retained->mpm->baton = retained;
    }
    ++retained->mpm->module_loads;

    /* sigh, want this only the second time around */
    if (retained->mpm->module_loads == 2) {
        if (!one_process && !foreground) {
            /* before we detach, setup crash handlers to log to errorlog */
            ap_fatal_signal_setup(ap_server_conf, p /* == pconf */);
            rv = apr_proc_detach(no_detach ? APR_PROC_DETACH_FOREGROUND
                                           : APR_PROC_DETACH_DAEMONIZE);
            if (rv != APR_SUCCESS) {
                ap_log_error(APLOG_MARK, APLOG_CRIT, rv, NULL, APLOGNO(00174)
                             "apr_proc_detach failed");
                return HTTP_INTERNAL_SERVER_ERROR;
            }
        }
    }

    parent_pid = ap_my_pid = getpid();

    ap_listen_pre_config();
    ap_daemons_to_start = DEFAULT_START_DAEMON;
    ap_daemons_min_free = DEFAULT_MIN_FREE_DAEMON;
    ap_daemons_max_free = DEFAULT_MAX_FREE_DAEMON;
    server_limit = DEFAULT_SERVER_LIMIT;
    ap_daemons_limit = server_limit;
    ap_extended_status = 0;

    return OK;
}

static int prefork_check_config(apr_pool_t *p, apr_pool_t *plog,
                                apr_pool_t *ptemp, server_rec *s)
{
    int startup = 0;

    /* the reverse of pre_config, we want this only the first time around */
    if (retained->mpm->module_loads == 1) {
        startup = 1;
    }

    if (server_limit > MAX_SERVER_LIMIT) {
        if (startup) {
            ap_log_error(APLOG_MARK, APLOG_WARNING | APLOG_STARTUP, 0, NULL, APLOGNO(00175)
                         "WARNING: ServerLimit of %d exceeds compile-time "
                         "limit of %d servers, decreasing to %d.",
                         server_limit, MAX_SERVER_LIMIT, MAX_SERVER_LIMIT);
        } else {
            ap_log_error(APLOG_MARK, APLOG_WARNING, 0, s, APLOGNO(00176)
                         "ServerLimit of %d exceeds compile-time limit "
                         "of %d, decreasing to match",
                         server_limit, MAX_SERVER_LIMIT);
        }
        server_limit = MAX_SERVER_LIMIT;
    }
    else if (server_limit < 1) {
        if (startup) {
            ap_log_error(APLOG_MARK, APLOG_WARNING | APLOG_STARTUP, 0, NULL, APLOGNO(00177)
                         "WARNING: ServerLimit of %d not allowed, "
                         "increasing to 1.", server_limit);
        } else {
            ap_log_error(APLOG_MARK, APLOG_WARNING, 0, s, APLOGNO(00178)
                         "ServerLimit of %d not allowed, increasing to 1",
                         server_limit);
        }
        server_limit = 1;
    }

    /* you cannot change ServerLimit across a restart; ignore
     * any such attempts
     */
    if (!retained->first_server_limit) {
        retained->first_server_limit = server_limit;
    }
    else if (server_limit != retained->first_server_limit) {
        /* don't need a startup console version here */
        ap_log_error(APLOG_MARK, APLOG_WARNING, 0, s, APLOGNO(00179)
                     "changing ServerLimit to %d from original value of %d "
                     "not allowed during restart",
                     server_limit, retained->first_server_limit);
        server_limit = retained->first_server_limit;
    }

    if (ap_daemons_limit > server_limit) {
        if (startup) {
            ap_log_error(APLOG_MARK, APLOG_WARNING | APLOG_STARTUP, 0, NULL, APLOGNO(00180)
                         "WARNING: MaxRequestWorkers of %d exceeds ServerLimit "
                         "value of %d servers, decreasing MaxRequestWorkers to %d. "
                         "To increase, please see the ServerLimit directive.",
                         ap_daemons_limit, server_limit, server_limit);
        } else {
            ap_log_error(APLOG_MARK, APLOG_WARNING, 0, s, APLOGNO(00181)
                         "MaxRequestWorkers of %d exceeds ServerLimit value "
                         "of %d, decreasing to match",
                         ap_daemons_limit, server_limit);
        }
        ap_daemons_limit = server_limit;
    }
    else if (ap_daemons_limit < 1) {
        if (startup) {
            ap_log_error(APLOG_MARK, APLOG_WARNING | APLOG_STARTUP, 0, NULL, APLOGNO(00182)
                         "WARNING: MaxRequestWorkers of %d not allowed, "
                         "increasing to 1.", ap_daemons_limit);
        } else {
            ap_log_error(APLOG_MARK, APLOG_WARNING, 0, s, APLOGNO(00183)
                         "MaxRequestWorkers of %d not allowed, increasing to 1",
                         ap_daemons_limit);
        }
        ap_daemons_limit = 1;
    }

    /* ap_daemons_to_start > ap_daemons_limit checked in prefork_run() */
    if (ap_daemons_to_start < 1) {
        if (startup) {
            ap_log_error(APLOG_MARK, APLOG_WARNING | APLOG_STARTUP, 0, NULL, APLOGNO(00184)
                         "WARNING: StartServers of %d not allowed, "
                         "increasing to 1.", ap_daemons_to_start);
        } else {
            ap_log_error(APLOG_MARK, APLOG_WARNING, 0, s, APLOGNO(00185)
                         "StartServers of %d not allowed, increasing to 1",
                         ap_daemons_to_start);
        }
        ap_daemons_to_start = 1;
    }

    if (ap_daemons_min_free < 1) {
        if (startup) {
            ap_log_error(APLOG_MARK, APLOG_WARNING | APLOG_STARTUP, 0, NULL, APLOGNO(00186)
                         "WARNING: MinSpareServers of %d not allowed, "
                         "increasing to 1 to avoid almost certain server failure. "
                         "Please read the documentation.", ap_daemons_min_free);
        } else {
            ap_log_error(APLOG_MARK, APLOG_WARNING, 0, s, APLOGNO(00187)
                         "MinSpareServers of %d not allowed, increasing to 1",
                         ap_daemons_min_free);
        }
        ap_daemons_min_free = 1;
    }

    /* ap_daemons_max_free < ap_daemons_min_free + 1 checked in prefork_run() */

    return OK;
}

static void prefork_hooks(apr_pool_t *p)
{
    /* Our open_logs hook function must run before the core's, or stderr
     * will be redirected to a file, and the messages won't print to the
     * console.
     */
    static const char *const aszSucc[] = {"core.c", NULL};

    ap_hook_open_logs(prefork_open_logs, NULL, aszSucc, APR_HOOK_REALLY_FIRST);
    /* we need to set the MPM state before other pre-config hooks use MPM query
     * to retrieve it, so register as REALLY_FIRST
     */
    ap_hook_pre_config(prefork_pre_config, NULL, NULL, APR_HOOK_REALLY_FIRST);
    ap_hook_check_config(prefork_check_config, NULL, NULL, APR_HOOK_MIDDLE);
    ap_hook_mpm(prefork_run, NULL, NULL, APR_HOOK_MIDDLE);
    ap_hook_mpm_query(prefork_query, NULL, NULL, APR_HOOK_MIDDLE);
    ap_hook_mpm_get_name(prefork_get_name, NULL, NULL, APR_HOOK_MIDDLE);
}

static const char *set_daemons_to_start(cmd_parms *cmd, void *dummy, const char *arg)
{
    const char *err = ap_check_cmd_context(cmd, GLOBAL_ONLY);
    if (err != NULL) {
        return err;
    }

    ap_daemons_to_start = atoi(arg);
    return NULL;
}

static const char *set_min_free_servers(cmd_parms *cmd, void *dummy, const char *arg)
{
    const char *err = ap_check_cmd_context(cmd, GLOBAL_ONLY);
    if (err != NULL) {
        return err;
    }

    ap_daemons_min_free = atoi(arg);
    return NULL;
}

static const char *set_max_free_servers(cmd_parms *cmd, void *dummy, const char *arg)
{
    const char *err = ap_check_cmd_context(cmd, GLOBAL_ONLY);
    if (err != NULL) {
        return err;
    }

    ap_daemons_max_free = atoi(arg);
    return NULL;
}

static const char *set_max_clients (cmd_parms *cmd, void *dummy, const char *arg)
{
    const char *err = ap_check_cmd_context(cmd, GLOBAL_ONLY);
    if (err != NULL) {
        return err;
    }
    if (!strcasecmp(cmd->cmd->name, "MaxClients")) {
        ap_log_error(APLOG_MARK, APLOG_INFO, 0, NULL, APLOGNO(00188)
                     "MaxClients is deprecated, use MaxRequestWorkers "
                     "instead.");
    }
    ap_daemons_limit = atoi(arg);
    return NULL;
}

static const char *set_server_limit (cmd_parms *cmd, void *dummy, const char *arg)
{
    const char *err = ap_check_cmd_context(cmd, GLOBAL_ONLY);
    if (err != NULL) {
        return err;
    }

    server_limit = atoi(arg);
    return NULL;
}

static const command_rec prefork_cmds[] = {
LISTEN_COMMANDS,
AP_INIT_TAKE1("StartServers", set_daemons_to_start, NULL, RSRC_CONF,
              "Number of child processes launched at server startup"),
AP_INIT_TAKE1("MinSpareServers", set_min_free_servers, NULL, RSRC_CONF,
              "Minimum number of idle children, to handle request spikes"),
AP_INIT_TAKE1("MaxSpareServers", set_max_free_servers, NULL, RSRC_CONF,
              "Maximum number of idle children"),
AP_INIT_TAKE1("MaxClients", set_max_clients, NULL, RSRC_CONF,
              "Deprecated name of MaxRequestWorkers"),
AP_INIT_TAKE1("MaxRequestWorkers", set_max_clients, NULL, RSRC_CONF,
              "Maximum number of children alive at the same time"),
AP_INIT_TAKE1("ServerLimit", set_server_limit, NULL, RSRC_CONF,
              "Maximum value of MaxRequestWorkers for this run of Apache"),
AP_GRACEFUL_SHUTDOWN_TIMEOUT_COMMAND,
{ NULL }
};

AP_DECLARE_MODULE(mpm_prefork) = {
    MPM20_MODULE_STUFF,
    NULL,                       /* hook to run before apache parses args */
    NULL,                       /* create per-directory config structure */
    NULL,                       /* merge per-directory config structures */
    NULL,                       /* create per-server config structure */
    NULL,                       /* merge per-server config structures */
    prefork_cmds,               /* command apr_table_t */
    prefork_hooks,              /* register hooks */
};

#endif  /* __VMS */
