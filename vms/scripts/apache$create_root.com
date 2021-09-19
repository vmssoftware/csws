$! *************************************************************************
$! *                                                                       *
$! * HP CONFIDENTIAL. This software is confidential proprietary software   *
$! * licensed by Hewlett-Packard Development Company, L.P., and is not     *
$! * authorized to be used, duplicated or disclosed to anyone without the  *
$! * prior written permission of HP. © 2015 Copyright Hewlett-Packard      *
$! * Development Company, L.P.                                             *
$! *                                                                       *
$! * VMS SOFTWARE, INC. CONFIDENTIAL. This software is confidential        *
$! * proprietary software licensed by VMS Software, Inc., and is not       *
$! * authorized to be used, duplicated or disclosed to anyone without the  *
$! * prior written permission of VMS Software, Inc. © 2015 Copyright VMS   *
$! * Software, Inc.                                                        *
$! *                                                                       *
$! *************************************************************************
$!
$! Module:   APACHE$CREATE_ROOT.COM
$! Version:  2.1-000
$!
$! Modification History:
$!
$!	01-Aug-2006	2.1-000		Scott LePage
$!	Fix bug where non-exitant directory is checked (file_attributes)
$!	and fails.  PTR #75-62-520
$!
$!	18-Jul-2003	2.0-002		Scott LePage
$!	Fix ownership of httpd.conf file when populating.
$!
$!	21-Apr-2003	2.0-001		Scott LePage
$!	Check for overwrite condition of template directory (APACHE$COMMON)
$!	and abort if conflict.  Also check for overwrite of other roots
$!	and give warning.
$!
$!	19-Dec-2002	2.0-000		Scott LePage
$!	Initial creation.
$!
$!
$
$	set :=
$	dir*ectory := directory
$	del*ete := delete
$	set symbol/scope=(nolocal,noglobal)
$	echo := write sys$output
$	uaf := $authorize
$	comfil = f$environment("procedure")
$	comnam = f$parse(comfil,,,"name","syntax_only") + -
                 f$parse(comfil,,,"type","syntax_only")
$	tmpfil = f$trnlnm("sys$scratch") + f$getjpi("", "pid")
$	save_message = f$environment("message")
$	no_message = "/nofacility/noseverity/noidentification/notext"
$	verbose = f$trnlnm("apache$create_verbose")
$	if (verbose .eqs. "") then verbose := FALSE
$	logicals_defined = "FALSE"
$	saved_parse_style = f$getjpi("", "PARSE_STYLE_PERM")
$	set process/parse_style=Extended
$
$!
$!  Enable privileges for the interactive process.
$!
$	temp1 = "SYSPRV, SYSNAM, OPER, DETACH, BYPASS, CMKRNL, ALTPRI, WORLD"
$	save_privs = f$setprv(temp1)
$	if (.not. f$privileges(temp1))
$	then
$	   echo "ERROR: Insufficient privileges to run ''comnam'"
$	   echo "   Required privileges: ", temp1
$	   exit_status = 36
$	   goto abort_procedure
$	endif
$
$!----------------------------------------------------------------------
$!
$!	Create new root for the Secure Web Server
$!
$!----------------------------------------------------------------------
$
$	gosub dialog_intro
$
$	gosub setup_logicals
$	gosub dialog_main
$	gosub grant_user_rights
$	gosub create_root
$	gosub populate_root
$	gosub update_db
$
$	echo "Root created: ''cr_root'"
$	echo "Template server configuration file created: ''cr_conf'"
$	echo "Please review this file for accuracy."
$	echo ""
$	exit_status = 1
$	goto stop_procedure

$-----------------------------------------------------------------------
$ d i a l o g _ i n t r o
$
$	Output introductory text and allow user to abort
$
$dialog_intro:
$	echo ""
$	echo "      APACHE$CREATE_ROOT"
$	echo ""
$	echo " Create a set of directories and files where a Secure"
$	echo " Web Server can run.  You will be prompted for the"
$	echo " location of the root, the user to run under, the"
$	echo " TCP/IP port to monitor, the unique server tag, the"
$	echo " privileged routines the user will be allowed to use,"
$	echo " and optional startup and shutdown procedures."
$	echo ""
$	exit_status = %X1000002c
$ QCont:
$	inquire/nopunctuation ans "Continue [YES]? "
$	if ans .eqs. "" then ans = "YES"
$	if f$edit(f$extract(0, 1, ans), "UPCASE") .eqs. "Y" then return
$	if f$edit(f$extract(0, 1, ans), "UPCASE") .eqs. "N" then goto stop_procedure
$	goto QCont

$!--------------------------------------------------------------
$! s e t u p _ l o g i c a l s
$!
$!	Check the logicals and define them to run this procedure.
$!
$setup_logicals:
$	if f$trnlnm("APACHE$COMMON") .eqs. ""
$	then
$	    if verbose then echo "[ Defining APACHE logicals in this process ]"
$	    @sys$manager:apache$logicals "" "/process" "NOINSTALL"
$	    logicals_defined = "TRUE"
$	endif
$	return


$!--------------------------------------------------------------
$! d i a l o g _ m a i n
$!
$!	Prompt the user for all the required information
$!	saving his/her answers in local variables
$!
$dialog_main:
$
$!   CR_ROOT: Location where the root should be created
$
$	type sys$input
$	deck

 Root location:  Give the location of where to create the directory
 tree and configuration template file for the new instance of the server.

    e.g.  USER2:[SMITH.CSWS]

 This will create a series of directories under the USER2:[SMITH.CSWS]
 directory.  This will become the new APACHE$SPECIFIC location.

    $ DIRECTORY USER2:[SMITH.CSWS]

    Directory USER2:[SMITH.CSWS]

    BIN.DIR;1           CGI-BIN.DIR;1       CONF.DIR;1          HTDOCS.DIR;1
    ICONS.DIR;1         KIT.DIR;1           LOGS.DIR;1          MODULES.DIR;1
    OPENSSL.DIR;1

    Total of 9 files.

$	eod
$ QRoot:
$!	inquire/nopunctuation cr_root "Root Location: "
$	read/end=QRoot/Prompt="Root Location: " sys$command cr_root
$	if cr_root .eqs. "" then goto QRoot
$	if f$locate(":",cr_root) .eq. f$length(cr_root)
$	then
$	    echo ""
$	    echo "  Enter a root specification similar to device:[directory]"
$	    echo ""
$	    cr_root = ""
$	    temp1 = ""
$	    temp2 = ""
$	    goto QRoot
$	endif
$
$	if f$locate("[",cr_root) .eq. f$length(cr_root)
$	then
$	    echo ""
$	    echo "  Enter a root specification similar to device:[directory]"
$	    echo ""
$	    cr_root = ""
$	    temp1 = ""
$	    temp2 = ""
$	    goto QRoot
$	endif
$
$	gosub check_for_overwrite
$	if exit_status .eq. 148
$	then
$	    echo ""
$	    echo "  The root specified conflicts with APACHE$COMMON root."
$	    echo "  This is not allowed.  Please specify another location."
$	    echo ""
$	    cr_root = ""
$	    temp1 = ""
$	    temp2 = ""
$	    exit_status = 1
$	    goto QRoot
$	endif
$	if exit_status .eq. 144
$	then
$	    echo ""
$	    echo "  The root specified appears to conflict with a previously"
$	    echo "  created root.  You can continue, however the previously"
$	    echo "  defined root may not be runable."
$	    echo ""
$ QRCont:
$	    inquire/nopunct ans "Continue to overwrite [NO]? "
$	    if ans .eqs. "" then ans = "NO"
$	    if f$edit(f$extract(0,1,ans), "UPCASE") .eqs. "Y" then goto QRok
$	    if f$edit(f$extract(0,1,ans), "UPCASE") .nes. "N" then goto QRCont
$	    cr_root = ""
$	    temp1 = ""
$	    temp2 = ""
$	    goto QRoot
$	endif
$ QRok:
	exit_status = 1
$
$! CR_USER: Username that will own/control this root
$
$	type sys$input
$	deck

 Username: Enter the user that will own and control the content of
 this root.  The ownership of the directories and files will be set
 to the given user.  The user must be a valid user in the SYSUAF.

$	eod
$ QUser:
$!	inquire/nopunctuation cr_user "Username: "
$	read/end=QUser/Prompt="Username: " sys$command cr_user
$	if cr_user .eqs. "" then goto QUser
$	temp1 = f$identifier(cr_user,"name_to_number")
$	if temp1 .eq. 0
$	then
$	    echo "The entered user was not found in the SYSUAF"
$	    goto QUser
$	endif
$
$! CR_PRIVS: Get privileged routines allowed for this user
$
$	type sys$input
$	deck

 The Secure Web Server has several privileged routines to allow the
 server to run in a basic fashion.  These routines can be blocked from
 other users of the web server to run in a more restrictive mode.
 These routines are protected by a series of rights identifiers:

    APACHE$APR_ALL           Allow access to all of the protected routines
    APACHE$APR_CREMBX        Allow access to create a groupwide mailbox
    APACHE$APR_GETPWNAM      Allow access to other user's information
    APACHE$APR_SETSOCKOPT    Allow user to set socket options
    APACHE$APR_SOCKET        Allow creation of a privileged socket
    APACHE$APR_AUTH_OPENVMS  Allow user to authorize using SYSUAF
    APACHE$APR_GALAXY_GBLSEC Allow user to manage galactic memory sections

$	eod
$	cr_privs = "|"
$ QPAll:
$	inquire/nopunctuation temp1 "Grant access to ALL routines? "
$	if f$edit(f$extract(0,1,temp1),"UPCASE") .eqs. "Y"
$	then
$	    cr_privs = cr_privs + "APACHE$APR_ALL" + "|"
$	    goto PrivDone
$	endif
$	if f$edit(f$extract(0,1,temp1),"UPCASE") .nes. "N" then goto QPAll
$
$ QPCrembx:
$	inquire/nopunctuation temp1 "Grant access to CreMbx? "
$	if f$edit(f$extract(0,1,temp1),"UPCASE") .eqs. "Y"
$	then
$	    cr_privs = cr_privs + "APACHE$APR_CREMBX" + "|"
$	else
$	    if f$edit(f$extract(0,1,temp1),"UPCASE") .nes. "N" then goto QPCrembx
$	endif
$
$ QPGetpwnam:
$	inquire/nopunctuation temp1 "Grant access to GetPwNam? "
$	if f$edit(f$extract(0,1,temp1),"UPCASE") .eqs. "Y"
$	then
$	    cr_privs = cr_privs + "APACHE$APR_GETPWNAM" + "|"
$	else
$	    if f$edit(f$extract(0,1,temp1),"UPCASE") .nes. "N" then goto QPGetpwnam
$	endif
$
$ QPSetsockopt:
$	inquire/nopunctuation temp1 "Grant access to SetSockOpt? "
$	if f$edit(f$extract(0,1,temp1),"UPCASE") .eqs. "Y"
$	then
$	    cr_privs = cr_privs + "APACHE$APR_SETSOCKOPT" + "|"
$	else
$	    if f$edit(f$extract(0,1,temp1),"UPCASE") .nes. "N" then goto QPSetsockopt
$	endif
$
$ QPSocket:
$	inquire/nopunctuation temp1 "Grant access to Create a Privileged Socket? "
$	if f$edit(f$extract(0,1,temp1),"UPCASE") .eqs. "Y"
$	then
$	    cr_privs = cr_privs + "APACHE$APR_SOCKET" + "|"
$	else
$	    if f$edit(f$extract(0,1,temp1),"UPCASE") .nes. "N" then goto QPSocket
$	endif
$
$ QPAuthVMS:
$	inquire/nopunctuation temp1 "Grant authorization via SYSUAF? "
$	if f$edit(f$extract(0,1,temp1),"UPCASE") .eqs. "Y"
$	then
$	   cr_privs = cr_privs + "APACHE$APR_AUTH_OPENVMS" + "|"
$	else
$	   if f$edit(f$extract(0,1,temp1),"UPCASE") .nes. "N" then goto QPAuthVMS
$	endif
$
$ QPGalaxy:
$	inquire/nopunctuation temp1 "Grant user ability to access galactic sections? "
$	if f$edit(f$extract(0,1,temp1),"UPCASE") .eqs. "Y"
$	then
$	   cr_privs = cr_privs + "APACHE$APR_GALAXY_GBLSEC" + "|"
$	else
$	   if f$edit(f$extract(0,1,temp1),"UPCASE") .nes. "N" then goto QPGalaxy
$	endif
$
$
$ PrivDone:
$	cr_privs = cr_privs + "APACHE$READ|APACHE$EXECUTE|"
$
$! CR_PORT: TCP/IP port to monitor
$
$	type sys$input
$	deck

 Each instance of the Secure Web Server must have a unique TCP/IP port
 to monitor as it runs.  If you have not granted this user the Socket
 privilege, then the port must be greater than 1024 (non-privileged).
 Note that this routine does not keep track of previously specified
 ports to other instances.  It the system manager's responsibility to
 maintain this information.

$	eod
$	if f$locate("_ALL", cr_privs) .nes. f$length(cr_privs) then goto PortPriv
$	if f$locate("_SOCKET", cr_privs) .nes. f$length(cr_privs) then goto PortPriv
$	min_port = 1024
$	goto QPort
$ PortPriv:
$	min_port = 0
$
$ QPort:
$	inquire/nopunctuation cr_port "Port number: "
$	if f$type(cr_port) .nes. "INTEGER"
$	then
$	    echo "Entry must be an integer number"
$	    goto Qport
$	endif
$	if 'cr_port' .lt. min_port .or. -
           'cr_port' .gt. 65535
$	then
$	    echo "Enter an integer number between ''min_port' and 65535"
$	    goto Qport
$	endif
$
$! CR_TAG: Unique tag to use for this instance of CSWS
$
$	type sys$input
$	deck

 Each instance of the Secure Web Server must have a unique tag associated
 with it on the system.  The tag is 1 to 4 characters (A-Z, 0-9).

$	eod
$ QTag:
$!	inquire/nopunctuation cr_tag "Unique Tag: "
$	read/end=QTag/Prompt="Unique Tag: " sys$command cr_tag
$	if f$length(cr_tag) .eq. 0 then goto QTag
$	if f$length(cr_tag) .gt. 4
$	then
$	    echo "Tag length can be a maximum of 4 characters"
$	    goto QTag
$	endif
$
$
$! CR_STARTUP & CR_SHUTDOWN: Commands to execute before startup & after shutdown
$
$	type sys$input
$	deck

 The instance of Secure Web Server can have a startup and a shutdown
 command procedure defined to run accordingly.

$	eod
$	cr_startup = ""
$	cr_shutdown = ""
$ QSProc:
$	inquire/nopunctuation cr_sors "Define a startup or shutdown procedure? "
$	cr_sors = f$edit(f$extract(0,1,cr_sors),"UPCASE")
$	if cr_sors .eqs. "Y" then goto QSStart
$	if cr_sors .nes. "N" then goto QSProc
$	goto QSDone
$ QSStart:
$	read/end=QSStart/Prompt="Startup procedure filename [NONE]: " sys$command cr_startup
$	if f$length(cr_startup) .eq. 0 then goto QSShut
$	if f$parse("''cr_startup'") .eqs. ""
$	then
$	    echo "Invalid filename: ''cr_startup'"
$	    echo "Please try again"
$	    echo ""
$	    cr_startup = ""
$	    goto QSStart
$	endif
$
$ QSShut:
$	read/end=QSShut/Prompt="Shutdown procedure filename [NONE]: " sys$command cr_shutdown
$	if f$length(cr_shutdown) .eqs. 0 then goto QSDone
$	if f$parse("''cr_shutdown'") .eqs. ""
$	then
$	    echo "Invalid filename: ''cr_shutdown'"
$	    echo "Please try again"
$	    echo ""
$	    cr_shutdown = ""
$	    goto QSShut
$	endif
$
$ QSDone:
$	if f$length(cr_startup) .eq. 0 .and. -
           f$length(cr_shutdown) .eq. 0 then -
            cr_sors = "N"
$
$
$	echo ""
$	return



$!--------------------------------------------------------------
$! g r a n t _ u s e r _ r i g h t s
$!
$!	Grant the input rights to the input user
$!
$! Inputs:
$!	cr_user - username to get the rights
$!	cr_privs - list of rights identifiers to be granted
$!
$! Outputs:
$!	none
$!
$grant_user_rights:
$	echo "Granting rights to ''cr_user' UAF account..."
$	gosub revoke_rights
$	set noon
$	set message 'no_message'
$	if verbose then echo "[ Granting: ''cr_privs' ]"
$	temp1 = f$extract(1,f$length(cr_privs)-1, cr_privs)
$	idx = 0
$ GURLoop:
$	right = f$element(idx,"|",temp1)
$	if right .eqs. "|" then goto GURDone
$	if f$trnlnm("SYSUAF") .eqs. "" then -
	    define/user SYSUAF SYS$SYSTEM:SYSUAF.DAT
$	uaf grant/ident 'right' 'cr_user'
$	idx = idx + 1
$	goto GURLoop
$ GURDone:
$	set on
$	set message 'save_message'
$
$	return

$!--------------------------------------------------------------
$! r e v o k e _ r i g h t s
$!
$!	Revoke all known APACHE$* rights so there are
$!	no confilcts.
$!
$! Inputs:
$!	cr_user - user to be stripped of their rights
$!
$! Outputs:
$!	none
$!
$revoke_rights:
$	set noon
$	set message 'no_message'
$
$	uaf_defined = "FALSE"
$	if f$trnlnm("SYSUAF") .eqs. ""
$	then
$	    uaf_defined = "TRUE"
	    define SYSUAF SYS$SYSTEM:SYSUAF.DAT
$	endif
$	if verbose then echo "[ Revoking rights from ''cr_user' account ]"
$	uaf revoke/ident APACHE$APR_ALL 'cr_user'
$	uaf revoke/ident APACHE$APR_CREMBX 'cr_user'
$	uaf revoke/ident APACHE$APR_GETPWNAM 'cr_user'
$	uaf revoke/ident APACHE$APR_SETSOCKOPT 'cr_user'
$	uaf revoke/ident APACHE$APR_SOCKET 'cr_user'
$	uaf revoke/ident APACHE$APR_AUTH_OPENVMS 'cr_user'
$	uaf revoke/ident APACHE$APR_GALAXY_GBLSEC 'cr_user'
$	uaf revoke/ident APACHE$READ 'cr_user'
$	uaf revoke/ident APACHE$EXECUTE 'cr_user'
$	if uaf_defined .eqs. "TRUE" then -
	    deassign SYSUAF
$
$	set on
$	set message 'save_message'
$	return


$!--------------------------------------------------------------
$! c r e a t _ r o o t
$!
$!	Create a directory tree in the target area
$!	that mirrors the directories found under
$!	APACHE$COMMON:[000000]
$!
$! Inputs:
$!	cr_root - Target directory to become APACHE$SPECIFIC
$!	cr_user - User to own the directory tree
$!
$! Outputs:
$!	none
$!
$create_root:
$	echo "Creating directory tree under ''cr_root'"
$	outfil = tmpfil + ".temp"
$	dir/output='outfil'/column=1/nohead/notrail-
           /exclude=([SPECIFIC...],SPECIFIC.DIR) -
           APACHE$COMMON:[000000...]*.dir
$
$	open/read infil 'outfil'
$ CRLoop:
$	read/end=CRDone infil recd
$	dirfile = f$parse(recd,,,"NAME","SYNTAX_ONLY")
$	target = f$extract(0,f$length(cr_root)-1, cr_root)
$	newspec = recd - "APACHE$COMMON:[000000"
$	newfile = target + newspec
$	endspec = "]" + dirfile + ".DIR;1"
$	target = newfile - endspec
$	target = target + "." + dirfile + "]"
$	create/directory/owner=['cr_user'] 'target'
$	if verbose then echo" [ create directory: ''target' ]"
$	goto CRLoop
$ CRDone:
$	close infil
$	delete/nolog 'outfil';*
$
$	return


$!--------------------------------------------------------------
$! p o p u l a t e _ r o o t
$!
$!	Create a Secure Web Server configuration file
$!	based on the template and the input to this procedure.
$!
$! Inputs:
$!	cr_root - ServerRoot
$!	cr_port - TCP/IP Port
$!	cr_user - Username
$!	cr_tag  - VmsServerTag
$!	cr_startup - VmsServerStartup
$!	cr_shutdown - VmsServerShutdown
$!
$! Outputs:
$!	cr_conf - New configuration filespec
$!
$! Other considerations:
$!	The tokens that are searched for are contained in the template
$!	configuration file, APACHE$COMMON:[CONF]HTTPD-VMS.CONF.  If those
$!	change for any reason, this routine will break.
$!
$populate_root:
$!
$! Configuration file
$	q = """
$	conf = f$extract(0,f$length(cr_root)-1, cr_root)
$	cr_conf = conf + ".CONF]httpd.conf"
$	echo "Generating Apache configuration file ''cr_conf'"
$	template = "APACHE$COMMON:[CONF]HTTPD-VMS.CONF"
$
$	fdlfile = tmpfil + ".temp"
$	analyze/rms/fdl/output='fdlfile' 'template'
$	create/fdl='fdlfile' 'cr_conf'
$	delete 'fdlfile';*
$	set file/owner=['cr_user'] 'cr_conf'
$	if verbose then echo "[Empty configuration created from ''template']
$
$	copy APACHE$COMMON:[CONF]MIME.TYPES 'conf'.CONF]mime.types
$	set file/owner=['cr_user'] 'conf'.CONF]mime.types
$	if verbose then echo "[MIME.TYPES copied]"
$
$	copy APACHE$COMMON:[CONF]SSL-VMS.CONF 'conf'.CONF]SSL.CONF
$	set file/owner=['cr_user'] 'conf'.CONF]SSL.CONF
$	if verbose then echo "[SSL.CONF copied]"
$
$	open/read/error=PRConfDone tmplt 'template'
$	open/append/error=PRConfDone out 'cr_conf'
$ PRConfLoop:
$	read/end=PRConfDone tmplt recd
$	token = "ServerRoot"
$	if f$extract(0,f$length(token),recd) .eqs. token
$	then
$	    write out "ServerRoot ""''cr_root'"""
$	    if verbose then echo "[ServerRoot updated - ''cr_root']"
$	    goto PRConfLoop
$	endif
$
$	token = "VmsServerTag"
$	if f$extract(0,f$length(token),recd) .eqs. token
$	then
$	    write out "VmsServerTag ''cr_tag'"
$	    if verbose then echo "[VmsServerTag updated - ''cr_tag']"
$	    goto PRConfLoop
$	endif
$
$	token = "User APACHE$WWW"
$	if f$extract(0,f$length(token),recd) .eqs. token
$	then
$	    write out "User ''cr_user'"
$	    if verbose then echo "[User updated - ''cr_user']"
$	    goto PRConfLoop
$	endif
$
$	token = "Listen 80"
$	if f$extract(0,f$length(token),recd) .eqs. token
$	then
$	    write out "Listen ''cr_port'"
$	    if verbose then echo "[Port updated - ''cr_port']"
$	    goto PRConfLoop
$	endif
$
$	token = "#VmsServerStartup"
$	if f$extract(0,f$length(token),recd) .eqs. token
$	then
$	    if cr_sors .eqs. "Y"
$	    then
$	        if f$length(cr_startup) .gt. 0
$	        then
$	            cr_startup = cr_startup - ";"
$	            write out "VmsServerStartup ''cr_startup'"
$	            if verbose then echo "[VmsServerStartup updated - ''cr_startup']"
$	            goto PRConfLoop
$	        endif
$	    endif
$	endif
$
$	token = "#VmsServerShutdown"
$	if f$extract(0,f$length(token),recd) .eqs. token
$	then
$	    if cr_sors .eqs. "Y"
$	    then
$	        if f$length(cr_shutdown) .gt. 0
$	        then
$	            cr_shutdown = cr_shutdown - ";"
$	            write out "VmsServerShutdown ''cr_shutdown'"
$	            if verbose then echo "[VmsServerShutdown updated - ''cr_shutdown']"
$	            goto PRConfLoop
$	        endif
$	    endif
$	endif
$
$	if f$locate(q,recd) .ne. f$length(recd) then
	    gosub handle_quotes
$
$	write out "''recd'"
$	goto PRConfLoop
$ PRConfDone:
$	close tmplt
$	close out
$
$	return


$!--------------------------------------------------------------
$! h a n d l e _ q u o t e s
$!
$!	The DCL processor chokes on strings that contain quotes
$!	that it is trying to write to a file.  This doubles
$!	them up.
$!
$! Inputs:
$!	recd - contains record to fix
$!
$! Outputs:
$!	recd - updated record
$!
$handle_quotes:
$	q = """
$	r = recd
$	newr = ""
$ HQLoop:
$	ql = f$locate(q, r)
$	if ql .eq. f$length(r) then goto HQDone
$	newr = newr + f$extract(0, ql, r) + q + q
$	r = f$extract(ql+1, f$length(r) - (ql+1), r)
$	goto HQLoop
$ HQDone:
$	if f$length(r) .gt. 0 then
$	    newr = newr + r
$	recd = newr
$
$	return


$!--------------------------------------------------------------
$! u p d a t e _ d b
$!
$!	Updates the APACHE$COMMON:[000000]APACHE$CONFIG.DAT adding
$!	the new instance of Apache to the file.  If the tag
$!	already exists, the instance is updated with a warning
$!	and the original database is saved in a .BAK file
$!
$! Inputs:
$!	cr_tag  - Unique tag to add to file
$!	cr_conf - Location of the configuration file
$!
$! Outputs:
$!	SYS$COMMON:[000000]APACHE$CONFIG.DAT is updated
$!
$update_db:
$	dbfile = "APACHE$COMMON:[000000]APACHE$CONFIG.DAT"
$	tmpfile = "APACHE$COMMON:[000000]APACHE$CONFIG.TMP"
$	bakfile = "APACHE$COMMON:[000000]APACHE$CONFIG.BAK"
$
$	if f$search("''dbfile'") .eqs. "" then return
$
$	echo "Updating the configuration database"
$	open/read apdat 'dbfile'
$ DBLoop:
$	read/end=DBDone apdat recd
$	recd = f$edit(recd,"TRIM,COMPRESS")
$	if f$length(recd) .eq. 0 then goto DBLoop
$	if f$extract(0,1,recd) .eqs. "#" then goto DBLoop
$	if f$extract(0,1,recd) .eqs. "!" then goto DBLoop
$
$	tag = f$element(0," ",recd)
$	conf = f$element(1," ",recd)
$
$	if tag .eqs. cr_tag
$	then
$	    echo ""
$	    echo "The tag ''cr_tag' was already found in the instance database."
$	    if conf .eqs. cr_conf
$	    then
$	        echo "Skipping the update..."
$	        close apdat
$	        return
$	    else
$	        echo "Changing the associated configuration file from:"
$	        echo "    ''conf'"
$	        echo "to:"
$	        echo "    ''cr_conf'"
$	        echo ""
$	        echo "This should be checked into.  There may be a lingering configuration"
$	        echo "that you are overwriting.  The original database is being saved into:"
$	        echo "   ''bakfile'"
$	        close apdat
$	        goto DBUpdate
$	    endif
$	endif
$	goto DBLoop
$ DBDone:
$	close apdat
$
$	open/append apdat 'dbfile'
$	tab = "    "
$	pad = 4 - f$length(cr_tag)
$	tab = tab + f$extract(0,pad,tab)
$
$	write apdat "''cr_tag'''tab'''cr_conf'"
$	if verbose then echo "[Added tag ''cr_tag' to config database]"
$	close apdat
$	return
$
$ DBUpdate:
$	open/read apdat 'dbfile'
$	open/write tfile 'tmpfile'
$ DBULoop:
$	read/end=DBUDone apdat recd
$	temp1 = f$edit(recd,"TRIM,COMPRESS")
$	tag = f$element(0," ",temp1)
$	if tag .eqs. cr_tag
$	then
$	    tab = "    "
$	    pad = 4 - f$length(cr_tag)
$	    tab = tab + f$extract(0,pad,tab)
$	    write tmpfile "''cr_tag'''tab'''cr_conf'"
$	    if verbose then echo "[Updated tag ''cr_tag' in config database]"
$	else
$	    write tfile "''recd'"
$	endif
$	goto DBULoop
$ DBUDone:
$	close tfile
$	close apdat
$
$	rename 'dbfile' 'bakfile'
$	rename 'tmpfile' 'dbfile'
$
$	return



$!--------------------------------------------------------------
$! c h e c k _ f o r _ o v e r w r i t e
$!
$!	Check input root location for overwrite of APACHE$COMMON (fatal)
$!	or overwrite of existing root (warning).
$!
$! Inputs:
$!	cr_root - location to be created
$!
$! Outputs:
$!	exit_status = 1 => ok to continue with creation (no overwrite)
$!	exit_status = 148 => fatal error (duplicate name)
$!	exit_status = 144 => warning (duplicate name)
$!
$check_for_overwrite:
$!
$! Get the DID of the input directory specification
$!
$	indev = f$parse(cr_root,,,"Device")
$	dd = f$parse(cr_root,,,"Directory")
$	temp1 = indev - ":"
$	if f$trnlnm(temp1) .nes. ""
$	then
$	    temp1 = f$trnlnm(temp1)
$	    colon = f$locate(":",temp1)
$	    if f$extract(colon+1,1,temp1) .eqs. "["
$	    then
$	        spec = f$trnlnm(indev) - ".]"
$	        dd = f$extract(1,f$length(dd)-1,dd)
$	        spec = spec + "." + dd
$	        indev = f$parse(spec,,,"Device")
$	        dd = f$parse(spec,,,"Directory")
$	    else
$	        device = temp1
$	    endif
$	endif
$	indev = f$getdvi(indev,"DEVNAM")
$	gosub get_directory_file
$	temp1 = indev + retval
$!
$! If directory does not exist return success (no conflicts)
$!
$	if f$search(temp1) .eqs. ""
$	then
$	    exit_status = 1
$	    return
$	endif
$
$	indid = f$file_attributes(temp1,"FID")
$
$!
$! Get the DID of the APACHE$COMMON:[000000] specification
$!
$	spec = (f$trnlnm("APACHE$COMMON") - ".]") + "]"
$	device = f$parse(spec,,,"Device")
$	device = f$getdvi(device,"DEVNAM")
$	dd = f$parse(spec,,,"Directory")
$	gosub get_directory_file
$	temp1 = device + retval
$	did = f$file_attributes(temp1,"FID")
$
$!
$! If they're the same - fatal error, return to reprompt for new spec
$!
$	if indev .eqs. device .and. -
	   indid .eqs. did
$	then
$	    exit_status = 148
$	    return
$	endif
$
$!
$! Get the DID of the APACHE$SPECIFIC:[000000] directory
$!
$	spec = (f$trnlnm("APACHE$SPECIFIC") - ".]") + "]"
$	device = f$parse(spec,,,"Device")
$	device = f$getdvi(device,"DEVNAM")
$	dd = f$parse(spec,,,"Directory")
$	gosub get_directory_file
$	temp1 = device + retval
$	did = f$file_attributes(temp1,"FID")
$
$!
$! If they're the same - fatal error
$!
$	if indev .eqs. device .and. -
	   indid .eqs. did
$	then
$	    exit_status = 148
$	    return
$	endif
$
$!
$! Now check existing roots in the APACHE$CONFIG.DAT file
$!
$	exit_status = 1
$	open/read/share cfg_dat apache$common:[000000]apache$config.dat
$ RCDLoop:
$	read/end=RCDDone cfg_dat recd
$	if f$length(recd) .eq. 0 then goto RCDLoop
$	recd = f$edit(recd,"TRIM,COMPRESS")
$	temp1 = f$extract(0,1,recd)
$	if temp1 .eqs. "#" then goto RCDLoop
$	if temp1 .eqs. "!" then goto RCDLoop
$
$	id = f$element(0," ",recd)
$	if id .eqs. "SWS" then goto RCDLoop
$	conf = f$element(1," ",recd)
$
$	device = f$parse(conf,,,"Device")
$	dd = f$parse(conf,,,"Directory")
$	temp1 = device - ":"
$	if f$trnlnm(temp1) .nes. ""
$	then
$	    temp1 = f$trnlnm(temp1)
$	    colon = f$locate(":",temp1)
$	    if f$extract(colon+1,1,temp1) .eqs. "["
$	    then
$	        device = f$trnlnm(temp1) - ".]"
$	        dd = f$extract(1,f$length(dd)-1,dd)
$	        spec = device + "." + dd
$	        device = f$parse(spec,,,"Device")
$	        dd = f$parse(spec,,,"Directory")
$	    else
$	        device = temp1
$	    endif
$	endif
$	device = f$getdvi(device,"DEVNAM")
$	gosub get_directory_file
$	temp2 = device + retval
$	dd = f$parse(temp2,,,"Directory")
$	gosub get_directory_file
$	temp2 = device + retval
$	did = f$file_attributes(temp2,"FID")
$
$	if indev .eqs. device .and. -
	   indid .eqs. did
$	then
$	    exit_status = 144
$	    goto RCDDone
$	endif
$	goto RCDLoop
$ RCDDone:
$	close cfg_dat
$	return



$!--------------------------------------------------------------
$! g e t _ d i r e c t o r y _ f i l e
$!
$!	Takes a directory string [DIR1.DIR2.DIR3] and returns
$!	[DIR1.DIR2]DIR3.DIR.  If the directory only contains
$!	[000000] it will return [000000]000000.DIR.
$!
$!	Also be careful of ODS-5...
$!
$! Inputs:
$!	dd - the directory specification
$!
$! Outputs:
$!	retval - the returned directory file specification
$!
$get_directory_file:
$	retval = ""
$	if dd .eqs. "[000000]"
$	then
$	    retval = "[000000]000000.DIR"
$	    return
$	endif
$	ddspec = dd
$ GDFOLoop:
$	token = ""
$ GDFILoop:
$	dot = f$locate(".",ddspec)
$	if dot .eq. f$length(ddspec) then goto GDFDone
$	if f$extract(dot-1,1,ddspec) .eqs. "^"
$	then
$	    token = token + f$extract(0,dot,ddspec) + "."
$	    ddspec = f$extract(dot+1,f$length(ddspec)-(dot+1),ddspec)
$	    goto GDFILoop
$	else
$	    token = token + f$extract(0,dot,ddspec)
$	    if retval .eqs. ""
$	    then
$	        retval = token
$	    else
$	        retval = retval + "." + token
$	    endif
$	    ddspec = f$extract(dot+1,f$length(ddspec)-(dot+1),ddspec)
$	    goto GDFOLoop
$	endif
$ GDFDone:
$	if retval .eqs. ""
$	then
$	    retval = "[000000]" + f$extract(1,f$length(dd)-2,dd) + ".DIR"
$	else
$	    token = token + f$extract(0,f$length(ddspec)-1,ddspec)
$	    retval = retval + "]" + token + ".DIR"
$	endif
$	return



$!--------------------------------------------------------------
$stop_procedure:
$
$       set noon
$       if (f$type(exit_status) .eqs. "") then exit_status = 1
$       tmpfil = f$trnlnm("sys$scratch") + f$getjpi("","pid")
$       temp1 = tmpfil + ".temp"
$	if f$search(temp1) .nes. "" then -
	   delete 'temp1';*
$	if logicals_defined .eqs. "TRUE" then -
	   @sys$startup:apache$logicals "deassign" "/process" "NOINSTALL"
$	set process/parse_style='saved_parse_style'
$       if (f$type(save_message) .nes. "") then set message 'save_message'
$       if (f$type(save_privs) .nes. "") then temp1 = f$setprv(save_privs)
$       if (f$type(save_verify) .eqs. "") then exit 'exit_status'
$       exit 'exit_status' + (0 * 'f$verify(save_verify)')


$!--------------------------------------------------------------
$abort_procedure:
$       set noon
$       goto stop_procedure
