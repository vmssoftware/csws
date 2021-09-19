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
$! Module:   APACHE$CONFIG.COM
$! Version:  2.0-006
$!
$! Modification History:
$!
$!	02-Dec-2002	2.0-000		Scott LePage
$!	Initial creation for T2.0 alpha kit
$!
$!	13-Jan-2003	2.0-001		Scott LePage
$!	Added APACHE$READ and APACHE$EXECUTE rights for T2.0 beta kit
$!	Added these rights to the files required.
$!
$!	04-Jun-2003	2.0-002		Scott LePage
$!	Fix problem enabling suEXEC for the 2nd time.
$!
$!	05-Jun-2003	2.0-003		Scott LePage
$!	Changed default answer = NO to Enable suEXEC question.
$!
$!	12-Jun-2003	2.0-004		Scott LePage
$!	Fix extra %DUPIDENT messages coming out at startup of procedure.
$!	(Leftover debugging messages...doh!)
$!
$!	18-Jul-2003	2.0-005		Scott LePage
$!	Fix ACE propagation on directories
$!
$!	04-Aug-2006	2.0-006		Scott LePage
$!	Add special check for FIS'd installations
$!
$
$	set :=
$	set symbol/scope=(nolocal,noglobal)
$	echo := write sys$output
$	comfil = f$environment("procedure")
$	comnam = f$parse(comfil,,,"name","syntax_only") + -
	      f$parse(comfil,,,"type","syntax_only")
$	tmpfil = f$trnlnm("sys$scratch") + f$getjpi("","pid")
$	save_message = f$environment("message")
$	no_message = "/nofacility/noseverity/noidentification/notext"
$	verbose = f$trnlnm("apache$config_verbose")
$	if (verbose .eqs. "") then verbose := FALSE
$	logicals_defined = "FALSE"
$!
$!  Enable privileges for the interactive process.
$!
$	temp1 = "SYSPRV, SYSNAM, OPER, DETACH, BYPASS, CMKRNL, ALTPRI, WORLD"
$	save_privs = f$setprv(temp1)
$	if (.not. f$privileges(temp1))
$	then
$	   echo "ERROR: Insufficient privileges to run ''comnam'"
$	   echo "   Required privileges: ", temp1
$	   goto abort_procedure
$	endif
$
$!----------------------------------------------------------------------
$!
$!   Perform the Configuration
$!
$!----------------------------------------------------------------------
$	gosub dialog_intro		! Type the introductory text
$
$	server_running = "FALSE"
$	gosub find_server		! Look for running server
$	if server_running .eqs. "TRUE"
$	then
$	   echo ""
$	   echo "ERROR: Do not configure while the servers are still running"
$	   echo "    Please shutdown any servers first, then reconfigure."
$	   goto abort_procedure
$	endif
$!
$!  To perform a complete configuration we must have a valid server account.
$!  Otherwise we don't have a UIC to use when setting the ownership of the
$!  various files and directories.
$!
$
$	if f$search("SYS$STARTUP:APACHE$LOGICALS.COM") .eqs. ""
$	then
$	    if f$search("SYS$MANAGER:APACHE$FIS.COM") .nes. ""
$	    then
$	        copy/nolog SYS$MANAGER:APACHE$FIS.COM SYS$COMMON:[SYSMGR]APACHE$LOGICALS.COM
$	    else
$	        echo "ERROR: Could not establish installation directory"
$	        echo "       Looked for APACHE$LOGICALS.COM - not found"
$	        goto abort_procedure
$	    endif
$	endif
$	@sys$startup:apache$logicals "" "/process" "NOINSTALL"
$	logicals_defined = "TRUE"
$
$	gosub define_rights		! Create rights identifiers
$	arg1 := false			! Do not fail if account is missing
$	gosub check_server_account	! Check to see if server account exists
$	if (.not. retval) then gosub create_server_account
$	if (.not. retval) then goto abort_procedure
$!
$!
$	gosub dialog_main
$	if (cnf_set_owner_uic) then gosub set_owner_uic
$       gosub config_suexec
$       if cnf_suexec then @apache$common:[000000]apache$manage_suexec
$
$	@sys$manager:apache$logicals "" "" "INSTALL"
$	gosub create_config_data
$	temp1 = ""
$	echo  "Configuration is complete.  To start the server:"
$	echo  ""
$	echo  "    $ @SYS$STARTUP:APACHE$STARTUP.COM", temp1
$	echo  ""
$	goto  stop_procedure
$
$!----------------------------------------------------------------------
$!
$!  c h e c k _ s e r v e r _ a c c o u n t
$!
$!  Check to see if the default server account is missing.
$!  Return TRUE if found or FALSE if not.
$!  If not found and ARG1 is TRUE, then the error is fatal.
$!
$check_server_account:
$	cnf_user := apache$www
$	cnf_uic = f$identifier(cnf_user,"name_to_number")
$	if (cnf_uic .ne. 0)
$	then
$	   gosub grant_account_rights
$	   if (verbose) then echo f$fao("[Server account !AS !%I ]", -
	         cnf_user, cnf_uic)
$	   retval := true
$	   return
$	endif
$	retval := false
$	if (.not. arg1) then return
$	echo "ERROR: Required OpenVMS ''cnf_user' account does not exist"
$	echo "   Please reconfigure (''comnam') or create the account manually."
$	goto abort_procedure
$
$!----------------------------------------------------------------------
$!
$!  c r e a t e _ s e r v e r _ a c c o u n t
$!
$!  Create the server account (if necessary).
$!
$create_server_account:
$	cnf_user := apache$www
$	cnf_uic = f$identifier(cnf_user,"name_to_number")
$	if (cnf_uic .ne. 0)
$	then
$	   gosub grant_account_rights
$	   if (verbose) then echo f$fao("[Server account (!AS) UIC !%I ]", -
	         cnf_user, cnf_uic)
$	   retval := true
$	   return
$	endif
$	temp2 := APACHE$COMMON:[000000]APACHE$ADDUSER.COM
$	if (f$search(temp2) .eqs. "")
$	then
$	   echo "ERROR: Required OpenVMS ''cnf_user' account does not exist"
$	   echo "ERROR: Missing required command procedure: ", temp2
$	   goto abort_procedure
$	endif
$	echo ""
$	echo "[Creating OpenVMS username ""''cnf_user'"" ]"
$	echo "[Starting ''temp2' ]"
$	echo ""
$       read sys$command unused /prompt="Press enter to continue... "
$	echo ""
$	type sys$input
$	deck

PLEASE NOTE:

You will be prompted for the following information:

    Full name for APACHE$WWW: ! Full name of site server administrator/owner.

    Password:  ! Password for the APACHE$WWW account

    UIC Group Number:	? ! Question mark will display a list of all
			  ! UIC groups currently in use. Quite useful.
			  ! Please pick a group separate from all other
			  ! usernames.
			  ! Servers are usually given the first unused group,
			  ! starting at [377,*] and working down.  DO _not_
			  ! go below SYSGEN parameter MAXSYSGROUP.

    UIC Member Number:	1 ! Question mark will display a list of all
			  ! UIC members currently in use in that group.
			  ! %UAF-W-BADSPC, no user matches specification
			  ! means the group is empty.

$	eod
$	define/user apache$root nla:
$	@'temp2'
$	cnf_uic = f$identifier(cnf_user,"name_to_number")
$	if (cnf_uic .eq. 0)
$	then
$	   echo "ERROR: Required OpenVMS ''cnf_user' username does not exist"
$	   echo "We recommend creating the account manually, ",-
	         "and then rerunning ", comnam
$	   goto abort_procedure
$	endif
$	type sys$input
$	deck

PLEASE NOTE:

The APACHE$WWW account was created with the minimum SYSUAF quotas to
run the server.  On almost all systems, the server should start but
these parameters will need to be increased to improve performance or
to keep up with increased demands.

See Release notes for details.

$	eod
$	retval := true
$	return
$
$!----------------------------------------------------------------------
$!
$!   d e f i n e _ r i g h t s
$!
$!   Rights identifiers for the privileged image
$!	APACHE$APR_ALL - Allows user to call any of the protected routines
$!	APACHE$APR_CREMBX - Allows user to create a mailbox with groupwide name
$!	APACHE$APR_GETPWNAM -
$!	APACHE$APR_SETSOCKOPT - Allows user to set socket options
$!	APACHE$APR_SOCKET - Allows user to create a privleged socket (<1024)
$!	APACHE$APR_AUTH_OPENVMS - Allows user to access VMS UAF for authentication
$!	APACHE$APR_GALAXY_GBLSEC - Allows user to create and manage galactic sections
$!	APACHE$READ - Allows user to read Apache files
$!	APACHE$EXECUTE - Allows user to execute .EXE and .COM files
$!
$define_rights:
$	set noon
$	set message 'no_message'
$	arg1 = "APACHE$APR_ALL"
$	gosub create_rights_ident
$	if ('retval' .and. 1) .eq. 0 then return
$
$	arg1 = "APACHE$APR_CREMBX"
$	gosub create_rights_ident
$	if ('retval' .and. 1) .eq. 0 then return
$
$	arg1 = "APACHE$APR_GETPWNAM"
$	gosub create_rights_ident
$	if ('retval' .and. 1) .eq. 0 then return
$
$	arg1 = "APACHE$APR_SETSOCKOPT"
$	gosub create_rights_ident
$	if ('retval' .and. 1) .eq. 0 then return
$
$	arg1 = "APACHE$APR_SOCKET"
$	gosub create_rights_ident
$	if ('retval' .and. 1) .eq. 0 then return
$
$	arg1 = "APACHE$APR_AUTH_OPENVMS"
$	gosub create_rights_ident
$	if ('retval' .and. 1) .eq. 0 then return
$
$	arg1 = "APACHE$APR_GALAXY_GBLSEC"
$	gosub create_rights_ident
$	if ('retval' .and. 1) .eq. 0 then return
$
$	arg1 = "APACHE$READ"
$	gosub create_rights_ident
$	if ('retval' .and. 1) .eq. 0 then return
$
$	arg1 = "APACHE$EXECUTE"
$	gosub create_rights_ident
$	if ('retval' .and. 1) .eq. 0 then return
$
$!	arg1 = "APACHE$SUEXEC_USER"
$!	gosub create_rights_ident
$!	if ('retval' .and. 1) .eq. 0 then return
$
$!	arg1 = "APACHE$SUEXEC_SRVR"
$!	gosub create_rights_ident
$
$	set message 'save_message'
$	set on
$	return
$
$!----------------------------------------------------------------------
$!
$!  c r e a t e _ r i g h t s _ i d e n t
$!
$!  Create a rights identifier.  Ignores any duplicates.
$!
$!  Inputs:
$!	arg1 - Name of right to create
$!
$!  Outputs:
$!	retval - returned status of creation
$!
$create_rights_ident:
$	uaf = "$authorize"
$	if (verbose) then echo "[Creating Identifier ''arg1' ]"
$	if f$trnlnm ("sysuaf") .eqs. "" -
	then define/user sysuaf sys$system:sysuaf.dat
$	uaf add/identifier 'arg1'
$	retval = $status
$	if f$message (retval, "IDENT") .nes. "%NORMAL" .AND. -
	   f$message (retval, "IDENT") .nes. "%NOMSG" .AND. -
	   f$message (retval, "IDENT") .nes. "%DUPLNAM"
$	then
$	    echo f$message (retval)
$	else
$	    retval = "%X00000001"
$	endif
$	return
$

$!------------------------------------------------------------
$!
$!  r e m o v e _ r i g h t s _ i d e n t
$!
$!  Remove a rights identifier
$!
$!  Inputs:
$!
$!  Outputs:
$!
$remove_rights_ident:
$       uaf = "$authorize"
$       if (verbose) then echo "[Removing Identifier ''arg1' ]"
$       if f$trnlnm ("sysuaf") .eqs. "" -
        then define/user sysuaf sys$system:sysuaf.dat
$       uaf remove/identifier 'arg1'
$       retval = $status
$       if f$message (retval, "IDENT") .nes. "%NORMAL" .AND. -
           f$message (retval, "IDENT") .nes. "%NOMSG" .AND. -
           f$message (retval, "IDENT") .nes. "%NOSUCHID"
$       then
$           echo f$message (retval)
$	else
$	    retval = "%X00000001"
$       endif
$       return



$!------------------------------------------------------------
$! g r a n t _ a c c o u n t _ r i g h t s
$!
$!	This routine guards against an existing account (APACHE$WWW)
$!	that has no rights identifier used for CSWS V2.0.  This would
$!	be the upgrade case.  If the account does not have the right,
$!	it is granted here.
$!
$grant_account_rights:
$	set noon
$	set message 'no_message'
$
$	cnf_user = "apache$www"
$	cnf_apr_right = "APACHE$APR_ALL"
$	cnf_read_right = "APACHE$READ"
$	cnf_exec_right = "APACHE$EXECUTE"
$
$	arg2 = cnf_user
$	arg1 = cnf_apr_right
$	gosub grant_rights_ident
$	if ('retval' .and. 1) .eq. 0 then goto gar_ret
$	arg1 = cnf_read_right
$	gosub grant_rights_ident
$	if ('retval' .and. 1) .eq. 0 then goto gar_ret
$	arg1 = cnf_exec_right
$	gosub grant_rights_ident
$
$ gar_ret:
$	set on
$	set message 'save_message'
$	return


$!--------------------------------------------------------------
$!
$!  g r a n t _ r i g h t s _ i d e n t
$!
$!  Grant a rights identifier to a user
$!
$!  Inputs:
$!	arg1 = right to be granted
$!	arg2 = username to receive right
$!
$!  Outputs:
$!	retval = status if a non-expected error occurs
$!
$grant_rights_ident:
$       uaf = "$authorize"
$       if (verbose) then echo "[Granting Identifier ''arg1' to ''arg2' ]"
$       if f$trnlnm ("sysuaf") .eqs. "" -
        then define/user sysuaf sys$system:sysuaf.dat
$       uaf grant/identifier 'arg1' 'arg2'
$       retval = $status
$       if f$message (retval, "IDENT") .nes. "%NORMAL" .AND. -
           f$message (retval, "IDENT") .nes. "%NOMSG" .AND. -
           f$message (retval, "IDENT") .nes. "%DUPIDENT"
$       then
$           echo f$message (retval)
$	else
$	    retval = "%X00000001"
$       endif
$
$       return


$!------------------------------------------------------------
$!
$!  r e v o k e _ r i g h t s _ i d e n t
$!
$!  Revoke a rights identifier from a user
$!
$!  Inputs:
$!
$!  Outputs:
$!
$revoke_rights_ident:
$       uaf = "$authorize"
$       if (verbose) then echo "[Revoking Identifier ''arg1' from ''arg2' ]"
$       if f$trnlnm ("sysuaf") .eqs. "" -
        then define/user sysuaf sys$system:sysuaf.dat
$       uaf revoke/identifier 'arg1' 'arg2'
$       retval = $status
$       if f$message (retval, "IDENT") .nes. "%NORMAL" .AND. -
           f$message (retval, "IDENT") .nes. "%NOMSG" .AND. -
           f$message (retval, "IDENT") .nes. "%NOSUCHID"
$       then
$           echo f$message (retval)
$	else
$	    retval = "%X00000001"
$       endif
$       return



$!------------------------------------------------------------
$!
$! d i a l o g _ i n t r o
$!
$!
$dialog_intro:
$	type sys$input
$	deck

                Secure Web Server for OpenVMS
                                      [based on Apache]

        This procedure helps you define the operating environment
        required to run the Secure Web Server on this system.
$	eod
$	return


$!------------------------------------------------------------
$!
$! d i a l o g _ m a i n
$!
$!
$dialog_main:
$!
$!  CNF_SET_OWNER_UIC: boolean indicating whether to set the owner UIC on
$!  all of the Apache installed files.
$!
$!  NOTE: This value is not set by the configuration file because the default
$!  value is fixed.
$!
$	type sys$input
$	deck

To operate successfully, the server processes must have read access
to the installed files and read-write access to certain other files
and directories.  It is recommended that you use this procedure to
set the owner UIC on the CSWS files and directories to match the server.
You should do this each time the product is installed, but it only has
to be done once for each installation on a cluster.

$	eod
$	arg1 = "Set owner UIC on CSWS files?" ! Prompt string
$	arg2 = ""			! Symbol name (prefix)
$	arg3 = "YES"			! Default is YES
$	gosub ask_value_boolean
$	cnf_set_owner_uic = retval
$	echo ""
$
$
$!
$!
$!  CNF_SUEXEC: boolean indicating whether SUEXEC is enabled or disabled
$!
$       type sys$input
$       deck

Do you want to enable the impersonation features provided by suEXEC?
If so, the server will support running CGIs using specified usernames.

$       eod
$       arg1 = "Enable suEXEC?"
$       arg2 = ""
$       arg3 = "NO"
$       gosub ask_value_boolean
$       cnf_suexec = retval
$
$	return


$!-----------------------------------------------------------
$!
$!  c r e a t e _ c o n f i g _ d a t a
$!
$!	This checks for the existance of CONFIG.DAT
$!	If found looks for the basic Apache instance "SWS".
$!	Adds it if it needs to.  Creates a new file if it
$!	needs to.
$!
$create_config_data:
$	front = "APACHE$COMMON:[000000]APACHE$CONFIG"
$	file = front + ".DAT"
$	tmpfile = front + ".TMP"
$	bakfile = front + ".BAK"
$	inst = "SWS"
$	conf = "APACHE$COMMON:[CONF]HTTPD.CONF"
$
$	if f$search("''file'") .eqs. ""
$	then
$	    if (verbose) then echo "[Creating new configuration data ''file']"
$	    open/write apdat 'file'
$	    write apdat "#"
$	    write apdat "# APACHE$CONFIG.DAT"
$	    write apdat "#"
$	    write apdat "#	Created by APACHE$CONFIG.COM on ''f$time()'"
$	    write apdat "#"
$	    write apdat "#	Defines the instances of Apache on this system"
$	    write apdat "#"
$	    write apdat ""
$	    write apdat "''inst'     ''conf'"
$	    close apdat
$	else
$	    open/read apdat 'file'
$ RDatLoop:
$	    read/end=RDatDone apdat recd
$	    recd = f$edit(recd,"TRIM,COMPRESS")
$	    if f$length(recd) .eq. 0 then goto RDatLoop
$	    if f$extract(0,1,recd) .eqs. "#" then goto RDatLoop
$	    if f$extract(0,1,recd) .eqs. "!" then goto RDatLoop
$	    tag = f$element(0, " ", recd)
$	    cfg = f$edit(f$element(1, " ", recd),"UPCASE")
$	    if tag .eqs. inst .and. -
	       cfg .eqs. conf then goto RDatRet
$	    goto RDatLoop
$ RDatDone:
$	    close apdat
$
$	    open/read apdat 'file'
$	    open/write newdat 'tmpfile'
$ UDatLoop:
$	    read/end=UDatDone apdat recd
$	    temp1 = f$edit(recd,"TRIM,COMPRESS")
$	    tag = f$element(0," ",temp1)
$	    if tag .eqs. inst
$	    then
$	        write newdat "!''recd'"
$	    else
$	        write newdat "''recd'"
$	    endif
$	    goto UDatLoop
$ UDatDone:
$	    write newdat "''inst'     ''conf'"
$	    close apdat
$	    close newdat
$	    rename 'file' 'bakfile'
$	    rename 'tmpfile' 'file'
$	    echo ""
$	    echo "*** WARNING ***"
$	    echo "   ''file' was updated"
$	    echo "   Previous version saved in ''bakfile'"
$	    echo ""
$	endif
$
$	return
$
$ RDatRet:
$	close apdat
$	return

$!-----------------------------------------------------------
$!
$!  s e t _ o w n e r _ u i c
$!
$!	Set the owner UIC on all disk files.
$!
$!	NOTE: In the original APACHE_DAEMON.COM we copy this command procedure
$!	because we cannot set the owner UIC on the file that is currently in
$!	use.  We then purge back the old copy.  Hopefully we can avoid this
$!	because this procedure should be residing in SYS$MANAGER.
$!
$set_owner_uic:
$	echo "Setting ownership on files.", -
	      "  This could take a minute or two.  . . . "
$	set noon
$	set message 'no_message'
$	arg1 = f$trnlnm("APACHE$COMMON") - ".]" + "]"
$	topdir = arg1
$	gosub convert_path_file
$	topdirfile = retval
$	set file /owner_uic=['cnf_user']/protection=(o:rwed)/nolog 'retval'
$	set file /owner_uic=['cnf_user']/protection=(o:rwed)/nolog apache$common:[000000...]*.*;* -
	    /exclude=[htdocs.suexec...]*.*;*
$!
$	set security/acl=(IDENT=APACHE$READ,OPTIONS=NOPROPAGATE,ACCESS=READ) 'topdirfile'
$	set security/acl=(IDENT=APACHE$READ,ACCESS=READ) 'topdirfile'
$	set security/like=(object_type=file,object_name='topdirfile') apache$common:[000000...]*.dir-
            /exclude=[htdocs.suexec...]*.*;*
$	set security/acl=(IDENT=APACHE$EXECUTE,ACCESS=READ+EXECUTE) apache$common:[000000...]*.exe-
            /exclude=[htdocs.suexec...]*.*;*
$	set security/acl=(IDENT=APACHE$EXECUTE,ACCESS=READ+EXECUTE) apache$common:[*...]*.com-
            /exclude=([htdocs.suexec...]*.*;*,[000000]apache$create_root.com,[000000]apache$menu.com)
$	set security/acl=(IDENT=APACHE$EXECUTE,ACCESS=READ+EXECUTE) apache$common:[000000]apache$cert_tool.com
$	set security/acl=(IDENT=APACHE$EXECUTE,ACCESS=READ+EXECUTE) apache$common:[000000]apache$dcl.com
$	set security/acl=(IDENT=APACHE$EXECUTE,ACCESS=READ+EXECUTE) apache$common:[000000]apache$setup.com
$	set security/acl=(IDENT=APACHE$READ,ACCESS=READ) apache$common:[conf...]*.*;*
$	set security/acl=(IDENT=APACHE$READ,ACCESS=READ) apache$common:[htdocs...]*.*;*-
            /exclude=[htdocs.suexec...]*.*;*
$	set security/acl=(IDENT=APACHE$READ,ACCESS=READ) apache$common:[icons...]*.*;*
$	set message 'save_message'
$	set on
$	echo ""
$	return


$!----------------------------------------------------------------------
$!
$! c o n f i g _ s u e x e c
$!
$!	Configure suEXEC to run (or not run) on this installation.
$!
$! Inputs:
$!	cnf_suexec - Boolean to enable/disable suEXEC.
$!
$! Outputs:
$!
$config_suexec:
$       set noon
$       set message 'no_message'
$       cnf_user := APACHE$WWW
$	_cnf_path_common = f$trnlnm("APACHE$COMMON") - ".]"
$!       _cnf_path_common = f$extract(0,f$length(cnf_path_common)-1,cnf_path_common)
$	cnf_path_common = _cnf_path_common + "]"
$       if .not. cnf_suexec
$       then
$           echo "Disabling suEXEC configuration.", -
                 "  This could take a minute or two.  . . . "
$           if (verbose) then echo "[Removing APACHE$SUEXEC_USER security ACEs ]"
$           arg1 = cnf_path_common
$           gosub convert_path_file
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,OPTIONS=NOPROPAGATE,ACCESS=E)/delete -
                'retval'
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                'cnf_path_common'APACHE$DCL.COM
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                'cnf_path_common'APACHE$DCL_BIN.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                'cnf_path_common'APACHE$DCL_ENV.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                'cnf_path_common'APACHE$DCL_RUN.EXE
$!           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
$!                'cnf_path_common'APACHE$FIXBG.EXE_'arch'
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                'cnf_path_common'APACHE$SET_CCL.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                'cnf_path_common'APACHE$HTTPD_SHR.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                'cnf_path_common'APACHE$APU_SHR.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                'cnf_path_common'APACHE$APR_SHR.EXE
$	    if f$search("''cnf_path_common'APACHE$APR_SHRP.EXE") .nes. "" Then -
                set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                    'cnf_path_common'APACHE$APR_SHRP.EXE
$	    if f$search("''cnf_path_common'APACHE$APR_SHRP_V7.EXE") .nes. "" Then -
                set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                    'cnf_path_common'APACHE$APR_SHRP_V7.EXE
$	    if f$search("''cnf_path_common'APACHE$APR_SHRP_V8.EXE") .nes. "" Then -
                set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                    'cnf_path_common'APACHE$APR_SHRP_V8.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,OPTIONS=NOPROPAGATE,ACCESS=R+E)/delete -
                'cnf_path_common'HTDOCS.DIR
$
$
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                '_cnf_path_common'.BIN]HTPASSWD.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                '_cnf_path_common'.BIN]AB.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                '_cnf_path_common'.BIN]HTDBM.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                '_cnf_path_common'.BIN]HTDIGEST.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                '_cnf_path_common'.BIN]LOGRESOLVE.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                '_cnf_path_common'.BIN]ROTATELOGS.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E)/delete -
                '_cnf_path_common'.BIN]SUEXEC.EXE
$           if f$search ("''_cnf_path_common'.HTDOCS]SUEXEC.DIR") .nes. ""
$           then
$               set security/acl=(IDENTIFIER=APACHE$SUEXEC_SRVR,ACCESS=R+E)/delete -
                    '_cnf_path_common'.HTDOCS]SUEXEC.DIR
$               set security/acl=(IDENTIFIER=APACHE$SUEXEC_SRVR,OPTIONS=DEFAULT,ACCESS=R+E)/delete -
                    '_cnf_path_common'.HTDOCS]SUEXEC.DIR
$               set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,OPTIONS=NOPROPAGATE,ACCESS=R+E)/delete -
                    '_cnf_path_common'.HTDOCS]SUEXEC.DIR
$               if f$search ("''_cnf_path_common'.HTDOCS.SUEXEC...]*.*") .nes. ""
$               then
$                   set security/acl=(IDENTIFIER=APACHE$SUEXEC_SRVR,OPTIONS=DEFAULT,ACCESS=R+E)/delete -
                        '_cnf_path_common'.HTDOCS.SUEXEC...]*.DIR;*
$                   set security/acl=(IDENTIFIER=APACHE$SUEXEC_SRVR,ACCESS=R+E)/delete -
                        '_cnf_path_common'.HTDOCS.SUEXEC...]*.*;*
$               endif
$           endif
$           arg1 := APACHE$SUEXEC_SRVR
$           arg2 = cnf_user
$           gosub revoke_rights_ident
$           arg1 := APACHE$SUEXEC_SRVR
$           gosub find_ident_holders
$           if ident_holder_max .gt. 0
$           then
$               arg1 = "Disable all suEXEC server accounts?"
$               arg2 = ""
$               arg3 = "NO"
$               arg4 = ""
$               gosub ask_value_boolean
$               if retval
$               then
$                   ident_holder_cnt = 1
$revoke_suexec_srvr:
$                   if ident_holder_cnt .le. ident_holder_max
$                   then
$                       arg1 := APACHE$SUEXEC_SRVR
$                       arg2 = ident_holder_'ident_holder_cnt'
$                       gosub revoke_rights_ident
$                       ident_holder_cnt = ident_holder_cnt + 1
$                       goto revoke_suexec_srvr
$                   endif
$                   arg1 := APACHE$SUEXEC_SRVR
$                   gosub remove_rights_ident
$               endif
$           else
$               arg1 := APACHE$SUEXEC_SRVR
$               gosub remove_rights_ident
$           endif
$           arg1 := APACHE$SUEXEC_USER
$           gosub find_ident_holders
$           if ident_holder_max .gt. 0
$           then
$               arg1 = "Disable all suEXEC user accounts?"
$               arg2 = ""
$               arg3 = "NO"
$               arg4 = ""
$               gosub ask_value_boolean
$               if retval
$               then
$                   ident_holder_cnt = 1
$revoke_suexec_user:
$                   if ident_holder_cnt .le. ident_holder_max
$                   then
$                       arg1 := APACHE$SUEXEC_USER
$                       arg2 = ident_holder_'ident_holder_cnt'
$                       gosub revoke_rights_ident
$                       ident_holder_cnt = ident_holder_cnt + 1
$                       goto revoke_suexec_user
$                   endif
$                   arg1 := APACHE$SUEXEC_USER
$                   gosub remove_rights_ident
$               endif
$           else
$               arg1 := APACHE$SUEXEC_USER
$               gosub remove_rights_ident
$           endif
$       else
$           echo "Enabling suEXEC configuration.", -
                 "  This could take a minute or two.  . . . "
$           arg1 := APACHE$SUEXEC_SRVR
$           gosub create_rights_ident
$           arg1 := APACHE$SUEXEC_USER
$           gosub create_rights_ident
$           arg1 := APACHE$SUEXEC_SRVR
$           arg2 = cnf_user
$           gosub grant_rights_ident
$           if (verbose) then echo "[Adding APACHE$SUEXEC_USER security ACEs ]"
$           arg1 = cnf_path_common
$           gosub convert_path_file
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,OPTIONS=NOPROPAGATE,ACCESS=E) -
                'retval'
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                'cnf_path_common'APACHE$DCL.COM
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                'cnf_path_common'APACHE$DCL_BIN.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                'cnf_path_common'APACHE$DCL_ENV.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                'cnf_path_common'APACHE$DCL_RUN.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                'cnf_path_common'APACHE$SET_CCL.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                'cnf_path_common'APACHE$HTTPD_SHR.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                'cnf_path_common'APACHE$APU_SHR.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                'cnf_path_common'APACHE$APR_SHR.EXE
$	    if f$search("''cnf_path_common'APACHE$APR_SHRP.EXE") .nes. "" Then -
                set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                    'cnf_path_common'APACHE$APR_SHRP.EXE
$	    if f$search("''cnf_path_common'APACHE$APR_SHRP_V7.EXE") .nes. "" Then -
                set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                    'cnf_path_common'APACHE$APR_SHRP.EXE
$	    if f$search("''cnf_path_common'APACHE$APR_SHRP_V8.EXE") .nes. "" Then -
                set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                    'cnf_path_common'APACHE$APR_SHRP.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,OPTIONS=NOPROPAGATE,ACCESS=R+E) -
                'cnf_path_common'HTDOCS.DIR
$
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                '_cnf_path_common'.BIN]HTPASSWD.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                '_cnf_path_common'.BIN]AB.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                '_cnf_path_common'.BIN]HTDBM.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                '_cnf_path_common'.BIN]HTDIGEST.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                '_cnf_path_common'.BIN]LOGRESOLVE.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                '_cnf_path_common'.BIN]PCREGREP.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                '_cnf_path_common'.BIN]PGREP.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                '_cnf_path_common'.BIN]ROTATELOGS.EXE
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,ACCESS=R+E) -
                '_cnf_path_common'.BIN]SUEXEC.EXE
$
$           if f$search ("''_cnf_path_common'.HTDOCS]SUEXEC.DIR") .eqs. ""
$           then
$               create/directory/nolog '_cnf_path_common'.HTDOCS.SUEXEC]
$           endif
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_SRVR,ACCESS=R+E) -
                '_cnf_path_common'.HTDOCS]SUEXEC.DIR
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_SRVR,OPTIONS=DEFAULT,ACCESS=R+E) -
                '_cnf_path_common'.HTDOCS]SUEXEC.DIR
$           set security/acl=(IDENTIFIER=APACHE$SUEXEC_USER,OPTIONS=NOPROPAGATE,ACCESS=R+E) -
                '_cnf_path_common'.HTDOCS]SUEXEC.DIR
$           if f$search ("''_cnf_path_common'.HTDOCS.SUEXEC...]*.*") .nes. ""
$           then
$               set security/acl=(IDENTIFIER=APACHE$SUEXEC_SRVR,OPTIONS=DEFAULT,ACCESS=R+E) -
                    '_cnf_path_common'.HTDOCS.SUEXEC...]*.DIR;*
$               set security/acl=(IDENTIFIER=APACHE$SUEXEC_SRVR,ACCESS=R+E) -
                    '_cnf_path_common'.HTDOCS.SUEXEC...]*.*;*
$           endif
$       endif
$       set on
$       set message 'save_message'
$       return


$!----------------------------------------------------------------------
$!
$!  f i n d _ i d e n t _ h o l d e r s
$!
$!  Find holders of a specified identifier
$!
$!  Inputs:
$!
$!  Outputs:
$!      A value IDENT_HOLDER_MAX will contain the number of holders found
$!      An array IDENT_HOLDER_n will contain the user names of the holders
$!
$find_ident_holders:
$       uaf = "$authorize"
$       ident_holder_max = 0
$       ifile := sys$login:apache$user_ident.tmp
$       define/user sys$error 'ifile'
$       define/user sys$output 'ifile'
$       if f$trnlnm ("sysuaf") .eqs. "" -
        then define/user sysuaf sys$system:sysuaf.dat
$       uaf show/rights/user=*
$       retval = $status
$       if f$message (retval, "IDENT") .nes. "%NORMAL" .AND. -
           f$message (retval, "IDENT") .nes. "%NOMSG"
$       then
$           echo f$message (retval)
$       endif
$       open /read ifile 'ifile'
$ident_loop:
$       read /end=ident_done ifile irec
$       irec = f$edit (irec,"trim,upcase")
$       if f$extract (0, 19, irec) .eqs. "IDENTIFIERS HELD BY"
$       then
$           iuser = f$element (3," ",irec)
$           goto ident_loop
$       endif
$       ident = f$element (0," ",irec)
$       if ident .eqs. arg1
$       then
$           ident_holder_max = ident_holder_max + 1
$           ident_holder_'ident_holder_max' = iuser
$       endif
$       goto ident_loop
$ident_done:
$       close ifile
$       delete/nolog/noconfirm 'ifile';*
$       return


$!----------------------------------------------------------------------
$!
$! f i n d _ s e r v e r
$!
$!	Locate a running server.
$!
$! Inputs:
$!
$! Outputs:
$!	server_running - boolean
$!
$find_server:
$	server_running = "FALSE"
$	outfil = tmpfil + ".temp"
$	show system/output='outfil'/process=APACHE$*
$	open/read plist 'outfil'
$	read/end=SRDone plist recd
$	read/end=SRDone plist recd
$ SRLoop:
$	read/end=SRDone plist recd
$	recd = f$edit(recd, "trim,compress")
$	pid = f$element(0, " ", recd)
$	image = f$getjpi(pid, "imagname")
$	if f$locate("APACHE$HTTPD.EXE", image) .ne f$length(image)
$	then
$	   server_running = "TRUE"
$	   goto SRDone
$	endif
$	goto SRLoop
$ SRDone:
$	close plist
$	delete 'outfil';*
$	return
$
$!--------------------------------------------------------------
$!
$!  a s k _ v a l u e _ b o o l e a n
$!
$!	Get a boolean value.
$!
$! Inputs:
$!      arg1            Prompt string
$!      arg2            Symbol name (prefix)
$!      arg3            Default (if any)
$!
$! Outputs:
$!      retval          Return value
$!
$ask_value_boolean:
$       arg4 = "ask_value_boolean_check" ! Validation routine
$       goto ask_value_string
$ask_value_boolean_check:
$       if (temp1 .eqs. "Y") then temp1 = "YES"
$       if (temp1 .eqs. "N") then temp1 = "NO"
$       if (temp1 .eqs. "YES") .or. (temp1 .eqs. "NO") then return
$       echo "ERROR: Invalid value """, temp1, """: Must be YES or NO"
$       temp1 = ""
$       goto ask_value_string
$
$!--------------------------------------------------------------
$!
$!  a s k _ v a l u e _ s t r i n g
$!
$!	Get a string value.
$!
$! Inputs:
$!      arg1            Prompt string
$!      arg2            Symbol name (prefix)
$!      arg3            Optional default value
$!      arg4            Optional validity checking routine
$!
$! Outputs:
$!      retval          Return value
$!
$ask_value_string:
$ask_value_string_1:
$       if (arg3 .nes. "")
$       then temp2 = arg1 + " [" + arg3 + "] "
$       else temp2 = arg1 + " "
$       endif
$ask_value_string_2:
$       inquire/nopunctuation temp1 "''temp2'"
$       if (temp1 .eqs. "")
$       then
$          if (arg3 .eqs. "") then goto ask_value_string_2
$          temp1 = arg3
$       endif
$       if (arg4 .nes. "") then gosub 'arg4'
$       if (temp1 .eqs. "") then goto ask_value_string_1
$       retval = temp1
$       return


$!--------------------------------------------------------------
$!
$!  c o n v e r t _ p a t h _ f i l e
$!
$!	Given a path specification of the form "DEVICE:[DIR]", return with the
$!	file specification of the last directory.
$!
$!	For example, the path specification "SYS$COMMON:[APACHE]" gets converted
$!	to "SYS$COMMON:[000000]APACHE.DIR".
$!
$!	TODO: Actually, we should report a fatal INTERNAL ERROR and abort if we
$!	cannot parse the path specification.
$!
$convert_path_file:
$       temp3 = f$parse(arg1,,,"directory","syntax_only")
$       if (temp3 .eqs. "")
$       then
$          retval = ""
$          return
$       endif
$       temp3 = temp3 - "][" - "]<" - ">[" - "><" - ".000000"
$       temp3 = f$extract(1,f$length(temp3)-2,temp3)
$       if (f$extract(f$length(temp3)-1,1,temp3) .eqs. ".") then -
              temp3 = f$extract(0,f$length(temp3)-1,temp3)
$!
$!
$       temp2 = ""
$convert_path_file_1:
$       if (f$locate(".",temp3) .lt. f$length(temp3))
$       then
$          temp1 = f$element(0,".",temp3)
$          temp2 = temp2 + "." + temp1
$          temp3 = temp3 - temp1 - "."
$          goto convert_path_file_1
$       endif
$       temp2 = temp2 - "."
$       if (temp2 .eqs. "") then temp2 = "000000"
$       retval = f$parse(arg1,,,"device","syntax_only") + -
              "[''temp2']''temp3'.DIR"
$       return


$!
$! This is the end
$!
$stop_procedure:
$       set noon
$       if (f$type(status) .eqs. "") then status = 1
$       tmpfil = f$trnlnm("sys$scratch") + f$getjpi("","pid")
$       temp1 = tmpfil + "*.temp"
$	if logicals_defined .eqs. "TRUE" then -
	   @sys$startup:apache$logicals "deassign" "/process" "NOINSTALL"
$       if (f$search(temp1) .nes. "") then delete/nolog 'temp1';*
$       if (f$type(save_message) .nes. "") then set message 'save_message'
$       if (f$type(save_privs) .nes. "") then temp1 = f$setprv(save_privs)
$       if (f$type(save_verify) .eqs. "") then exit 'status'
$       exit 'status' + (0 * 'f$verify(save_verify)')
$abort_procedure:
$       set noon
$       status = 44
$       goto stop_procedure
