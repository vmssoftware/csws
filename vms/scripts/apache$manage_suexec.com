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
$! Module:   APACHE$MANAGE_SUEXEC.COM
$! Version:  2.0-002
$!
$! Modification History:
$!
$!	28-Jan-2003	2.0-000		Scott LePage
$!	Initial creation.
$!
$!	05-Jun-2003	2.0-001		Scott LePage
$!	Add default answer "YES" to "Continue?" prompt.
$!
$!	14-Jun-2005	2.0-002		Scott LePage
$!	Grant/Revoke addtional id (APACHE$EXECUTE) to suEXEC user.
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
$	verbose = f$trnlnm("apache$suexec_verbose")
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
$	   goto abort_procedure
$	endif
$
$!----------------------------------------------------------------------
$!
$!	Manage user accounts and access via suEXEC
$!
$!----------------------------------------------------------------------
$
$	gosub dialog_intro
$	gosub setup_logicals
$	gosub manage_suexec
$
$	goto stop_procedure


$-----------------------------------------------------------------------
$! d i a l o g _ i n t r o
$!
$!	Output introductory text and allow user to abort
$!
$dialog_intro:
$	echo ""
$	echo "      APACHE$MANAGE_SUEXEC"
$	echo ""
$	echo " This procedure allows the system manager to grant"
$	echo " users the ability to utilize the suEXEC feature of"
$	echo " the Secure Web Server.  Users will be granted/revoked"
$	echo " VMS rights identifiers to allow access."
$	echo ""
$	gosub check_suexec
$	if .not. retval then goto QNot
$ QCont:
$	inquire/nopunctuation ans "Continue [YES]? "
$	if ans .eqs. "" then ans = "YES"
$	if f$edit(f$extract(0, 1, ans), "UPCASE") .eqs. "Y" then return
$	if f$edit(f$extract(0, 1, ans), "UPCASE") .eqs. "N" then goto stop_procedure
$	goto QCont
$ QNot:
$	echo " suEXEC is currently not enabled on this system."
$	echo " To enable it, run the APACHE$CONFIG procedure."
$	echo ""
$	goto stop_procedure


$!--------------------------------------------------------------
$! s e t u p _ l o g i c a l s
$!
$!	Check the logicals and define them to run this procedure.
$!
$setup_logicals:
$	if f$trnlnm("APACHE$COMMON") .eqs. ""
$	then
$	    @sys$manager:apache$logicals "" "/process" "NOINSTALL"
$	    logicals_defined = "TRUE"
$	    if (verbose) then echo "[Defining Apache logicals in this process]"
$	endif
$	return



$!--------------------------------------------------------------
$! c h e c k _ s u e x e c
$!
$!	Checks to see if suEXEC has been enabled on this system.
$!	If the rights identifier APACHE$SUEXEC_SRVR is defined,
$!	then suEXEC has been enabled.
$!
$check_suexec:
$	set noon
$	set message 'no_message'
$	retval = "TRUE"
$	define/user sys$error nla0:
$	define/user sys$output nla0:
$	if f$trnlnm("sysuaf") .eqs. "" then -
	    define/user sysuaf sys$system:sysuaf.dat
$	uaf show/ident APACHE$SUEXEC_SRVR
$	if ('$status' .and. 1) .eq. 0 then -
	    retval = "FALSE"
$
$	return



$!--------------------------------------------------------------
$! m a n a g e _ s u e x e c
$!
$!	Main dialog to manage suEXEC
$!
$!
$manage_suexec:
$
$	set noon
$	set message 'no_message'
$	echo ""
$	echo "Enter '?' for help"
$manage_suexec_opt:
$	echo ""
$	arg1 = "Manage suEXEC user accounts (SHOW/GRANT/REVOKE/DONE/?):"
$	arg2 = ""
$	arg3 = "DONE"
$	arg4 = ""
$	gosub ask_value_string
$	retval = f$edit (retval, "trim,upcase")
$	retlen = f$length (retval)
$
$	if retval .eqs. f$extract (0,retlen, "?")
$	then
$	    gosub help_text
$	    goto manage_suexec_opt
$	endif
$
$	if retval .eqs. f$extract (0,retlen, "SHOW")
$	then
$	    gosub show_suexec
$	    goto manage_suexec_opt
$	endif
$
$	if retval .eqs. f$extract (0,retlen, "GRANT")
$	then
$	    gosub grant_suexec
$	    goto manage_suexec_opt
$	endif
$
$	if retval .eqs. f$extract (0,retlen, "REVOKE")
$	then
$	    gosub revoke_suexec
$	    goto manage_suexec_opt
$	endif
$
$	if retval .eqs. f$extract (0,retlen, "DONE")
$	then
$	    goto manage_suexec_end
$	endif
$
$	echo "Invalid option!"
$	goto manage_suexec_opt
$
$manage_suexec_end:
$	set on
$	set message 'save_message'
$	return


$!--------------------------------------------------------------
$! h e l p _ t e x t
$!
$!	Display a short help text about each option
$!
$help_text:
$	echo ""
$	echo "   SHOW    -  Displays all UAF accounts that are granted the"
$	echo "              APACHE$SUEXEC_SRVR and APACHE$SUEXEC_USER rights"
$	echo "              identifiers."
$	echo ""
$	echo "   GRANT   -  Prompts for a UAF user account and grants the"
$	echo "              APACHE$SUEXEC_USER and APACHE$EXECUTE rights"
$	echo "              identifier to that account."
$	echo ""
$	echo "   REVOKE  -  Prompts for a UAF user account and revokes the"
$	echo "              APACHE$SUEXEC_USER and APACHE$EXECUTE rights"
$	echo "              identfier from that account."
$	echo ""
$	echo "   DONE    -  Exits this procedure."
$	echo ""
$	return


$!--------------------------------------------------------------
$! s h o w _ s u e x e c
$!
$!	Display all user accounts that hold the APACHE$SUEXEC_SRVR
$!	and APACHE$SUEXEC_USER rights identifiers.
$!
$show_suexec:
$	arg1 := APACHE$SUEXEC_SRVR
$	gosub find_ident_holders
$	echo "  suEXEC server accounts:"
$	ident_holder_cnt = 1
$show_suexec_srvr:
$	if ident_holder_cnt .le. ident_holder_max
$	then
$	    echo "     ", ident_holder_'ident_holder_cnt'
$	    ident_holder_cnt = ident_holder_cnt + 1
$	    goto show_suexec_srvr
$	endif
$	arg1 := APACHE$SUEXEC_USER
$	gosub find_ident_holders
$	echo "  suEXEC user accounts:"
$	ident_holder_cnt = 1
$show_suexec_user:
$	if ident_holder_cnt .le. ident_holder_max
$	then
$	    echo "     ", ident_holder_'ident_holder_cnt'
$	    ident_holder_cnt = ident_holder_cnt + 1
$	    goto show_suexec_user
$	endif
$
$	return


$!--------------------------------------------------------------
$! g r a n t _ s u e x e c
$!
$!	Grant APACHE$SUEXEC_USER and APACHE$EXECUTE rights identifier
$!	to a speicfied user
$!
$grant_suexec:
$	arg1 = "Enter Username:"
$	arg2 = ""
$	arg3 = ""
$	arg4 = ""
$	gosub ask_value_string
$	retval = f$edit (retval,"trim,upcase")
$	if retval .nes. ""
$	then
$	    arg1 := APACHE$SUEXEC_USER
$	    arg2 = retval
$	    gosub grant_rights_ident
$	    arg1 := APACHE$EXECUTE
$	    gosub grant_rights_ident
$	endif
$
$	return


$!--------------------------------------------------------------
$! r e v o k e _ s u e x e c
$!
$!	Revoke the APACHE$SUEXEC_USER and APACHE$EXECUTE rights
$!	identifier from a specified user.
$!
$revoke_suexec:
$	arg1 = "Enter Username:"
$	arg2 = ""
$	arg3 = ""
$	arg4 = ""
$	gosub ask_value_string
$	retval = f$edit (retval,"trim,upcase")
$	if retval .nes. ""
$	then
$	    arg1 := APACHE$SUEXEC_USER
$	    arg2 = retval
$	    gosub revoke_rights_ident
$	    arg1 := APACHE$EXECUTE
$	    gosub revoke_rights_ident
$	endif
$
$	return


$!--------------------------------------------------------------
$!  f i n d _ i d e n t _ h o l d e r s
$!
$!  Find holders of a specified identifier
$!
$!  Inputs:
$!	arg1 - identifier to search for
$!
$!  Outputs:
$!	A value IDENT_HOLDER_MAX will contain the number of holders found
$!	An array IDENT_HOLDER_n will contain the user names of the holders
$!
$find_ident_holders:
$       uaf = "$authorize"
$       ident_holder_max = 0
$	if (verbose) then echo "[Searching for holders of ''arg1' identifier]"
$       ifile := sys$login:apache$user_ident.tmp
$       define/user sys$error 'ifile'
$       define/user sys$output 'ifile'
$	if f$trnlnm ("sysuaf") .eqs. "" -
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
$	delete/nolog/noconfirm 'ifile';*
$       return


$!--------------------------------------------------------------
$!  g r a n t _ r i g h t s _ i d e n t
$!
$!  Grant a rights identifier to a user
$!
$!  Inputs:
$!
$!  Outputs:
$!
$grant_rights_ident:
$	uaf = "$authorize"
$	if (verbose) then echo "[Granting Identifier ''arg1' to ''arg2' ]"
$	if f$trnlnm ("sysuaf") .eqs. "" -
	then define/user sysuaf sys$system:sysuaf.dat
$	uaf grant/identifier 'arg1' 'arg2'
$	retval = $status
$	if f$message (retval, "IDENT") .nes. "%NORMAL" .AND. -
	   f$message (retval, "IDENT") .nes. "%NOMSG" .AND. -
	   f$message (retval, "IDENT") .nes. "%DUPIDENT"
$	then
$	    echo f$message (retval)
$	else
$	    echo "%UAF-I-GRANTMSG, identifer ''arg1' granted to ''arg2'"
$	endif
$	return


$!--------------------------------------------------------------
$!  r e v o k e _ r i g h t s _ i d e n t
$!
$!  Revoke a rights identifier from a user
$!
$!  Inputs:
$!
$!  Outputs:
$!
$revoke_rights_ident:
$	uaf = "$authorize"
$	if (verbose) then echo "[Revoking Identifier ''arg1' from ''arg2' ]"
$	if f$trnlnm ("sysuaf") .eqs. "" -
	then define/user sysuaf sys$system:sysuaf.dat
$	uaf revoke/identifier 'arg1' 'arg2'
$	retval = $status
$	if f$message (retval, "IDENT") .nes. "%NORMAL" .AND. -
	   f$message (retval, "IDENT") .nes. "%NOMSG" .AND. -
	   f$message (retval, "IDENT") .nes. "%NOSUCHID"
$	then
$	    echo f$message (retval)
$	else
$	    echo "%UAF-I-REVOKEMSG, identifer ''arg1' revoked from ''arg2'"
$	endif
$	return


$!--------------------------------------------------------------
$!  a s k _ v a l u e _ s t r i n g
$!
$!  Get a string value.
$!
$!  Inputs:
$!	arg1		Prompt string
$!	arg2		Symbol name (prefix)
$!	arg3		Optional default value
$!	arg4		Optional validity checking routine
$!
$!  Outputs:
$!	retval		Return value
$!
$ask_value_string:
$ask_value_string_1:
$	if (arg3 .nes. "")
$	then temp2 = arg1 + " [" + arg3 + "] "
$	else temp2 = arg1 + " "
$	endif
$ask_value_string_2:
$	inquire/nopunctuaion temp1 "''temp2'"
$	if (temp1 .eqs. "")
$	then
$	   if (arg3 .eqs. "") then goto ask_value_string_2
$	   temp1 = arg3
$	endif
$	if (arg4 .nes. "") then gosub 'arg4'
$	if (temp1 .eqs. "") then goto ask_value_string_1
$	retval = temp1
$	return


$!
$! This is the end
$!
$stop_procedure:
$       set noon
$       if (f$type(status) .eqs. "") then status = 1
$       temp1 = tmpfil + "*.temp"
$	if logicals_defined .eqs. "TRUE" then -
	   @sys$startup:apache$logicals "deassign" "/process" "NOINSTALL"
$	set process/parse_style='saved_parse_style'
$       if (f$search(temp1) .nes. "") then delete/nolog 'temp1';*
$       if (f$type(save_message) .nes. "") then set message 'save_message'
$       if (f$type(save_privs) .nes. "") then temp1 = f$setprv(save_privs)
$       if (f$type(save_verify) .eqs. "") then exit 'status'
$       exit 'status' + (0 * 'f$verify(save_verify)')
$abort_procedure:
$       set noon
$       status = 44
$       goto stop_procedure
