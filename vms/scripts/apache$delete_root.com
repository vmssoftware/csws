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
$! Version:  2.0-000
$!
$! Modification History:
$!
$!	25-Apr-2003	2.0-000		Scott LePage
$!	Initial creation.
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
$	verbose = f$trnlnm("apache$delete_verbose")
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
$!	Delete existing root for the Secure Web Server
$!
$!----------------------------------------------------------------------
$
$	gosub dialog_intro
$
$	gosub setup_logicals
$	gosub dialog_main
$	if exit_status .eq. %x1000002c then goto stop_procedure
$	gosub read_config
$	gosub revoke_user_rights
$	gosub delete_root
$	gosub update_db
$
$	echo "Root deleted: ''dr_root'"
$	echo ""
$	exit_status = 1
$	goto stop_procedure
$
$-----------------------------------------------------------------------
$ d i a l o g _ i n t r o
$
$	Output introductory text and allow user to abort
$
$dialog_intro:
$	echo ""
$	echo "      APACHE$DELETE_ROOT"
$	echo ""
$	echo " Deletes a previously defined set of directories and"
$	echo " all files contained therein.  Also revokes all user"
$	echo " rights granted when the root was created."
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
$!	Show user roots that can be deleted and ask which one.
$!
$! Inputs:
$!	none
$!
$! Outputs:
$!	dr_id - VmsServerTag of instance to be deleted
$!	dr_conf - HTTPD configuration file of instance
$!	exit_status - %x00000001 => selection made and loaded
$!	              %x1000002C => no selection made (ABORT)
$!
$dialog_main:
$	i = 0
$	open/read cfg_dat apache$common:[000000]apache$config.dat
$ RLoop:
$	read/end=RDone cfg_dat recd
$	if f$length(recd) .eq. 0 then goto RLoop
$	recd = f$edit(recd,"TRIM,COMPRESS")
$	temp1 = f$extract(0,1,recd)
$	if temp1 .eqs. "#" then goto RLoop
$	if temp1 .eqs. "!" then goto RLoop
$
$	id = f$element(0," ",recd)
$	if id .eqs. "SWS" then goto RLoop
$	conf = f$element(1," ",recd)
$
$	i = i + 1
$	key'i' = id
$	cnf'i' = conf
$	goto Rloop
$ RDone:
$	close cfg_dat
$
$	if i .eq. 0
$	then
$	    echo ""
$	    echo "  There are currently no instances of Apache that can be deleted."
$	    echo ""
$	    exit_status = %x1000002c
$	    return
$	endif
$
$	echo "[H[J"
$	echo ""
$	echo ""
$	echo "            Apache Instances available for deletion"
$	echo ""
$	echo ""
$	j = 0
$	blnk = " "
$ DLoop:
$	j = j + 1
$	if j .gt. 9 then blnk = ""
$	if j .gt. i then goto DDone
$	id = key'j'
$	conf = cnf'j'
$	echo "            ''blnk'''j'. ''id'	''conf'"
$	goto DLoop
$ DDone:
$	echo "            ''blnk'''j'. Exit"
$
$	pos = i + 9
$ DPrompt:
$	read/prompt="[''pos';20H[KChoice: " sys$command inp
$	if 'inp' .gt. i+1 then goto DPrompt
$	if 'inp' .lt. 1 then goto DPrompt
$
$	if 'inp' .eq. i+1
$	then
$	    exit_status = %X1000002C
$	    echo "[H[J"
$	    return
$	endif
$
$	dr_id = key'inp'
$	dr_conf = cnf'inp'
$
$	exit_status = 1
$	echo "[H[J"
$	return


$!-----------------------------------------------------------------------
$! r e a d _ c o n f i g
$!
$!	Reads an HTTPD configuration file and pulls the user
$!	from the contents so that rights identifiers can be revoked.
$!
$! Inputs:
$!	dr_conf - Configuration filename
$!
$! Outputs:
$!	dr_user - Username to be acted upon (revoke).
$!
$read_config:
$	dr_user = ""
$	open/read/error=RCDone httpd_c 'dr_conf'
$ RCLoop:
$	read/end=RCDone httpd_c recd
$	recd = f$edit(recd,"TRIM,COMPRESS")
$	if f$extract(0,5,recd) .nes. "User " then goto RCLoop
$	dr_user = f$element(1," ",recd)
$ RCDone:
$	close httpd_c
$	return


$!-----------------------------------------------------------------------
$! r e v o k e _ u s e r _ r i g h t s
$!
$!	Revoke all APACHE rights associated with the user that
$!	were granted in order to run the web server.
$!
$! Inputs:
$!	dr_user - username
$!
$! Outputs:
$!	none
$!
$revoke_user_rights:
$	echo "Revoking rights from ''dr_user' UAF account..."
$	set noon
$	set message 'no_message'
$
$	uaf_defined = "FALSE"
$	if f$trnlnm("SYSUAF") .eqs. ""
$	then
$	    uaf_defined = "TRUE"
$	    define SYSUAF SYS$SYSTEM:SYSUAF.DAT
$	endif
$
$	uaf revoke/ident APACHE$APR_ALL 'dr_user'
$	uaf revoke/ident APACHE$APR_CREMBX 'dr_user'
$	uaf revoke/ident APACHE$APR_GETPWNAM 'dr_user'
$	uaf revoke/ident APACHE$APR_SETSOCKOPT 'dr_user'
$	uaf revoke/ident APACHE$APR_SOCKET 'dr_user'
$	uaf revoke/ident APACHE$APR_AUTH_OPENVMS 'dr_user'
$	uaf revoke/ident APACHE$APR_GALAXY_GBLSEC 'dr_user'
$	uaf revoke/ident APACHE$READ 'dr_user'
$	uaf revoke/ident APACHE$EXECUTE 'dr_user'
$
$	if uaf_defined .eqs. "TRUE" then -
	    deassign SYSUAF
$
$	set on
$	set message 'save_message'
$	return


$!-----------------------------------------------------------------------
$! d e l e t e _ r o o t
$!
$!	Delete all files found under a root.  The root is determined
$!	by the location of the HTTPD.CONF file.
$!
$! Inputs:
$!	dr_conf - location of HTTPD configuration file
$!
$! Outputs:
$!	none
$!
$! Side effects:
$!	all files deleted
$!
$delete_root:
$	set noon
$	set message 'no_message'
$
$	device = f$parse(dr_conf,,,"Device")
$	dd = f$parse(dr_conf,,,"Directory")
$	gosub get_directory_file
$	temp1 = device + retval
$	dd = f$parse(temp1,,,"Directory")
$	dr_root = device + dd
$	temp1 = device + f$extract(0,f$length(dd)-1,dd) + "...]"
$
$	echo "Deleting all files under ''temp1'"
$
$ DELoop:
$	delete 'temp1'*.*;*
$	if $status .nes. "%X10000001" then goto DELoop
$
$	set on
$	set message 'save_message'
$	return


$!--------------------------------------------------------------
$! u p d a t e _ d b
$!
$!	Updates the config database (APACHE$CONFIG.DAT) to
$!	remove this instance.
$!
$! Inputs:
$!	dr_id - identifier of this instance
$!
$! Outputs:
$!	none
$!
$update_db:
$	dbfile = "APACHE$COMMON:[000000]APACHE$CONFIG.DAT"
$	tmpfile = "APACHE$COMMON:[000000]APACHE$CONFIG.TMP"
$	bakfile = "APACHE$COMMON:[000000]APACHE$CONFIG.BAK"
$
$	if f$search(dbfile) .eqs. "" then return
$
$	echo "Updating the configuration database..."
$
$	open/read cfgdb 'dbfile'
$	open/write tmpf 'tmpfile'
$ DBRead:
$	read/end=DBDone cfgdb recd
$	if f$length(recd) .eq. 0
$	then
$	    write tmpf ""
$	    goto DBRead
$	endif
$	temp1 = f$edit(recd,"TRIM,COMPRESS")
$	if f$element(0," ",temp1) .nes. dr_id then -
	    write tmpf "''recd'"
$	goto DBRead
$ DBDone:
$	close cfgdb
$	close tmpf
$
$	if f$search(bakfile) .nes. "" then delete 'bakfile';*
$	rename 'dbfile' 'bakfile'
$	rename 'tmpfile' 'dbfile'
$
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

