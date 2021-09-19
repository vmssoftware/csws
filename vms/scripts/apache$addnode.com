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
$! Module:   APACHE$ADDNODE.COM
$! Version:  2.0-001
$!
$! Modification History:
$!
$!	24-May-2005	2.0-000		Scott LePage
$!	Initial creation.
$!
$!	08-Feb-2006	2.0-001		Scott LePage
$!	Fix ident search to look at all ACEs incase they
$!	don't come in the expected order.
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
$	exit_status = 1
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
$!	Create new specific root for a node in the cluster
$!
$!----------------------------------------------------------------------
$
$	gosub dialog_intro
$
$	gosub setup_logicals
$	gosub get_root
$	if (exit_status .and. 1) .eq. 0 then goto abort_procedure
$	gosub check_user
$	if (exit_status .and. 1) .eq. 0 then goto abort_procedure
$	gosub grant_user_rights
$	if (exit_status .and. 1) .eq. 0 then goto abort_procedure
$	gosub create_root
$	gosub populate_root
$
$	echo ""
$	echo "Node ''an_root' added successfully"
$	echo "  Node specific directories created: ''an_specific'"
$	conf = an_specific - "]" + ".CONF]"
$	echo "  Configuration files created in: ''conf'"
$	echo "  Please review these files for accuracy."
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
$	echo "      APACHE$ADDNODE"
$	echo ""
$	echo " Create a set of directories and files another node"
$	echo " in a cluster environment for the Secure Web Server."
$	echo " The node name used is that defined by TCPIP$INET_HOST"
$	echo " A directory by that name will be created under the"
$	echo " APACHE$SPECIFIC: area.  The top level directories under"
$	echo " APACHE$COMMON are essentially duplicated here."
$	echo ""
$	echo " A new version of HTTPD.CONF is created in APACHE$ROOT:[CONF]."
$	echo " This will be used by default.  The common configuration in"
$	echo " APACHE$COMMON:[CONF] remains untouched.  Remove this new"
$	echo " configuration if you wish to use the common one."
$	echo ""
$	echo " The rights identifiers for the user account APACHE$WWW on"
$	echo " this node are set to the defaults.  If this is a common"
$	echo " SYSUAF/RIGHTSLIST, then the account should be checked as"
$	echo " it might be changed."
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
$	    @sys$common:[sysmgr]apache$logicals "" "/process" "NOINSTALL"
$	    logicals_defined = "TRUE"
$	endif
$	return


$!--------------------------------------------------------------
$! g e t _ r o o t
$!
$!	Find the name of the specific directory and check it
$!
$! Inputs:
$!	none
$!
$! Outputs:
$!	an_root - hostname of current system
$!	an_specific - filespec of new APACHE$SPECIFIC
$!
$get_root:
$	an_root = f$trnlnm("TCPIP$INET_HOST")
$	if (an_root .eqs. "")
$	then
$	    echo ""
$	    echo " Could not translate TCPIP$INET_HOST logical name.  Required"
$	    exit_status = 444
$	    return
$	endif
$	an_root = f$edit(an_root,"UPCASE")

$	common = f$trnlnm("APACHE$COMMON")
$	an_specific = common - "]" + "SPECIFIC.''an_root']"
$	dd = f$parse(an_specific,,,"DIRECTORY")
$	dev = f$parse(an_specific,,,"DEVICE")
$	gosub get_directory_file
$	open/read/error=GRErr dtmp 'dev''retval'
$	close dtmp
$	echo ""
$	echo " Directory for node ''an_root' already exists.  Aborting"
$	exit_status = 9507474
$	return
$
$ GRErr:
$	exit_status = 1
$	return




$!--------------------------------------------------------------
$! c h e c k _ u s e r
$!
$!	Checks for existance of APACHE$WWW user
$!	Creates the account if not found.
$!
$! Inputs:
$!	none
$!
$! Outputs:
$!	an_user - username (APACHE$WWW)
$!	an_privs - list of privleges that was granted
$!
$check_user:
$	an_user = "APACHE$WWW"
$	an_privs = "|APACHE$READ|APACHE$EXECUTE|APACHE$APR_ALL"
$
$	set noon
$	set message 'no_message'
$	if verbose then echo "[ Checking UAF for: ''an_user' ]"
$	uic = f$identifier(an_user,"name_to_number")
$	if (uic .eq. 0)
$	then
$	    if verbose then echo "[ Creating account: ''an_user' ]"
$	    gosub create_account
$	else
$	    if verbose then echo "[ UAF record found: ''an_user' ]"
$	    uic = f$file_attributes("APACHE$COMMON:[000000]APACHE$HTTPD.EXE","UIC")
$	    if f$locate("APACHE$WWW",uic) .eqs. f$length(uic)
$	    then
$	        echo " The UIC of the APACHE$WWW account on this system does not"
$	        echo " match the owner of the files in the CSWS directories."
$	        echo " These identifiers must match for CSWS to operate correctly"
$	        echo " in a cluster environment."
$	        exit_status = 8740
$	    endif
$	endif
$
$	set on
$	set message 'save_message'
$	return



$!--------------------------------------------------------------
$! c r e a t _ a c c o u n t
$!
$!	Create the APACHE$WWW UAF account
$!
$! Inputs:
$!	an_user - "APACHE$WWW"
$!
$! Outputs:
$!	none
$!
$create_account:
$
$	temp2 := APACHE$COMMON:[000000]APACHE$ADDUSER.COM
$	if (f$search(temp2) .eqs. "")
$	then
$	   echo "ERROR: Required OpenVMS ''an_user' account does not exist"
$	   echo "ERROR: Missing required command procedure: ", temp2
$	   goto abort_procedure
$	endif
$	echo ""
$	echo "[Creating OpenVMS username ""''an_user'"" ]"
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


    *****************************************************************
    It is important the UIC of this account match the UIC of the
    APACHE$WWW account on all nodes of the cluster.  Failure to do
    this may result in the Web Server being unable to access program
    files and/or content to be served.
    *****************************************************************

$	eod
$	define/user apache$root nla0:
$	@'temp2'
$	uic = f$identifier(an_user,"name_to_number")
$	if (uic .eq. 0)
$	then
$	   echo "ERROR: Required OpenVMS ''an_user' username does not exist"
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
$	exit_status = 1
$	return




$!--------------------------------------------------------------
$! g r a n t _ u s e r _ r i g h t s
$!
$!	Grant the input rights to the input user
$!
$! Inputs:
$!	an_user - username to get the rights
$!	an_privs - list of rights identifiers to be granted
$!
$! Outputs:
$!	none
$!
$grant_user_rights:
$	echo "Granting rights to ''an_user' UAF account..."
$	gosub check_rights
$	if (exit_status .and. 1) .eqs. 0 then return
$	gosub revoke_rights
$	set noon
$	set message 'no_message'
$	if verbose then echo "[ Granting: ''an_privs' ]"
$	temp1 = f$extract(1,f$length(an_privs)-1, an_privs)
$	idx = 0
$ GURLoop:
$	right = f$element(idx,"|",temp1)
$	if right .eqs. "|" then goto GURDone
$	if f$trnlnm("SYSUAF") .eqs. "" then -
	    define/user SYSUAF SYS$SYSTEM:SYSUAF.DAT
$	uaf grant/ident 'right' 'an_user'
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
$!	an_user - user to be stripped of their rights
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
$	if verbose then echo "[ Revoking rights from ''an_user' account ]"
$	uaf revoke/ident APACHE$APR_ALL 'an_user'
$	uaf revoke/ident APACHE$APR_CREMBX 'an_user'
$	uaf revoke/ident APACHE$APR_GETPWNAM 'an_user'
$	uaf revoke/ident APACHE$APR_SETSOCKOPT 'an_user'
$	uaf revoke/ident APACHE$APR_SOCKET 'an_user'
$	uaf revoke/ident APACHE$APR_AUTH_OPENVMS 'an_user'
$	uaf revoke/ident APACHE$APR_GALAXY_GBLSEC 'an_user'
$	uaf revoke/ident APACHE$READ 'an_user'
$	uaf revoke/ident APACHE$EXECUTE 'an_user'
$	if uaf_defined .eqs. "TRUE" then -
	    deassign SYSUAF
$
$	set on
$	set message 'save_message'
$	return


$!--------------------------------------------------------------
$! c h e c k _ r i g h t s
$!
$!	Check to see if the APACHE$* rights have been
$!	created.  Situation exists in heterogenous clusters
$!	where the rights exist on one node, but not on
$!	another.  They have to be in sync...ie same numeric
$!	identifiers on all nodes.
$!
$! inputs:
$!	an_privs - list of APACHE identifier names
$!
$! outputs:
$!	none
$!
$check_rights:
$	set NoOn
$	set message 'no_message'
$
$	exit_status = 1
$	temp1 = f$extract(1,f$length(an_privs)-1,an_privs)
$	idx = 0
$ CRLoop:
$	right = f$element(idx,"|",temp1)
$	if right .eqs. "|" then goto CRDone
$	if f$trnlnm("SYSUAF") .eqs. "" then -
	    define/user SYSUAF SYS$SYSTEM:SYSUAF.DAT
$	define/user sys$output NLA0:
$	uaf show/ident 'right'
$	save_status = $status
$	if ($status .and. 1) .eq. 0
$	then
$	    if verbose then echo "[ rights id ''right' not found, checking value ]"
$	    dir/acl/output='tmpfil'.read APACHE$COMMON:[000000]HTDOCS.DIR
$	    open/read rtmp 'tmpfil'.read
$	    ar_found = 0
$ ARLoop:
$	    read/end=ARDone rtmp recd
$	    eq = f$locate("=", recd) + 1
$	    if eq .eq. f$length(recd) then goto ARLoop
$	    ar_found = 1
$ ARDone:
$	    close rtmp
$	    delete 'tmpfil'.read;*
$
$	    if ar_found .eq. 0
$	    then
$	        echo " The CSWS installation does not seem to have any Access Control"
$	        echo " Lists on the file system.  To run CSWS, you must execute the"
$	        echo " SYS$MANAGER:APACHE$CONFIG command procedure to set up file"
$	        echo " protections and ACLs.  Please perform this first before attempting"
$	        echo " to add a node from a cluster to CSWS."
$	        exit_status = 8740
$	        return
$	    else
$               echo " The CSWS installation is protected by ACLs that have not been"
$	        echo " registered on this node.  Since this appears to be a heterogeneous"
$	        echo " cluster, it is not possible for this procedure to know which ACE"
$	        echo " is which.  The system manager will have to add those identifiers"
$	        echo " to the RIGHTSLIST manually so that they match the other node(s)"
$	        echo " in the cluster that are running CSWS.  It is important that the
$	        echo " APACHE$* identifiers on all nodes match up exactly."
$	        exit_status = 8740
$	        return
$	    endif
$
$	else
$	    if right .eqs. "APACHE$READ"
$	    then
$	        if verbose then echo "[ rights id APACHE$READ found, checking value ]"
$	        dir/acl/output='tmpfil'.read APACHE$COMMON:[000000]HTDOCS.DIR
$	        open/read rtmp 'tmpfil'.read
$	        read_found = 0
$ CRRLoop:
$	        read/end=CRRDone rtmp recd
$	        eq = f$locate("=", recd) + 1
$	        if eq .eq. f$length(recd) then goto CRRLoop
$	        comma = f$locate(",", recd)
$	        ident = f$extract(eq, (comma - eq), recd)
$	        if ident .nes. "APACHE$READ" then goto CRRLoop
$	        read_found = 1
$ CRRDone:
$	        close rtmp
$	        delete 'tmpfil'.read;*
$
$	        if read_found .eq. 0
$	        then
$	            echo " The APACHE$READ identifier of this system does not"
$	            echo " match the identifier found in the ACL of the CSWS"
$	            echo " files.  This situation needs to be fixed manually"
$	            echo " by the system manager."
$	            echo ""
$	            echo " APACHE$READ and APACHE$EXECUTE must be common on"
$	            echo " all nodes on the cluster."
$	            exit_status = 8748
$	            set on
$	            set message 'save_message'
$	            return
$	        endif
$	    endif
$
$	    if right .eqs. "APACHE$EXECUTE"
$	    then
$	        if verbose then echo "[ rights id APACHE$READ found, checking value ]"
$	        dir/acl/output='tmpfil'.exec APACHE$COMMON:[000000]APACHE$HTTPD.EXE
$	        open/read etmp 'tmpfil'.exec
$	        exec_found = 0
$ CRELoop:
$	        read/end=CREDone etmp recd
$	        eq = f$locate("=", recd) + 1
$	        if eq .eq. f$length(recd) then goto CRELoop
$	        comma = f$locate(",", recd)
$	        ident = f$extract(eq, (comma - eq), recd)
$	        if ident .nes. "APACHE$EXECUTE" then goto CRELoop
$	        exec_found = 1
$ CREDone:
$	        close etmp
$	        delete 'tmpfil'.exec;*
$
$	        if exec_found .eq. 0
$	        then
$	            echo " The APACHE$EXECUTE identifier of this system does not"
$	            echo " match the identifier found in the ACL of the CSWS"
$	            echo " files.  This situation needs to be fixed manually"
$	            echo " by the system manager."
$	            echo ""
$	            echo " APACHE$READ and APACHE$EXECUTE must be common on"
$	            echo " all nodes on the cluster."
$	            exit_status = 8748
$	            set on
$	            set message 'save_message'
$	            return
$	        endif
$	    endif
$	endif
$
$	idx = idx + 1
$	goto CRLoop
$ CRDone:
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
$!	an_specific - Target directory to become APACHE$SPECIFIC
$!	an_user - User to own the directory tree
$!
$! Outputs:
$!	none
$!
$create_root:
$	echo "Creating directory tree under ''an_specific'"
$	outfil = tmpfil + ".temp"
$	dir/output='outfil'/column=1/nohead/notrail-
           /exclude=([SPECIFIC...],SPECIFIC.DIR,[KIT...],KIT.DIR) -
           APACHE$COMMON:[000000...]*.DIR
$
$	open/read infil 'outfil'
$ CRLoop:
$	read/end=CRDone infil recd
$	dirfile = f$parse(recd,,,"NAME","SYNTAX_ONLY")
$	target = an_specific - "]"
$	newspec = recd - "APACHE$COMMON:[000000"
$	newfile = target + newspec
$	endspec = "]" + dirfile + ".DIR;1"
$	target = newfile - endspec
$	target = target + "." + dirfile + "]"
$	create/directory/owner=['an_user'] 'target'
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
$!	an_root - ServerRoot
$!	an_user - Username
$!
$! Outputs:
$!	an_conf - New configuration filespec
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
$	conf = f$extract(0,f$length(an_specific)-1, an_specific)
$	an_conf = conf + ".CONF]httpd.conf"
$	echo "Generating Apache configuration file ''an_conf'"
$	template = "APACHE$COMMON:[CONF]HTTPD-VMS.CONF"
$
$	fdlfile = tmpfil + ".temp"
$	analyze/rms/fdl/output='fdlfile' 'template'
$	create/fdl='fdlfile' 'an_conf'
$	delete 'fdlfile';*
$	set file/owner=['an_user'] 'an_conf'
$	if verbose then echo "[ Empty configuration created from ''template' ]
$
$	copy APACHE$COMMON:[CONF]MIME.TYPES 'conf'.CONF]mime.types
$	set file/owner=['an_user'] 'conf'.CONF]mime.types
$	if verbose then echo "[ MIME.TYPES copied ]"
$
$	copy APACHE$COMMON:[CONF]SSL-VMS.CONF 'conf'.CONF]SSL.CONF
$	set file/owner=['an_user'] 'conf'.CONF]SSL.CONF
$	if verbose then echo "[ SSL.CONF copied ]"
$
$	open/read/error=PRConfDone tmplt 'template'
$	open/append/error=PRConfDone out 'an_conf'
$ PRConfLoop:
$	read/end=PRConfDone tmplt recd
$	token = "ServerRoot"
$	if f$extract(0,f$length(token),recd) .eqs. token
$	then
$	    write out "ServerRoot ""/apache$root"""
$	    if verbose then echo "[ ServerRoot updated - /apache$root ]"
$	    goto PRConfLoop
$	endif
$
$	token = "VmsServerTag"
$	if f$extract(0,f$length(token),recd) .eqs. token
$	then
$	    write out "VmsServerTag SWS"
$	    if verbose then echo "[ VmsServerTag updated - SWS ]"
$	    goto PRConfLoop
$	endif
$
$	if f$locate(q,recd) .ne. f$length(recd) then -
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
