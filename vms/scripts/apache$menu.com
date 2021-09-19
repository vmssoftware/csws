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
$! Module:   APACHE$MENU.COM
$! Version:  2.0-008
$!
$! Modification History:
$!
$!	30-Dec-2002	2.0-000		Scott LePage
$!	Initial creation.
$!
$!	04-Mar-2003	2.0-001		Scott LePage
$!	Added Cert Tool to menu
$!
$!	25-Mar-2003	2.0-002		Scott LePage
$!	Added Stream_LF conversion tool
$!
$!	25-Apr-2003	2.0-003		Scott LePage
$!	Add check for return status from APACHE$CREATE_ROOT
$!
$!	25-Apr-2003	2.0-004		Scott LePage
$!	Add new function to check the status of an instance
$!
$!	28-Apr-2003	2.0-005		Scott LePage
$!	Add new function to delete an existing root
$!
$!	25-Sep-2003	2.0-006		Scott LePage
$!	Fix text to reference 'tree' as 'instance'
$!
$!	27-May-2005	2.0-007		Scott LePage
$!	Added Add Node function for cluster support
$!
$!	04-Aug-2006	2.0-008		Scott LePage
$!	Added check for special FIS environment
$!
$! *************************************************************************
$
$	on warning then goto welcome
$	on error then goto welcome
$	on severe_error then goto welcome
$	on control_y then goto welcome
$
$	echo := write sys$output
$	del*ete := delete
$	dir*ectory := directory
$
$	if f$trnlnm("APACHE$COMMON") .eqs. ""
$	then
$	    if f$search("SYS$MANAGER:APACHE$LOGICALS") .EQS. ""
$	    then
$	        if f$search("SYS$MANAGER:APACHE$FIS.COM") .NES. ""
$	        then
$	            copy/nolog/noconfirm SYS$MANAGER:APACHE$FIS.COM SYS$COMMON:[SYSMGR]APACHE$LOGICALS.COM
$	        endif
$	    endif
$	    @sys$manager:apache$logicals "" "" "NOINSTALL"
$	endif
$
$ welcome:
$
$	echo "[2J[H"
$
$	echo ""
$	echo ""
$	echo "#3          Apache$Menu"
$	echo "#4          Apache$Menu"
$	echo ""
$	echo ""
$	echo ""
$	echo "                  1. Configure the Secure Web Server"
$	echo "                  2. Create an Apache instance"
$	echo "                  3. Delete an Apache instance"
$	echo "                  4. Manage suEXEC users"
$	echo "                  5. Run OpenSSL Certificate tool"
$	echo "                  6. Convert directory tree to Stream_LF"
$	echo "                  7. Start up an Apache instance"
$	echo "                  8. Shut down an Apache instance"
$	echo "                  9. Show status of an Apache instance"
$	echo "                 10. Add a node to CSWS in a cluster environment"
$	echo "                 11. Exit"
$
$ get_menu_choice:
$	choice = 11
$	pos = choice + 10
$	read/prompt="[''pos';30H[KEnter Menu Choice: " sys$command choice
$
$	if choice .ne. 1 .and. -
	   choice .ne. 2 .and. -
	   choice .ne. 3 .and. -
	   choice .ne. 4 .and. -
	   choice .ne. 5 .and. -
	   choice .ne. 6 .and. -
	   choice .ne. 7 .and. -
	   choice .ne. 8 .and. -
	   choice .ne. 9 .and. -
	   choice .ne. 10 .and. -
	   choice .ne. 11
$	then
$	    echo "[23;1H  Invalid menu choice"
$	    wait 0:0:3
$	    echo "[23;1H[K"
$	    goto get_menu_choice
$	endif
$
$	if choice .eq. 1
$	then
$	    echo "[H[J"
$	    @sys$manager:apache$config
$	endif
$
$	if choice .eq. 2
$	then
$	    echo "[H[J"
$	    @apache$common:[000000]apache$create_root
$	endif
$
$	if choice .eq. 3
$	then
$	    echo "[H[J"
$	    @apache$common:[000000]apache$delete_root
$	endif
$
$	if choice .eq. 4
$	then
$	    echo "[H[J"
$	    @apache$common:[000000]apache$manage_suexec
$	endif
$
$	if choice .eq. 5
$	then
$	    echo "[H[J"
$	    @apache$common:[000000]apache$cert_tool
$	endif
$
$	if choice .eq. 6
$	then
$	    echo "[H[J"
$	    @apache$common:[000000]apache$convert_streamlf
$	endif
$
$	if choice .eq. 7
$	then
$	    arg1 = "START"
$	    gosub handle_instance
$	endif
$
$	if choice .eq. 8
$	then
$	    arg1 = "SHUTDOWN"
$	    gosub handle_instance
$	endif
$
$	if choice .eq. 9
$	then
$	    arg1 = "STATUS"
$	    gosub handle_instance
$	endif
$
$	if choice .eq. 10
$	then
$	    echo "[H[J"
$	    @apache$common:[000000]apache$addnode
$	endif
$
$	if choice .eq. 11 then goto stop_procedure
$
$	if $status .eqs. "%X1000002C" then goto welcome
$	echo ""
$	read/prompt="Press return to continue..." sys$command dummy
$	goto welcome
$
$ stop_procedure:
$	echo "[2J[H"
$	echo ""
$	set noon
$	on warning then continue
$	on error then continue
$	on sever_error then contine
$	exit


$!=============================================================================
$! h a n d l e _ i n s t a n c e
$!
$!	Startup or shutdown an instance of Apache
$!
$! Inputs
$!	arg1 - either "START" or "SHUTDOWN"
$!
$! Outputs
$!	none
$!
$! Other
$!	requires file APACHE$COMMON:[000000]APACHE$CONFIG.DAT
$!
$ handle_instance:
$	file = "APACHE$COMMON:[000000]APACHE$CONFIG.DAT"
$	inst = "SWS"
$	cfg = "APACHE$COMMON:[CONF]HTTPD.CONF"
$
$	if f$search("''file'") .eqs. ""
$	then
$	    echo ""
$	    echo "''file' was not found"
$	    read/prompt="Create it [YES] ? " sys$command inp
$	    if inp .eqs. "" then inp = "YES"
$	    if f$edit(f$extract(0,1,inp), "UPCASE") .eqs. "N" then return
$	    if f$edit(f$extract(0,1,inp), "UPCASE") .nes. "Y" then goto handle_instance
$
$	    open/write apdat 'file'
$	    write apdat "#"
$	    write apdat "# APACHE$CONFIG.DAT"
$	    write apdat "#"
$	    write apdat "#	Created by APACHE$MENU.COM on ''f$time()'"
$	    write apdat "#"
$	    write apdat "#	Defines the instances of Apache on this system"
$	    write apdat "#"
$	    write apdat ""
$	    write apdat "''inst'     ''cfg'"
$	    close apdat
$	endif
$
$	open/read apdat 'file'
$	i = 0
$ RLoop:
$	read/end=RDone apdat recd
$	recd = f$edit(recd,"TRIM,COMPRESS")
$	if f$length(recd) .eq. 0 then goto RLoop
$	if f$extract(0,1,recd) .eqs. "#" then goto RLoop
$	if f$extract(0,1,recd) .eqs. "!" then goto RLoop
$	i = i + 1
$	key'i' = f$element(0," ",recd)
$	conf'i' = f$element(1," ",recd)
$	goto Rloop
$ RDone:
$	close apdat
$	maxmenu = i + 1
$
$ AIMenu:
$	i = 1
$	echo "[H[J"
$	echo ""
$	echo ""
$	echo "          Registered Apache Instances"
$	echo ""
$	echo ""
$ MLoop:
$	if i .eq. maxmenu then goto MDone
$	blnk = ""
$	if i .lt. 10 then blnk = " "
$	tag = key'i'
$	tab = "    "
$	pad = 4 - f$length(tag)
$	tab = tab + f$extract(0,pad,tab)
$	file = conf'i'
$	echo "               ''blnk'''i'. ''tag'''tab'''file'"
$	i = i + 1
$	goto MLoop
$ MDone:
$	blnk = ""
$	if i .lt. 10 then blnk = " "
$	echo "               ''blnk'''i'. Exit"
$
$	echo ""
$	ypos = maxmenu + 6 + 2
$ MPrompt:
$	read/prompt="[''ypos';20H[KChoice: " sys$command msel
$	if msel .gt. maxmenu then goto MPrompt
$	if msel .lt. 1 then goto MPrompt
$
$	$status = "%X1000002C"
$	if msel .eq. maxmenu then return
$
$	file = conf'msel'
$	tag = key'msel'
$	if arg1 .eqs. "START"
$	then
$	    echo ""
$	    echo "Starting ''tag' instance of Apache..."
$	    echo ""
$	    echo "$ @SYS$STARTUP:APACHE$STARTUP ""START"" ""''file'"""
$	    @sys$startup:apache$startup "START" "''file'"
$	endif
$
$	if arg1 .eqs. "SHUTDOWN"
$	then
$	    echo ""
$	    echo "Shutting down ''tag' instance of Apache..."
$	    echo ""
$	    echo "$ @SYS$STARTUP:APACHE$SHUTDOWN ""SHUTDOWN"" ""''file'"""
$	    @sys$startup:apache$shutdown "SHUTDOWN" "''file'"
$	endif
$
$	if arg1 .eqs. "STATUS"
$	then
$	    echo ""
$	    echo "Status of ''tag' instance of Apache..."
$	    echo ""
$	    show system/process=apache$'tag'*
$	    echo ""
$	    echo "End status."
$	    echo ""
$	    read/prompt="Press return to continue..." sys$command dummy
$	    goto AIMenu
$	endif
$
$	echo ""
$	read/prompt="Press return to continue..." sys$command dummy
$	goto AIMenu
