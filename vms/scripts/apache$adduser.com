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
$	if (f$trnlnm("apache$verify")) then save_verify = f$verify(1)
$!
$!  Module:  APACHE$ADDUSER.COM
$!  Version: 1.0-000
$!
$!  (Revision history at the end of this file.)
$!
$!  Define symbols to describe the operating environment.
$!
$ RMS$_RNF = %X100182B2           ! Account not found value
$ INVCHAR  = %X00038050		  ! Invalid character
$ username = "APACHE$WWW"
$ account  = "AP_HTTPD"
$ privs    = "(TMPMBX,NETMBX)"
$ quota    = 80000
$ overdraft = 10000
$
$!! userdisk = f$trnlnm("APACHE$ADDUSER_USERDISK")
$!! userdir  = f$trnlnm("APACHE$ADDUSER_USERDIR")
$ userdisk = "APACHE$ROOT"
$ userdir  = "[000000]"
$
$ options1 = "/ASTLM=610/BYTLM=200000/ENQLM=2000/FILLM=300/BIOLM=300"
$ options2 = "/DIOLM=300/PGFLQUO=250000/TQELM=610/PRCLM=20"
$ options3 = "/WSDEF=15000/WSQUO=30000/WSEXT=30000"
$ options4 = "/FLAGS=(NoDisUser,LockPwd,DisNewMail,DisMail,DisReport)"
$ options5 = "/Network/NoBatch/NoLocal/NoDial/NoRemote/Lgicmd=LOGIN/NoExpiration"
$
$ type sys$input:
***************************************************************************
*  Creating a NEW user account...                                         *
*                                                                         *
*  If at ANY TIME you need help about a prompt, just type "?".            *
***************************************************************************
$ !
$ !	Add a new user account to the system
$ !
$ !
$ ! Default values
$ !
$ on controly then goto bad_message
$ defgrp = "200"                        ! Default group number
$ defmem = ""                           ! Default member number
$ defacc = ""                           ! Default account name
$ grp = ""
$
$ uaf = "$authorize"
$ on warning then goto cleanup
$ olddir = f$environment("DEFAULT")
$ prevpriv = f$setprv("SYSPRV")
$ if .not. f$priv("SYSPRV") then goto nopriv
$ set default sys$system
$ write sys$output ""
$ !
$ ! Process extracted name, get full name and password
$ !
$full_name:
$ write sys$output ""
$ write sys$output " *** Processing ",username,"'s account ***"
$ write sys$output ""
$!
$! Determine if the account already exists
$!
$ set noon
$ define/user sys$output nla0:
$ define/user sys$error nla0:
$ uaf show 'username'
$ stat = $status
$ set on
$ if stat .eq. rms$_rnf   ! Account was not found
$ then
$   exists = 0
$ else
$   if .not. stat
$   then
$     goto cleanup	!Error other than Account Not Found
$   else
$     exists = 1  ! Account was found
$!
$! Check to see if the identifier exists. This is to handle the rare
$! case where the account exists but the identifier doesn't
$!
$     cnf_uic = f$identifier(username,"name_to_number")
$     if (cnf_uic .eq. 0)
$     then
$       write sys$output "ERROR: Account ''username' exists but the identifier ''username'"
$       write sys$output "       does not exist. Please correct this situation manually then"
$       write sys$output "       rerun this command procedure"
$       inquire ok "Enter RETURN to proceed"
$       goto cleanup
$     endif
$     write sys$output " *** Account ",username," already exists."
$     write sys$output " *** This procedure will only modify the quotas and flags."
$     inquire ok "Enter RETURN to proceed"
$   endif
$ endif
$ if exists then $ goto create_account
$full_name_1:
$ inquire full_name "Full name for ''username'"
$ if (full_name .nes. "?") then goto skip_help_2
$ type sys$input:

Type in the full name of the person who is going to receive this account. For
instance, an account with the username SMITH, might be for a person with the
full name of John Smith.

$ goto full_name_1
$skip_help_2:
$ set term/noecho
$ inquire password "Password (password is not echoed to terminal) [''username']"
$ set term/echo
$ if (password .nes. "?") then goto skip_help_3
$ type sys$input:

This is the password that the user must know in order to get into his account.
Since the user can use SET PASSWORD to change it, it defaults to being the same
as his username.

Note that when the user first logs in, the system will tell him that his
password has already expired. This forces him to change it to something else.

$ goto skip_help_2
$skip_help_3:
$ if password .eqs. "" then password = username
$ !
$ ! List containing all accounts.  Use list to acquire an unspecified
$ ! UIC group and member number to give to the new user.
$ !
$ goto get_uic
$display_UIC_list:
$ type sys$input:

Each user has a specific User Identification Code (UIC) which consists of two
numbers, and is usually displayed in the format [group,member]. The first
number is the "group". People with the same group number can access each
other's files through the group protection code.

For instance, if you set protection using the following command:
	$ SET PROTECTION=(GROUP:R,WORLD) NEWS.TXT
only people in the same group could read the file "NEWS.TXT".

The following is a list of UIC's that already exist on the system: (You do not
have to specify one of these groups, if you do not want to.)

$ uaf show [*,*]/brief
$ write sys$output ""
$ goto get_uic
$display_UIC_members:
$ type sys$input:

Since the UIC should uniquely identify every person on the system, a member
number must also be specified for each individual. If a person is in
the same group, he must have a different member number, or it will not be
possible to put him into the RIGHTSLIST database.

The following is a list of all users in the specified group:

$ set noon
$ uaf show ['grp',*]/brief
$ set on
$ write sys$output ""
$ goto get_member
$ !
$ ! Get group and member numbers
$ !
$get_uic:
$ write sys$output ""
$ inquire grp "UIC Group number [''defgrp']"
$ if grp .eqs. "?" then goto display_UIC_list
$ if grp .eqs. "" then grp = defgrp
$ retval = 0
$ temp1 = grp
$ gosub check_octal
$ if .not. retval
$ then
$  write sys$output "UIC Group number must be octal"
$  goto get_uic
$ endif
$get_member:
$ write sys$output ""
$ inquire uic "UIC Member number"
$ if((uic .eqs. "?").or.(uic.eqs."")) then goto display_UIC_members
$ temp1 = uic
$ gosub check_octal
$ if .not. retval
$ then
$  write sys$output "UIC Member number must be octal"
$  goto get_member
$ endif
$ !
$ ! Combine group and member numbers to create complete UIC
$ ! in the form - [group,member]
$ !
$create_uic:
$ if f$loc("[",uic) .eq. f$len(uic) .and. -
	f$loc("<",uic) .eq. f$len(uic) then uic = "[" + grp + "," + uic + "]"
$ write sys$output ""
$!
$! Determine if an account with this UIC already exists
$!
$ set noon
$ define/user sys$output nla0:
$ define/user sys$error nla0:
$ uaf show 'uic' /brief
$ stat = $status
$ set on
$ if stat .ne. rms$_rnf   ! An account with this UIC exists
$ then
$  uaf show 'uic' /brief
$  write sys$output ""
$  write sys$output "WARNING: An Account with UIC ''uic' already exists."
$  write sys$output "         You can continue using this UIC or you can select a different UIC"
$  inquire ok       "         Do you wish to continue using UIC ''uic' [Y/N] "
$  if ok .nes. "Y" .and. ok .nes. "y" then goto get_uic
$ endif
$ !
$ ! Get disk quota and overdraft quota if enabled on the volume.
$ !
$get_quotas:
$ dquota = 0
$ rootdisk = f$parse(userdisk,,,"DEVICE","NO_CONCEAL")
$ if f$search("''rootdisk'[0,0]QUOTA.SYS") .eqs. "" then goto create_account
$ dquota = 1
$ defquo = quota
$ defovr = overdraft
$ open/write file sys$scratch:addquota.tmp
$ write file "$ SET DEFAULT ''userdisk' "
$ write file "$ RUN SYS$SYSTEM:DISKQUOTA
$ write file "ADD ",uic,"/PERM=",quota,"/OVERDRAFT=",overdraft
$ write file "$ SET DEFAULT SYS$SYSTEM"
$ close file
$ @sys$scratch:addquota.tmp
$ delete sys$scratch:addquota.tmp;*/nolog
$ !
$ ! Create new user directory. Create new account.
$ !
$create_account:
$ open/write file sys$scratch:adduaf.tmp
$ write file "$ RUN SYS$SYSTEM:AUTHORIZE"
$ !
$ ! If the account already exists don't execute the ADD command
$ !
$ if .not. exists
$ then
$    write file "ADD ",username,"/OWN=""",full_name,"""/ACCO=",account,-
	"/DEV=APACHE$ROOT:/DIR=[000000]/UIC=",uic,"/PRIV=",privs,"/PASSW=",password
$ endif
$ write file "MODIFY ",username,options1,options2,options3
$ write file "MODIFY ",username,options4,options5
$ write file "GRANT/IDENT APACHE$APR_ALL ''username'"
$ close file
$ @sys$scratch:adduaf.tmp
$ delete sys$scratch:adduaf.tmp;*/nolog
$ on controly then goto good_message
$ !
$ ! Show newly created account to check for possible errors
$ !
$show_account:
$ write sys$output ""
$ if .not. exists
$ then
$   write sys$output "Check newly created account:"
$ else
$   write sys$output "Check modified account:"
$ endif
$ write sys$output ""
$ open/write file sys$scratch:shouaf.tmp
$ write file "$ RUN SYS$SYSTEM:AUTHORIZE"
$ write file "SHOW ",username,""
$ close file
$ @sys$scratch:shouaf.tmp
$ delete sys$scratch:shouaf.tmp;*/nolog
$ !
$ ! If an error in account then remove account.
$ ! If no error then process next user name, create next account.
$ !
$ write sys$output ""
$ write sys$output "Please verify that this account does not violate any site-specific"
$ write sys$output "security policy. This account will be enabled and it will have no"
$ write sys$output "expiration date."
$ write sys$output ""
$ !
$ ! If the account already existed and all we did was modify then
$ ! prompt to proceed. i.e. don't allow the user to remove the account
$ !
$ if exists
$ then
$   inquire ok "Enter RETURN to proceed"
$   goto cleanup
$ endif
$ask_ok:
$ inquire ok "Is everything satisfactory with the account [YES]"
$ if ok .nes. "?" then goto skip_help_10
$ type sys$input:

If you feel there is something wrong with the account type "NO". This will
erase what you have done, and allow you to start over. Otherwise, press
[return] and the account will be left in the authorization file.

$ goto ask_ok
$skip_help_10:
$ if ok .eqs. "" then ok = "yes"
$ if .not. ok then goto remove_uaf
$cleanup:
$ set term /echo
$ prevpriv = f$setpriv(prevpriv)
$ set default 'olddir'
$ exit
$ !
$ ! Remove account, then return and process same account again
$ !
$remove_uaf:
$ write sys$output ""
$ write sys$output "Removing newly created account"
$ write sys$output ""
$ open/write file sys$scratch:remuaf.tmp
$ write file "$ RUN SYS$SYSTEM:AUTHORIZE"
$ write file "REMOVE ",username,""
$ write file "$ SET DEFAULT ''userdisk' "
$ write file "$ RUN SYS$SYSTEM:DISKQUOTA
$ write file "REMOVE ",uic
$ write file "$ SET DEFAULT SYS$SYSTEM"
$ close file
$ @sys$scratch:remuaf.tmp
$ del sys$scratch:remuaf.tmp;*/nolog
$ on controly then goto good_message
$ write sys$output ""
$ goto full_name                               ! go process same account
$good_message:
$ write sys$output ""
$ write sys$output "Program halted by control_y, account has already been created."
$ write sys$output ""
$ goto cleanup
$bad_message:
$ write sys$output ""
$ write sys$output "Program halted by control_y, account has not yet been created."
$ write sys$output ""
$ goto cleanup
$nopriv:
$ write sys$output "You need SETPRV or SYSPRV privilege to run this procedure"
$ goto cleanup

$!
$! Subroutine: check_octal
$! Determine if the value is octal
$!
$ check_octal:
$ set noon
$ define/user sys$output nla0:
$ define/user sys$error nla0:
$ temp2 = "%O"+temp1
$ temp3 = 'temp2
$ stat = $status
$ set on
$ deass/user sys$output
$ deass/user sys$error
$ if stat .eq. invchar
$ then
$   retval = 0
$ else
$   retval = 1
$ endif
$ return

$!-----------------------------------------------------------------------------
$!
$!  ABSTRACT:
$!	This command procedure is a copy of SYS$EXAMPLES:ADDUSER.COM, modified
$!      for CSWS. It is used to create an account with the parameters required
$!      to run the HTTP Server.
$!
$!  AUTHORS:
$!	The original command procedure, SYS$EXAMPLES:ADDUSER.COM, is provided
$!	with the OpenVMS operating system. Gaitan D'Antoni made the original
$!	modifications.
$!
$!  REVISION HISTORY:
$!
$!	(GBD/27-Jun-2000)
$!	Created.
$!
$!	(GBD/28-Jun-2000)
$!	Add code to handle the case where the account already exists. In this
$!	case we don't execute the UAF ADD command and we don't set disk quotas.
$!
$!-----------------------------------------------------------------------------
