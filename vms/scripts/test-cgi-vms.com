$ ! Test-CGI.Com   Lee Tibbert 1999-06-22
$ ! Print out the CGI variables available to a script.
$ ! See documentation at bottom of script.
$ !
$ ! 2000-06-15	Rick Barry
$ !	Rename APACHE_INPUT to APACHE$INPUT
$ !
$ ! 2001-12-18	Steve Fesler
$ !	Inhibit output if values are NULL
$ !
$ set noon
$
$ write sys$output f$fao("!AS!/!/", "Content-type: text/plain") 
$
$ write sys$output f$fao("!AS!/", -
			"CGI test script (test-cgi-vms.com) report:") 
$
$ hostName = f$trnlnm("TCPIP$INET_HOST")
$ if "''hostName'" .nes. ""
$ then
$    fullHostName = "''hostName'." + f$trnlnm("TCPIP$INET_DOMAIN")
$ else  ! Fake it
$    fullHostName = f$edit(f$getsyi("NODENAME"),"TRIM")
$ endif
$
$ write sys$output f$fao("!/Generated on host ''fullHostName' at !%D.!/", 0) 
$
$ cwd = f$environment("DEFAULT")
$ write sys$output f$fao("Default directory is ''cwd'.!/!/") 
$
$
$ apache_input := APACHE$INPUT
$ write sys$output f$fao("!AS is !AS.!/", -
			"''apache_input'", -
			f$trnlnm("''apache_input'"))
$
$ write sys$output f$fao("For PUT methods in .COM scripts:!/!_!AS.!/!/", -
		"$ define/NoLog SYS$INPUT APACHE$INPUT: ! early in .COM file")
$
$
$ call printenv DOCUMENT_ROOT
$ call printenv GATEWAY_INTERFACE
$ call printenv HTTP_ACCEPT
$ call printenv HTTP_ACCEPT_CHARSET
$ call printenv HTTP_ACCEPT_ENCODING
$ call printenv HTTP_ACCEPT_LANGUAGE
$ call printenv HTTP_CONNECTION
$ call printenv HTTP_HOST
$ call printenv HTTP_USER_AGENT
$ call printenv PATH
$ call printenv QUERY_STRING
$ call printenv REMOTE_ADDR
$ call printenv REMOTE_PORT
$ call printenv REQUEST_METHOD
$ call printenv REQUEST_URI
$ call printenv SCRIPT_FILENAME
$ call printenv SCRIPT_NAME
$ call printenv SERVER_ADMIN
$ call printenv SERVER_NAME
$ call printenv SERVER_PORT
$ call printenv SERVER_PROTOCOL
$ call printenv SERVER_SIGNATURE
$ call printenv SERVER_SOFTWARE
$
$ write sys$output f$fao("!/!AS!/", -
	"UNIQUE_ID is non-blank only if Apache was built with MOD_UNIQUE_ID.")
$			
$ call printenv UNIQUE_ID
$
$ write sys$output f$fao("!2(/)!3(_)- The End -!/") 
$
$ exit
$ !____________________________________________________________________________
$ printenv:  subroutine
$ 
$  
$  symbolName := 'P1'
$
$  if f$type('symbolName') .eqs. ""
$  then 
$    symbolValue = ""
$  else
$    symbolValue = 'symbolName'
$  endif
$
$  if symbolValue .nes. ""
$  then
$  write sys$output f$fao("symbol ''symbolName' = ''symbolValue'")
$  endif
$!
$  symbolName := WWW_'P1'
$
$  if f$type('symbolName') .eqs. ""
$  then 
$    symbolValue = ""
$  else
$    symbolValue = 'symbolName'
$  endif
$
$  if symbolValue .nes. ""
$  then
$  write sys$output f$fao("symbol ''symbolName' = ''symbolValue'")
$  endif
$
$  logname = f$trnlnm("''P1'")
$
$  if logname .nes. ""
$  then
$  write sys$output f$fao("logical    ''P1' = ''logname'!/")
$  endif
$
$  exit
$ endsubroutine !printenv
$ !____________________________________________________________________________
$ !
$ ! This program shows that CGI variables are OpenVMS symbols.
$ ! It also shows how a final Carriage Return and Line Feed (CRLF)
$ ! must be added to DCL lines.
$ !
$ ! The output from most DCL commands does _not_ have this terminal CRLF
$ ! so adding such DCL commands to this script will break it.  Fixing
$ ! this situation is a work in progress.
$ !
$ ! This script is useful to check if CGI access has been properly
$ ! configured (in [apache.conf]httpd.conf). One can also use
$ ! this script as in a ping to see if a server is responding.
$ !
$ ! Modification History
$ ! 1999-06-30	Lee Tibbert	    V1.0-1
$ !		Removed the 1.0 from the CGI string.  I believe
$ !		that Apache is now using CGI 1.1.  The orignal .com was
$ !		translated from the distribution test-cgi. and the
$ !		variables printed out do not match those passed in
$ !		envp to the test-cgi-vms.exe.  I changed the .com to
$ !		show the same variables as the .exe. Here I alphabetize
$ !		them.
$ !
$ !		Added printout of default (current) directory to help
$ !		me get it right.
$
$ ! End of file
