$!
$!------------------------------------------------------------------------------
$! APACHE$SHUTDOWN.COM
$!------------------------------------------------------------------------------
$!
$ Set NoOn
$ VERIFY = F$VERIFY (0)
$!
$!------------------------------------------------------------------------------
$! Define symbols 
$!------------------------------------------------------------------------------
$!
$ SAY := WRITE SYS$OUTPUT
$ ASK := READ SYS$COMMAND/PROMPT=
$!
$ IF P1 .EQS. ""
$ THEN
$     Option = "STOP"
$ ELSE
$     Option = F$EDIT (P1, "TRIM,UPCASE")
$ ENDIF
$!
$ IF P2 .EQS. ""
$ THEN
$     ConfigFile = "APACHE$COMMON:[CONF]HTTPD.CONF"
$ ELSE
$     ConfigFile = F$EDIT (P2, "TRIM,UPCASE")
$ ENDIF
$!
$ IF F$SEARCH ("SYS$MANAGER:APACHE$SYMBOLS.COM") .NES. "" -
  THEN @SYS$MANAGER:APACHE$SYMBOLS.COM
$!
$ IF F$SEARCH ("APACHE$COMMON:[000000]APACHE$SETUP.COM") .NES. "" -
  THEN @APACHE$COMMON:[000000]APACHE$SETUP.COM
$!
$!------------------------------------------------------------------------------
$! Parse the input
$!------------------------------------------------------------------------------
$!
$ OptionList = "HELP,STOP,SHUTDOWN,RESTART,GRACEFUL"
$! 
$ IF F$LOCATE (",''Option',", ",''OptionList',") .GT. F$LENGTH (OptionList)
$ THEN
$     SAY "ERROR: Invalid parameter #1 """, Option, """"
$     SAY "   Must be one of the following: ''OptionList'"
$     GOTO EXIT
$ ENDIF
$!
$ IF Option .EQS. "HELP"
$ THEN  
$     SAY ""
$     SAY "The APACHE$SHUTDOWN.COM procedure accepts the following functions as the"
$     SAY "first parameter:"
$     SAY ""
$     SAY "HELP        Displays this help message."
$     SAY "STOP        Stops the web server process."
$     SAY "SHUTDOWN    Stops the web server process (alias for STOP)."
$     SAY "RESTART     Sends a restart message to the server."
$     SAY "GRACEFUL    Sends a restart message to the server allowing servers"
$     SAY "            to complete existing requests."
$     SAY ""
$     SAY "The default value for the first parameter is START."
$     SAY ""
$     SAY "This procedure accepts a file specification for the second parameter."
$     SAY "The file specification points to a configuration file defining the"
$     SAY "server's operating environment."
$     SAY ""
$     SAY "The default for the second parameter is APACHE$COMMON:[CONF]HTTPD.CONF."
$     SAY ""
$     GOTO EXIT
$ ENDIF
$!
$ IF Option .EQS. "SHUTDOWN"
$ THEN 
$     Option = "-k STOP"
$ ELSE
$     Option = "-k ''Option'"
$ ENDIF
$!
$!------------------------------------------------------------------------------
$! Shutdown the Apache server
$!------------------------------------------------------------------------------
$!
$ HTTPD -f 'ConfigFile' 'Option'
$!
$!------------------------------------------------------------------------------
$! Exit
$!------------------------------------------------------------------------------
$!
$EXIT:
$!
$ VERIFY = F$VERIFY (VERIFY)
$!
$ EXIT 
