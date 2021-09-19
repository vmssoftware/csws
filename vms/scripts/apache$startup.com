$!
$!------------------------------------------------------------------------------
$! APACHE$STARTUP.COM
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
$     Option = "START"
$ ELSE
$     Option = F$EDIT (P1, "TRIM,UPCASE")
$ ENDIF
$!
$! FIX for QUIX CASE QXCM1000994039
$ IF P2 .EQS. ""
$ THEN
$     ConfigFile = "APACHE$ROOT:[CONF]HTTPD.CONF"
$ ELSE
$     ConfigFile = F$EDIT (P2, "TRIM,UPCASE")
$ ENDIF
$!
$ IF F$TRNLNM ("APACHE$COMMON","LNM$SYSTEM_TABLE") .EQS. "" .AND. -
     F$SEARCH ("SYS$MANAGER:APACHE$LOGICALS.COM") .NES. "" -
  THEN @SYS$MANAGER:APACHE$LOGICALS.COM
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
$ OptionList = "HELP,RUN,START,RESTART,GRACEFUL"
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
$     SAY "The APACHE$STARTUP.COM procedure accepts the following functions as the"
$     SAY "first parameter:"
$     SAY ""
$     SAY "HELP        Displays this help message."
$     SAY "RUN         Runs the web server in the current process."
$     SAY "START       Starts the web server as a detached process."
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
$ IF Option .EQS. "RUN"
$ THEN  
$     Option = "-p 0 -""D"" ""ONE_PROCESS"""
$ ELSE
$     Option = "-k ''Option'"
$ ENDIF
$!
$!------------------------------------------------------------------------------
$! Startup the Apache server
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
