$!
$!------------------------------------------------------------------------------
$! OPENSSL_INIT_TERM.COM - OpenSSL Initialize Terminal procedure
$!------------------------------------------------------------------------------
$!
$ Verify = F$VERIFY (0)
$ Set NoOn
$ Set NoControl=Y
$!
$!------------------------------------------------------------------------------
$! Description 
$!------------------------------------------------------------------------------
$!
$! This procedure initializes the terminal attributes.
$!
$! The parameters used are:
$!
$!------------------------------------------------------------------------------
$! Define symbols
$!------------------------------------------------------------------------------
$!
$ On Control_Y THEN GOTO EXIT
$ Set Control=Y
$!
$ EDIT := EDIT
$!
$!------------------------------------------------------------------------------
$! Initialize the terminal with TPU
$!------------------------------------------------------------------------------
$!
$ IF F$SEARCH ("OPENSSL_COM:OPENSSL_EXIT_CMD.TPU") .EQS. ""
$ THEN 
$     OPEN /WRITE OFILE OPENSSL_COM:OPENSSL_EXIT_CMD.TPU
$     WRITE OFILE "EXIT"
$     CLOSE OFILE
$ ENDIF
$!
$ DEFINE /USER /NOLOG SYS$OUTPUT NL:
$ DEFINE /USER /NOLOG SYS$ERROR  NL:
$ DEFINE /USER /NOLOG SYS$INPUT  SYS$COMMAND
$ EDIT /TPU /COMMAND=OPENSS_COM:OPENSSL_EXIT_CMD.TPU
$!
$!------------------------------------------------------------------------------
$! Exit 
$!------------------------------------------------------------------------------
$!
$EXIT:
$!
$ DEFINE /USER /NOLOG SYS$ERROR  NL:
$ DEFINE /USER /NOLOG SYS$OUTPUT NL:
$ CLOSE OFILE
$!
$ Verify = F$VERIFY (Verify)
$!
$ EXIT
