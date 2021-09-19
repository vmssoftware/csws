$!
$!------------------------------------------------------------------------------
$! APACHE$SYMBOLS.COM
$!------------------------------------------------------------------------------
$!
$ Set NoOn
$ VERIFY = F$VERIFY (0)
$!
$!------------------------------------------------------------------------------
$! Define the apache symbols
$!------------------------------------------------------------------------------
$!
$ HTTPD :== $ APACHE$COMMON:[000000]APACHE$HTTPD.EXE
$ APACHE$DCL_BIN :== $ APACHE$COMMON:[000000]APACHE$DCL_BIN.EXE
$ APACHE$DCL_ENV :== $ APACHE$COMMON:[000000]APACHE$DCL_ENV.EXE
$ APACHE$DCL_RUN :== $ APACHE$COMMON:[000000]APACHE$DCL_RUN.EXE
$ APACHE$SET_CCL :== $ APACHE$COMMON:[000000]APACHE$SET_CCL.EXE
$ APACHE$FLIP_CCL :== $ APACHE$COMMON:[000000]APACHE$SET_CCL.EXE
$!
$!------------------------------------------------------------------------------
$! Exit
$!------------------------------------------------------------------------------
$!
$EXIT:
$!
$ VERIFY = F$VERIFY (VERIFY)
$ EXIT
