$!
$!------------------------------------------------------------------------------
$! APACHE$DCL.COM
$!------------------------------------------------------------------------------
$!
$ Set NoOn
$ VERIFY = F$VERIFY (0)
$!
$!------------------------------------------------------------------------------
$! Define symbols & logicals for this procedure
$!------------------------------------------------------------------------------
$!
$ IF F$SEARCH ("SYS$MANAGER:APACHE$SYMBOLS.COM") .NES. "" THEN @SYS$MANAGER:APACHE$SYMBOLS.COM
$ IF F$SEARCH ("APACHE$COMMON:APACHE$LOGIN.COM") .NES. "" THEN @APACHE$COMMON:APACHE$LOGIN.COM
$ IF F$TYPE (APACHE$DCL_CMD) .NES. "" THEN DELETE /SYMBOL /GLOBAL APACHE$DCL_CMD
$ DEFINE /NOLOG /JOB APACHE$DCL_SYNC_'F$GETJPI("","PID")' 1
$ DEFINE /NOLOG APACHE$INPUT SYS$COMMAND
$ DEFINE /NOLOG SYS$INPUT SYS$COMMAND
$ APACHE$DCL_PROC_HDL := 'P1'
$ APACHE$DCL_PROC_CTX := 'P2'
$ On ERROR THEN GOTO EXIT
$!
$!------------------------------------------------------------------------------
$! Execute the DCL Run routine
$!------------------------------------------------------------------------------
$!
$ APACHE$DCL_ENV -c
$!
$ APACHE$DCL_RUN
$!
$ APACHE$DCL_ENV -d
$!
$!------------------------------------------------------------------------------
$! Exit
$!------------------------------------------------------------------------------
$!
$EXIT:
$!
$ SAVED_STATUS = $STATUS
$!
$ DEASSIGN /JOB APACHE$DCL_SYNC_'F$GETJPI("","PID")'
$!
$ VERIFY = F$VERIFY (VERIFY)
$ EXIT 'SAVED_STATUS'
