$ Set NoOn
$!
$!------------------------------------------------------------------------------
$! APACHE$SETUP.COM
$!------------------------------------------------------------------------------
$!
$ VERIFY = F$VERIFY (0)
$!
$!------------------------------------------------------------------------------
$! Define symbols & logicals for this procedure
$!------------------------------------------------------------------------------
$!
$ IF F$TRNLNM ("APACHE$COMMON","LNM$SYSTEM_TABLE") .EQS. "" .AND. -
     F$SEARCH ("SYS$MANAGER:APACHE$LOGICALS.COM") .NES. "" -
  THEN @SYS$MANAGER:APACHE$LOGICALS.COM
$!
$ If F$SEARCH ("SYS$MANAGER:APACHE$SYMBOLS.COM") .NES. "" -
  Then @SYS$MANAGER:APACHE$SYMBOLS.COM
$!
$ Define /NoLog DECC$EFS_CASE_PRESERVE  ENABLED
$ Define /NoLog DECC$EFS_CASE_SPECIAL   ENABLED
$ Define /NoLog DECC$EFS_CHARSET        ENABLED
$ Define /NoLog DECC$FILE_SHARING 	ENABLED
$!
$!------------------------------------------------------------------------------
$! Exit
$!------------------------------------------------------------------------------
$!
$EXIT:
$!
$ VERIFY = F$VERIFY (VERIFY)
$ EXIT
