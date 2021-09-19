$ Set NoOn
$!
$!------------------------------------------------------------------------------
$! APACHE$LOGICALS.COM
$!------------------------------------------------------------------------------
$!
$ Verify = F$VERIFY (0)
$!
$!------------------------------------------------------------------------------
$! Define the installation location
$!------------------------------------------------------------------------------
$!
$ INSTALL_DEV = "SYS$SYSDEVICE:"
$ INSTALL_DIR = "[SYS0.SYSCOMMON]"
$!
$!------------------------------------------------------------------------------
$! Determine the Apache logical name table
$!------------------------------------------------------------------------------
$!
$ IF P1 .EQS. ""
$ THEN
$     LnmCmd = "Define /NoLog"
$ ELSE
$     LnmCmd = P1
$ ENDIF
$!
$ IF P2 .EQS. ""
$ THEN
$     LnmTbl = "/System /Executive"
$ ELSE
$     LnmTbl = P2
$ ENDIF
$!
$ IF P3 .EQS. ""
$ THEN
$     ShrImg = "Install"
$ ELSE
$     ShrImg = P3
$ ENDIF
$!
$ IF P4 .EQS. ""
$ THEN
$     CreateSpecificDir = "CREATE"
$ ELSE
$     CreateSpecificDir = "NOCREATE"
$ ENDIF
$!
$!------------------------------------------------------------------------------
$! Remove the Apache images
$!------------------------------------------------------------------------------
$!
$!  APACHE$APR_SHRP
$!
$ IF F$TRNLNM ("APACHE$APR_SHRP") .NES. "" .AND. -
     F$EDIT (ShrImg,"TRIM,UPCASE") .EQS. "UNINSTALL"
$ THEN
$     IF F$FILE_ATTRIBUTES ("APACHE$APR_SHRP","KNOWN")
$     THEN
$         DEFINE /USER /NOLOG SYS$ERROR NL:
$         DEFINE /USER /NOLOG SYS$OUTPUT NL:
$         INSTALL REMOVE APACHE$APR_SHRP
$         STATUS = $STATUS
$         IF F$MESSAGE (STATUS, "IDENT") .NES. "%NORMAL" .AND. -
             F$MESSAGE (STATUS, "IDENT") .NES. "%NOMSG"
$         THEN
$             WRITE SYS$OUTPUT F$MESSAGE (STATUS)
$         ENDIF
$     ENDIF
$ ENDIF
$!
$!  APACHE$APR_SHR
$!
$ IF F$TRNLNM ("APACHE$APR_SHR") .NES. "" .AND. -
     F$EDIT (ShrImg,"TRIM,UPCASE") .EQS. "UNINSTALL"
$ THEN
$     IF F$FILE_ATTRIBUTES ("APACHE$APR_SHR","KNOWN")
$     THEN
$         DEFINE /USER /NOLOG SYS$ERROR NL:
$         DEFINE /USER /NOLOG SYS$OUTPUT NL:
$         INSTALL REMOVE APACHE$APR_SHR
$         STATUS = $STATUS
$         IF F$MESSAGE (STATUS, "IDENT") .NES. "%NORMAL" .AND. -
             F$MESSAGE (STATUS, "IDENT") .NES. "%NOMSG"
$         THEN
$             WRITE SYS$OUTPUT F$MESSAGE (STATUS)
$         ENDIF
$     ENDIF
$ ENDIF
$!
$!  APACHE$APU_SHR
$!
$ IF F$TRNLNM ("APACHE$APU_SHR") .NES. "" .AND. -
     F$EDIT (ShrImg,"TRIM,UPCASE") .EQS. "UNINSTALL"
$ THEN
$     IF F$FILE_ATTRIBUTES ("APACHE$APU_SHR","KNOWN")
$     THEN
$         DEFINE /USER /NOLOG SYS$ERROR NL:
$         DEFINE /USER /NOLOG SYS$OUTPUT NL:
$         INSTALL REMOVE APACHE$APU_SHR
$         STATUS = $STATUS
$         IF F$MESSAGE (STATUS, "IDENT") .NES. "%NORMAL" .AND. -
             F$MESSAGE (STATUS, "IDENT") .NES. "%NOMSG"
$         THEN
$             WRITE SYS$OUTPUT F$MESSAGE (STATUS)
$         ENDIF
$     ENDIF
$ ENDIF
$!
$!  SUEXEC
$!
$ IF F$TRNLNM ("SUEXEC") .NES. "" .AND. -
     F$EDIT (ShrImg,"TRIM,UPCASE") .EQS. "UNINSTALL"
$ THEN
$     IF F$FILE_ATTRIBUTES ("SUEXEC","KNOWN")
$     THEN
$         DEFINE /USER /NOLOG SYS$ERROR NL:
$         DEFINE /USER /NOLOG SYS$OUTPUT NL:
$         INSTALL REMOVE SUEXEC
$         STATUS = $STATUS
$         IF F$MESSAGE (STATUS, "IDENT") .NES. "%NORMAL" .AND. -
             F$MESSAGE (STATUS, "IDENT") .NES. "%NOMSG"
$         THEN
$             WRITE SYS$OUTPUT F$MESSAGE (STATUS)
$         ENDIF
$     ENDIF
$ ENDIF
$!
$!------------------------------------------------------------------------------
$! Define the Apache logical names
$!------------------------------------------------------------------------------
$!
$ IF F$EDIT (F$EXTRACT (0,6,LnmCmd), "TRIM,UPCASE") .EQS. "DEFINE"
$ THEN
$     Host = F$EDIT(F$TRNLNM("TCPIP$INET_HOST"),"UPCASE")
$     IF Host .EQS. "" THEN -
$         Host = F$GETSYI("NODENAME")
$     IF Host .EQS. "" THEN -
$         Host = F$EDIT(F$GETSYI("SCSNODE"), "TRIM,COMPRESS")
$!
$     Device = F$PARSE(INSTALL_DEV,,,"DEVICE","NO_CONCEAL")
$     COMMON_ROOT = Device + INSTALL_DIR - "]" + ".APACHE.]"
$     SPECIFIC_ROOT = Device + INSTALL_DIR - "]" + ".APACHE.SPECIFIC." + Host + ".]"
$     SPECIFIC_DIR = Device + INSTALL_DIR - "]" + ".APACHE.SPECIFIC]" + Host + ".DIR"
$!
$     IF CreateSpecificDir .EQS. "CREATE"
$     THEN
$        IF F$SEARCH(SPECIFIC_DIR) .EQS. "" THEN GOSUB CreateSpecific
$     ENDIF
$!
$     'LnmCmd' 'LnmTbl' APACHE$COMMON    'COMMON_ROOT'/Translation=(Concealed,Terminal)
$     'LnmCmd' 'LnmTbl' APACHE$SPECIFIC  'SPECIFIC_ROOT'/Translation=(Concealed,Terminal)
$     'LnmCmd' 'LnmTbl' APACHE$ROOT      APACHE$SPECIFIC,APACHE$COMMON
$     'LnmCmd' 'LnmTbl' APACHE$HTTPD_SHR APACHE$COMMON:[000000]APACHE$HTTPD_SHR.EXE
$     'LnmCmd' 'LnmTbl' APACHE$APU_SHR   APACHE$COMMON:[000000]APACHE$APU_SHR.EXE
$     'LnmCmd' 'LnmTbl' APACHE$APR_SHR   APACHE$COMMON:[000000]APACHE$APR_SHR.EXE
$     'LnmCmd' 'LnmTbl' SUEXEC           APACHE$COMMON:[BIN]SUEXEC.EXE
$!
$     Suffix = ""
$!     IF F$GETSYI("ARCH_NAME") .EQS. "Alpha"
$!     THEN
$!         VMS_Vers = F$GETSYI("VERSION")
$!         IF F$LOCATE(".", VMS_Vers) .EQ. F$LENGTH(VMS_Vers)
$!         THEN
$!             Suffix = "_V8"
$!         ELSE
$!             Eos = F$LOCATE("-", VMS_Vers)
$!             IF Eos .EQ. F$LENGTH(VMS_Vers) THEN Eos = F$LOCATE(" ", VMS_Vers)
$!             IF F$EXTRACT(1,Eos-1,VMS_Vers) .LTS. "8.2" THEN Suffix = "_V7"
$!             IF F$EXTRACT(1,Eos-1,VMS_Vers) .GES. "8.2" THEN Suffix = "_V8"
$!         ENDIF
$!     ENDIF
$     'LnmCmd' 'LnmTbl' APACHE$APR_SHRP  APACHE$COMMON:[000000]APACHE$APR_SHRP'Suffix'.EXE
$ ELSE
$     'LnmCmd' 'LnmTbl' APACHE$APR_SHRP
$     'LnmCmd' 'LnmTbl' APACHE$APR_SHR
$     'LnmCmd' 'LnmTbl' APACHE$APU_SHR
$     'LnmCmd' 'LnmTbl' APACHE$HTTPD_SHR
$     'LnmCmd' 'LnmTbl' APACHE$ROOT
$     'LnmCmd' 'LnmTbl' APACHE$SPECIFIC
$     'LnmCmd' 'LnmTbl' APACHE$COMMON
$     'LnmCmd' 'LnmTbl' SUEXEC
$ ENDIF
$!
$!------------------------------------------------------------------------------
$! Install the Apache images
$!------------------------------------------------------------------------------
$!
$!  APACHE$APR_SHRP
$!
$ IF F$TRNLNM ("APACHE$APR_SHRP") .NES. "" .AND. -
     F$EDIT (ShrImg,"TRIM,UPCASE") .EQS. "INSTALL"
$ THEN
$     IF F$FILE_ATTRIBUTES ("APACHE$APR_SHRP","KNOWN")
$     THEN
$         InstallCmd = "REPLACE"
$     ELSE
$         InstallCmd = "ADD"
$     ENDIF
$     DEFINE /USER /NOLOG SYS$ERROR NL:
$     DEFINE /USER /NOLOG SYS$OUTPUT NL:
$     INSTALL 'InstallCmd' APACHE$APR_SHRP/HEADER_RESIDENT/OPEN/PROTECTED/SHARED
$     STATUS = $STATUS
$     IF F$MESSAGE (STATUS, "IDENT") .NES. "%NORMAL" .AND. -
         F$MESSAGE (STATUS, "IDENT") .NES. "%NOMSG"
$     THEN
$         WRITE SYS$OUTPUT F$MESSAGE (STATUS)
$     ENDIF
$ ENDIF
$!
$!  APACHE$APR_SHR
$!
$ IF F$TRNLNM ("APACHE$APR_SHR") .NES. "" .AND. -
     F$EDIT (ShrImg,"TRIM,UPCASE") .EQS. "INSTALL"
$ THEN
$     IF F$FILE_ATTRIBUTES ("APACHE$APR_SHR","KNOWN")
$     THEN
$         InstallCmd = "REPLACE"
$     ELSE
$         InstallCmd = "ADD"
$     ENDIF
$     DEFINE /USER /NOLOG SYS$ERROR NL:
$     DEFINE /USER /NOLOG SYS$OUTPUT NL:
$     INSTALL 'InstallCmd' APACHE$APR_SHR/HEADER_RESIDENT/OPEN/SHARED
$     STATUS = $STATUS
$     IF F$MESSAGE (STATUS, "IDENT") .NES. "%NORMAL" .AND. -
         F$MESSAGE (STATUS, "IDENT") .NES. "%NOMSG"
$     THEN
$         WRITE SYS$OUTPUT F$MESSAGE (STATUS)
$     ENDIF
$ ENDIF
$!
$!  APACHE$APU_SHR
$!
$ IF F$TRNLNM ("APACHE$APU_SHR") .NES. "" .AND. -
     F$EDIT (ShrImg,"TRIM,UPCASE") .EQS. "INSTALL"
$ THEN
$     IF F$FILE_ATTRIBUTES ("APACHE$APU_SHR","KNOWN")
$     THEN
$         InstallCmd = "REPLACE"
$     ELSE
$         InstallCmd = "ADD"
$     ENDIF
$     DEFINE /USER /NOLOG SYS$ERROR NL:
$     DEFINE /USER /NOLOG SYS$OUTPUT NL:
$     INSTALL 'InstallCmd' APACHE$APU_SHR/HEADER_RESIDENT/OPEN/SHARED
$     STATUS = $STATUS
$     IF F$MESSAGE (STATUS, "IDENT") .NES. "%NORMAL" .AND. -
         F$MESSAGE (STATUS, "IDENT") .NES. "%NOMSG"
$     THEN
$         WRITE SYS$OUTPUT F$MESSAGE (STATUS)
$     ENDIF
$ ENDIF
$!
$!  SUEXEC
$!
$ IF F$TRNLNM ("SUEXEC") .NES. "" .AND. -
     F$EDIT (ShrImg,"TRIM,UPCASE") .EQS. "INSTALL"
$ THEN
$     IF F$FILE_ATTRIBUTES ("SUEXEC","KNOWN")
$     THEN
$         InstallCmd = "REPLACE"
$     ELSE
$         InstallCmd = "ADD"
$     ENDIF
$     DEFINE /USER /NOLOG SYS$ERROR NL:
$     DEFINE /USER /NOLOG SYS$OUTPUT NL:
$     INSTALL 'InstallCmd' SUEXEC/HEADER_RESIDENT/OPEN/SHARED/PRIV=(IMPERSONATE,SYSPRV)
$     STATUS = $STATUS
$     IF F$MESSAGE (STATUS, "IDENT") .NES. "%NORMAL" .AND. -
         F$MESSAGE (STATUS, "IDENT") .NES. "%NOMSG"
$     THEN
$         WRITE SYS$OUTPUT F$MESSAGE (STATUS)
$     ENDIF
$ ENDIF
$!
$!  SYS$LIBRARY:LDAP$SHR.EXE
$!
$ IF F$EDIT (ShrImg,"TRIM,UPCASE") .EQS. "INSTALL"
$ THEN
$     IF .NOT. F$FILE_ATTRIBUTES ("SYS$LIBRARY:LDAP$SHR.EXE","KNOWN")
$     THEN
$         InstallCmd = "ADD"
$         DEFINE /USER /NOLOG SYS$ERROR NL:
$         DEFINE /USER /NOLOG SYS$OUTPUT NL:
$         INSTALL 'InstallCmd' SYS$LIBRARY:LDAP$SHR.EXE/HEADER_RESIDENT/OPEN/SHARED
$         STATUS = $STATUS
$         IF F$MESSAGE (STATUS, "IDENT") .NES. "%NORMAL" .AND. -
             F$MESSAGE (STATUS, "IDENT") .NES. "%NOMSG"
$         THEN
$             WRITE SYS$OUTPUT F$MESSAGE (STATUS)
$         ENDIF
$     ENDIF
$ ENDIF
$!
$!------------------------------------------------------------------------------
$! Exit
$!------------------------------------------------------------------------------
$!
$ Verify = F$VERIFY (Verify)
$ Exit

$!
$!------------------------------------------------------------------------------
$! CreateSpecific
$!
$!	Create the specific directories for the current node
$!
$! Inputs:
$!	Host - Hostname (Node name) of the current node
$!
$!------------------------------------------------------------------------------
$!
$CreateSpecific:
$	Parse_Style = F$GETJPI("", "PARSE_STYLE_PERM")
$	SET PROCESS/PARSE_STYLE=EXTENDED
$
$	InsDir = COMMON_ROOT - ".]"
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host']
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.BIN]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.CGI-BIN]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.CONF]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.CONF.SSL_CRL]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.CONF.SSL_CRT]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.CONF.SSL_CRS]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.CONF.SSL_KEY]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.CONF.SSL_PRM]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.error]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.error.include]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.developer]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.faq]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.howto]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.images]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.misc]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.mod]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.platform]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.programs]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.rewrite]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.ssl]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.style]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.vhosts]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.style.css]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.style.lang]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.style.latex]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.style.scripts]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.style.xsl]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.HTDOCS.manual.style.xsl.util]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.ICONS]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.ICONS.small]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.LOGS]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.MODULES]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.OPENSSL]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.OPENSSL.COM]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.OPENSSL.CRT]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.OPENSSL.CSR]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.OPENSSL.DB]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.OPENSSL.EXE]
$	CREATE /DIRECTORY /NOLOG 'InsDir'.SPECIFIC.'Host'.OPENSSL.KEY]
$
$	SET PROCESS/PARSE_STYLE='Parse_Style'
$	Return
