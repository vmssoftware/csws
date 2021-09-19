$!
$!------------------------------------------------------------------------------
$! OPENSSL_AUTO_CERT.COM - OpenSSL Automatic Self-Signed Certificate procedure
$!------------------------------------------------------------------------------
$!
$ Verify = F$VERIFY (0)
$!
$ Set NoOn
$ Set NoControl=Y
$!
$!------------------------------------------------------------------------------
$! Define Symbols
$!------------------------------------------------------------------------------
$!
$ OPENSSL := $ APACHE$ROOT:[OPENSSL.EXE]OPENSSL.EXE 
$ HOSTNAME := $ APACHE$ROOT:[OPENSSL.EXE]HOSTNAME.EXE 
$!
$ HOSTNAME -s HOST_NAME
$ PID = F$GETJPI ("","PID")
$ USER = F$EDIT (F$GETJPI ("","USERNAME"),"TRIM")
$ KEY_FILE = "APACHE$ROOT:[CONF.SSL_KEY]SERVER.KEY"
$ CRT_FILE = "APACHE$ROOT:[CONF.SSL_CRT]SERVER.CRT"
$!
$!------------------------------------------------------------------------------
$! Create a Temporary OpenSSL Configuration
$!------------------------------------------------------------------------------
$!
$ OPEN /WRITE CFILE SYS$LOGIN:OPENSSL_'PID'.CNF
$ WRITE CFILE "[req]"
$ WRITE CFILE "default_bits = 1024"
$ WRITE CFILE "distinguished_name = REQ_distinguished_name"
$ WRITE CFILE "[REQ_distinguished_name]"
$ WRITE CFILE "countryName = Country Name ?"
$ WRITE CFILE "countryName_default = "
$ WRITE CFILE "stateOrProvinceName = State or Province Name ?"
$ WRITE CFILE "stateOrProvinceName_default = "
$ WRITE CFILE "localityName = City Name ?"
$ WRITE CFILE "localityName_default = "
$ WRITE CFILE "0.organizationName = Organization Name ?"
$ WRITE CFILE "0.organizationName_default = "
$ WRITE CFILE "organizationalUnitName = Organization Unit Name ?
$ WRITE CFILE "organizationalUnitName_default = "
$ WRITE CFILE "commonName = Common Name ?"
$ WRITE CFILE "commonName_default = ''HOST_NAME'"
$ WRITE CFILE "emailAddress = Email Address ?"
$ WRITE CFILE "emailAddress_default = ''USER'@''HOST_NAME'"
$ CLOSE CFILE
$!
$!------------------------------------------------------------------------------
$! Create the Self-Signed Server Certificiate
$!------------------------------------------------------------------------------
$!
$ DEFINE /USER /NOLOG SYS$ERROR  NL:
$ DEFINE /USER /NOLOG SYS$OUTPUT NL:
$ SHOW SYSTEM /FULL /OUT=SYS$LOGIN:OPENSSL_'PID'.RND
$!
$ OPEN /WRITE OFILE SYS$LOGIN:OPENSSL_'PID'.COM
$ WRITE OFILE "$ DEFINE /USER /NOLOG RANDFILE    SYS$LOGIN:OPENSSL_''PID'.RND"
$ WRITE OFILE "$ DEFINE /USER /NOLOG SYS$ERROR   SYS$LOGIN:OPENSSL_''PID'.LOG"
$ WRITE OFILE "$ DEFINE /USER /NOLOG SYS$OUTPUT  SYS$LOGIN:OPENSSL_''PID'.LOG"
$ WRITE OFILE "$ DEFINE /USER /NOLOG SYS$COMMAND SYS$INPUT"
$ WRITE OFILE "$ OPENSSL req -nodes -new -days 30 -x509 -config SYS$LOGIN:OPENSSL_''PID'.CNF -keyout ''KEY_FILE' -out ''CRT_FILE'"
$ WRITE OFILE ""
$ WRITE OFILE ""
$ WRITE OFILE ""
$ WRITE OFILE ""
$ WRITE OFILE ""
$ WRITE OFILE ""
$ WRITE OFILE ""
$ CLOSE OFILE
$!
$ @SYS$LOGIN:OPENSSL_'PID'.COM
$!
$ DELETE /NOLOG /NOCONFIRM SYS$LOGIN:OPENSSL_'PID'.CNF;*
$ DELETE /NOLOG /NOCONFIRM SYS$LOGIN:OPENSSL_'PID'.RND;*
$ DELETE /NOLOG /NOCONFIRM SYS$LOGIN:OPENSSL_'PID'.COM;*
$!
$ DEFINE /USER /NOLOG SYS$ERROR  NL:
$ DEFINE /USER /NOLOG SYS$OUTPUT NL:
$ SEARCH SYS$LOGIN:OPENSSL_'PID'.LOG /OUT=SYS$LOGIN:OPENSSL_'PID'.ERR ":error:"
$!
$ IF F$SEARCH ("SYS$LOGIN:OPENSSL_''PID'.ERR") .NES. "" 
$ THEN 
$     IF F$FILE_ATTRIBUTE ("SYS$LOGIN:OPENSSL_''PID'.ERR","ALQ") .NE. 0
$     THEN 
$         TYPE SYS$LOGIN:OPENSSL_'PID'.LOG
$     ENDIF
$     DELETE /NOLOG /NOCONFIRM SYS$LOGIN:OPENSSL_'PID'.ERR;*
$ ENDIF
$!
$ DELETE /NOLOG /NOCONFIRM SYS$LOGIN:OPENSSL_'PID'.LOG;*
$!
$!------------------------------------------------------------------------------
$! Exit
$!------------------------------------------------------------------------------
$!
$EXIT:
$!
$ Verify = F$VERIFY (Verify)
$!
$ EXIT
