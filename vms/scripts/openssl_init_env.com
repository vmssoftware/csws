$!
$!------------------------------------------------------------------------------
$! OPENSSL_INIT_ENV.COM - Apache OpenSSL Initialize Environment
$!------------------------------------------------------------------------------
$!
$ Verify = F$VERIFY (0)
$ Set NoOn
$!
$!------------------------------------------------------------------------------
$! Description 
$!------------------------------------------------------------------------------
$!
$! This procedure sets up the OpenSSL environment logicals & symbols.
$!
$! The parameters used are:
$!
$! 	P1	- Logical Name table to use (i.e. "SYSTEM" or "JOB")
$!
$!------------------------------------------------------------------------------
$! Define local symbols
$!------------------------------------------------------------------------------
$!
$ APACHE_ROOT = F$TRNLNM ("APACHE$COMMON")
$ APACHE_DEV = F$PARSE (APACHE_ROOT,"DUMMY$DEV:",,"DEVICE")
$ APACHE_DIR = F$PARSE (APACHE_ROOT,"[DUMMY$DIR]",,"DIRECTORY")
$ OPENSSL_ROOT = APACHE_DEV + APACHE_DIR - ".][DUMMY$DIR]" + ".OPENSSL.]"
$!
$!------------------------------------------------------------------------------
$! Define logicals
$!------------------------------------------------------------------------------
$!
$ DEFINE /NOLOG'P1 OPENSSL_ROOT		'OPENSSL_ROOT' /TRANS=(Conceal,Terminal)
$ DEFINE /NOLOG'P1 OPENSSL_CA_CONF	APACHE$ROOT:[CONF]OPENSSL_CA.CONF
$ DEFINE /NOLOG'P1 OPENSSL_CONF		APACHE$ROOT:[CONF]OPENSSL.CONF
$ DEFINE /NOLOG'P1 OPENSSL_EXE		OPENSSL_ROOT:[EXE]
$ DEFINE /NOLOG'P1 OPENSSL_COM		OPENSSL_ROOT:[COM]
$ DEFINE /NOLOG'P1 OPENSSL_CRT		OPENSSL_ROOT:[CRT]
$ DEFINE /NOLOG'P1 OPENSSL_CSR		OPENSSL_ROOT:[CSR]
$ DEFINE /NOLOG'P1 OPENSSL_KEY		OPENSSL_ROOT:[KEY]
$ DEFINE /NOLOG'P1 OPENSSL_DB		OPENSSL_ROOT:[DB]
$!
$!------------------------------------------------------------------------------
$! Define foreign symbols
$!------------------------------------------------------------------------------
$!
$ OPENSSL	:== $ OPENSSL_EXE:OPENSSL
$ HOSTADDR	:== $ OPENSSL_EXE:HOSTADDR
$ HOSTNAME	:== $ OPENSSL_EXE:HOSTNAME
$!
$!------------------------------------------------------------------------------
$! Exit
$!------------------------------------------------------------------------------
$!
$ EXIT
