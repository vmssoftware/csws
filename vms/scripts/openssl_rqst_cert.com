$!
$!------------------------------------------------------------------------------
$! OPENSSL_SRVR_CERT.COM - OpenSSL Certificate Request procedure
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
$! This procedure prompts the user through creating a Certificate Request.
$!
$! There are no parameters used.
$!
$!------------------------------------------------------------------------------
$! Define symbols
$!------------------------------------------------------------------------------
$!
$ DELETE := DELETE
$ SAY := WRITE SYS$OUTPUT
$ ASK := READ SYS$COMMAND /END_OF_FILE=EXIT /PROMPT=
$ PID = F$GETJPI ("","PID")
$ TT_NOECHO = F$GETDVI ("TT:","TT_NOECHO")
$ On Control_Y THEN GOTO EXIT
$ Set Control=Y
$!
$ TT_ROWS = F$GETDVI ("TT:","TT_PAGE")
$ TT_COLS = F$GETDVI ("TT:","DEVBUFSIZ")
$!
$ GET_USER_DATA := CALL GET_USER_DATA
$ SET_USER_DATA := CALL SET_USER_DATA
$ DEL_USER_DATA := CALL DEL_USER_DATA
$ INIT_TERM := @OPENSSL_COM:OPENSSL_INIT_TERM
$ SHOW_FILE := @OPENSSL_COM:OPENSSL_SHOW_FILE 
$ OPENSSL_CONF_FILE = F$TRNLNM ("OPENSSL_CONF")
$ GET_CONF_DATA := @OPENSSL_COM:OPENSSL_CONF_UTIL 'OPENSSL_CONF_FILE' GET
$ SET_CONF_DATA := @OPENSSL_COM:OPENSSL_CONF_UTIL 'OPENSSL_CONF_FILE' SET
$!
$ ESC[0,8] = 27 	! Set the Escape Character
$ BELL[0,8] = 7 	! Ring the terminal Bell
$ RED = 1		! Color - Red
$ FGD = 30		! Foreground
$ BGD = 0		! Background
$ CSCR = ESC + "[2J"	! Clear the Screen 
$ CEOS = ESC + "[0J"	! Clear to the End of the Screen 
$ CEOL = ESC + "[0K"	! Clear to the End of the Line
$ NORM = ESC + "[0m"	! Turn Attributes off
$ BLNK = ESC + "[5m"    ! Turn on BLINK Attribute
$ WIDE = ESC + "#6"     ! Turn on WIDE Attribute
$!
$!------------------------------------------------------------------------------
$! Run the OpenSSL setup if it hasn't been run yet
$!------------------------------------------------------------------------------
$!
$ IF F$TRNLNM ("OPENSSL_ROOT") .EQS. ""
$ THEN
$     IF F$SEARCH ("APACHE$ROOT:[OPENSSL.COM]OPENSSL_INIT_ENV.COM") .NES. ""
$     THEN 
$         @APACHE$ROOT:[OPENSSL.COM]OPENSSL_INIT_ENV.COM
$     ELSE
$         SAY BELL, "Unable to locate APACHE$ROOT:[OPENSSL.COM]OPENSSL_INIT_ENV.COM ..."
$	  GOTO EXIT
$     ENDIF
$ ENDIF
$!
$!------------------------------------------------------------------------------
$! Display the Page Header
$!------------------------------------------------------------------------------
$!
$ INIT_TERM
$ BCOLOR = BGD
$ FCOLOR = FGD + RED
$ COLOR = ESC + "[''BCOLOR';''FCOLOR'm"
$!
$ TEXT = "OpenSSL Certificate Tool"
$ COL = (TT_COLS - (F$LENGTH (TEXT) * 2)) / 4
$!
$ SAY ESC + "[01;01H", CSCR
$ SAY ESC + "[02;''COL'H", COLOR, WIDE, TEXT, NORM
$!
$ TEXT = "Create Certificate Request"
$ COL = (TT_COLS - F$LENGTH (TEXT)) / 2
$!
$ SAY ESC + "[04;01H"
$ SAY ESC + "[04;''COL'H", COLOR, TEXT, NORM
$!
$ ROW = 6
$ COL = 2
$ TOP_ROW = ROW
$ MSG_ROW = TT_ROWS - 1
$!
$!------------------------------------------------------------------------------
$! Initialize the Request Data
$!------------------------------------------------------------------------------
$!
$ IF F$SEARCH ("''OPENSSL_CONF_FILE'") .NES. ""
$ THEN 
$     SAY ESC + "[''MSG_ROW';01H", BLNK, " Reading Configuration ...", NORM
$ ELSE
$     SAY ESC + "[''MSG_ROW';01H", BLNK, " Initializing Configuration ...", NORM
$ ENDIF
$!
$ _request_name = "req"
$!
$ _distinguished_name = "REQ_distinguished_name"
$ _distinguished_name_upd = "Y"
$!
$ _default_bits = "1024"
$ _default_bits_min = "384"
$ _default_bits_upd = "Y"
$!
$ _default_keyfile = "OPENSSL_ROOT:[KEY]SERVER.KEY"
$ _default_keyfile_upd = "Y"
$!
$ _default_csrfile = "OPENSSL_ROOT:[CSR]SERVER.CSR"
$ _default_csrfile_upd = "Y"
$!
$ _countryName_prompt = "Country Name ?"
$ _countryName_min = "2"
$ _countryName_max = "2"
$ _countryName_default = "US"
$ _countryName_upd = "Y"
$ _countryName_cnt = 4
$!
$ _stateOrProvinceName_prompt = "State or Province Name ?"
$ _stateOrProvinceName_default = ""
$ _stateOrProvinceName_upd = "Y"
$ _stateOrProvinceName_cnt = 2
$!
$ _localityName_prompt = "City Name ?"
$ _localityName_default = ""
$ _localityName_upd = "Y"
$ _localityName_cnt = 2
$!
$ _0organizationName_prompt = "Organization Name ?"
$ _0organizationName_default = ""
$ _0organizationName_upd = "Y"
$ _0organizationName_cnt = 2
$!
$ _organizationalUnitName_prompt = "Organization Unit Name ?"
$ _organizationalUnitName_default = ""
$ _organizationalUnitName_upd = "Y"
$ _organizationalUnitName_cnt = 2
$!
$ _commonName_prompt = "Common Name ?"
$ _commonName_max = "64"
$ HOSTNAME -s _commonName_default
$ _commonName_upd = "Y"
$ _commonName_cnt = 3
$!
$ _emailAddress_prompt = "Email Address ?"
$ _emailAddress_max = "40"
$ _emailAddress_default = "webmaster@''_commonName_default'"
$ _emailAddress_upd = "Y"
$ _emailAddress_cnt = 3
$!
$ IF F$SEARCH ("''OPENSSL_CONF_FILE'") .NES. ""
$ THEN 
$     GET_CONF_DATA "[''_request_name']#distinguished_name"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _distinguished_name = OPENSSL_CONF_DATA
$         _distinguished_name_upd = "N"
$     ENDIF
$!
$     GET_CONF_DATA "[''_request_name']#default_bits"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _default_bits = OPENSSL_CONF_DATA
$         _default_bits_upd = "N"
$     ENDIF
$!
$     GET_CONF_DATA "[''_request_name']#default_keyfile"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _default_keyfile = F$PARSE (OPENSSL_CONF_DATA,"OPENSSL_ROOT:",,"DEVICE") + -
 		             F$PARSE (OPENSSL_CONF_DATA,"[KEY]",,"DIRECTORY") + -
 		             F$PARSE (OPENSSL_CONF_DATA,"SERVER",,"NAME") + -
 		             F$PARSE (OPENSSL_CONF_DATA,".KEY",,"TYPE") 
$         _default_keyfile_upd = "N"
$     ENDIF
$!
$     GET_CONF_DATA "[''_request_name']#default_csrfile"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _default_csrfile = F$PARSE (OPENSSL_CONF_DATA,"OPENSSL_ROOT:",,"DEVICE") + -
 		             F$PARSE (OPENSSL_CONF_DATA,"[CSR]",,"DIRECTORY") + -
 		             F$PARSE (OPENSSL_CONF_DATA,"SERVER",,"NAME") + -
 		             F$PARSE (OPENSSL_CONF_DATA,".CSR",,"TYPE") 
$         _default_csrfile_upd = "N"
$     ENDIF
$!
$     CTR = 0
$     GET_CONF_DATA "[''_distinguished_name']#countryName"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _countryName_prompt = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     GET_CONF_DATA "[''_distinguished_name']#countryName_min"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _countryName_min = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     GET_CONF_DATA "[''_distinguished_name']#countryName_max"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _countryName_max = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     GET_CONF_DATA "[''_distinguished_name']#countryName_default"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _countryName_default = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     IF _countryName_cnt .EQ. CTR THEN _countryName_upd = "N"
$!
$     CTR = 0
$     GET_CONF_DATA "[''_distinguished_name']#stateOrProvinceName"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _stateOrProvinceName_prompt = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     GET_CONF_DATA "[''_distinguished_name']#stateOrProvinceName_default"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _stateOrProvinceName_default = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     IF _stateOrProvinceName_cnt .EQ. CTR THEN _stateOrProvinceName_upd = "N"
$!
$     CTR = 0
$     GET_CONF_DATA "[''_distinguished_name']#localityName"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _localityName_prompt = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     GET_CONF_DATA "[''_distinguished_name']#localityName_default"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _localityName_default = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     IF _localityName_cnt .EQ. CTR THEN _localityName_upd = "N"
$!
$     CTR = 0
$     GET_CONF_DATA "[''_distinguished_name']#0.organizationName"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _0organizationName_prompt = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     GET_CONF_DATA "[''_distinguished_name']#0.organizationName_default"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _0organizationName_default = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     IF _0organizationName_cnt .EQ. CTR THEN _0organizationName_upd = "N"
$!
$     CTR = 0
$     GET_CONF_DATA "[''_distinguished_name']#organizationalUnitName"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _organizationalUnitName_prompt = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     GET_CONF_DATA "[''_distinguished_name']#organizationalUnitName_default"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _organizationalUnitName_default = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     IF _organizationalUnitName_cnt .EQ. CTR THEN _organizationalUnitName_upd = "N"
$!
$     CTR = 0
$     GET_CONF_DATA "[''_distinguished_name']#commonName"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _commonName_prompt = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     GET_CONF_DATA "[''_distinguished_name']#commonName_max"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _commonName_max = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     GET_CONF_DATA "[''_distinguished_name']#commonName_default"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _commonName_default = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     IF _commonName_cnt .EQ. CTR THEN _commonName_upd = "N"
$!
$     CTR = 0
$     GET_CONF_DATA "[''_distinguished_name']#emailAddress"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _emailAddress_prompt = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     GET_CONF_DATA "[''_distinguished_name']#emailAddress_max"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _emailAddress_max = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     GET_CONF_DATA "[''_distinguished_name']#emailAddress_default"
$     IF OPENSSL_CONF_DATA .NES. ""
$     THEN 
$         _emailAddress_default = OPENSSL_CONF_DATA
$         CTR = CTR + 1
$     ENDIF
$     IF _emailAddress_cnt .EQ. CTR THEN _emailAddress_upd = "N"
$ ENDIF
$!
$ SET_USER_DATA "[]#encrypt_key#-#N#Encrypt Private Key ?#S##1##Y#N"
$ SET_USER_DATA "[]#pem_pass_phrase#-##PEM Pass Phrase ?#P#1###Y#Y"
$ SET_USER_DATA "[''_request_name']#default_bits#D#''_default_bits'#Encryption Bits ?#I#''_default_bits_min'##''_default_bits_upd'#Y#N"
$ SET_USER_DATA "[''_request_name']#default_keyfile#D#''_default_keyfile'#Certificate Key File ?#F###''_default_keyfile_upd'#Y#N"
$ SET_USER_DATA "[''_request_name']#default_csrfile#D#''_default_csrfile'#Certificate Request File ?#F###''_default_csrfile_upd'#Y#N"
$ SET_USER_DATA "[''_request_name']#distinguished_name#D#''_distinguished_name'##S###''_distinguished_name_upd'#N#N"
$ SET_USER_DATA "[''_distinguished_name']#countryName#P#''_countryName_default'#''_countryName_prompt'#S#''_countryName_min'#''_countryName_max'#''_countryName_upd'#Y#N" 
$ SET_USER_DATA "[''_distinguished_name']#stateOrProvinceName#P#''_stateOrProvinceName_default'#''_stateOrProvinceName_prompt'#S###''_stateOrProvinceName_upd'#Y#N"
$ SET_USER_DATA "[''_distinguished_name']#localityName#P#''_localityName_default'#''_localityName_prompt'#S###''_localityName_upd'#Y#N"
$ SET_USER_DATA "[''_distinguished_name']#0.organizationName#P#''_0organizationName_default'#''_0organizationName_prompt'#S###''_0organizationName_upd'#Y#N"
$ SET_USER_DATA "[''_distinguished_name']#organizationalUnitName#P#''_organizationalUnitName_default'#''_organizationalUnitName_prompt'#S###''_organizationUnitName_upd'#Y#N"
$ SET_USER_DATA "[''_distinguished_name']#commonName#P#''_commonName_default'#''_commonName_prompt'#S##''_commonName_max'#''_commonName_upd'#Y#N"
$ SET_USER_DATA "[''_distinguished_name']#emailAddress#P#''_emailAddress_default'#''_emailAddress_prompt'#S##''_emailAddress_max'#''_emailAddress_upd'#Y#N"
$ SET_USER_DATA "[]#display_certificate#-#N#Display the Certificate ?#S##1##Y#N"
$!
$ SAY ESC + "[''MSG_ROW';01H", CEOS
$!
$!------------------------------------------------------------------------------
$! Confirm/Update the OpenSSL Configuration Data
$!------------------------------------------------------------------------------
$!
$ CTR = 1
$!
$PROMPT_LOOP:
$!
$ IF CTR .LE. OPENSSL_USER_DATA_MAX
$ THEN 
$     KEY = F$ELEMENT (0,"#",OPENSSL_USER_DATA_'CTR') ! Key Name
$     ITM = F$ELEMENT (1,"#",OPENSSL_USER_DATA_'CTR') ! Item Name
$     VAL = F$ELEMENT (2,"#",OPENSSL_USER_DATA_'CTR') ! Item Value Contains Default or Prompt
$     DEF = F$ELEMENT (3,"#",OPENSSL_USER_DATA_'CTR') ! Default Value
$     PRM = F$ELEMENT (4,"#",OPENSSL_USER_DATA_'CTR') ! Prompt Value
$     TYP = F$ELEMENT (5,"#",OPENSSL_USER_DATA_'CTR') ! Value Type
$     MIN = F$ELEMENT (6,"#",OPENSSL_USER_DATA_'CTR') ! Value Minimum Length
$     MAX = F$ELEMENT (7,"#",OPENSSL_USER_DATA_'CTR') ! Value Maximum Length
$     UPD = F$ELEMENT (8,"#",OPENSSL_USER_DATA_'CTR') ! Entry Updated ?
$     REQ = F$ELEMENT (9,"#",OPENSSL_USER_DATA_'CTR') ! Entry Required for Input ?
$     CFM = F$ELEMENT (10,"#",OPENSSL_USER_DATA_'CTR')! Confirm Input  ?
$!
$! The PEM Pass Phrase prompt is dependant on the answer to encrypt the private key
$!
$     IF KEY .EQS. "[]" .AND. ITM .EQS. "pem_pass_phrase"
$     THEN
$         GET_USER_DATA "[]#encrypt_key"
$ 	  _encrypt_key = OPENSSL_USER_DATA
$         IF F$EDIT (_encrypt_key,"UPCASE") .NES. "Y"
$	  THEN
$             CTR = CTR + 1
$             GOTO PROMPT_LOOP
$	  ENDIF
$     ENDIF
$!
$     CONFIRMED = 0
$     IF REQ .EQS. "N"
$     THEN 
$         CTR = CTR + 1
$         GOTO PROMPT_LOOP
$     ENDIF
$     IF ROW .GT. MSG_ROW - 2
$     THEN 
$         SAY ESC + "[''TOP_ROW';01H", CEOS
$	  ROW = TOP_ROW
$     ENDIF
$!
$CONFIRM_LOOP:
$!
$     IF PRM .EQS. "" 
$     THEN 
$         PROMPT = ESC + "[''ROW';''COL'H''ITM' ? [''DEF'] ''CEOL'"
$     ELSE
$         PROMPT = ESC + "[''ROW';''COL'H''PRM' [''DEF'] ''CEOL'"
$     ENDIF
$     IF TYP .EQS. "P" THEN SET TERMINAL /NOECHO
$     ASK "''PROMPT'" ANS /END_OF_FILE=EXIT
$     IF TYP .EQS. "P" THEN SET TERMINAL /ECHO
$     ANS = F$EDIT (ANS,"TRIM")
$     IF ANS .EQS. "" THEN ANS = DEF
$     IF TYP .EQS. "F"
$     THEN
$         ANS = F$PARSE ("''ANS'","''DEF'",,,"SYNTAX_ONLY")
$     ENDIF
$     IF TYP .EQS. "I" .AND. F$TYPE (ANS) .NES. "INTEGER"
$     THEN 
$         CALL INVALID_ENTRY
$         SAY ESC + "[''ROW';01H", CEOS
$         GOTO PROMPT_LOOP
$     ENDIF
$     IF TYP .EQS. "I" .AND. -
         ((MIN .NES. "" .AND. F$INTEGER (ANS) .LT. F$INTEGER(MIN)) .OR. -
          (MAX .NES. "" .AND. F$INTEGER (ANS) .GT. F$INTEGER(MAX)))
$     THEN 
$         CALL INVALID_ENTRY
$         SAY ESC + "[''ROW';01H", CEOS
$	  GOTO PROMPT_LOOP
$     ENDIF
$     IF (TYP .EQS. "S" .OR. TYP .EQS. "P") .AND. -
         ((MIN .NES. "" .AND. F$LENGTH (ANS) .LT. F$INTEGER(MIN)) .OR. -
          (MAX .NES. "" .AND. F$LENGTH (ANS) .GT. F$INTEGER(MAX)))
$     THEN 
$         CALL INVALID_ENTRY
$         SAY ESC + "[''ROW';01H", CEOS
$	  IF TYP .EQS. "S" THEN GOTO PROMPT_LOOP
$         IF TYP .EQS. "P" THEN GOTO CONFIRM_LOOP
$     ENDIF
$     ROW = ROW + 1
$     IF CFM .EQS. "Y"
$     THEN
$         IF CONFIRMED .EQ. 0
$	  THEN
$	      CONFIRMED = 1
$	      CONFIRMED_ANS = ANS
$	      PRM = "Confirm ''PRM'"
$	      GOTO CONFIRM_LOOP
$         ELSE
$	      IF ANS .NES. CONFIRMED_ANS
$	      THEN 
$                 CALL INVALID_ENTRY
$		  ROW = ROW - 2
$                 SAY ESC + "[''ROW';01H", CEOS
$                 GOTO PROMPT_LOOP
$	      ENDIF
$         ENDIF
$     ENDIF
$     IF ANS .NES. DEF THEN OPENSSL_USER_DATA_'CTR' = "''KEY'#''ITM'#''VAL'#''ANS'#''PRM'#''TYP'#''MIN'#''MAX'#Y#''REQ'#''CFM'"
$     CTR = CTR + 1
$     GOTO PROMPT_LOOP
$ ENDIF
$!
$!------------------------------------------------------------------------------
$! Save the OpenSSL Configuration Data
$!------------------------------------------------------------------------------
$!
$ CTR = 1
$ SAY ESC + "[''MSG_ROW';01H", BLNK, " Saving Configuration ...", NORM
$!
$SAVE_CONF_LOOP:
$!
$ IF CTR .LE. OPENSSL_USER_DATA_MAX
$ THEN 
$     KEY = F$ELEMENT (0,"#",OPENSSL_USER_DATA_'CTR') ! Key Name
$     ITM = F$ELEMENT (1,"#",OPENSSL_USER_DATA_'CTR') ! Item Name
$     VAL = F$ELEMENT (2,"#",OPENSSL_USER_DATA_'CTR') ! Item Value Contains Default or Prompt
$     DEF = F$ELEMENT (3,"#",OPENSSL_USER_DATA_'CTR') ! Default Value
$     PRM = F$ELEMENT (4,"#",OPENSSL_USER_DATA_'CTR') ! Prompt Value
$     TYP = F$ELEMENT (5,"#",OPENSSL_USER_DATA_'CTR') ! Value Type
$     MIN = F$ELEMENT (6,"#",OPENSSL_USER_DATA_'CTR') ! Value Minimum Length
$     MAX = F$ELEMENT (7,"#",OPENSSL_USER_DATA_'CTR') ! Value Maximum Length
$     UPD = F$ELEMENT (8,"#",OPENSSL_USER_DATA_'CTR') ! Entry Updated ?
$     REQ = F$ELEMENT (9,"#",OPENSSL_USER_DATA_'CTR') ! Entry Required for Input ?
$     CFM = F$ELEMENT (10,"#",OPENSSL_USER_DATA_'CTR')! Confirm Input ?
$     IF UPD .NES. "Y" .OR. VAL .EQS. "-"
$     THEN 
$         CTR = CTR + 1
$         GOTO SAVE_CONF_LOOP
$     ENDIF
$     IF VAL .EQS. "D"
$     THEN 
$         SET_CONF_DATA "''KEY'#''ITM'" "''DEF'"
$     ELSE
$         SET_CONF_DATA "''KEY'#''ITM'" "''PRM'"
$         SET_CONF_DATA "''KEY'#''ITM'_default" "''DEF'"
$     ENDIF
$     IF MIN .NES. "" THEN SET_CONF_DATA "''KEY'#''ITM'_min" "''MIN'"
$     IF MAX .NES. "" THEN SET_CONF_DATA "''KEY'#''ITM'_max" "''MAX'"
$     CTR = CTR + 1
$     GOTO SAVE_CONF_LOOP
$ ENDIF
$!
$ PURGE /NOLOG /NOCONFIRM 'OPENSSL_CONF_FILE'
$ RENAME 'OPENSSL_CONF_FILE'; ;1
$!
$ SAY ESC + "[''MSG_ROW';01H", CEOS
$!
$!------------------------------------------------------------------------------
$! Create the Server Certificiate
$!------------------------------------------------------------------------------
$!
$ SAY ESC + "[''MSG_ROW';01H", BLNK, " Creating Certificate Request ...", NORM
$!
$ X1 = 2
$ Y1 = TOP_ROW
$ X2 = TT_COLS - 2
$ Y2 = MSG_ROW - 1
$!
$ GET_USER_DATA "[]#encrypt_key"
$ _encrypt_key = OPENSSL_USER_DATA
$ IF F$EDIT (_encrypt_key,"UPCASE") .EQS. "Y"
$ THEN 
$     GET_USER_DATA "[]#pem_pass_phrase"
$     _pem_pass_phrase = OPENSSL_USER_DATA
$ ENDIF
$ GET_USER_DATA "[req]#default_bits"
$ _default_bits = OPENSSL_USER_DATA
$ GET_USER_DATA "[req]#default_keyfile"
$ _default_keyfile = OPENSSL_USER_DATA
$ GET_USER_DATA "[req]#default_csrfile"
$ _default_csrfile = OPENSSL_USER_DATA
$ GET_USER_DATA "[]#display_certificate"
$ _display_certificate = OPENSSL_USER_DATA
$!
$ DEFINE /USER /NOLOG SYS$ERROR  NL:
$ DEFINE /USER /NOLOG SYS$OUTPUT NL:
$ SHOW SYSTEM /FULL /OUT=SYS$LOGIN:OPENSSL_GENRSA_'PID'.RND
$!
$ OPEN /WRITE OFILE SYS$LOGIN:OPENSSL_GENRSA_'PID'.COM
$ WRITE OFILE "$ DEFINE /USER /NOLOG RANDFILE    SYS$LOGIN:OPENSSL_GENRSA_''PID'.RND"
$ WRITE OFILE "$ DEFINE /USER /NOLOG SYS$ERROR   SYS$LOGIN:OPENSSL_GENRSA_''PID'.LOG"
$ WRITE OFILE "$ DEFINE /USER /NOLOG SYS$OUTPUT  SYS$LOGIN:OPENSSL_GENRSA_''PID'.LOG"
$ WRITE OFILE "$ DEFINE /USER /NOLOG SYS$COMMAND SYS$INPUT"
$ IF F$EDIT (_encrypt_key,"UPCASE") .EQS. "Y"
$ THEN 
$     WRITE OFILE "$ OPENSSL genrsa -des3 -rand RANDFILE -out ''_default_keyfile' ''_default_bits'"
$     WRITE OFILE "''_pem_pass_phrase'"
$     WRITE OFILE "''_pem_pass_phrase'"
$ ELSE
$     WRITE OFILE "$ OPENSSL genrsa -rand RANDFILE -out ''_default_keyfile' ''_default_bits'"
$ ENDIF
$ CLOSE OFILE
$!
$ @SYS$LOGIN:OPENSSL_GENRSA_'PID'.COM
$!
$ DELETE/NOLOG/NOCONFIRM SYS$LOGIN:OPENSSL_GENRSA_'PID'.RND;*
$ DELETE/NOLOG/NOCONFIRM SYS$LOGIN:OPENSSL_GENRSA_'PID'.COM;*
$!
$ DEFINE /USER /NOLOG SYS$ERROR  NL:
$ DEFINE /USER /NOLOG SYS$OUTPUT NL:
$ SEARCH SYS$LOGIN:OPENSSL_GENRSA_'PID'.LOG /OUT=SYS$LOGIN:OPENSSL_GENRSA_'PID'.ERR ":error:"
$ IF F$SEARCH ("SYS$LOGIN:OPENSSL_GENRSA_''PID'.ERR") .NES. "" 
$ THEN 
$     IF F$FILE_ATTRIBUTE ("SYS$LOGIN:OPENSSL_GENRSA_''PID'.ERR","ALQ") .NE. 0
$     THEN 
$         DELETE /NOLOG /NOCONFIRM SYS$LOGIN:OPENSSL_GENRSA_'PID'.ERR;*
$         SAY ESC + "[''MSG_ROW';01H''BELL'''CEOS'"
$         SHOW_FILE "SYS$LOGIN:OPENSSL_GENRSA_''PID'.LOG" 'X1' 'Y1' 'X2' 'Y2' "< ERROR >" 
$         DELETE /NOLOG /NOCONFIRM SYS$LOGIN:OPENSSL_GENRSA_'PID'.LOG;*
$         GOTO EXIT
$     ENDIF
$     DELETE /NOLOG /NOCONFIRM SYS$LOGIN:OPENSSL_GENRSA_'PID'.ERR;*
$ ENDIF
$! 
$ DELETE /NOLOG /NOCONFIRM SYS$LOGIN:OPENSSL_GENRSA_'PID'.LOG;*
$!
$ DEFINE /USER /NOLOG SYS$ERROR  NL:
$ DEFINE /USER /NOLOG SYS$OUTPUT NL:
$ SHOW SYSTEM /FULL /OUT=SYS$LOGIN:OPENSSL_REQ_'PID'.RND
$!
$ OPEN /WRITE OFILE SYS$LOGIN:OPENSSL_REQ_'PID'.COM
$ WRITE OFILE "$ DEFINE /USER /NOLOG RANDFILE    SYS$LOGIN:OPENSSL_REQ_''PID'.RND"
$ WRITE OFILE "$ DEFINE /USER /NOLOG SYS$ERROR   SYS$LOGIN:OPENSSL_REQ_''PID'.LOG"
$ WRITE OFILE "$ DEFINE /USER /NOLOG SYS$OUTPUT  SYS$LOGIN:OPENSSL_REQ_''PID'.LOG"
$ WRITE OFILE "$ DEFINE /USER /NOLOG SYS$COMMAND SYS$INPUT"
$ IF F$EDIT (_encrypt_key,"UPCASE") .EQS. "Y"
$ THEN 
$     WRITE OFILE "$ OPENSSL req -new -config ''OPENSSL_CONF_FILE' -key ''_default_keyfile' -out ''_default_csrfile'"
$     WRITE OFILE "''_pem_pass_phrase'"
$     WRITE OFILE "''_pem_pass_phrase'"
$ ELSE
$     WRITE OFILE "$ OPENSSL req -new -nodes -config ''OPENSSL_CONF_FILE' -key ''_default_keyfile' -out ''_default_csrfile'"
$ ENDIF
$ WRITE OFILE ""
$ WRITE OFILE ""
$ WRITE OFILE ""
$ WRITE OFILE ""
$ WRITE OFILE ""
$ WRITE OFILE ""
$ WRITE OFILE ""
$ CLOSE OFILE
$!
$ @SYS$LOGIN:OPENSSL_REQ_'PID'.COM
$!
$ DELETE/NOLOG/NOCONFIRM SYS$LOGIN:OPENSSL_REQ_'PID'.COM;*
$!
$ DEFINE /USER /NOLOG SYS$ERROR  NL:
$ DEFINE /USER /NOLOG SYS$OUTPUT NL:
$ SEARCH SYS$LOGIN:OPENSSL_REQ_'PID'.LOG /OUT=SYS$LOGIN:OPENSSL_REQ_'PID'.ERR ":error:"
$ IF F$SEARCH ("SYS$LOGIN:OPENSSL_REQ_''PID'.ERR") .NES. "" 
$ THEN 
$     IF F$FILE_ATTRIBUTE ("SYS$LOGIN:OPENSSL_REQ_''PID'.ERR","ALQ") .NE. 0
$     THEN 
$         DELETE /NOLOG /NOCONFIRM SYS$LOGIN:OPENSSL_REQ_'PID'.ERR;*
$         SAY ESC + "[''MSG_ROW';01H''BELL'''CEOS'"
$         SHOW_FILE "SYS$LOGIN:OPENSSL_REQ_''PID'.LOG" 'X1' 'Y1' 'X2' 'Y2' "< ERROR >" 
$         DELETE /NOLOG /NOCONFIRM SYS$LOGIN:OPENSSL_REQ_'PID'.LOG;*
$         GOTO EXIT
$     ENDIF
$     DELETE /NOLOG /NOCONFIRM SYS$LOGIN:OPENSSL_REQ_'PID'.ERR;*
$ ENDIF
$!
$ DELETE /NOLOG /NOCONFIRM SYS$LOGIN:OPENSSL_REQ_'PID'.LOG;*
$! 
$ IF F$EDIT (_display_certificate,"TRIM,UPCASE") .EQS. "Y"
$ THEN 
$     SAY ESC + "[''MSG_ROW';01H", BLNK, " Generating Output ...", NORM, CEOL
$!
$     OPEN /WRITE OFILE SYS$LOGIN:OPENSSL_REQ_'PID'.COM
$     WRITE OFILE "$ DEFINE /USER /NOLOG SYS$ERROR   SYS$LOGIN:OPENSSL_REQ_''PID'.LOG"
$     WRITE OFILE "$ DEFINE /USER /NOLOG SYS$OUTPUT  SYS$LOGIN:OPENSSL_REQ_''PID'.LOG"
$     WRITE OFILE "$ DEFINE /USER /NOLOG SYS$COMMAND SYS$INPUT"
$     WRITE OFILE "$ OPENSSL req -noout -text -config ''OPENSSL_CONF_FILE' -in ''_default_csrfile'"
$     CLOSE OFILE
$!
$     @SYS$LOGIN:OPENSSL_REQ_'PID'.COM
$!
$     DELETE/NOLOG/NOCONFIRM SYS$LOGIN:OPENSSL_REQ_'PID'.COM;*
$!
$     DEFINE /USER /NOLOG SYS$ERROR  NL:
$     DEFINE /USER /NOLOG SYS$OUTPUT NL:
$     SEARCH SYS$LOGIN:OPENSSL_REQ_'PID'.LOG /OUT=SYS$LOGIN:OPENSSL_REQ_'PID'.ERR ":error:"
$     IF F$SEARCH ("SYS$LOGIN:OPENSSL_REQ_''PID'.ERR") .NES. "" 
$     THEN 
$         IF F$FILE_ATTRIBUTE ("SYS$LOGIN:OPENSSL_REQ_''PID'.ERR","ALQ") .NE. 0
$         THEN 
$             DELETE /NOLOG /NOCONFIRM SYS$LOGIN:OPENSSL_REQ_'PID'.ERR;*
$             SAY ESC + "[''MSG_ROW';01H''BELL'''CEOS'"
$             SHOW_FILE "SYS$LOGIN:OPENSSL_REQ_''PID'.LOG" 'X1' 'Y1' 'X2' 'Y2' "< ERROR >" 
$             DELETE /NOLOG /NOCONFIRM SYS$LOGIN:OPENSSL_REQ_'PID'.LOG;*
$             GOTO EXIT
$         ENDIF
$         DELETE /NOLOG /NOCONFIRM SYS$LOGIN:OPENSSL_REQ_'PID'.ERR;*
$     ENDIF
$!
$     SAY ESC + "[''MSG_ROW';01H''CEOS'"
$     SHOW_FILE "SYS$LOGIN:OPENSSL_REQ_''PID'.LOG" 'X1' 'Y1' 'X2' 'Y2' "< ''_default_csrfile' >" 
$     DELETE /NOLOG /NOCONFIRM SYS$LOGIN:OPENSSL_REQ_'PID'.LOG;*
$     GOTO EXIT
$ ENDIF
$!
$ TEXT = "Press return to continue"
$ COL = (TT_COLS - F$LENGTH (TEXT)) / 2
$!
$ SAY ESC + "[''MSG_ROW';01H", CEOS
$ PROMPT = ESC + "[''MSG_ROW';''COL'H''TEXT'"
$ ASK "''PROMPT'" OPT
$!
$GOTO EXIT
$!
$!------------------------------------------------------------------------------
$! Set the User Data
$!------------------------------------------------------------------------------
$!
$SET_USER_DATA: SUBROUTINE
$!
$ IF F$TYPE (OPENSSL_USER_DATA_MAX) .EQS. ""
$ THEN
$     OPENSSL_USER_DATA_MAX == 1
$ ELSE
$     OPENSSL_USER_DATA_MAX == OPENSSL_USER_DATA_MAX + 1
$ ENDIF
$!
$ OPENSSL_USER_DATA_'OPENSSL_USER_DATA_MAX' == "''P1'"
$!
$ EXIT
$!
$ ENDSUBROUTINE
$!
$!------------------------------------------------------------------------------
$! Get the User Data
$!------------------------------------------------------------------------------
$!
$GET_USER_DATA: SUBROUTINE
$!
$ CTR = 1
$ USER_KEY = F$ELEMENT (0,"#",P1)
$ USER_ITM = F$ELEMENT (1,"#",P1)
$!
$GET_USER_DATA_LOOP:
$!
$ IF CTR .LE. OPENSSL_USER_DATA_MAX
$ THEN
$     KEY = F$ELEMENT (0,"#",OPENSSL_USER_DATA_'CTR') ! Key Name
$     ITM = F$ELEMENT (1,"#",OPENSSL_USER_DATA_'CTR') ! Item Name
$     VAL = F$ELEMENT (2,"#",OPENSSL_USER_DATA_'CTR') ! Item Value Contains Default or Prompt
$     DEF = F$ELEMENT (3,"#",OPENSSL_USER_DATA_'CTR') ! Default Value
$     PRM = F$ELEMENT (4,"#",OPENSSL_USER_DATA_'CTR') ! Prompt Value
$     IF USER_KEY .NES. KEY .OR. USER_ITM .NES. ITM
$     THEN 
$         CTR = CTR + 1
$         GOTO GET_USER_DATA_LOOP
$     ENDIF
$     IF VAL .EQS. "-" THEN OPENSSL_USER_DATA == "''DEF'"
$     IF VAL .EQS. "D" THEN OPENSSL_USER_DATA == "''DEF'"
$     IF VAL .EQS. "P" THEN OPENSSL_USER_DATA == "''PRM'"
$ ENDIF
$!
$ EXIT
$!
$ ENDSUBROUTINE
$!
$!------------------------------------------------------------------------------
$! Delete the User Data
$!------------------------------------------------------------------------------
$!
$DEL_USER_DATA: SUBROUTINE
$!
$ IF F$TYPE (OPENSSL_USER_DATA_MAX) .EQS. "" THEN GOTO DEL_USER_DATA_END
$!
$DEL_USER_DATA_LOOP:
$!
$ IF F$TYPE (OPENSSL_USER_DATA_'OPENSSL_USER_DATA_MAX') .NES. "" 
$ THEN
$     DELETE /SYMBOL /GLOBAL OPENSSL_USER_DATA_'OPENSSL_USER_DATA_MAX'
$     OPENSSL_USER_DATA_MAX == OPENSSL_USER_DATA_MAX - 1
$     GOTO DEL_USER_DATA_LOOP
$ ENDIF
$!
$ DELETE /SYMBOL /GLOBAL OPENSSL_USER_DATA_MAX
$!
$DEL_USER_DATA_END:
$!
$ IF F$TYPE (OPENSSL_USER_DATA) .NES. "" THEN DELETE /SYMBOL /GLOBAL OPENSSL_USER_DATA
$!
$ EXIT
$!
$ ENDSUBROUTINE
$!
$!------------------------------------------------------------------------------
$! Display the invalid entry 
$!------------------------------------------------------------------------------
$!
$INVALID_ENTRY: SUBROUTINE
$!
$ SAY ESC + "[''MSG_ROW';01H", BELL, " Invalid Entry, Try again ...''CEOL'"
$ Wait 00:00:01.5
$ SAY ESC + "[''MSG_ROW';01H", CEOL
$!
$ EXIT
$!
$ ENDSUBROUTINE
$!
$!------------------------------------------------------------------------------
$!
$!------------------------------------------------------------------------------
$!
$EXIT:
$!
$ DEFINE /USER /NOLOG SYS$ERROR  NL:
$ DEFINE /USER /NOLOG SYS$OUTPUT NL:
$ CLOSE OFILE
$!
$ DEL_USER_DATA
$!
$ IF F$TYPE (OPENSSL_CONF_DATA) .NES. "" THEN DELETE /SYMBOL /GLOBAL OPENSSL_CONF_DATA
$!
$ IF F$GETDVI ("TT:","TT_NOECHO") .AND. .NOT. TT_NOECHO THEN SET TERMINAL /ECHO
$!
$ IF F$SEARCH ("SYS$LOGIN:OPENSSL_GENRSA_''PID'.%%%;*") .NES. "" THEN DELETE/NOLOG/NOCONFIRM SYS$LOGIN:OPENSSL_GENRSA_'PID'.%%%;*
$ IF F$SEARCH ("SYS$LOGIN:OPENSSL_REQ_''PID'.%%%;*") .NES. "" THEN DELETE/NOLOG/NOCONFIRM SYS$LOGIN:OPENSSL_REQ_'PID'.%%%;*
$!
$ Verify = F$VERIFY (Verify)
$!
$ EXIT
