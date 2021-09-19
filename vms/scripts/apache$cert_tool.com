$!
$!------------------------------------------------------------------------------
$! APACHE$CERT_TOOL.COM - OpenSSL Certificate Tool procedure
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
$! This procedure provides the user a menu from which they can choose desired 
$! OpenSSL Certificate processing.
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
$ On Control_Y THEN GOTO EXIT
$ Set Control=Y
$!
$ TT_ROWS = f$getdvi ("TT:","TT_PAGE")
$ TT_COLS = f$getdvi ("TT:","DEVBUFSIZ")
$!
$ SET_MENU_DATA := CALL SET_MENU_DATA
$ DEL_MENU_DATA := CALL DEL_MENU_DATA
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
$ BOLD = ESC + "[1m"    ! Turn on BOLD Attribute
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
$! Initialize the Menu Items
$!------------------------------------------------------------------------------
$!
$ SET_MENU_DATA "View a Certificate#@OPENSSL_COM:OPENSSL_VIEW_CERT.COM CRT"
$ SET_MENU_DATA "View a Certificate Request#@OPENSSL_COM:OPENSSL_VIEW_CERT.COM CSR"
$ SET_MENU_DATA "Create a Certificate Request#@OPENSSL_COM:OPENSSL_RQST_CERT.COM"
$ SET_MENU_DATA "Create a Self-Signed Certificate#@OPENSSL_COM:OPENSSL_SELF_CERT.COM"
$ SET_MENU_DATA "Create a Certificate Authority#@OPENSSL_COM:OPENSSL_AUTH_CERT.COM"
$ SET_MENU_DATA "Sign a Certificate Request#@OPENSSL_COM:OPENSSL_SIGN_CERT.COM"
$ SET_MENU_DATA "Hash Certificate Authorities#@OPENSSL_COM:OPENSSL_HASH_CERT.COM CRT"
$ SET_MENU_DATA "Hash Certificate Revocations#@OPENSSL_COM:OPENSSL_HASH_CERT.COM CRL"
$ SET_MENU_DATA "Exit#GOTO EXIT"
$!
$!------------------------------------------------------------------------------
$! Display the Page Header
$!------------------------------------------------------------------------------
$!
$PAGE_LOOP:
$!
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
$ TEXT = "Main Menu"
$ COL = (TT_COLS - F$LENGTH (TEXT)) / 2
$!
$ SAY ESC + "[04;01H"
$ SAY ESC + "[04;''COL'H", COLOR, TEXT, NORM
$!
$ CTR = 1
$ ROW = 6
$ COL = (TT_COLS - (OPENSSL_MENU_ITEM_MAX + 4)) / 2
$ TOP_ROW = ROW
$ SEP_ROWS = 2
$ MSG_ROW = TT_ROWS - 1
$!
$!------------------------------------------------------------------------------
$! Process the menu options
$!------------------------------------------------------------------------------
$!
$MENU_LOOP: 
$!
$ IF CTR .LE. OPENSSL_MENU_DATA_MAX
$ THEN
$     OPT = F$ELEMENT (0,"#",OPENSSL_MENU_DATA_'CTR') ! Option String
$     CMD = F$ELEMENT (1,"#",OPENSSL_MENU_DATA_'CTR') ! Command String
$     IF ROW .GE. (MSG_ROW - (SEP_ROWS + 2)) .AND. SEP_ROWS .GT. 1
$     THEN
$         SAY ESC + "[''TOP_ROW';01H", CEOS
$	  ROW = TOP_ROW
$         SEP_ROWS = 1
$         CTR = 1
$     ELSE
$	  NUM = F$FAO ("!2SL", CTR)
$         SAY ESC + "[''ROW';''COL'H", BOLD, "''NUM'. ", NORM, "''OPT'"
$         ROW = ROW + SEP_ROWS
$         CTR = CTR + 1
$     ENDIF	   
$     GOTO MENU_LOOP
$ ENDIF    
$!
$ ROW = ROW + 1
$!
$!------------------------------------------------------------------------------
$! Prompt the user for input
$!------------------------------------------------------------------------------
$!
$PROMPT_LOOP:
$!
$ PROMPT = ESC + "[''ROW';''COL'HEnter Option: ''CEOL'"
$ ASK "''PROMPT'" OPT /END_OF_FILE=EXIT
$ OPT = F$EDIT (OPT, "TRIM")
$ IF OPT .EQS. ""  THEN GOTO PROMPT_LOOP
$!
$ IF F$TYPE (OPT) .NES. "INTEGER" .OR. -
     F$INTEGER (OPT) .LE. 0 .OR. -
     F$INTEGER (OPT) .GT. OPENSSL_MENU_DATA_MAX
$ THEN 
$     CALL INVALID_OPTION
$     GOTO PROMPT_LOOP
$ ENDIF
$!
$ CMD = F$ELEMENT (1,"#",OPENSSL_MENU_DATA_'OPT')
$!
$ 'CMD'
$!
$ GOTO PAGE_LOOP
$!
$!------------------------------------------------------------------------------
$! Set the Menu Data
$!------------------------------------------------------------------------------
$!
$SET_MENU_DATA: SUBROUTINE
$!
$ IF F$TYPE (OPENSSL_MENU_DATA_MAX) .EQS. ""
$ THEN
$     OPENSSL_MENU_DATA_MAX == 1
$     OPENSSL_MENU_ITEM_MAX == 0
$ ELSE
$     OPENSSL_MENU_DATA_MAX == OPENSSL_MENU_DATA_MAX + 1
$ ENDIF
$!
$ OPENSSL_MENU_DATA_'OPENSSL_MENU_DATA_MAX' == "''P1'"
$!
$ MENU_ITEM = F$ELEMENT (0,"#",OPENSSL_MENU_DATA_'OPENSSL_MENU_DATA_MAX')
$ IF F$LENGTH (MENU_ITEM) .GT. OPENSSL_MENU_ITEM_MAX THEN OPENSSL_MENU_ITEM_MAX == F$LENGTH (MENU_ITEM)
$!
$ EXIT
$!
$ ENDSUBROUTINE
$!
$!------------------------------------------------------------------------------
$! Delete the Menu Data
$!------------------------------------------------------------------------------
$!
$DEL_MENU_DATA: SUBROUTINE
$!
$ IF F$TYPE (OPENSSL_MENU_DATA_MAX) .EQS. "" THEN GOTO DEL_MENU_DATA_END
$!
$DEL_MENU_DATA_LOOP:
$!
$ IF F$TYPE (OPENSSL_MENU_DATA_'OPENSSL_MENU_DATA_MAX') .NES. "" 
$ THEN
$     DELETE /SYMBOL /GLOBAL OPENSSL_MENU_DATA_'OPENSSL_MENU_DATA_MAX'
$     OPENSSL_MENU_DATA_MAX == OPENSSL_MENU_DATA_MAX - 1
$     GOTO DEL_MENU_DATA_LOOP
$ ENDIF
$!
$ DELETE /SYMBOL /GLOBAL OPENSSL_MENU_DATA_MAX
$!
$DEL_MENU_DATA_END:
$!
$ IF F$TYPE (OPENSSL_MENU_ITEM_MAX) .NES. "" THEN DELETE /SYMBOL /GLOBAL OPENSSL_MENU_ITEM_MAX
$!
$ EXIT
$!
$ ENDSUBROUTINE
$!
$!------------------------------------------------------------------------------
$! Display the invalid entry 
$!------------------------------------------------------------------------------
$!
$INVALID_OPTION: SUBROUTINE
$!
$ SAY ESC + "[''MSG_ROW';01H", BELL, " Invalid Option, Try again ...''CEOL'"
$ Wait 00:00:01.5
$ SAY ESC + "[''MSG_ROW';01H", CEOL
$!
$ EXIT
$!
$ ENDSUBROUTINE
$!
$!------------------------------------------------------------------------------
$! Exit
$!------------------------------------------------------------------------------
$!
$EXIT:
$!
$ DEL_MENU_DATA
$!
$ Verify = F$VERIFY (Verify)
$!
$ EXIT
