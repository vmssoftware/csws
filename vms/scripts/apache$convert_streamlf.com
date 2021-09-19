$! APACHE$CONVERT_STREAMLF.COM
$!
$! *************************************************************************
$! *                                                                       *
$! * HP CONFIDENTIAL. This software is confidential proprietary software   *
$! * licensed by Hewlett-Packard Development Company, L.P., and is not     *
$! * authorized to be used, duplicated or disclosed to anyone without the  *
$! * prior written permission of HP. © 2015 Copyright Hewlett-Packard      *
$! * Development Company, L.P.                                             *
$! *                                                                       *
$! * VMS SOFTWARE, INC. CONFIDENTIAL. This software is confidential        *
$! * proprietary software licensed by VMS Software, Inc., and is not       *
$! * authorized to be used, duplicated or disclosed to anyone without the  *
$! * prior written permission of VMS Software, Inc. © 2015 Copyright VMS   *
$! * Software, Inc.                                                        *
$! *                                                                       *
$! *************************************************************************
$!
$!	Convert all files recursivley in a directory
$!	tree to stream_lf.  It avoids converting the directory
$!	files and any executables files (*.DIR & *.EXE).
$!
$! History:
$!
$!	V2.0	15-Sep-2003	Scott LePage
$!	Changed the conversion to use a filetype scheme.
$!
$!	X1.1	21-Apr-2003	Scott LePage
$!	Added status message before convert starts
$!
$!	X1.0	14-Mar-2003	Scott LePage
$!	Initial creation.
$!
$! *************************************************************************
$
$!
$! p1 = Top level directory
$!
$
$	dir*ectory := directory
$	del*lete := delete/nolog
$	set := set
$	logfile = "SYS$SCRATCH:Convert_Dir.Log"
$	incfile = "APACHE$COMMON:[000000]APACHE$CVT_TYPES.DAT"
$	echo := write sys$output
$
$	verbose = "FALSE"
$	if f$trnlnm("APACHE$CONVERT_VERBOSE") .nes. "" then verbose = "TRUE"
$	nomessage = "/NOTEXT/NOSEVERITY/NOFACILTY/NOIDENT"
$	save_message = f$environment("MESSAGE")
$	saved_parse_style = f$getjpi("", "PARSE_STYLE_PERM")
$	set process/parse_style=Extended
$
$ a1:
$	if p1 .nes. "" then goto a2
$	read/prompt="Top Directory: " sys$command p1
$	goto a1
$
$ a2:
$	temp1 = f$edit(p1,"TRIM,COMPRESS,UPCASE")
$	if temp1 .eqs. "HELP" then goto usage
$	if temp1 .eqs. "-H" then goto usage
$	if temp1 .eqs. "?" then goto usage
$
$!
$! Read filetypes file
$!	If there is no filetypes file or filetypes in that file
$!	then the default filetypes will be converted.
$!
$
$	dirstr = ""
$	if f$search("''incfile'") .nes. ""
$	then
$	    open/read incf 'incfile'
$ INCloop:
$	    read/end=INCdone incf ftype
$	    if f$length(ftype) .eq. 0 then goto INCloop
$	    if f$extract(0,1,ftype) .eqs. "!" then goto INCloop
$	    if f$extract(0,1,ftype) .eqs. "#" then goto INCloop
$	    ftype = f$edit(ftype,"TRIM,COMPRESS,UPCASE")
$	    ftype = f$element(0," ",ftype)
$	    dirstr = dirstr + "*" + ftype + ";0,"
$	    goto INCloop
$ INCdone:
$	    close incf
$	endif
$	if dirstr .eqs. ""
$	then
$	    dirstr = "*.HTM*;0,*.SHTML;0,*.TXT;0"
$	else
$	    dirstr = f$extract(0,f$length(dirstr)-1,dirstr)
$	endif
$
$	pid = f$getjpi("","PID")
$
$	save_default = f$environment("default")
$
$	open/write cvtlog 'logfile'
$	elips = f$extract(0, f$length(p1)-1, p1) + "...]"
$	write cvtlog ""
$	write cvtlog "Converting files found in ''elips'  -  ''f$time()'"
$	write cvtlog ""
$	write cvtlog "  Searching for filetypes:  ''dirstr'"
$
$	echo ""
$	echo "Starting conversion of ''elips'"
$	echo "This could take a while..."
$
$	set message'nomessage'
$	dir/output=sys$scratch:Dir'pid'.tmp/nohead/notrail/col=1 'elips'*.dir
$	set message'save_message'
$	open/read dirs sys$scratch:Dir'pid'.tmp
$ DLoop:
$	read/end=DDone dirs recd
$	if .not. f$file_attributes(recd, "DIRECTORY") then goto DLoop
$	device = f$parse(recd,,,"DEVICE")
$	dd = f$parse(recd,,,"DIRECTORY")
$	name = f$parse(recd,,,"NAME")
$	temp1 = device + dd
$	temp1 = f$extract(0, f$length(temp1)-1, temp1)
$	temp1 = temp1 + "." + name + "]"
$	if verbose then echo "Directory: ''temp1'"
$	write cvtlog "  Searching Directory: ''temp1'"
$	set default 'temp1'
$	set message'nomessage'
$	dir/output=sys$scratch:Files'pid'.tmp-
           /nohead/notrail/col=1 'dirstr'
$	open/read fils sys$scratch:Files'pid'.tmp
$	set noon
$ FLoop:
$	read/end=FDone fils file
$	if f$file_attributes(file, "DIRECTORY")
$	then
$	    write cvtlog "    Skipping: ''file' (directory)"
$	    goto FLoop
$	endif
$	analyze/image/select=FILE_TYPE/output=NLA0: 'file'
$	if '$status'
$	then
$	    write cvtlog "    Skipping: ''file' (VMS image/object)"
$	    goto FLoop
$	endif
$
$	if verbose then echo "  File: ''file'"
$	gosub cvt_file
$	goto Floop
$ FDone:
$	set message'save_message'
$	set on
$	close fils
$	delete sys$scratch:Files'pid'.tmp;*
$	goto Dloop
$ DDone:
$	close dirs
$	delete sys$scratch:Dir'pid'.tmp;*
$
$	if verbose then echo "Directory: ''p1'"
$	write cvtlog "  Searching Directory: ''p1'"
$	set def 'p1'
$	set message'nomessage'
$	dir/output=sys$scratch:Files'pid'.tmp-
           /nohead/notrail/col=1 'dirstr'
$	open/read fils sys$scratch:Files'pid'.tmp
$	set noon
$ F0Loop:
$	read/end=F0Done fils file
$	if f$file_attributes(file, "DIRECTORY")
$	then
$	    write cvtlog "    Skipping: ''file' (directory)"
$	    goto F0Loop
$	endif
$	analyze/image/select=FILE_TYPE/output=NLA0: 'file'
$	if '$status'
$	then
$	    write cvtlog "    Skipping: ''file' (VMS image/object)"
$	    goto F0Loop
$	endif
$	if verbose then echo "  File: ''file'"
$	gosub cvt_file
$	goto F0Loop
$ F0Done:
$	set message'save_message'
$	set on
$	close fils
$	delete sys$scratch:Files'pid'.tmp;*
$
$	write cvtlog ""
$	write cvtlog "Done."
$	close cvtlog
$
$	echo ""
$	echo "Conversions complete."
$	echo "  See ''logfile' for a log of transactions."
$	echo ""
$	set default 'save_default'
$	set process/parse_style='saved_parse_style'
$	exit



$! c v t _ f i l e
$!
$!	Convert a file to stream_lf
$!
$! Inputs:
$!	file - full filespec to be converted
$!
$! Outputs:
$!	none
$!
$!
$cvt_file:
$	semi = f$locate(";", file)
$	file1 = f$extract(0, semi, file)
$	analyze/rms/fdl/output=sys$scratch:stmlf'pid'.tmp 'file1'
$
$	open/read infdl sys$scratch:stmlf'pid'.tmp
$	open/write outfdl sys$scratch:stmlf'pid'.fdl
$ CLoop:
$	read/end=CDone infdl recd
$	gosub handle_quotes
$	temp1 = f$edit(recd,"TRIM,COMPRESS")
$	tok1 = f$element(0," ",temp1)
$	tok2 = f$element(1," ",temp1)
$	if tok1 .eqs. "ORGANIZATION"
$	then
$	    if tok2 .nes. "sequential"
$	    then
$	        write cvtlog "    Skipping:  ''file' (Organization = ''tok2')"
$	        goto CSkip
$	    endif
$	endif
$	if recd .eqs. "RECORD" then goto CDone
$	write outfdl "''recd'"
$	goto CLoop
$ CDone:
$	close infdl
$	delete sys$scratch:stmlf'pid'.tmp;*
$
$	write outfdl "''recd'"
$	write outfdl "	BLOCK_SPAN               yes"
$	write outfdl "	CARRIAGE_CONTROL         carriage_return"
$	write outfdl "	FORMAT                   stream_lf"
$	write outfdl "	SIZE                     0"
$
$	close outfdl
$
$	write cvtlog "    Converting:  ''file'"
$	convert/fdl=sys$scratch:stmlf'pid'.fdl 'file1' 'file1'
$
$	delete sys$scratch:stmlf'pid'.fdl;*
$
$	return
$
$ CSkip:
$	close infdl
$	delete sys$scratch:stmlf'pid'.tmp;*
$	close outfdl
$	delete sys$scratch:stmlf'pid'.fdl;*
$	return


$! h a n d l e _ q u o t e s
$!
$!	DCL chokes on writing records with quotes imbedded.
$!	This routine doubles all single quotes found in the
$!	record so the write works correctly.
$!
$! Inputs:
$!	recd - input record to be scanned
$!
$! Outputs:
$!	recd - modified if it contains quotes
$!
$!
$handle_quotes:
$	q = """
$	r = recd
$	newr = ""
$ HQLoop:
$	ql = f$locate(q, r)
$	if ql .eq. f$length(r) then goto HQDone
$	newr = newr + f$extract(0,ql,r) + q + q
$	r = f$extract(ql+1, f$length(r) - (ql+1), r)
$	goto HQLoop
$ HQDone:
$	if f$length(r) .gt. 0 then
$	    newr = newr + r
$	recd = newr
$
$	return



$! u s a g e
$!
$usage:
$	echo ""
$	echo "     CONVERT_DIR.COM"
$	echo ""
$	echo " Converts all sequential files under a directory (recusively)
$	echo " to STREAM_LF format.  Define the logical ""APACHE$CONVERT_VERBOSE"""
$	echo " for long processing comments."
$	echo ""
$	echo " See APACHE$COMMON:[000000]APACHE$CVT_TYPES.DAT for a list of"
$	echo " filetypes to be converted. (default = '*.HTM*' '*.TXT')"
$	echo ""
$	echo "   @CONVERT_DIR <directory>"
$	echo ""
$	exit
