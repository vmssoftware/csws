$ ! Login.Com for Apache HTTP (WWW) Server
$ !
$ ! set verify
$ ! define decc$acl_access_check true
$ ! define decc$allow_remove_open_files true
$ !
$ ! define apache$auth_use_acm true
$ ! define apache$stat_cache true
$ ! define define apache$stat_reset_after 50
$ ! define apache$spl_disabled true
$ !
$   set process/parse=extend
$   set process/units=bytes
$ !
$ exit
$ !
$ ! Use the following DCL commands to prevent the APACHE$SPECIFIC:[000000]
$ ! directory from filling up with old LOG files by limiting the number of
$ ! versions of each file.
$ !
$ temp1 = f$trnlnm("apache$specific")
$ temp2 = f$length(temp1)
$ temp3 = f$extract(temp2-1,1,temp1)
$ if (f$extract(temp2-2,1,temp1) .eqs. ".")
$ then temp1 = f$extract(0,temp2-2,temp1) + temp3
$ else temp1 = f$extract(0,temp2-1,temp1) + temp3
$ endif
$ set directory /version_limit=5 'temp1'
$ !
$ exit
$ ! End of file
