$ set verify
$ @sys$startup:apache$shutdown
$
$ copy [-.srclib.apr.vms]apache$apr_shr.exe apache$common:[000000]
$ copy [-.srclib.apr.vms]apache$apr_shrp.exe apache$common:[000000]
$ copy [-.srclib.apr-util.vms]apache$apu_shr.exe apache$common:[000000]
$ copy apache$httpd.exe apache$common:[000000]
$ copy apache$httpd_shr.exe apache$common:[000000]
$
$ copy apache$dcl_bin.exe apache$common:[000000]
$ copy apache$dcl_env.exe apache$common:[000000]
$ copy apache$dcl_run.exe apache$common:[000000]
$ copy apache$set_ccl.exe apache$common:[000000]
$
$ purge/log apache$common:[000000]
$
$ @sys$startup:APACHE$LOGICALS.COM
$ @sys$startup:apache$startup
$ exit
