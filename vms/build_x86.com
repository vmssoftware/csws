$ set noon
$ @sys$login:OSS$STARTUP_X86.COM
$ @SYS$MANAGER:X86_XTOOLS$SYLOGIN 1
$
$ set noon
$ deassign/job/nolog DECC$ARGV_PARSE_STYLE
$ deassign/job/nolog DECC$EFS_CASE_PRESERVE
$ deassign/job/nolog DECC$EFS_CASE_SPECIAL
$ deassign/job/nolog DECC$EFS_CHARSET
$ deassign/job/nolog DECC$ENABLE_GETENV_CACHE
$ deassign/job/nolog DECC$FILE_SHARING
$ deassign/job/nolog DECC$POSIX_SEEK_STREAM_FILE
$ deassign/job/nolog DECC$READDIR_DROPDOTNOTYPE
$ set on
$ set verify
$ @sys$startup:ssl111$startup.com
$
$! **** APR
$ copy apr_escape_test_char.h [-.srclib.apr.include.private]apr_escape_test_char.h
$ set default [-.srclib.apr.vms]
$ @build_x86.com
$ set default [---.vms]
$
$! **** APR Utils
$ set default [-.srclib.apr-util.vms]
$ @build.com
$ set default [---.vms]
$
$
$! **** Core components
$!
$ core:
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.proxy],[-.modules.core],oss$root:[include]
$ ccopt = "/names=(as_is,shortened)/nolist/define=(LDAP_DEPRECATED=1,_SOCKADDR_LEN,HAVE_CONFIG_H,_REENTRANT,_USE_STD_STAT,_LARGEFILE)/include=cc$include/warn=disable=(QUESTCOMPARE,QUESTCOMPARE1)"
$
$ purge/log [-...]
$ cc'ccopt' [-.os.unix]unixd.c
$
$ lib/create libos.olb
$ purge/log
$
$ lib/insert libos.olb unixd.obj
$ delete/log/noconf *.obj;*
$
$
$ cc'ccopt' [-.server.mpm.prefork]prefork.c
$ cc'ccopt' spl.c
$ cc'ccopt' sps.c
$
$ lib/create libprefork.olb
$ purge/log
$
$ lib/insert libprefork.olb prefork.obj
$ lib/insert libprefork.olb spl.obj
$ lib/insert libprefork.olb sps.obj
$ delete/log/noconf *.obj;*
$
$
$ cc'ccopt' [-.server]gen_test_char.c
$ link gen_test_char.obj
$ purge/log
$ delete/log/noconf gen_test_char.obj;*
$!!! >>> TBD pipe run gen_test_char.EXE | type/output=test_char.h sys$input
$!!! >>> TBD delete/log/noconf gen_test_char.exe;*
$ purge/log [-...]
$
$ cc'ccopt' [-.server]config.c
$ cc'ccopt' [-.server]log.c
$ cc'ccopt' [-.server]main.c
$ cc'ccopt' [-.server]vhost.c
$ cc'ccopt' [-.server]util.c
$ cc'ccopt' [-.server]util_fcgi.c
$ cc'ccopt' [-.server]util_script.c
$ cc'ccopt' [-.server]util_md5.c
$ cc'ccopt' [-.server]util_cfgtree.c
$ cc'ccopt' [-.server]util_ebcdic.c
$ cc'ccopt' [-.server]util_time.c
$ cc'ccopt' [-.server]connection.c
$ cc'ccopt' [-.server]listen.c
$ cc'ccopt' [-.server]util_mutex.c
$ cc'ccopt' [-.server]mpm_common.c
$ cc'ccopt' [-.server]mpm_unix.c
$ cc'ccopt' [-.server]util_charset.c
$ cc'ccopt' [-.server]util_cookies.c
$ cc'ccopt' [-.server]util_debug.c
$ cc'ccopt' [-.server]util_xml.c
$ cc'ccopt' [-.server]util_filter.c
$ cc'ccopt' [-.server]util_pcre.c
$ cc'ccopt' [-.server]util_regex.c
$ cc'ccopt' [-.server]scoreboard.c
$ cc'ccopt' [-.server]error_bucket.c
$ cc'ccopt' [-.server]protocol.c
$ cc'ccopt' [-.server]core.c
$ cc'ccopt' [-.server]request.c
$ cc'ccopt' [-.server]provider.c
$ cc'ccopt' [-.server]eoc_bucket.c
$ cc'ccopt' [-.server]eor_bucket.c
$ cc'ccopt' [-.server]core_filters.c
$ cc'ccopt' [-.server]util_expr_parse.c
$ cc'ccopt' [-.server]util_expr_scan.c
$ cc'ccopt' [-.server]util_expr_eval.c
$
$!!! >>> TBD delete/log/noconf test_char.h;*
$
$ lib/create libmain.olb
$ purge/log
$
$ lib/insert libmain.olb config.obj
$ lib/insert libmain.olb log.obj
$ lib/insert libmain.olb main.obj
$ lib/insert libmain.olb vhost.obj
$ lib/insert libmain.olb util.obj
$ lib/insert libmain.olb util_fcgi.obj
$ lib/insert libmain.olb util_script.obj
$ lib/insert libmain.olb util_md5.obj
$ lib/insert libmain.olb util_cfgtree.obj
$ lib/insert libmain.olb util_ebcdic.obj
$ lib/insert libmain.olb util_time.obj
$ lib/insert libmain.olb connection.obj
$ lib/insert libmain.olb listen.obj
$ lib/insert libmain.olb util_mutex.obj
$ lib/insert libmain.olb mpm_common.obj
$ lib/insert libmain.olb mpm_unix.obj
$ lib/insert libmain.olb util_charset.obj
$ lib/insert libmain.olb util_cookies.obj
$ lib/insert libmain.olb util_debug.obj
$ lib/insert libmain.olb util_xml.obj
$ lib/insert libmain.olb util_filter.obj
$ lib/insert libmain.olb util_pcre.obj
$ lib/insert libmain.olb util_regex.obj
$ lib/insert libmain.olb scoreboard.obj
$ lib/insert libmain.olb error_bucket.obj
$ lib/insert libmain.olb protocol.obj
$ lib/insert libmain.olb core.obj
$ lib/insert libmain.olb request.obj
$ lib/insert libmain.olb provider.obj
$ lib/insert libmain.olb eoc_bucket.obj
$ lib/insert libmain.olb eor_bucket.obj
$ lib/insert libmain.olb core_filters.obj
$ lib/insert libmain.olb util_expr_parse.obj
$ lib/insert libmain.olb util_expr_scan.obj
$ lib/insert libmain.olb util_expr_eval.obj
$ delete/log/noconf *.obj;*
$
$
$ cc'ccopt' [-.modules.core]mod_so.c
$
$ lib/create libmod_so.olb
$ purge/log
$
$ lib/insert libmod_so.olb mod_so.obj
$ delete/log/noconf mod_so.obj;*
$
$
$ cc'ccopt' [-.modules.http]http_core.c
$ cc'ccopt' [-.modules.http]http_protocol.c
$ cc'ccopt' [-.modules.http]http_request.c
$ cc'ccopt' [-.modules.http]http_filters.c
$ cc'ccopt' [-.modules.http]chunk_filter.c
$ cc'ccopt' [-.modules.http]byterange_filter.c
$ cc'ccopt' [-.modules.http]http_etag.c
$
$ lib/create libmod_http.olb
$ purge/log
$
$ lib/insert libmod_http.olb http_core.obj
$ lib/insert libmod_http.olb http_protocol.obj
$ lib/insert libmod_http.olb http_request.obj
$ lib/insert libmod_http.olb http_filters.obj
$ lib/insert libmod_http.olb chunk_filter.obj
$ lib/insert libmod_http.olb byterange_filter.obj
$ lib/insert libmod_http.olb http_etag.obj
$
$ delete/log/noconf *.obj;*
$
$
$ cc'ccopt' [-.support]htpasswd.c
$ cc'ccopt' [-.support]passwd_common.c
$ link/exe=htpasswd.exe htpasswd.obj,passwd_common.obj,sys$input/opt
[-.srclib.apr.vms]apache$apr_shr.exe/share
[-.srclib.apr-util.vms]apache$apu_shr.exe/share
$
$ cc'ccopt' [-.support]htdigest.c
$ link/exe=htdigest.exe htdigest.obj,sys$input/opt
[-.srclib.apr.vms]apache$apr_shr.exe/share
[-.srclib.apr-util.vms]apache$apu_shr.exe/share
$
$ cc'ccopt' [-.support]htdbm.c
$ link/exe=htdbm.exe htdbm.obj,passwd_common.obj,sys$input/opt
[-.srclib.apr.vms]apache$apr_shr.exe/share
[-.srclib.apr-util.vms]apache$apu_shr.exe/share
$
$ cc'ccopt' [-.support]ab.c
$ link/exe=ab.exe ab.obj,sys$input/opt
[-.srclib.apr.vms]apache$apr_shr.exe/share
[-.srclib.apr-util.vms]apache$apu_shr.exe/share
oss$root:[lib]ssl111$libssl32.olb/lib
oss$root:[lib]ssl111$libcrypto32.olb/lib
$
$ cc'ccopt' [-.support]logresolve.c
$ link/exe=logresolve.exe logresolve.obj,sys$input/opt
[-.srclib.apr.vms]apache$apr_shr.exe/share
$
$ cc'ccopt' [-.support]httxt2dbm.c
$ link/exe=httxt2dbm.exe httxt2dbm.obj,sys$input/opt
[-.srclib.apr.vms]apache$apr_shr.exe/share
[-.srclib.apr-util.vms]apache$apu_shr.exe/share
$
$ cc'ccopt' [-.support]htcacheclean.c
$ link/exe=htcacheclean.exe htcacheclean.obj,sys$input/opt
[-.srclib.apr.vms]apache$apr_shr.exe/share
[-.srclib.apr-util.vms]apache$apu_shr.exe/share
$
$ cc'ccopt' [-.support]rotatelogs.c
$ link/exe=rotatelogs.exe rotatelogs.obj,sys$input/opt
[-.srclib.apr.vms]apache$apr_shr.exe/share
$
$ cc'ccopt' [-.support]checkgid.c
$ link/exe=checkgid.exe checkgid.obj
$
$ cc'ccopt' [-.support]fcgistarter.c
$ link/exe=fcgistarter.exe fcgistarter.obj,sys$input/opt
[-.srclib.apr.vms]apache$apr_shr.exe/share
$
$!!! >>> TBD cc'ccopt' [-.support]suexec.c+sys$library:sys$lib_c.tlb/lib
$ cc'ccopt' [-.support]suexec.c+x86$library:sys$lib_c.tlb/lib
$ link/notrace/exe=suexec.exe suexec.obj,sys$input/opt
[-.srclib.apr.vms]apache$apr_shr.exe/share
$
$ cc'ccopt' apache$dcl_bin.c
$ link/exe=apache$dcl_bin.exe apache$dcl_bin.obj
$
$ cc'ccopt' apache$dcl_env.c
$ link/exe=apache$dcl_env.exe apache$dcl_env.obj,sys$input/opt
[-.srclib.apr.vms]apache$apr_shr.exe/share
$
$!!! >>> TBD cc'ccopt' apache$dcl_run.c+sys$library:sys$lib_c.tlb/lib
$ cc'ccopt' apache$dcl_run.c+x86$library:sys$lib_c.tlb/lib
$ link/exe=apache$dcl_run.exe apache$dcl_run.obj,sys$input/opt
[-.srclib.apr.vms]apache$apr_shr.exe/share
$
$ cc'ccopt' apache$set_ccl.c
$ link/exe=apache$set_ccl.exe apache$set_ccl.obj,sys$input/opt
[-.srclib.apr.vms]apache$apr_shrp.exe/share
$
$ cc'ccopt' test-cgi-vms.c
$ link/exe=test-cgi-vms.exe test-cgi-vms.obj
$
$!!! >>> TBD cc'ccopt' hostaddr.c+sys$library:sys$lib_c/library
$ cc'ccopt' hostaddr.c+x86$library:sys$lib_c/library
$ link/exe=hostaddr.exe hostaddr.obj
$
$!!! >>> TBD cc'ccopt' hostname.c+sys$library:sys$lib_c/library
$ cc'ccopt' hostname.c+x86$library:sys$lib_c/library
$ link/exe=hostname.exe hostname.obj
$
$ cc'ccopt' hostname.c
$ delete/log/noconf *.obj;*
$
$
$ cc'ccopt' [-]modules.c
$ cc'ccopt' [-.server]buildmark.c
$
$ link/share=apache$httpd_shr.exe modules.obj,buildmark.obj,httpd_shr.opt/opt
$ purge/log
$ delete/log/noconf *.obj;*
$
$
$ link/exe=apache$httpd.exe/seg=code=p0 libmain.olb/include=("main"),sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
$ purge/log
$ delete/log/noconf *.olb;*
$
$
$! **********************************************************************
$! **********************************************************************
$! Modules...
$ modules:
$ ccopt = "/warn=noinfo/names=(as_is,shortened)/nolist/define=(LDAP_DEPRECATED=1,_SOCKADDR_LEN,OPENSSL_NO_SRP,HAVE_CONFIG_H,_REENTRANT,_USE_STD_STAT,_LARGEFILE)/include=cc$include/warn=disable=(QUESTCOMPARE,QUESTCOMPARE1)"
$
$! ----------------------------------------------------------------------
$! Aaa...
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.aaa],[-.modules.database],[-.modules.session],ldap$root:[include]
$
$ cc'ccopt' [-.modules.aaa]mod_authn_file.c
$ link/share=mod_authn_file.exe mod_authn_file.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(authn_file_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_authn_dbm.c
$ link/share=mod_authn_dbm.exe mod_authn_dbm.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(authn_dbm_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_authn_anon.c
$ link/share=mod_authn_anon.exe mod_authn_anon.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(authn_anon_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_authn_dbd.c
$ link/share=mod_authn_dbd.exe mod_authn_dbd.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(authn_dbd_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_authn_socache.c
$ link/share=mod_authn_socache.exe mod_authn_socache.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(authn_socache_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_authn_core.c
$ link/share=mod_authn_core.exe mod_authn_core.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(authn_core_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_authz_host.c
$ link/share=mod_authz_host.exe mod_authz_host.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(authz_host_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_authnz_ldap.c
$ link/share=mod_authnz_ldap.exe mod_authnz_ldap.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
ldap$root:[lib.ia64]ldap$libldap32.olb/lib
ldap$root:[lib.ia64]ldap$liblber32.olb/lib
ssl111$root:[lib]ssl111$libssl32.olb/lib
ssl111$root:[lib]ssl111$libcrypto32.olb/lib
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(authnz_ldap_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_authz_groupfile.c
$ link/share=mod_authz_groupfile.exe mod_authz_groupfile.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(authz_groupfile_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_authz_user.c
$ link/share=mod_authz_user.exe mod_authz_user.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(authz_user_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_authz_dbm.c
$ link/share=mod_authz_dbm.exe mod_authz_dbm.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(authz_dbm_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_authz_owner.c
$ link/share=mod_authz_owner.exe mod_authz_owner.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(authz_owner_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_authz_dbd.c
$ link/share=mod_authz_dbd.exe mod_authz_dbd.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(authz_dbd_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_authz_core.c
$ link/share=mod_authz_core.exe mod_authz_core.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(authz_core_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_access_compat.c
$ link/share=mod_access_compat.exe mod_access_compat.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(access_compat_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_auth_basic.c
$ link/share=mod_auth_basic.exe mod_auth_basic.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(auth_basic_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_auth_form.c
$ link/share=mod_auth_form.exe mod_auth_form.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(auth_form_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_auth_digest.c
$ link/share=mod_auth_digest.exe mod_auth_digest.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(auth_digest_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_allowmethods.c
$ link/share=mod_allowmethods.exe mod_allowmethods.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(allowmethods_module=DATA)
$
$ cc'ccopt' [-.modules.aaa]mod_authnz_openvms.c
$ link/share=mod_authnz_openvms.exe mod_authnz_openvms.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr.vms]apache$apr_shrp.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(authnz_openvms_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! Cache...
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.cache],[-.modules.generators]
$
$ cc'ccopt' [-.modules.cache]mod_file_cache.c
$ link/share=mod_file_cache.exe mod_file_cache.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(file_cache_module=DATA)
$
$ cc'ccopt' [-.modules.cache]mod_cache.c
$ cc'ccopt' [-.modules.cache]cache_storage.c
$ cc'ccopt' [-.modules.cache]cache_util.c
$ link/share=mod_cache.exe mod_cache.obj,cache_storage.obj,cache_util.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(cache_module=DATA)
$
$ cc'ccopt' [-.modules.cache]mod_cache_disk.c
$ link/share=mod_cache_disk.exe mod_cache_disk.obj,mod_cache.obj,cache_storage.obj,cache_util.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(cache_disk_module=DATA)
$
$ cc'ccopt' [-.modules.cache]mod_cache_socache.c
$ link/share=mod_cache_socache.exe mod_cache_socache.obj,mod_cache.obj,cache_storage.obj,cache_util.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(cache_socache_module=DATA)
$
$ cc'ccopt' [-.modules.cache]mod_socache_shmcb.c
$ link/share=mod_socache_shmcb.exe mod_socache_shmcb.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(socache_shmcb_module=DATA)
$
$ cc'ccopt' [-.modules.cache]mod_socache_dbm.c
$ link/share=mod_socache_dbm.exe mod_socache_dbm.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(socache_dbm_module=DATA)
$
$ cc'ccopt' [-.modules.cache]mod_socache_memcache.c
$ link/share=mod_socache_memcache.exe mod_socache_memcache.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(socache_memcache_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! Core...
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.core]
$
$ cc'ccopt' [-.modules.core]mod_macro.c
$ link/share=mod_macro.exe mod_macro.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(macro_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! Database
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.database]
$
$ cc'ccopt' [-.modules.database]mod_dbd.c
$ link/share=mod_dbd.exe mod_dbd.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(dbd_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! Debugging...
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.debugging]
$
$ cc'ccopt' [-.modules.debugging]mod_dumpio.c
$ link/share=mod_dumpio.exe mod_dumpio.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(dumpio_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! Filters...
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.filters],[-.modules.ssl],oss$root:[include]
$
$ cc'ccopt' [-.modules.filters]mod_buffer.c
$ link/share=mod_buffer.exe mod_buffer.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(buffer_module=DATA)
$
$ cc'ccopt' [-.modules.filters]mod_charset_lite.c
$ link/share=mod_charset_lite.exe mod_charset_lite.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(charset_lite_module=DATA)
$
$ cc'ccopt' [-.modules.filters]mod_ratelimit.c
$ link/share=mod_ratelimit.exe mod_ratelimit.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(ratelimit_module=DATA)
$
$ cc'ccopt' [-.modules.filters]mod_reqtimeout.c
$ link/share=mod_reqtimeout.exe mod_reqtimeout.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(reqtimeout_module=DATA)
$
$ cc'ccopt' [-.modules.filters]mod_ext_filter.c
$ link/share=mod_ext_filter.exe mod_ext_filter.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(ext_filter_module=DATA)
$
$ cc'ccopt' [-.modules.filters]mod_request.c
$ link/share=mod_request.exe mod_request.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(request_module=DATA)
$
$ cc'ccopt' [-.modules.filters]mod_include.c
$ link/share=mod_include.exe mod_include.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(include_module=DATA)
$
$ cc'ccopt' [-.modules.filters]mod_filter.c
$ link/share=mod_filter.exe mod_filter.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(filter_module=DATA)
$
$ cc'ccopt' [-.modules.filters]mod_substitute.c
$ link/share=mod_substitute.exe mod_substitute.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(substitute_module=DATA)
$
$ cc'ccopt' [-.modules.filters]mod_sed.c
$ cc'ccopt' [-.modules.filters]sed0.c
$ cc'ccopt' [-.modules.filters]sed1.c
$ cc'ccopt' [-.modules.filters]regexp.c
$ link/share=mod_sed.exe mod_sed.obj,sed0.obj,sed1.obj,regexp.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(sed_module=DATA)
$
$ cc'ccopt' [-.modules.filters]mod_deflate.c
$ link/share=mod_deflate.exe mod_deflate.obj,sys$input/opt
CASE_SENSITIVE=YES
oss$root:[lib]libz32.olb/lib
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
SYMBOL_VECTOR=(deflate_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! HTTP...
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.http]
$
$ cc'ccopt' [-.modules.http]mod_mime.c
$ link/share=mod_mime.exe mod_mime.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(mime_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! Loggers
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.loggers]
$
$ cc'ccopt' [-.modules.loggers]mod_log_config.c
$ link/share=mod_log_config.exe mod_log_config.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(log_config_module=DATA)
$
$ cc'ccopt' [-.modules.loggers]mod_log_debug.c
$ link/share=mod_log_debug.exe mod_log_debug.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(log_debug_module=DATA)
$
$ cc'ccopt' [-.modules.loggers]mod_logio.c
$ link/share=mod_logio.exe mod_logio.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(logio_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! Metadata
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.metadata],[-.modules.ssl]
$
$ cc'ccopt' [-.modules.metadata]mod_cern_meta.c
$ link/share=mod_cern_meta.exe mod_cern_meta.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(cern_meta_module=DATA)
$
$ cc'ccopt' [-.modules.metadata]mod_usertrack.c
$ link/share=mod_usertrack.exe mod_usertrack.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(usertrack_module=DATA)
$
$ cc'ccopt' [-.modules.metadata]mod_mime_magic.c
$ link/share=mod_mime_magic.exe mod_mime_magic.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(mime_magic_module=DATA)
$
$ cc'ccopt' [-.modules.metadata]mod_env.c
$ link/share=mod_env.exe mod_env.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(env_module=DATA)
$
$ cc'ccopt' [-.modules.metadata]mod_expires.c
$ link/share=mod_expires.exe mod_expires.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(expires_module=DATA)
$
$ cc'ccopt' [-.modules.metadata]mod_headers.c
$ link/share=mod_headers.exe mod_headers.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(headers_module=DATA)
$
$ cc'ccopt' [-.modules.metadata]mod_unique_id.c
$ link/share=mod_unique_id.exe mod_unique_id.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(unique_id_module=DATA)
$
$ cc'ccopt' [-.modules.metadata]mod_setenvif.c
$ link/share=mod_setenvif.exe mod_setenvif.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(setenvif_module=DATA)
$
$ cc'ccopt' [-.modules.metadata]mod_version.c
$ link/share=mod_version.exe mod_version.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(version_module=DATA)
$
$ cc'ccopt' [-.modules.metadata]mod_remoteip.c
$ link/share=mod_remoteip.exe mod_remoteip.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(remoteip_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! Proxy...
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.proxy],[-.modules.generators],[-.modules.ssl],[-.modules.http2], -
[-.modules.core]
$
$ cc'ccopt' [-.modules.proxy]mod_proxy.c
$ cc'ccopt' [-.modules.proxy]proxy_util.c
$ link/share=mod_proxy.exe mod_proxy.obj,proxy_util.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(-
    ap_proxy_c2hex=PROCEDURE,-
    ap_proxy_canon_netloc=PROCEDURE,-
    ap_proxy_canonenc=PROCEDURE,-
    ap_proxy_checkproxyblock=PROCEDURE,-
    ap_proxy_connect_to_backend=PROCEDURE,-
    ap_proxy_hex2c=PROCEDURE,-
    ap_proxy_is_domainname=PROCEDURE,-
    ap_proxy_is_hostname=PROCEDURE,-
    ap_proxy_is_ipaddr=PROCEDURE,-
    ap_proxy_is_word=PROCEDURE,-
    ap_proxy_pre_http_request=PROCEDURE,-
    ap_proxy_ssl_disable=PROCEDURE,-
    ap_proxy_ssl_enable=PROCEDURE,-
    ap_proxyerror=PROCEDURE,-
    ap_proxy_acquire_connection=PROCEDURE,-
    ap_proxy_backend_broke=PROCEDURE,-
    ap_proxy_conn_is_https=PROCEDURE,-
    ap_proxy_connect_backend=PROCEDURE,-
    ap_proxy_cookie_reverse_map=PROCEDURE,-
    ap_proxy_determine_connection=PROCEDURE,-
    ap_proxy_location_reverse_map=PROCEDURE,-
    ap_proxy_port_of_scheme=PROCEDURE,-
    ap_proxy_release_connection=PROCEDURE,-
    ap_proxy_ssl_val=PROCEDURE,-
    ap_proxy_checkproxyblock2 =PROCEDURE, -
    ap_proxy_connection_create =PROCEDURE, -
    ap_proxy_create_hdrbrgd =PROCEDURE, -
    ap_proxy_define_worker =PROCEDURE, -
    ap_proxy_find_balancershm =PROCEDURE, -
    ap_proxy_find_workershm =PROCEDURE, -
    ap_proxy_get_balancer =PROCEDURE, -
    ap_proxy_get_worker =PROCEDURE, -
    ap_proxy_hashfunc =PROCEDURE, -
    ap_proxy_initialize_balancer =PROCEDURE, -
    ap_proxy_initialize_worker =PROCEDURE, -
    ap_proxy_parse_wstatus =PROCEDURE, -
    ap_proxy_pass_brigade =PROCEDURE, -
    ap_proxy_set_wstatus =PROCEDURE, -
    ap_proxy_share_balancer =PROCEDURE, -
    ap_proxy_share_worker =PROCEDURE, -
    ap_proxy_ssl_connection_cleanup =PROCEDURE, -
    ap_proxy_strncpy =PROCEDURE, -
    ap_proxy_sync_balancer =PROCEDURE, -
    ap_proxy_worker_name =PROCEDURE, -
    proxy_hook_post_request =PROCEDURE, -
    proxy_hook_pre_request =PROCEDURE, -
    proxy_run_create_req =PROCEDURE, -
    proxy_hook_canon_handler=PROCEDURE,-
    proxy_hook_scheme_handler=PROCEDURE,-
    proxy_module=DATA,-
    proxy_run_fixups=PROCEDURE)
!
SYMBOL_VECTOR = ( -
	ap_proxy_connection_create_ex = PROCEDURE, -
	ap_proxy_ssl_engine = PROCEDURE, -
	ap_proxy_buckets_lifeti20si6l1$ = PROCEDURE, -
	ap_proxy_check_connection = PROCEDURE, -
    	ap_proxy_transfer_betwe141hi26$ = PROCEDURE, -
	ap_proxy_show_hcmethod = PROCEDURE, -
    	ap_proxy_connection_reusable = PROCEDURE)
!
SYMBOL_VECTOR = ( -
	proxy_hcmethods = DATA)
!
SYMBOL_VECTOR = ( -
        ap_proxy_prefetch_input = PROCEDURE, -
        ap_proxy_read_input = PROCEDURE, -
        ap_proxy_should_override = PROCEDURE, -
        ap_proxy_spool_input = PROCEDURE, -
        ap_proxy_tunnel_create = PROCEDURE, -
        ap_proxy_tunnel_run = PROCEDURE, -
        ap_proxy_worker_can_upgrade = PROCEDURE)
!
$
$ cc'ccopt' [-.modules.proxy]mod_proxy_connect.c
$ link/share=mod_proxy_connect.exe mod_proxy_connect.obj,mod_proxy.obj,proxy_util.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(proxy_connect_module=DATA)
$
$ cc'ccopt' [-.modules.proxy]mod_proxy_ftp.c
$ link/share=mod_proxy_ftp.exe mod_proxy_ftp.obj,sys$input/opt
sys$disk:[]mod_proxy.exe/share
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(proxy_ftp_module=DATA)
$
$ cc'ccopt' [-.modules.proxy]mod_proxy_http.c
$ link/share=mod_proxy_http.exe mod_proxy_http.obj,sys$input/opt
sys$disk:[]mod_proxy.exe/share
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(proxy_http_module=DATA)
$
$ cc'ccopt' [-.modules.proxy]mod_proxy_fcgi.c
$ link/share=mod_proxy_fcgi.exe mod_proxy_fcgi.obj,sys$input/opt
sys$disk:[]mod_proxy.exe/share
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(proxy_fcgi_module=DATA)
$
$ cc'ccopt' [-.modules.proxy]mod_proxy_scgi.c
$ link/share=mod_proxy_scgi.exe mod_proxy_scgi.obj,sys$input/opt
sys$disk:[]mod_proxy.exe/share
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(proxy_scgi_module=DATA)
$
$ cc'ccopt' [-.modules.proxy]mod_proxy_wstunnel.c
$ link/share=mod_proxy_wstunnel.exe mod_proxy_wstunnel.obj,sys$input/opt
sys$disk:[]mod_proxy.exe/share
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(proxy_wstunnel_module=DATA)
$
$ cc'ccopt' [-.modules.proxy]mod_proxy_ajp.c
$ cc'ccopt' [-.modules.proxy]ajp_header.c
$ cc'ccopt' [-.modules.proxy]ajp_link.c
$ cc'ccopt' [-.modules.proxy]ajp_msg.c
$ cc'ccopt' [-.modules.proxy]ajp_utils.c
$ link/share=mod_proxy_ajp.exe mod_proxy_ajp.obj,ajp_header.obj,ajp_link.obj, -
ajp_msg.obj,ajp_utils.obj,sys$input/opt
sys$disk:[]mod_proxy.exe/share
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(proxy_ajp_module=DATA)
$
$ cc'ccopt' [-.modules.proxy]mod_proxy_balancer.c
$ link/share=mod_proxy_balancer.exe mod_proxy_balancer.obj,sys$input/opt
sys$disk:[]mod_proxy.exe/share
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(proxy_balancer_module=DATA)
$
$ cc'ccopt' [-.modules.proxy]mod_proxy_express.c
$ link/share=mod_proxy_express.exe mod_proxy_express.obj,sys$input/opt
sys$disk:[]mod_proxy.exe/share
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(proxy_express_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! Session
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.session],[-.modules.database]
$
$ cc'ccopt' [-.modules.session]mod_session.c
$ link/share=mod_session.exe mod_session.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(session_module=DATA)
$
$ cc'ccopt' [-.modules.session]mod_session_cookie.c
$ link/share=mod_session_cookie.exe mod_session_cookie.obj,mod_session.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(session_cookie_module=DATA)
$
$ cc'ccopt' [-.modules.session]mod_session_dbd.c
$ link/share=mod_session_dbd.exe mod_session_dbd.obj,mod_session.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(session_dbd_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! Slotmem...
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.slotmem]
$
$ cc'ccopt' [-.modules.slotmem]mod_slotmem_shm.c
$ link/share=mod_slotmem_shm.exe mod_slotmem_shm.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(slotmem_shm_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! SSL...
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.ssl],[-.modules.loggers],[-.modules.generators], -
[-.modules.proxy],[-.modules.md]
$
$ cc'ccopt' [-.modules.ssl]mod_ssl.c
$ cc'ccopt' [-.modules.ssl]ssl_engine_config.c
$ cc'ccopt' [-.modules.ssl]ssl_engine_init.c
$ cc'ccopt' [-.modules.ssl]ssl_engine_io.c
$ cc'ccopt' [-.modules.ssl]ssl_engine_kernel.c
$ cc'ccopt' [-.modules.ssl]ssl_engine_log.c
$ cc'ccopt' [-.modules.ssl]ssl_engine_mutex.c
$ cc'ccopt' [-.modules.ssl]ssl_engine_pphrase.c
$ cc'ccopt' [-.modules.ssl]ssl_engine_rand.c
$ cc'ccopt' [-.modules.ssl]ssl_engine_vars.c
$ cc'ccopt' [-.modules.ssl]ssl_scache.c
$ cc'ccopt' [-.modules.ssl]ssl_util_stapling.c
$ cc'ccopt' [-.modules.ssl]ssl_util.c
$ cc'ccopt' [-.modules.ssl]ssl_util_ssl.c
$ cc'ccopt' [-.modules.ssl]ssl_engine_ocsp.c
$ cc'ccopt' [-.modules.ssl]ssl_util_ocsp.c
$ link/share=mod_ssl.exe mod_ssl.obj,ssl_engine_config.obj,ssl_engine_init.obj,  -
ssl_engine_io.obj,ssl_engine_kernel.obj,ssl_engine_log.obj,ssl_engine_mutex.obj, -
ssl_engine_pphrase.obj,ssl_engine_rand.obj,ssl_engine_vars.obj,ssl_scache.obj,   -
ssl_util_stapling.obj,ssl_util.obj,ssl_util_ssl.obj,ssl_engine_ocsp.obj,ssl_util_ocsp.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
oss$root:[lib]ssl111$libssl32.olb/lib
oss$root:[lib]ssl111$libcrypto32.olb/lib
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(ssl_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! Balancers...
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.balancers],[-.modules.proxy]
$
$ cc'ccopt' [-.modules.proxy.balancers]mod_lbmethod_byrequests.c
$ link/share=mod_lbmethod_byrequests.exe mod_lbmethod_byrequests.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(lbmethod_byrequests_module=DATA)
$
$ cc'ccopt' [-.modules.proxy.balancers]mod_lbmethod_bytraffic.c
$ link/share=mod_lbmethod_bytraffic.exe mod_lbmethod_bytraffic.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(lbmethod_bytraffic_module=DATA)
$
$ cc'ccopt' [-.modules.proxy.balancers]mod_lbmethod_bybusyness.c
$ link/share=mod_lbmethod_bybusyness.exe mod_lbmethod_bybusyness.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(lbmethod_bybusyness_module=DATA)
$
$ cc'ccopt' [-.modules.proxy.balancers]mod_lbmethod_heartbeat.c
$ link/share=mod_lbmethod_heartbeat.exe mod_lbmethod_heartbeat.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(lbmethod_heartbeat_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! UNIX...
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.arch.unix]
$
$ cc'ccopt' [-.modules.arch.unix]mod_unixd.c
$ link/share=mod_unixd.exe mod_unixd.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(unixd_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! DAV...
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.dav.fs],[-.modules.dav.main]
$ cc'ccopt' [-.modules.dav.main]mod_dav.c
$ cc'ccopt' [-.modules.dav.main]props.c
$ cc'ccopt' [-.modules.dav.main]util.c
$ cc'ccopt' [-.modules.dav.main]util_lock.c
$ cc'ccopt' [-.modules.dav.main]liveprop.c
$ cc'ccopt' [-.modules.dav.main]providers.c
$ cc'ccopt' [-.modules.dav.main]std_liveprop.c
$ link/share=mod_dav.exe mod_dav.obj,props.obj,util.obj,util_lock.obj,liveprop.obj,providers.obj,std_liveprop.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(dav_module=DATA)
$
$ cc'ccopt' [-.modules.dav.fs]mod_dav_fs.c
$ cc'ccopt' [-.modules.dav.fs]dbm.c
$ cc'ccopt' [-.modules.dav.fs]lock.c
$ cc'ccopt' [-.modules.dav.fs]repos.c
$ link/share=mod_dav_fs.exe mod_dav_fs.obj,dbm.obj,lock.obj,repos.obj,mod_dav.obj, -
util.obj,util_lock.obj,props.obj,liveprop.obj,std_liveprop.obj,providers.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(dav_fs_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! Generators...
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.generators],[-.modules.filters]
$
$ cc'ccopt' [-.modules.generators]mod_status.c
$ link/share=mod_status.exe mod_status.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(status_module=DATA)
$
$ cc'ccopt' [-.modules.generators]mod_suexec.c
$ link/share=mod_suexec.exe mod_suexec.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr.vms]apache$apr_shrp.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(suexec_module=DATA)
$
$ cc'ccopt' [-.modules.generators]mod_asis.c
$ link/share=mod_asis.exe mod_asis.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(asis_module=DATA)
$
$ cc'ccopt' [-.modules.generators]mod_autoindex.c
$ link/share=mod_autoindex.exe mod_autoindex.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(autoindex_module=DATA)
$
$ cc'ccopt' [-.modules.generators]mod_info.c
$ link/share=mod_info.exe mod_info.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(info_module=DATA)
$
$ cc'ccopt' [-.modules.generators]mod_cgi.c
$ link/share=mod_cgi.exe mod_cgi.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(cgi_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! Mappers...
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.mappers],[-.modules.database],[-.modules.ssl]
$
$ cc'ccopt' [-.modules.mappers]mod_vhost_alias.c
$ link/share=mod_vhost_alias.exe mod_vhost_alias.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(vhost_alias_module=DATA)
$
$ cc'ccopt' [-.modules.mappers]mod_negotiation.c
$ link/share=mod_negotiation.exe mod_negotiation.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(negotiation_module=DATA)
$
$ cc'ccopt' [-.modules.mappers]mod_dir.c
$ link/share=mod_dir.exe mod_dir.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(dir_module=DATA)
$
$ cc'ccopt' [-.modules.mappers]mod_actions.c
$ link/share=mod_actions.exe mod_actions.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(actions_module=DATA)
$
$ cc'ccopt' [-.modules.mappers]mod_speling.c
$ link/share=mod_speling.exe mod_speling.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(speling_module=DATA)
$
$ cc'ccopt' [-.modules.mappers]mod_userdir.c
$ link/share=mod_userdir.exe mod_userdir.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(userdir_module=DATA)
$
$ cc'ccopt' [-.modules.mappers]mod_alias.c
$ link/share=mod_alias.exe mod_alias.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(alias_module=DATA)
$
$ cc'ccopt' [-.modules.mappers]mod_rewrite.c
$ link/share=mod_rewrite.exe mod_rewrite.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(rewrite_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$
$! ----------------------------------------------------------------------
$! Echo...
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.echo]
$
$ cc'ccopt' [-.modules.echo]mod_echo.c
$ link/share=mod_echo.exe mod_echo.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(echo_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! ----------------------------------------------------------------------
$! LDAP...
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.ldap],ldap$root:[include]
$
$ cc'ccopt' [-.modules.ldap]util_ldap.c
$ cc'ccopt' [-.modules.ldap]util_ldap_cache.c
$ cc'ccopt' [-.modules.ldap]util_ldap_cache_mgr.c
$ link/share=mod_ldap.exe util_ldap.obj,util_ldap_cache.obj,util_ldap_cache_mgr.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
ldap$root:[lib.ia64]ldap$libldap32.olb/lib
ldap$root:[lib.ia64]ldap$liblber32.olb/lib
ssl111$root:[lib]ssl111$libssl32.olb/lib
ssl111$root:[lib]ssl111$libcrypto32.olb/lib
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(ldap_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$
$! ----------------------------------------------------------------------
$! Our ISAPI...
$
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[.modules.isapi]
$
$ cc'ccopt' [.modules.isapi]mod_isapi.c
$ link/share=mod_isapi.exe mod_isapi.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(isapi_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! **********************************************************************
$! **********************************************************************
$! ----------------------------------------------------------------------
$! OSU scripts...
$
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[.modules.osuscript]
$
$ cc'ccopt' [.modules.osuscript]mod_osuscript.c
$ cc'ccopt' [.modules.osuscript]decnet_io.c
$
$ link/share=mod_osuscript.exe mod_osuscript.obj,decnet_io.obj,sys$input/opt
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(osuscript_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$! **********************************************************************
$! **********************************************************************
$! Documents
$ docs:
$
$ pwd = f$environment("default")
$ dir_name = f$parse(pwd,,, "directory", "no_conceal")
$ dev_name = f$parse(pwd,,, "device", "no_conceal")
$
$ dir_name = dir_name - "]["
$ dir_name = dir_name - "]"
$ dir_name = dir_name + ".]"
$
$ tmp = dev_name + dir_name
$ write sys$output "Temporary root is ''tmp'"
$
$ if f$search("htdocs.dir") .eqs. ""
$ then
$    create/dir [.htdocs]
$ else
$    delete/tree/noconf [.htdocs...]*.*;*
$ endif
$
$ copy/log [-.docs.docroot]index.html [.htdocs]
$ backup/nolog/replace [-.docs...]*.* [.htdocs...]*.*
$ purge/log [.htdocs...]
$
$ define/process/nolog/translation=(concealed) tmp$root 'tmp
$ backup/nolog tmp$root:[htdocs]index.html,tmp$root:[htdocs.manual...]*.*,tmp$root:[htdocs.error...]*.* []apache$htdocs.bck/save
$ deassign/process tmp$root
$ purge/log apache$htdocs.bck
$
$ delete/tree/noconf [.htdocs...]*.*;*
$ delete/log/noconf htdocs.dir;*
$
$! **********************************************************************
$! **********************************************************************
$! Icons
$ icons:
$
$ pwd = f$environment("default")
$ dir_name = f$parse(pwd,,, "directory", "no_conceal")
$ dev_name = f$parse(pwd,,, "device", "no_conceal")
$
$ dir_name = dir_name - "]["
$ dir_name = dir_name - ".vms]"
$ dir_name = dir_name + ".docs.]"
$
$ tmp = dev_name + dir_name
$ write sys$output "Temporary root is ''tmp'"
$
$ define/process/nolog/translation=(concealed) tmp$root 'tmp
$ backup/nolog tmp$root:[icons...]*.* []apache$icons.bck/save
$ deassign/process tmp$root
$ purge/log apache$icons.bck
$
$! **********************************************************************
$! **********************************************************************
$! Header files (text library)
$ headers:
$
$ lib/create apache$library.tlb/text
$ purge/log
$
$ lib/insert/log apache$library.tlb/text [-.include]*.h
$ lib/insert/log apache$library.tlb/text [-.modules.arch.unix]mod_unixd.h
$ lib/insert/log apache$library.tlb/text [-.modules.core]mod_so.h
$ lib/insert/log apache$library.tlb/text [-.modules.core]mod_watchdog.h
$ lib/insert/log apache$library.tlb/text [-.modules.cache]mod_cache.h
$ lib/insert/log apache$library.tlb/text [-.modules.cache]cache_common.h
$ lib/insert/log apache$library.tlb/text [-.modules.database]mod_dbd.h
$ lib/insert/log apache$library.tlb/text [-.modules.dav.main]mod_dav.h
$ lib/insert/log apache$library.tlb/text [-.modules.filters]mod_include.h
$ lib/insert/log apache$library.tlb/text [-.modules.filters]mod_xml2enc.h
$ lib/insert/log apache$library.tlb/text [-.modules.generators]mod_cgi.h
$ lib/insert/log apache$library.tlb/text [-.modules.generators]mod_status.h
$ lib/insert/log apache$library.tlb/text [-.modules.loggers]mod_log_config.h
$ lib/insert/log apache$library.tlb/text [-.modules.mappers]mod_rewrite.h
$ lib/insert/log apache$library.tlb/text [-.modules.proxy]mod_proxy.h
$ lib/insert/log apache$library.tlb/text [-.modules.session]mod_session.h
$ lib/insert/log apache$library.tlb/text [-.modules.ssl]mod_ssl.h
$ lib/insert/log apache$library.tlb/text [-.os.unix]*.h
$
$ lib/insert/log apache$library.tlb/text [-.srclib.apr.include]apr_*.h
$ lib/insert/log apache$library.tlb/text [-.srclib.apr.include]apr.h
$ lib/insert/log apache$library.tlb/text [-.srclib.apr-util.include]*.h
$
$
$! **********************************************************************
$! **********************************************************************
$! A few extras
$ extras:
$
$! Leave this out for now
$! @log2rabbitmq.com
$
$
$! **********************************************************************
$! **********************************************************************
$! Kit creation
$ pcsi:
$
$ delete/log/noconf *.pcsi;*
$ delete/log/noconf *.pcsi$compressed;*
$
$    product package -
/format=sequential -
/source=httpd_x86.pcsi$desc -
/destination=[] -
/opt=noconfirm -
/material=([],[-],[.scripts],[-.docs.conf],[-.docs.conf.extra],[.docs.cgi-examples],[-.srclib.apr.vms],[-.srclib.apr-util.vms],oss$root:[bin]) -
csws
$
$
$ product copy csws/dest=[]/format=compressed/opt=noconfirm
$ purge/log
$
$ exit
