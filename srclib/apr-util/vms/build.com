$ set verify
$
$ define cc$include [-.include],[-.include.private],[--.apr.include],[-.xml.expat.lib],[-.vms],[-.dbm.vms],ldap$root:[include]
$ ccopt = "/names=(as_is,shortened)/nolist/define=(LDAP_DEPRECATED=1,_SOCKADDR_LEN,HAVE_CONFIG_H,_REENTRANT,_USE_STD_STAT,_LARGEFILE)/include=cc$include/warn=disable=(QUESTCOMPARE,QUESTCOMPARE1)"
$
$ purge/log [-...]
$
$ cc'ccopt' [-.redis]apr_redis.c
$ cc'ccopt' [-.buckets]apr_brigade.c
$ cc'ccopt' [-.buckets]apr_buckets.c
$ cc'ccopt' [-.buckets]apr_buckets_alloc.c
$ cc'ccopt' [-.buckets]apr_buckets_eos.c
$ cc'ccopt' [-.buckets]apr_buckets_file.c
$ cc'ccopt' [-.buckets]apr_buckets_flush.c
$ cc'ccopt' [-.buckets]apr_buckets_heap.c
$ cc'ccopt' [-.buckets]apr_buckets_mmap.c
$ cc'ccopt' [-.buckets]apr_buckets_pipe.c
$ cc'ccopt' [-.buckets]apr_buckets_pool.c
$ cc'ccopt' [-.buckets]apr_buckets_refcount.c
$ cc'ccopt' [-.buckets]apr_buckets_simple.c
$ cc'ccopt' [-.buckets]apr_buckets_socket.c
$ cc'ccopt' [-.crypto]apr_crypto.c
$ cc'ccopt' [-.crypto]apr_md4.c
$ cc'ccopt' [-.crypto]apr_md5.c
$ cc'ccopt' [-.crypto]apr_passwd.c
$ cc'ccopt' [-.crypto]apr_sha1.c
$ cc'ccopt' [-.crypto]crypt_blowfish.c
$ cc'ccopt' [-.crypto]getuuid.c
$ cc'ccopt' [-.crypto]uuid.c
$ cc'ccopt' [-.dbd]apr_dbd.c
$ cc'ccopt' [-.dbm]apr_dbm.c
$ cc'ccopt' [-.dbm]apr_dbm_sdbm.c
$ cc'ccopt' [-.dbm.sdbm]sdbm.c
$ cc'ccopt' [-.dbm.sdbm]sdbm_hash.c
$ cc'ccopt' [-.dbm.sdbm]sdbm_lock.c
$ cc'ccopt' [-.dbm.sdbm]sdbm_pair.c
$ cc'ccopt' [-.dbm.vms]apr_dbm_vdbm.c
$ cc'ccopt' [-.dbm.vms]vdbm.c
$ cc'ccopt' [-.redis]apr_redis.c
$ cc'ccopt' [-.encoding]apr_base64.c
$ cc'ccopt' [-.hooks]apr_hooks.c
$ cc'ccopt' [-.ldap]apr_ldap_init.c
$ cc'ccopt' [-.ldap]apr_ldap_option.c
$ cc'ccopt' [-.ldap]apr_ldap_rebind.c
$ cc'ccopt' [-.ldap]apr_ldap_stub.c
$ cc'ccopt' [-.ldap]apr_ldap_url.c
$ cc'ccopt' [-.memcache]apr_memcache.c
$ cc'ccopt' [-.misc]apr_date.c
$ cc'ccopt' [-.misc]apr_queue.c
$ cc'ccopt' [-.misc]apr_reslist.c
$ cc'ccopt' [-.misc]apr_rmm.c
$ cc'ccopt' [-.misc]apr_thread_pool.c
$ cc'ccopt' [-.misc]apu_dso.c
$ cc'ccopt' [-.misc]apu_version.c
$ cc'ccopt' [-.strmatch]apr_strmatch.c
$ cc'ccopt' [-.uri]apr_uri.c
$ cc'ccopt' [-.xlate]xlate.c
$ cc'ccopt' [-.xml]apr_xml.c
$
$ lib/create libaprutil.olb
$ purge/log
$
$ lib/insert libaprutil.olb apr_redis.obj
$ lib/insert libaprutil.olb apr_brigade.obj
$ lib/insert libaprutil.olb apr_buckets.obj
$ lib/insert libaprutil.olb apr_buckets_alloc.obj
$ lib/insert libaprutil.olb apr_buckets_eos.obj
$ lib/insert libaprutil.olb apr_buckets_file.obj
$ lib/insert libaprutil.olb apr_buckets_flush.obj
$ lib/insert libaprutil.olb apr_buckets_heap.obj
$ lib/insert libaprutil.olb apr_buckets_mmap.obj
$ lib/insert libaprutil.olb apr_buckets_pipe.obj
$ lib/insert libaprutil.olb apr_buckets_pool.obj
$ lib/insert libaprutil.olb apr_buckets_refcount.obj
$ lib/insert libaprutil.olb apr_buckets_simple.obj
$ lib/insert libaprutil.olb apr_buckets_socket.obj
$ lib/insert libaprutil.olb apr_crypto.obj
$ lib/insert libaprutil.olb apr_md4.obj
$ lib/insert libaprutil.olb apr_md5.obj
$ lib/insert libaprutil.olb apr_passwd.obj
$ lib/insert libaprutil.olb apr_sha1.obj
$ lib/insert libaprutil.olb crypt_blowfish.obj
$ lib/insert libaprutil.olb getuuid.obj
$ lib/insert libaprutil.olb uuid.obj
$ lib/insert libaprutil.olb apr_dbd.obj
$ lib/insert libaprutil.olb apr_dbm.obj
$ lib/insert libaprutil.olb apr_dbm_sdbm.obj
$ lib/insert libaprutil.olb sdbm.obj
$ lib/insert libaprutil.olb sdbm_hash.obj
$ lib/insert libaprutil.olb sdbm_lock.obj
$ lib/insert libaprutil.olb sdbm_pair.obj
$ lib/insert libaprutil.olb apr_dbm_vdbm.obj
$ lib/insert libaprutil.olb vdbm.obj
$ lib/insert libaprutil.olb apr_base64.obj
$ lib/insert libaprutil.olb apr_hooks.obj
$ lib/insert libaprutil.olb apr_ldap_init.obj
$ lib/insert libaprutil.olb apr_ldap_option.obj
$ lib/insert libaprutil.olb apr_ldap_rebind.obj
$ lib/insert libaprutil.olb apr_ldap_stub.obj
$ lib/insert libaprutil.olb apr_ldap_url.obj
$ lib/insert libaprutil.olb apr_memcache.obj
$ lib/insert libaprutil.olb apr_date.obj
$ lib/insert libaprutil.olb apr_queue.obj
$ lib/insert libaprutil.olb apr_reslist.obj
$ lib/insert libaprutil.olb apr_rmm.obj
$ lib/insert libaprutil.olb apr_thread_pool.obj
$ lib/insert libaprutil.olb apu_dso.obj
$ lib/insert libaprutil.olb apu_version.obj
$ lib/insert libaprutil.olb apr_strmatch.obj
$ lib/insert libaprutil.olb apr_uri.obj
$ lib/insert libaprutil.olb xlate.obj
$ lib/insert libaprutil.olb apr_xml.obj
$
$ purge/log [-...]
$ delete/log/noconf *.obj;*
$
$ ccopt = "/names=(as_is,shortened)/nolist/define=(HAVE_EXPAT_CONFIG_H)/include=cc$include/warn=disable=(QUESTCOMPARE,QUESTCOMPARE1)"
$
$ cc'ccopt' [-.xml.expat.lib]xmlparse.c
$ cc'ccopt' [-.xml.expat.lib]xmltok.c
$ cc'ccopt' [-.xml.expat.lib]xmlrole.c
$
$ lib/create libexpat.olb
$ purge/log
$
$ lib/insert libexpat.olb xmlparse.obj
$ lib/insert libexpat.olb xmltok.obj
$ lib/insert libexpat.olb xmlrole.obj
$
$ purge/log [-...]
$ delete/log/noconf *.obj;*
$
$ link/share=apache$apu_shr.exe apu_shr.opt/opt
$ purge/log
$
$ exit

$ cc'ccopt' [-.dbd]apr_dbd_sqlite3.c
/bin/bash /home/cameron/httpd-2.4.12/srclib/apr/libtool --silent --mode=link gcc  -g -O2 -pthread       -release 1 -module -rpath
/usr/local/apache2/lib/apr-util-1 -o dbd/apr_dbd_sqlite3.la dbd/apr_dbd_sqlite3.lo -lsqlite3



