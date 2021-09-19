$ set verify
$
$ define cc$include [-.include],[-.include.arch],[-.include.arch.unix],[-.include.private],[-.vms]
$ ccopt = "/names=(as_is,shortened)/nolist/define=(_SOCKADDR_LEN,HAVE_CONFIG_H,_REENTRANT,_USE_STD_STAT,_LARGEFILE)/include=cc$include/warn=disable=(QUESTCOMPARE,QUESTCOMPARE1)"
$
$
$ cc'ccopt' [-.tools]gen_test_char.c
$ link gen_test_char.obj
$ purge/log
$ delete/log/noconf gen_test_char.obj;*
$
$ pipe run gen_test_char.EXE | type/output=[-.include.private]apr_escape_test_char.h sys$input
$ purge/log [-...]
$
$ cc'ccopt' [-.encoding]apr_escape.c
$ cc'ccopt' [-.passwd]apr_getpass.c
$ cc'ccopt' [-.strings]apr_cpystrn.c
$ cc'ccopt' [-.strings]apr_fnmatch.c
$ cc'ccopt' [-.strings]apr_snprintf.c
$ cc'ccopt' [-.strings]apr_strings.c
$ cc'ccopt' [-.strings]apr_strnatcmp.c
$ cc'ccopt' [-.strings]apr_strtok.c
$ cc'ccopt' [-.tables]apr_hash.c
$ cc'ccopt' [-.tables]apr_skiplist.c
$ cc'ccopt' [-.tables]apr_tables.c
$ cc'ccopt' [-.atomic.unix]builtins.c
$ cc'ccopt' [-.atomic.unix]ia32.c
$ cc'ccopt' [-.atomic.unix]mutex.c
$ cc'ccopt' [-.atomic.unix]mutex64.c
$ cc'ccopt' [-.atomic.unix]ppc.c
$ cc'ccopt' [-.atomic.unix]s390.c
$ cc'ccopt' [-.atomic.unix]solaris.c
$ cc'ccopt' [-.dso.unix]dso.c
$ cc'ccopt' [-.file_io.unix]buffer.c
$ cc'ccopt' [-.file_io.unix]copy.c
$ cc'ccopt' [-.file_io.unix]dir.c
$ cc'ccopt' [-.file_io.unix]fileacc.c
$ cc'ccopt' [-.file_io.unix]filedup.c
$ cc'ccopt' [-.file_io.unix]filepath.c
$ cc'ccopt' [-.file_io.unix]filepath_util.c
$ cc'ccopt' [-.file_io.unix]filestat.c
$ cc'ccopt' [-.file_io.unix]flock.c
$ cc'ccopt' [-.file_io.unix]fullrw.c
$ cc'ccopt' [-.file_io.unix]mktemp.c
$ cc'ccopt' [-.file_io.unix]open.c
$ cc'ccopt' [-.file_io.unix]pipe.c
$ cc'ccopt' [-.file_io.unix]readwrite.c
$ cc'ccopt' [-.file_io.unix]seek.c
$ cc'ccopt' [-.file_io.unix]tempdir.c
$ cc'ccopt' [-.locks.unix]global_mutex.c
$ cc'ccopt' [-.locks.unix]proc_mutex.c
$ cc'ccopt' [-.locks.unix]thread_cond.c
$ cc'ccopt' [-.locks.unix]thread_mutex.c
$ cc'ccopt' [-.locks.unix]thread_rwlock.c
$ cc'ccopt' [-.memory.unix]apr_pools.c
$ cc'ccopt' [-.misc.unix]charset.c
$ cc'ccopt' [-.misc.unix]env.c
$ cc'ccopt' [-.misc.unix]errorcodes.c
$ cc'ccopt' [-.misc.unix]getopt.c
$ cc'ccopt' [-.misc.unix]otherchild.c
$ cc'ccopt' [-.misc.unix]rand.c
$ cc'ccopt' [-.misc.unix]start.c
$ cc'ccopt' [-.misc.unix]version.c
$ cc'ccopt' [-.mmap.unix]common.c
$ cc'ccopt' [-.mmap.unix]mmap.c
$ cc'ccopt' [-.network_io.unix]inet_ntop.c
$ cc'ccopt' [-.network_io.unix]inet_pton.c
$ cc'ccopt' [-.network_io.unix]multicast.c
$ cc'ccopt' [-.network_io.unix]sendrecv.c
$ cc'ccopt' [-.network_io.unix]sockaddr.c
$ cc'ccopt' [-.network_io.unix]socket_util.c
$ cc'ccopt' [-.network_io.unix]sockets.c
$ cc'ccopt' [-.network_io.unix]sockopt.c
$ cc'ccopt' [-.poll.unix]epoll.c
$ cc'ccopt' [-.poll.unix]kqueue.c
$ cc'ccopt' [-.poll.unix]poll.c
$ cc'ccopt' [-.poll.unix]pollcb.c
$ cc'ccopt' [-.poll.unix]pollset.c
$ cc'ccopt' [-.poll.unix]wakeup.c
$ cc'ccopt' [-.poll.unix]port.c
$ cc'ccopt' [-.poll.unix]select.c
$ cc'ccopt' [-.poll.unix]z_asio.c
$ cc'ccopt' [-.random.unix]apr_random.c
$ cc'ccopt' [-.random.unix]sha2.c
$ cc'ccopt' [-.random.unix]sha2_glue.c
$ cc'ccopt' [-.shmem.unix]shm.c
$ cc'ccopt' [-.support.unix]waitio.c
$ cc'ccopt' [-.threadproc.unix]proc.c
$ cc'ccopt' [-.threadproc.unix]procsup.c
$ cc'ccopt' [-.threadproc.unix]signals.c
$ cc'ccopt' [-.threadproc.unix]thread.c
$ cc'ccopt' [-.threadproc.unix]threadpriv.c
$ cc'ccopt' [-.time.unix]time.c
$ cc'ccopt' [-.time.unix]timestr.c
$ cc'ccopt' [-.user.unix]groupinfo.c
$ cc'ccopt' [-.user.unix]userinfo.c
$ cc'ccopt' hash.c
$ cc'ccopt' list.c
$ cc'ccopt' sb_cache.c
$
$ lib/create libapr.olb
$ purge/log
$
$ lib/insert libapr.olb apr_escape.obj
$ lib/insert libapr.olb apr_getpass.obj
$ lib/insert libapr.olb apr_cpystrn.obj
$ lib/insert libapr.olb apr_fnmatch.obj
$ lib/insert libapr.olb apr_snprintf.obj
$ lib/insert libapr.olb apr_strings.obj
$ lib/insert libapr.olb apr_strnatcmp.obj
$ lib/insert libapr.olb apr_strtok.obj
$ lib/insert libapr.olb apr_hash.obj
$ lib/insert libapr.olb apr_skiplist.obj
$ lib/insert libapr.olb apr_tables.obj
$ lib/insert libapr.olb builtins.obj
$ lib/insert libapr.olb ia32.obj
$ lib/insert libapr.olb mutex.obj
$ lib/insert libapr.olb mutex64.obj
$ lib/insert libapr.olb ppc.obj
$ lib/insert libapr.olb s390.obj
$ lib/insert libapr.olb solaris.obj
$ lib/insert libapr.olb dso.obj
$ lib/insert libapr.olb buffer.obj
$ lib/insert libapr.olb copy.obj
$ lib/insert libapr.olb dir.obj
$ lib/insert libapr.olb fileacc.obj
$ lib/insert libapr.olb filedup.obj
$ lib/insert libapr.olb filepath.obj
$ lib/insert libapr.olb filepath_util.obj
$ lib/insert libapr.olb filestat.obj
$ lib/insert libapr.olb flock.obj
$ lib/insert libapr.olb fullrw.obj
$ lib/insert libapr.olb mktemp.obj
$ lib/insert libapr.olb open.obj
$ lib/insert libapr.olb pipe.obj
$ lib/insert libapr.olb readwrite.obj
$ lib/insert libapr.olb seek.obj
$ lib/insert libapr.olb tempdir.obj
$ lib/insert libapr.olb global_mutex.obj
$ lib/insert libapr.olb proc_mutex.obj
$ lib/insert libapr.olb thread_cond.obj
$ lib/insert libapr.olb thread_mutex.obj
$ lib/insert libapr.olb thread_rwlock.obj
$ lib/insert libapr.olb apr_pools.obj
$ lib/insert libapr.olb charset.obj
$ lib/insert libapr.olb env.obj
$ lib/insert libapr.olb errorcodes.obj
$ lib/insert libapr.olb getopt.obj
$ lib/insert libapr.olb otherchild.obj
$ lib/insert libapr.olb rand.obj
$ lib/insert libapr.olb start.obj
$ lib/insert libapr.olb version.obj
$ lib/insert libapr.olb common.obj
$ lib/insert libapr.olb mmap.obj
$ lib/insert libapr.olb inet_ntop.obj
$ lib/insert libapr.olb inet_pton.obj
$ lib/insert libapr.olb multicast.obj
$ lib/insert libapr.olb sendrecv.obj
$ lib/insert libapr.olb sockaddr.obj
$ lib/insert libapr.olb socket_util.obj
$ lib/insert libapr.olb sockets.obj
$ lib/insert libapr.olb sockopt.obj
$ lib/insert libapr.olb epoll.obj
$ lib/insert libapr.olb kqueue.obj
$ lib/insert libapr.olb poll.obj
$ lib/insert libapr.olb pollcb.obj
$ lib/insert libapr.olb pollset.obj
$ lib/insert libapr.olb wakeup.obj
$ lib/insert libapr.olb port.obj
$ lib/insert libapr.olb select.obj
$ lib/insert libapr.olb z_asio.obj
$ lib/insert libapr.olb apr_random.obj
$ lib/insert libapr.olb sha2.obj
$ lib/insert libapr.olb sha2_glue.obj
$ lib/insert libapr.olb shm.obj
$ lib/insert libapr.olb waitio.obj
$ lib/insert libapr.olb proc.obj
$ lib/insert libapr.olb procsup.obj
$ lib/insert libapr.olb signals.obj
$ lib/insert libapr.olb thread.obj
$ lib/insert libapr.olb threadpriv.obj
$ lib/insert libapr.olb time.obj
$ lib/insert libapr.olb timestr.obj
$ lib/insert libapr.olb groupinfo.obj
$ lib/insert libapr.olb userinfo.obj
$ lib/insert libapr.olb hash.obj
$ lib/insert libapr.olb list.obj
$ lib/insert libapr.olb sb_cache.obj
$ delete/log/noconf *.obj;*
$
$
$ cc'ccopt' sleep.c
$ cc'ccopt' mailbox.c
$ cc'ccopt' ipv6.c
$ cc'ccopt' vmsshm.c
$ cc'ccopt' protshr.c+sys$library:sys$lib_c.tlb/lib
$ cc'ccopt' getpids.c
$ cc'ccopt' auth_sysuaf.c
$ cc'ccopt' check_rights.c
$ cc'ccopt' vms_flock.c
$ cc'ccopt' cvtfnm.c
$ cc'ccopt' vms_stat.c+sys$library:sys$lib_c.tlb/lib
$
$ lib/create libapr_vms.olb
$ purge/log
$
$ lib/insert libapr_vms.olb sleep.obj
$ lib/insert libapr_vms.olb mailbox.obj
$ lib/insert libapr_vms.olb ipv6.obj
$ lib/insert libapr_vms.olb vmsshm.obj
$ lib/insert libapr_vms.olb protshr.obj
$ lib/insert libapr_vms.olb getpids.obj
$ lib/insert libapr_vms.olb auth_sysuaf.obj
$ lib/insert libapr_vms.olb check_rights.obj
$ lib/insert libapr_vms.olb vms_flock.obj
$ lib/insert libapr_vms.olb cvtfnm.obj
$ lib/insert libapr_vms.olb vms_stat.obj
$ delete/log/noconf *.obj;*
$
$ link/notrace/sysexe/share=apache$apr_shrp.exe apr_shrp.opt/opt
$ purge/log
$
$ link/share=apache$apr_shr.exe apr_shr.opt/opt
$ purge/log
$
$ exit

