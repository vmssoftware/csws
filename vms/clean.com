$ set verify
$
$! **** APR
$ set default [-.srclib.apr.vms]
$ @clean.com
$ set default [---.vms]
$
$! **** APR Utils
$ set default [-.srclib.apr-util.vms]
$ @clean.com
$ set default [---.vms]
$
$
$ purge/log
$ delete/log/noconf [-...]*.exe;*
$ delete/log/noconf [-...]*.obj;*
$ delete/log/noconf [-...]*.lis;*
$ delete/log/noconf [-...]*.olb;*
$ delete/log/noconf [-...]*.tlb;*
$ delete/log/noconf [-...]*.map;*
$ delete/log/noconf [-...]*.log;*
$
$ delete/log/noconf [.cxx_repository]cxx$demangler_db.;*
$ delete/log/noconf cxx_repository.dir;1
$
$ delete/log/noconf [.modules.mod_wsgi.src.server.cxx_repository]cxx$demangler_db.;*
$ delete/log/noconf [.modules.mod_wsgi.src.server]cxx_repository.dir;1
$
$ delete/log/noconf *.pcsi;*
$ delete/log/noconf *.pcsi$compressed;*
$ delete/log/noconf *.bck;*
$
$! This is generated when we do the full build
$ delete/log/noconf [-.srclib.apr.include.private]apr_escape_test_char.h;*
$
$ exit
