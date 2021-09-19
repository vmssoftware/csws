$ set verify
$
$ define cpython python$root:[include.cpython]
$ define cc$include oss$root:[include],python$root:[include]
$ ccopt = "/nolist/pointer_size=32/warn=disable=(QUESTCOMPARE)/define=(_USE_STD_STAT)/names=(as_is,shortened)/include=cc$include"
$
$ cc'ccopt' mod_wsgi.c		+[----]APACHE$LIBRARY.TLB/lib
$ cc'ccopt' wsgi_interp.c	+[----]APACHE$LIBRARY.TLB/lib
$ cc'ccopt' wsgi_convert.c	+[----]APACHE$LIBRARY.TLB/lib
$ cc'ccopt' wsgi_metrics.c	+[----]APACHE$LIBRARY.TLB/lib
$ cc'ccopt' wsgi_server.c	+[----]APACHE$LIBRARY.TLB/lib
$ cc'ccopt' wsgi_logger.c	+[----]APACHE$LIBRARY.TLB/lib
$ cc'ccopt' wsgi_validate.c	+[----]APACHE$LIBRARY.TLB/lib
$ cc'ccopt' wsgi_thread.c 	+[----]APACHE$LIBRARY.TLB/lib
$ cc'ccopt' wsgi_restrict.c	+[----]APACHE$LIBRARY.TLB/lib
$ cc'ccopt' wsgi_memory.c	+[----]APACHE$LIBRARY.TLB/lib
$ cc'ccopt' wsgi_stream.c 	+[----]APACHE$LIBRARY.TLB/lib
$
$ lib/create libwsgi.olb
$ purge/log
$
$ lib/insert libwsgi.olb wsgi_interp.obj
$ lib/insert libwsgi.olb wsgi_convert.obj
$ lib/insert libwsgi.olb wsgi_metrics.obj
$ lib/insert libwsgi.olb wsgi_server.obj
$ lib/insert libwsgi.olb wsgi_logger.obj
$ lib/insert libwsgi.olb wsgi_validate.obj
$ lib/insert libwsgi.olb wsgi_thread.obj
$ lib/insert libwsgi.olb wsgi_restrict.obj
$ lib/insert libwsgi.olb wsgi_memory.obj
$ lib/insert libwsgi.olb wsgi_stream.obj
$
$ link/share=[----]mod_wsgi.exe mod_wsgi.obj,libwsgi.olb/lib,sys$input/opt
apache$apr_shr/share
apache$apu_shr/share
apache$httpd_shr/share
python$shr/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(wsgi_module=DATA)
$
$
$ delete/log/noconf libwsgi.olb;*
$ purge/log
$ delete/log/noconf *.obj;*
$
$ exit

$ copy/log mod_wsgi.exe apache$common:[modules]
$ purge/log apache$common:[modules]
$ @sys$startup:apache$shutdown
$ wait 00:00:10
$ @sys$startup:apache$startup
$ exit
