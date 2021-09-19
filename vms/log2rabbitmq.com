$ set verify
$ define cc$include librabbitmq$root:[include]
$
$ cc/names=(as_is,shortened)/include=cc$include log2rabbitmq.c
$ link/exe=log2rabbitmq.exe log2rabbitmq.obj,sys$input/opt
librabbitmq$root:[lib]librmq091.olb/lib
librabbitmq$root:[lib]librabbitmq091.olb/lib
$
$ deassign cc$include
$ exit
