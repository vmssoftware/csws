$ set verify
$ @sys$startup:ssl1$startup.com
$
$
$ ccopt = "/names=(as_is,shortened)/nolist/define=(HAVE_CONFIG_H,_REENTRANT,_USE_STD_STAT,_LARGEFILE)/include=cc$include/warn=disable=(QUESTCOMPARE,QUESTCOMPARE1)"
$
$! ----------------------------------------------------------------------
$! Lua
$ define cc$include [-.os.unix],[-.include], -
[-.srclib.apr.include],[-.srclib.apr.include.arch.unix],[-.srclib.apr.include.arch], -
[-.srclib.apr.vms], -
[-.srclib.apr-util.include],[-.support],[-.vms],[-.modules.lua],[-.modules.database],[-.modules.ssl],lua$root:[include]
$
$ cc'ccopt' [-.modules.lua]lua_apr.c
$ cc'ccopt' [-.modules.lua]lua_config.c
$ cc'ccopt' [-.modules.lua]lua_dbd.c
$ cc'ccopt' [-.modules.lua]lua_passwd.c
$ cc'ccopt' [-.modules.lua]lua_request.c
$ cc'ccopt' [-.modules.lua]lua_vmprep.c
$ cc'ccopt' [-.modules.lua]mod_lua.c
$
$ link/share=mod_lua.exe lua_apr.obj,lua_config.obj,lua_dbd.obj,lua_passwd.obj,lua_request.obj,lua_vmprep.obj,mod_lua.obj,sys$input/opt
lua$root:[lib]liblua.olb/lib
sys$disk:[-.srclib.apr.vms]apache$apr_shr.exe/share
sys$disk:[-.srclib.apr-util.vms]apache$apu_shr.exe/share
sys$disk:[]apache$httpd_shr.exe/share
CASE_SENSITIVE=YES
SYMBOL_VECTOR=(lua_module=DATA)
$
$ purge/log
$ delete/log/noconf *.obj;*
$
$
$
$
$ exit
