/* Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * @file  ap_config_layout.h
 * @brief Apache Config Layout
 */

#ifndef AP_CONFIG_LAYOUT_H
#define AP_CONFIG_LAYOUT_H

/* Configured Apache directory layout */
#ifdef __VMS
#define DEFAULT_PREFIX "/apache$root"
#define DEFAULT_EXP_EXEC_PREFIX "/apache$root"
#define DEFAULT_REL_EXEC_PREFIX ""
#define DEFAULT_EXP_BINDIR "/apache$root/bin"
#define DEFAULT_REL_BINDIR "bin"
#define DEFAULT_EXP_SBINDIR "/apache$root/bin"
#define DEFAULT_REL_SBINDIR "bin"
#define DEFAULT_EXP_LIBEXECDIR "/apache$root/modules"
#define DEFAULT_REL_LIBEXECDIR "modules"
#define DEFAULT_EXP_MANDIR "/apache$root/man"
#define DEFAULT_REL_MANDIR "man"
#define DEFAULT_EXP_SYSCONFDIR "/apache$root/conf"
#define DEFAULT_REL_SYSCONFDIR "conf"
#define DEFAULT_EXP_DATADIR "/apache$root"
#define DEFAULT_REL_DATADIR ""
#define DEFAULT_EXP_INSTALLBUILDDIR "/apache$root/build"
#define DEFAULT_REL_INSTALLBUILDDIR "build"
#define DEFAULT_EXP_ERRORDIR "/apache$root/error"
#define DEFAULT_REL_ERRORDIR "error"
#define DEFAULT_EXP_ICONSDIR "/apache$root/icons"
#define DEFAULT_REL_ICONSDIR "icons"
#define DEFAULT_EXP_HTDOCSDIR "/apache$root/htdocs"
#define DEFAULT_REL_HTDOCSDIR "htdocs"
#define DEFAULT_EXP_MANUALDIR "/apache$root/manual"
#define DEFAULT_REL_MANUALDIR "manual"
#define DEFAULT_EXP_CGIDIR "/apache$root/cgi-bin"
#define DEFAULT_REL_CGIDIR "cgi-bin"
#define DEFAULT_EXP_INCLUDEDIR "/apache$root/include"
#define DEFAULT_REL_INCLUDEDIR "include"
#define DEFAULT_EXP_LOCALSTATEDIR "/apache$root"
#define DEFAULT_REL_LOCALSTATEDIR ""
#define DEFAULT_EXP_RUNTIMEDIR "/apache$root/logs"
#define DEFAULT_REL_RUNTIMEDIR "logs"
#define DEFAULT_EXP_LOGFILEDIR "/apache$root/logs"
#define DEFAULT_REL_LOGFILEDIR "logs"
#define DEFAULT_EXP_PROXYCACHEDIR "/apache$root/proxy"
#define DEFAULT_REL_PROXYCACHEDIR "proxy"
#else
#define DEFAULT_PREFIX "/usr/local/apache2"
#define DEFAULT_EXP_EXEC_PREFIX "/usr/local/apache2"
#define DEFAULT_REL_EXEC_PREFIX ""
#define DEFAULT_EXP_BINDIR "/usr/local/apache2/bin"
#define DEFAULT_REL_BINDIR "bin"
#define DEFAULT_EXP_SBINDIR "/usr/local/apache2/bin"
#define DEFAULT_REL_SBINDIR "bin"
#define DEFAULT_EXP_LIBEXECDIR "/usr/local/apache2/modules"
#define DEFAULT_REL_LIBEXECDIR "modules"
#define DEFAULT_EXP_MANDIR "/usr/local/apache2/man"
#define DEFAULT_REL_MANDIR "man"
#define DEFAULT_EXP_SYSCONFDIR "/usr/local/apache2/conf"
#define DEFAULT_REL_SYSCONFDIR "conf"
#define DEFAULT_EXP_DATADIR "/usr/local/apache2"
#define DEFAULT_REL_DATADIR ""
#define DEFAULT_EXP_INSTALLBUILDDIR "/usr/local/apache2/build"
#define DEFAULT_REL_INSTALLBUILDDIR "build"
#define DEFAULT_EXP_ERRORDIR "/usr/local/apache2/error"
#define DEFAULT_REL_ERRORDIR "error"
#define DEFAULT_EXP_ICONSDIR "/usr/local/apache2/icons"
#define DEFAULT_REL_ICONSDIR "icons"
#define DEFAULT_EXP_HTDOCSDIR "/usr/local/apache2/htdocs"
#define DEFAULT_REL_HTDOCSDIR "htdocs"
#define DEFAULT_EXP_MANUALDIR "/usr/local/apache2/manual"
#define DEFAULT_REL_MANUALDIR "manual"
#define DEFAULT_EXP_CGIDIR "/usr/local/apache2/cgi-bin"
#define DEFAULT_REL_CGIDIR "cgi-bin"
#define DEFAULT_EXP_INCLUDEDIR "/usr/local/apache2/include"
#define DEFAULT_REL_INCLUDEDIR "include"
#define DEFAULT_EXP_LOCALSTATEDIR "/usr/local/apache2"
#define DEFAULT_REL_LOCALSTATEDIR ""
#define DEFAULT_EXP_RUNTIMEDIR "/usr/local/apache2/logs"
#define DEFAULT_REL_RUNTIMEDIR "logs"
#define DEFAULT_EXP_LOGFILEDIR "/usr/local/apache2/logs"
#define DEFAULT_REL_LOGFILEDIR "logs"
#define DEFAULT_EXP_PROXYCACHEDIR "/usr/local/apache2/proxy"
#define DEFAULT_REL_PROXYCACHEDIR "proxy"
#endif

#endif /* AP_CONFIG_LAYOUT_H */
