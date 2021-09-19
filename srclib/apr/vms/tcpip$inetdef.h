/********************************************************************************************************************************/
/* Created:  8-Nov-2014 10:43:26 by OpenVMS SDL EV2-1      */
/* Source:  24-JUL-2008 21:21:38 BUILD32$:[TCPIP_V57_BLECO5.SRC.NET]INET_USER.SDL;1 */
/********************************************************************************************************************************/
/*** MODULE $$BEGIN ***/
#pragma __member_alignment __save
#pragma __nomember_alignment
#ifndef TCPIP$INETDEF_LOADED
# define TCPIP$INETDEF_LOADED 1
 
#pragma __member_alignment __restore
/*** MODULE $ARPREQDEF ***/
#pragma __member_alignment __save
#pragma __nomember_alignment
#define ARP$M_IN_USE 0x1
#define ARP$M_COM 0x2
#define ARP$M_PERM 0x4
#define ARP$M_PUBL 0x8
#define ARP$M_USETRAILERS 0x10
#define ARP$C_LENGTH 34
#define ARP$K_LENGTH 34
struct ARPREQDEF {
    char ARP$T_PA [16];                 /* IP address                       */
/* $SOCKADDRINDEF defines offsets                                           */
    char ARP$T_HA [16];                 /* Ethernet hardware address        */
/* $SOCKADDRDEF defines offsets                                             */
    union  {                            /*                                  */
        unsigned short int ARP$W_FLAGS; /* flags                            */
        struct  {                       /*                                  */
            unsigned ARP$V_IN_USE : 1;  /* ARP entry is in use              */
            unsigned ARP$V_COM : 1;     /* ARP entry is complete            */
            unsigned ARP$V_PERM : 1;    /* ARP entry is pemanent            */
            unsigned ARP$V_PUBL : 1;    /* ARP entry is public              */
            unsigned ARP$V_USETRAILERS : 1; /* hosts uses trailers          */
            unsigned ARP$V_FILL_0_ : 3;
            } ARP$R_O_FLAGS;
        } ARP$R_OVLY;
    } ;
 
#pragma __member_alignment __restore
/*** MODULE $IFREQDEF ***/
#pragma __member_alignment __save
#pragma __nomember_alignment
#define IFR$M_IFF_UP 0x1
#define IFR$M_IFF_BROADCAST 0x2
#define IFR$M_IFF_DEBUG 0x4
#define IFR$M_IFF_LOOPBACK 0x8
#define IFR$M_IFF_POINTOPOINT 0x10
#define IFR$M_IFF_NOTRAILERS 0x20
#define IFR$M_IFF_RUNNING 0x40
#define IFR$M_IFF_NOARP 0x80
#define IFR$M_IFF_PROMISC 0x100
#define IFR$M_IFF_ALLMULTI 0x200
#define IFR$M_IFF_MULTICAST 0x400
#define IFR$M_IFF_SIMPLEX 0x800
#define IFR$M_IFF_OACTIVE 0x1000
#define IFR$M_IFF_PFCOPYALL 0x2000
#define IFR$M_IFF_D1 0x4000
#define IFR$M_IFF_D2 0x8000
#define IFR$M_IFF_UIOMOVE 0x10000
#define IFR$M_IFF_PKTOK 0x20000
#define IFR$M_IFF_SOCKBUF 0x40000
#define IFR$M_IFF_VAR_MTU 0x80000
#define IFR$M_IFF_NOCHECKSUM 0x100000
#define IFR$M_IFF_DYNPROTO 0x200000
#define IFR$M_IFF_MOP 0x400000
#define IFR$M_IFF_SLIP 0x800000
#define IFR$M_IFF_DELETE 0x1000000
#define IFR$M_IFF_NONAME 0x2000000
#define IFR$M_IFF_CLUSTER 0x4000000
#define IFR$C_LENGTH 32
#define IFR$K_LENGTH 32
struct IFREQDEF {
    char IFR$T_NAME [16];               /* device name                      */
    union  {
        char IFR$T_ADDR [16];           /* SOCKADDRIN structure             */
        char IFR$T_DSTADDR [16];        /* SOCKADDRIN structure             */
        char IFR$T_BROADADDR [16];      /* SOCKADDRIN structure             */
        union  {
            unsigned short int IFR$W_FLAGS; /* flags                        */
            struct  {
                unsigned IFR$V_IFF_UP : 1; /* Interface is up               */
                unsigned IFR$V_IFF_BROADCAST : 1; /* Broadcast address valid */
                unsigned IFR$V_IFF_DEBUG : 1; /* Turn on tracing            */
                unsigned IFR$V_IFF_LOOPBACK : 1; /* Interface set to loopback */
                unsigned IFR$V_IFF_POINTOPOINT : 1; /* Interface is point-to-point link */
                unsigned IFR$V_IFF_NOTRAILERS : 1; /* Avoid use of trailers */
                unsigned IFR$V_IFF_RUNNING : 1; /* Resources are allocated  */
                unsigned IFR$V_IFF_NOARP : 1; /* No address resolution protocol */
                unsigned IFR$V_IFF_PROMISC : 1; /* Receive all packets      */
                unsigned IFR$V_IFF_ALLMULTI : 1; /* Receive all multicasting packets */
                unsigned IFR$V_IFF_MULTICAST : 1; /* supports multicast     */
                unsigned IFR$V_IFF_SIMPLEX : 1; /* can't hear own transmissions */
                unsigned IFR$V_IFF_OACTIVE : 1; /* transmission in progress  */
                unsigned IFR$V_IFF_PFCOPYALL : 1; /* pfilt gets packets to this host */
                unsigned IFR$V_IFF_D1 : 1; /* IFF_SNAP Ethernet driver outputs  */
/* SNAP hdr                                                                 */
                unsigned IFR$V_IFF_D2 : 1;
                unsigned IFR$V_IFF_UIOMOVE : 1; /* DART                     */
                unsigned IFR$V_IFF_PKTOK : 1; /* DART                       */
                unsigned IFR$V_IFF_SOCKBUF : 1; /* DART                     */
                unsigned IFR$V_IFF_VAR_MTU : 1; /* interface supports variable MTUs */
                unsigned IFR$V_IFF_NOCHECKSUM : 1; /* no checksums need reliable media */
                unsigned IFR$V_IFF_DYNPROTO : 1; /* Support dynamic proto dispatching */
                unsigned IFR$V_IFF_MOP : 1; /* Device in MOP mode; not in use */
                unsigned IFR$V_IFF_SLIP : 1; /* Interface is a SLIP IFNET   */
                unsigned IFR$V_IFF_DELETE : 1; /* Started DELETE on this interface  */
                unsigned IFR$V_IFF_NONAME : 1; /* Interface does not hold the cluster  */
/* name                                                                     */
                unsigned IFR$V_IFF_CLUSTER : 1; /* Interface is a cluster IFNET */
                unsigned IFR$V_FILL_1_ : 5;
                } IFR$R_DUMMY_1_BITS;
            } IFR$R_DUMMY_1_OVRL;
        int *IFR$L_DATA;                /* pointer to data                  */
        } IFR$R_DUMMY;
    } ;
 
#pragma __member_alignment __restore
/*** MODULE $INETERRDEF ***/
#pragma __member_alignment __save
#pragma __nomember_alignment
#define EPERM 1                         /* Not owner                        */
#define ENOENT 2                        /* No such file or directory        */
#define ESRCH 3                         /* No such process                  */
#define EINTR 4                         /* Interrupted system call          */
#define EIO 5                           /* I/O error                        */
#define ENXIO 6                         /* No such device or address        */
#define E2BIG 7                         /* Arg list too long                */
#define ENOEXEC 8                       /* Exec format error                */
#define EBADF 9                         /* Bad file number                  */
#define ECHILD 10                       /* No children                      */
#define EAGAIN 11                       /* No more processes                */
#define ENOMEM 12                       /* Not enough core                  */
#define EACCES 13                       /* Permission denied                */
#define EFAULT 14                       /* Bad address                      */
#define ENOTBLK 15                      /* Block device required            */
#define EBUSY 16                        /* Mount device busy                */
#define EEXIST 17                       /* File exists                      */
#define EXDEV 18                        /* Cross-device link                */
#define ENODEV 19                       /* No such device                   */
#define ENOTDIR 20                      /* Not a directory                  */
#define EISDIR 21                       /* Is a directory                   */
#define EINVAL 22                       /* Invalid argument                 */
#define ENFILE 23                       /* File table overflow              */
#define EMFILE 24                       /* Too many open files              */
#define ENOTTY 25                       /* Not a typewriter                 */
#define ETXTBSY 26                      /* Text file busy                   */
#define EFBIG 27                        /* File too large                   */
#define ENOSPC 28                       /* No space left on device          */
#define ESPIPE 29                       /* Illegal seek                     */
#define EROFS 30                        /* Read-only file system            */
#define EMLINK 31                       /* Too many links                   */
#define EPIPE 32                        /* Broken pipe                      */
/* math software                                                            */
#define EDOM 33                         /* Argument too large               */
#define ERANGE 34                       /* Result too large                 */
/* non-blocking and interrupt i/o                                           */
#define EWOULDBLOCK 35                  /* Operation would block            */
#define EINPROGRESS 36                  /* Operation now in progress        */
#define EALREADY 37                     /* Operation already in progress    */
/* ipc/network software                                                     */
/* argument errors                                                          */
#define ENOTSOCK 38                     /* Socket operation on non-socket   */
#define EDESTADDRREQ 39                 /* Destination address required     */
#define EMSGSIZE 40                     /* Message too long                 */
#define EPROTOTYPE 41                   /* Protocol wrong type for socket   */
#define ENOPROTOOPT 42                  /* Protocol not available           */
#define EPROTONOSUPPORT 43              /* Protocol not supported           */
#define ESOCKTNOSUPPORT 44              /* Socket type not supported        */
#define EOPNOTSUPP 45                   /* Operation not supported on socket  */
#define EPFNOSUPPORT 46                 /* Protocol family not supported    */
#define EAFNOSUPPORT 47                 /* Address family not supported by protocol family  */
#define EADDRINUSE 48                   /* Address already in use           */
#define EADDRNOTAVAIL 49                /* Can't assign requested address   */
/* operational errors                                                       */
#define ENETDOWN 50                     /* Network is down                  */
#define ENETUNREACH 51                  /* Network is unreachable           */
#define ENETRESET 52                    /* Network dropped connection on reset  */
#define ECONNABORTED 53                 /* Software caused connection abort  */
#define ECONNRESET 54                   /* Connection reset by peer         */
#define ENOBUFS 55                      /* No buffer space available        */
#define EISCONN 56                      /* Socket is already connected      */
#define ENOTCONN 57                     /* Socket is not connected          */
#define ESHUTDOWN 58                    /* Can't send after socket shutdown  */
#define ETOOMANYREFS 59                 /* Too many references: can't splice  */
#define ETIMEDOUT 60                    /* Connection timed out             */
#define ECONNREFUSED 61                 /* Connection refused               */
#define ELOOP 62                        /* Too many levels of symbolic links  */
#define ENAMETOOLONG 63                 /* File name too long               */
/* should be rearranged                                                     */
#define EHOSTDOWN 64                    /* Host is down                     */
#define EHOSTUNREACH 65                 /* No route to host                 */
/* quotas & mush                                                            */
#define EPROCLIM 67                     /* Too many processes               */
#define EUSERS 68                       /* Too many users                   */
#define EDQUOT 69                       /* Disc quota exceeded              */
 
#pragma __member_alignment __restore
/*** MODULE $INETSYMDEF ***/
#pragma __member_alignment __save
#pragma __nomember_alignment
#define DVI$_ACP_TCP 10
#define INET$C_IP 0
#define INET$C_ICMP 1
#define INET$C_IGMP 2
#define INET$C_GGP 3
#define INET$C_IPIP 4
#define INET$C_IPV4 4
#define INET$C_TCP 6
#define INET$C_EGP 8
#define INET$C_PUP 12
#define INET$C_UDP 17
#define INET$C_IDP 22
#define INET$C_TP 29
#define INET$C_RSVP 46
#define INET$C_IPV6 41
#define INET$C_ROUTING 43
#define INET$C_FRAG 44
#define INET$C_ESP 50
#define INET$C_AUTH 51
#define INET$C_ICMPV6 58
#define INET$C_NONEXTHDR 59
#define INET$C_DESTNODE 60
#define INET$C_EON 80
#define INET$C_AUXS 127
#define INET$C_REXEC 128
#define INET$C_SCTP 132
#define INET$C_RAW_IP 255
#define INET$C_MAX 256
#define IPPROTO$C_IP 0
#define IPPROTO$C_ICMP 1
#define IPPROTO$C_IGMP 2
#define IPPROTO$C_GGP 3
#define IPPROTO$C_IPIP 4
#define IPPROTO$C_IPV4 4
#define IPPROTO$C_TCP 6
#define IPPROTO$C_EGP 8
#define IPPROTO$C_PUP 12
#define IPPROTO$C_UDP 17
#define IPPROTO$C_IDP 22
#define IPPROTO$C_TP 29
#define IPPROTO$C_RSVP 46
#define IPPROTO$C_IPV6 41
#define IPPROTO$C_ROUTING 43
#define IPPROTO$C_FRAG 44
#define IPPROTO$C_ESP 50
#define IPPROTO$C_AUTH 51
#define IPPROTO$C_ICMPV6 58
#define IPPROTO$C_NONEXTHDR 59
#define IPPROTO$C_DESTNODE 60
#define IPPROTO$C_EON 80
#define IPPROTO$C_AUXS 127
#define IPPROTO$C_REXEC 128
#define IPPROTO$C_SCTP 132
#define IPPROTO$C_RAW_IP 255
#define IPPROTO$C_MAX 256
#define TCPIP$C_IP 0
#define TCPIP$C_ICMP 1
#define TCPIP$C_IGMP 2
#define TCPIP$C_GGP 3
#define TCPIP$C_IPIP 4
#define TCPIP$C_IPV4 4
#define TCPIP$C_TCP 6
#define TCPIP$C_EGP 8
#define TCPIP$C_PUP 12
#define TCPIP$C_UDP 17
#define TCPIP$C_IDP 22
#define TCPIP$C_TP 29
#define TCPIP$C_RSVP 46
#define TCPIP$C_IPV6 41
#define TCPIP$C_ROUTING 43
#define TCPIP$C_FRAG 44
#define TCPIP$C_ESP 50
#define TCPIP$C_AUTH 51
#define TCPIP$C_ICMPV6 58
#define TCPIP$C_NONEXTHDR 59
#define TCPIP$C_DESTNODE 60
#define TCPIP$C_EON 80
#define TCPIP$C_AUXS 127
#define TCPIP$C_REXEC 128
#define TCPIP$C_SCTP 132
#define TCPIP$C_RAW_IP 255
#define TCPIP$C_MAX 256
/*                                                                          */
/* Ports < IP_PROTO$C_RESERVED are reserved for                             */
/* privileged processes (e.g. root).                                        */
/*                                                                          */
#define IP_PROTO$C_RESERVED 1024
#define INET_PROTYP$C_STREAM 1          /* stream type                      */
#define INET_PROTYP$C_DGRAM 2           /* datagram type                    */
#define INET_PROTYP$C_RAW 3             /* raw type                         */
#define INET_PROTYP$C_RDM 4             /* reliably-delivered message - not used on OpenVMS */
#define INET_PROTYP$C_SEQPACKET 5       /* sequenced packet as used by SCTP */
/*                                                                          */
#define TCPIP$C_STREAM 1
#define TCPIP$C_DGRAM 2
#define TCPIP$C_RAW 3
#define TCPIP$C_RDM 4
#define TCPIP$C_SEQPACKET 5
#define INET$C_IPOPT 0                  /* IP opt type parameter            */
#define INET$C_SOCKOPT 1                /* setsockopt type parameter        */
#define INET$C_IOCTL 2                  /* ioctl type parameter             */
#define INET$C_DATA 3                   /* data                             */
#define INET$C_SOCK_NAME 4              /* socket name                      */
#define INET$C_RESERVE_1 5
#define INET$C_TCPOPT 6                 /* TCP option type                  */
/*                                                                          */
#define INET$C_SCTPOPT 132
#define INET$C_IPV6OPT 41
#define TCPIP$C_IPV6OPT 41
#define INET$C_ICMPV6OPT 58
#define TCPIP$C_ICMPV6OPT 58
#define TCPIP$C_IPOPT 0
#define TCPIP$C_SOCKOPT 1
#define TCPIP$C_TCPOPT 6
#define TCPIP$C_IOCTL 2
#define TCPIP$C_DATA 3
#define TCPIP$C_SOCK_NAME 4
#define INET$C_DSC_RCV 0                /* discard received messages        */
#define INET$C_DSC_SND 1                /* discard sent messages            */
#define INET$C_DSC_ALL 2                /* discard all messages             */
#define TCPIP$C_DSC_RCV 0
#define TCPIP$C_DSC_SND 1
#define TCPIP$C_DSC_ALL 2
#define TCPIP$C_SO_SNDBUF 4097          /* 0x1001 send buffer size          */
#define TCPIP$C_SO_RCVBUF 4098          /* 0x1002 receive buffer size       */
#define TCPIP$C_SO_SNDLOWAT 4099        /* 0x1003 send low-water mark       */
#define TCPIP$C_SO_RCVLOWAT 4100        /* 0x1004 receive low-water mark    */
#define TCPIP$C_SO_SNDTIMEO 4101        /* 0x1005 send timeout              */
#define TCPIP$C_SO_RCVTIMEO 4102        /* 0x1006 receive timeout           */
#define TCPIP$C_SO_ERROR 4103           /* 0x1007 get error status and clear */
#define TCPIP$C_SO_TYPE 4104            /* 0x1008 get socket type           */
#define TCPIP$C_SO_SHARE 4105           /* 0x1009 ovms Share between processes */
#define TCPIP$C_SO_CCL 4106             /* 0x100a ovms Carriage Control socket */
#define TCPIP$C_SO_STATE 4107           /* 0x100b get socket state bits     */
#define TCPIP$C_SO_FAMILY 4108          /* 0x100c get socket address family */
#define TCPIP$C_SO_XSE 4109             /* 0x100d _XOPEN_SOURCE_EXTENDED socket */
#define TCPIP$C_SO_NO_RCV_CHKSUM 16384
#define TCPIP$C_SO_NO_SND_CHKSUM 32768
#define TCPIP$C_SO_NO_CHKSUM 49152
/*;constant (                                                               */
/*;	DEBUGGING 	                                                    */
/*;	,ACCEPTCONN                                                         */
/*;	,REUSEADDR                                                          */
/*;	,KEEPALIVE                                                          */
/*;	,DONTROUTE                                                          */
/*;	,BROADCAST                                                          */
/*;	,USELOOPBACK                                                        */
/*;	,LINGER                                                             */
/*;	,OOBINLINE                                                          */
/*;	) equals 1 increment 1 prefix TCPIP$ tag C counter #types;          */
#define INET$C_TCPOPT_EOL 0
#define INET$C_TCPOPT_NOP 1
#define INET$C_TCPOPT_MAXSEG 2
#define INET$C_TCP_NODELAY 1            /* don't delay send to coalesce packets  */
#define INET$C_TCP_MAXSEG 2             /* set maximum segment size         */
#define INET$C_TCP_DUMMY1 3             /* reserve space                    */
#define INET$C_TCP_KEEPIDLE 4           /* seconds before initial keepalive probe */
#define INET$C_TCP_KEEPINTVL 5          /* seconds between keepalive probes  */
#define INET$C_TCP_KEEPCNT 6            /* number of keepalive probes before drop  */
#define INET$C_TCP_KEEPINIT 7           /* initial connect timeout (seconds)  */
#define INET$C_TCP_DUMMY2 8             /* reserve space                    */
#define INET$C_TCP_NODELACK 9           /* don't delay ACKs to await additional data */
#define INET$C_TCP_TSOPTENA 16          /* time stamp option                */
#define INET$C_TCP_PAWS 32              /* PAWS option                      */
#define INET$C_TCP_DUMMY3 48            /* reserve space                    */
#define INET$C_TCP_SACKENA 64           /* SACK enabled                     */
#define INET$C_TCP_PROBE_IDLE 128       /* probe idle timer                 */
#define INET$C_TCP_DROP_IDLE 129        /* drop idle timer                  */
#define TCPIP$C_TCPOPT_EOL 0
#define TCPIP$C_TCPOPT_NOP 1
#define TCPIP$C_TCPOPT_MAXSEG 2
#define TCPIP$C_TCP_NODELAY 1
#define TCPIP$C_TCP_MAXSEG 2
#define TCPIP$C_TCP_KEEPIDLE 4
#define TCPIP$C_TCP_KEEPINTVL 5
#define TCPIP$C_TCP_KEEPCNT 6
#define TCPIP$C_TCP_KEEPINIT 7
#define TCPIP$C_TCP_NODELACK 9
#define TCPIP$C_TCP_TSOPTENA 16
#define TCPIP$C_TCP_PAWS 32
#define TCPIP$C_TCP_SACKENA 64
#define TCPIP$C_TCP_PROBE_IDLE 128
#define TCPIP$C_TCP_DROP_IDLE 129
#define INET$C_IP_OPTIONS 1             /* buf/ip_opts; set/get IP per-packet options  */
#define INET$C_IP_HDRINCL 2             /* int; header is included with data (raw)  */
#define INET$C_IP_TOS 3                 /* int; IP type of service and precedence */
#define INET$C_IP_TTL 4                 /* int; IP time to live             */
#define INET$C_IP_RECVOPTS 5            /* bool; receive all IP options w/datagram  */
#define INET$C_IP_DUMMY1 6              /* reserve space                    */
#define INET$C_IP_RECVDSTADDR 7         /* bool; receive IP dst addr w/datagram  */
#define INET$C_IP_MULTICAST_IF 16       /* set/get IP multicast interface   */
#define INET$C_IP_MULTICAST_TTL 17      /* set/get IP multicast timetolive  */
#define INET$C_IP_MULTICAST_LOOP 18     /* set/get IP multicast loopback    */
/* For binary compatability with UCX 4.n, TCPIP kernel accepts both 12 & 19 */
/* for IP_ADD_MEMBERSHIP.                                                   */
#define INET$C_IP_ADD_MEMBERSHIP 19     /* add  an IP group membership      */
#define INET$C_IP_DROP_MEMBERSHIP 20    /* drop an IP group membership      */
#define INET$C_IP_MULTICAST_VIF 21      /* set/get IP mcast vir. interface  */
#define TCPIP$C_IP_OPTIONS 1            /* buf/ip_opts; set/get IP per-packet options  */
#define TCPIP$C_IP_HDRINCL 2            /* int; header is included with data (raw)  */
#define TCPIP$C_IP_TOS 3                /* int; IP type of service and precedence */
#define TCPIP$C_IP_TTL 4                /* int; IP time to live             */
#define TCPIP$C_IP_RECVOPTS 5           /* bool; receive all IP options w/datagram  */
#define TCPIP$C_IP_DUMMY1 6             /* reserve space                    */
#define TCPIP$C_IP_RECVDSTADDR 7        /* bool; receive IP dst addr w/datagram  */
#define TCPIP$C_IP_MULTICAST_IF 16      /* set/get IP multicast interface   */
#define TCPIP$C_IP_MULTICAST_TTL 17     /* set/get IP multicast timetolive  */
#define TCPIP$C_IP_MULTICAST_LOOP 18    /* set/get IP multicast loopback    */
/* For binary compatability with UCX 4.n, TCPIP kernel accepts both 12 & 19 */
/* for IP_ADD_MEMBERSHIP.                                                   */
#define TCPIP$C_IP_ADD_MEMBERSHIP 19    /* add  an IP group membership      */
#define TCPIP$C_IP_DROP_MEMBERSHIP 20   /* drop an IP group membership      */
#define TCPIP$C_IP_MULTICAST_VIF 21     /* set/get IP mcast vir. interface  */
#define INET$C_AF_UNSPEC 0              /* unspecified                      */
#define INET$C_AF_UNIX 1                /* local to host (pipes, portals)   */
#define INET$C_AF_INET 2                /* internetwork: UDP, TCP, etc.     */
#define INET$C_AF_IMPLINK 3             /* 3 arpanet imp addresses          */
#define INET$C_AF_PUP 4                 /* 4 pup protocols: e.g. BSP        */
#define INET$C_AF_CHAOS 5               /* 5 mit CHAOS protocols            */
#define INET$C_AF_NS 6                  /* 6 XEROX NS protocols             */
#define INET$C_AF_ISO 7                 /* 7 ISO protocols                  */
#define INET$C_AF_ECMA 8                /* 8 european computer manufacturers */
#define INET$C_AF_DATAKIT 9             /* 9 datakit protocols              */
#define INET$C_AF_CCITT 10              /* 10 CCITT protocols, X.25 etc     */
#define INET$C_AF_SNA 11                /* 11 IBM SNA                       */
#define INET$C_AF_DECnet 12             /* 12 DECnet                        */
#define INET$C_AF_DLI 13                /* 13 DEC Direct data link interface */
#define INET$C_AF_LAT 14                /* 14 LAT                           */
#define INET$C_AF_HYLINK 15             /* 15 NSC Hyperchannel              */
#define INET$C_AF_APPLETALK 16          /* 16 Apple Talk                    */
#define INET$C_AF_ROUTE 17              /* 17 Internal Routing Protocol     */
#define INET$C_AF_LINK 18               /* 18 Link layer interface          */
#define INET$C_pseudo_AF_XTP 19         /* 19 eXpress Transfer Protocol (no AF) */
#define INET$C_AF_NETMAN 20             /* 20 DNA Network Management        */
#define INET$C_AF_X25 21                /* 21 X25 protocol                  */
#define INET$C_AF_CTF 22                /* 22 Common Trace Facility         */
#define INET$C_AF_WAN 23                /* 23 Wide Area Network protocols   */
#define INET$C_AF_USER 24               /* 24 Wild card (user defined) protocol */
#define INET$C_AF_LAST 25               /* 25 Local Area System Transport protocol */
#define INET$C_AF_INET6 26              /* 26 IPV6: UDP, TCP, etc.          */
#define INET$C_AF_AAL 27                /* 27 Native AAL ATM                */
#define INET$C_AF_KEY 28                /* 28 Key management                */
#define INET$C_AF_UNUSED1 29
#define INET$C_AF_UNUSED2 30
#define INET$C_AF_UNUSED3 31
#define INET$C_AF_UNUSED4 32
#define INET$C_AF_UNUSED5 33
#define INET$C_AF_UNUSED6 34
#define INET$C_AF_MAX 35                /* 35 maximum value                 */
#define INET$C_INADDR_ANY 0
#define INET$C_INADDR_BROADCAST -1
/*                                                                          */
#define TCPIP$C_AF_UNSPEC 0
#define TCPIP$C_AF_UNIX 1
#define TCPIP$C_AF_INET 2
#define TCPIP$C_AF_INET6 26
#define TCPIP$C_AF_MAX 35
#define TCPIP$C_INADDR_ANY 0
#define TCPIP$C_INADDR_BROADCAST -1
/*                                                                          */
#define INET$M_MSG_OOB 0x1
#define INET$M_MSG_PEEK 0x2
#define INET$M_MSG_DONTROUTE 0x4
#define INET$M_DUMMYN_4 0x8
#define INET$M_DUMMYN_5 0x10
#define INET$M_MSG_PURGE 0x20
#define INET$M_MSG_NBIO 0x40
#define INET$M_MSG_BLOCKALL 0x80
struct MSGBITS {
    struct  {
/*                                                                          */
        unsigned INET$V_MSG_OOB : 1;    /* turn on event logging, not used  */
        unsigned INET$V_MSG_PEEK : 1;   /* socket has had LISTEN            */
        unsigned INET$V_MSG_DONTROUTE : 1; /* use only the interface addr   */
/*                                                                          */
        unsigned INET$V_DUMMYN_4 : 1;   /* reserve space                    */
/*                                                                          */
        unsigned INET$V_DUMMYN_5 : 1;   /* reserve space                    */
/*                                                                          */
        unsigned INET$V_MSG_PURGE : 1;  /* Purge I/O                        */
        unsigned INET$V_MSG_NBIO : 1;   /* NON-block I/O                    */
        unsigned INET$V_MSG_BLOCKALL : 1; /* record TCP I/O                 */
        } INET$R_MSG_BITS;
    } ;
#define INET$C_MSG_OOB 1                /* process out-of-band data         */
#define INET$C_MSG_PEEK 2               /* peek at incoming message         */
#define INET$C_MSG_DONTROUTE 4          /* send without                     */
/* using routing tables                                                     */
#define INET$C_MSG_PURGE 32             /* block read until fill buffer     */
#define INET$C_MSG_NBIO 64              /* block read until fill buffer     */
#define INET$C_MSG_BLOCKALL 128         /* block read until fill buffer     */
#define INET$C_MSG_MAXIOVLEN 16
/*                                                                          */
#define TCPIP$C_MSG_OOB 1               /* process out-of-band data         */
#define TCPIP$C_MSG_PEEK 2              /* peek at incoming message         */
#define TCPIP$C_MSG_DONTROUTE 4         /* send without                     */
/* using routing tables                                                     */
#define TCPIP$C_MSG_PURGE 32            /* block read until fill buffer     */
#define TCPIP$C_MSG_NBIO 64             /* block read until fill buffer     */
#define TCPIP$C_MSG_BLOCKALL 128        /* block read until fill buffer     */
#define TCPIP$C_MSG_MAXIOVLEN 16
#define TCPIP$M_MSG_OOB 0x1
#define TCPIP$M_MSG_PEEK 0x2
#define TCPIP$M_MSG_DONTROUTE 0x4
#define TCPIP$M_DUMMYX_4 0x8
#define TCPIP$M_DUMMYX_5 0x10
#define TCPIP$M_MSG_PURGE 0x20
#define TCPIP$M_MSG_NBIO 0x40
#define TCPIP$M_MSG_BLOCKALL 0x80
struct MSGBITS_1 {
    struct  {
/*                                                                          */
        unsigned TCPIP$V_MSG_OOB : 1;   /* turn on event logging, not used  */
        unsigned TCPIP$V_MSG_PEEK : 1;  /* socket has had LISTEN            */
        unsigned TCPIP$V_MSG_DONTROUTE : 1; /* use only the interface addr  */
/*                                                                          */
        unsigned TCPIP$V_DUMMYX_4 : 1;  /* reserve space                    */
/*                                                                          */
        unsigned TCPIP$V_DUMMYX_5 : 1;  /* reserve space                    */
/*                                                                          */
        unsigned TCPIP$V_MSG_PURGE : 1; /* Purge I/O                        */
        unsigned TCPIP$V_MSG_NBIO : 1;  /* NON-block I/O                    */
        unsigned TCPIP$V_MSG_BLOCKALL : 1; /* record TCP I/O                */
        } TCPIP$R_MSG_BITS;
    } ;
 
#pragma __member_alignment __restore
/*** MODULE $OPTDEF ***/
#pragma __member_alignment __save
#pragma __nomember_alignment
#define OPT$L_ADDRESS 4
#define OPT$C_SET_LENGTH 8
#define OPT$K_SET_LENGTH 8
#define OPT$C_GET_LENGTH 12
#define OPT$K_GET_LENGTH 12
struct OPTDEF {
    unsigned short int OPT$W_LENGTH;    /* length                           */
    unsigned short int OPT$W_NAME;      /* name                             */
    int *OPT$L_ADDR;                    /* address                          */
    int *OPT$L_RET_LENGTH;              /* address                          */
    } ;
 
#pragma __member_alignment __restore
/*** MODULE $ORTENTRYDEF ***/
#pragma __member_alignment __save
#pragma __nomember_alignment
/*                                                                          */
/* We distinguish between routes to hosts and routes to networks,           */
/* preferring the former if available.  For each route we infer             */
/* the interface to use from the gateway address supplied when              */
/* the route was entered.  Routes that forward packets through              */
/* gateways are marked so that the output routines know to address the      */
/* gateway rather than the ultimate destination.                            */
/*                                                                          */
#define ORT$M_RTF_UP 0x1
#define ORT$M_RTF_GATEWAY 0x2
#define ORT$M_RTF_HOST 0x4
#define ORT$M_RTF_DYNAMIC 0x8
#define ORT$M_RTF_MODIFIED 0x10
#define ORT$C_LENGTH 48
#define ORT$K_LENGTH 48
struct ORTENTRYDEF {
    unsigned int ORT$L_HASH;            /* Hash link                        */
    union  {
        struct  {
            unsigned short int ORT$W_DST_SIN_FAMILY; /* Address type        */
            unsigned short int ORT$W_DST_SIN_PORT; /* Port number           */
            unsigned int ORT$L_DST_SIN_ADDR; /* Internet address            */
            char ORT$T_DST_SIN_ZERO [8]; /* Unused space                    */
            } ORT$R_DST_FIELDS;
        char ORT$T_DST [16];            /* Destination SOCKADDR structure   */
        } ORT$R_DST_OVRLY;
    union  {
        struct  {
            unsigned short int ORT$W_GATEWAY_SIN_FAMILY; /* Address type    */
            unsigned short int ORT$W_GATEWAY_SIN_PORT; /* Port number       */
            unsigned int ORT$L_GATEWAY_SIN_ADDR; /* Internet address        */
            char ORT$T_GATEWAY_SIN_ZERO [8]; /* Unused space                */
            } ORT$R_GATEWAY_FIELDS;
        char ORT$T_GATEWAY [16];        /* Gateway SOCKADDR structure       */
        } ORT$R_GATEWAY_OVRLY;
    union  {
        unsigned short int ORT$W_FLAGS; /* up/down?, host/net               */
        struct  {
            unsigned ORT$V_RTF_UP : 1;  /* route useable                    */
            unsigned ORT$V_RTF_GATEWAY : 1; /* destination is a gateway     */
            unsigned ORT$V_RTF_HOST : 1; /* host entry (net otherwise)      */
            unsigned ORT$V_RTF_DYNAMIC : 1; /* created dynamically (by redirect) */
            unsigned ORT$V_RTF_MODIFIED : 1; /* changed by redirect         */
            unsigned ORT$V_FILL_2_ : 3;
            } ORT$R_FLAGS_BITS;
        } ORT$R_FLAGS_OVRLY;
    unsigned short int ORT$W_REFCNT;    /* # held references                */
    unsigned int ORT$L_USE;             /* raw # packets forwarded          */
    unsigned int ORT$L_IFP;             /* pointer to the IFNET interface to use */
    } ;
 
#pragma __member_alignment __restore
/*** MODULE $SIOCDEF ***/
#pragma __member_alignment __save
#pragma __nomember_alignment
/*                                                                          */
/* Only defined here for backward compatibility                             */
/*                                                                          */
#define FIONREAD 1074030207             /* Get # bytes to read              */
#define OFIONREAD -2147195265           /* Get # bytes to read              */
#define FIONBIO -2147195266             /* non block I/O                    */
#define FIOASYNC -2147195267            /* asynch I/O                       */
#define SIOCSHIWAT -2147192064          /* high water mark                  */
#define SIOCGHIWAT 1074033409           /* high water mark                  */
#define SIOCSLOWAT -2147192062          /* low water mark                   */
#define SIOCGLOWAT 1074033411           /* low water mark                   */
#define SIOCATMARK 1074033415           /* at OOB mark                      */
#define SIOCSPGRP -2147192056           /* Process group                    */
#define SIOCGPGRP 1074033417            /* Process group                    */
#define SIOCADDRT -2144308726           /* add RT                           */
#define SIOCDELRT -2144308725           /* delete RT                        */
#define SIOCGETRT -1070566869           /* get RT                           */
#define SIOCSIFADDR -2145359604         /* set IF address                   */
#define SIOCGIFADDR -1071617779         /* Get IF address                   */
#define SIOCSIFDSTADDR -2145359602      /* Destination addr                 */
#define SIOCGIFDSTADDR -1071617777      /* BDestination addr                */
#define SIOCSIFFLAGS -2145359600        /* IF flags                         */
#define SIOCGIFFLAGS -1071617775        /* IF flags                         */
#define SIOCGIFBRDADDR -1071617774      /* Broadcast addr                   */
#define SIOCSIFBRDADDR -2145359597      /* Broadcats addr                   */
#define SIOCGIFCONF -1073190636         /* IF configuration                 */
#define SIOCGIFNETMASK -1071617771      /* Network mask                     */
#define SIOCSIFNETMASK -2145359594      /* Network mask                     */
#define SIOCDIFADDR -2145359591         /* delete addr                      */
#define SIOCAIFADDR -2143262438         /* add/change alias addr            */
#define SIOCSARP -2145097442            /* set ARP                          */
#define SIOCGARP -1071355617            /* get ARP                          */
#define SIOCDARP -2145097440            /* delete ARP                       */
#define SIOCARPREQ -1071355608          /* ARP request                      */
#define SIOCENABLBACK -2145359583       /* enable loopback                  */
#define SIOCDISABLBACK -2145359582      /* disable loopback                 */
#define SIOCSTATE -1072273117           /* state                            */
#define I_STR 536892168
 
#pragma __member_alignment __restore
/*** MODULE $SOCKETOPTDEF ***/
#pragma __member_alignment __save
#pragma __nomember_alignment
/*                                                                          */
/* Socket options data structure.                                           */
/*                                                                          */
#define TCPIP$W_OPTIONS 0
#define INET$W_OPTIONS 0
#define SOCKOPT$M_SO_DEBUG 0x1
#define SOCKOPT$M_ACCEPTCONN 0x2
#define SOCKOPT$M_REUSEADDR 0x4
#define SOCKOPT$M_KEEPALIVE 0x8
#define SOCKOPT$M_DONTROUTE 0x10
#define SOCKOPT$M_BROADCAST 0x20
#define SOCKOPT$M_USELOOPBACK 0x40
#define SOCKOPT$M_LINGER 0x80
#define SOCKOPT$M_OOBINLINE 0x100
#define SOCKOPT$M_REUSEPORT 0x200
#define SOCKOPT$M_DUMMYB_3 0x400
#define SOCKOPT$M_DUMMYB_4 0x800
#define SOCKOPT$M_DUMMYB_5 0x1000
#define SOCKOPT$M_FULL_DUPLEX_CLOSE 0x2000
#define SOCKOPT$M_NO_RCV_CHKSUM 0x4000
#define SOCKOPT$M_NO_SND_CHKSUM 0x8000
#define SOCKOPT$M_NO_CHKSUM 49152       /* no checksum calculation          */
#define SOCKOPT$C_NO_CHKSUM 49152       /* no checksum calculation          */
#define TCPIP$M_SO_DEBUG 0x1
#define TCPIP$M_ACCEPTCONN 0x2
#define TCPIP$M_REUSEADDR 0x4
#define TCPIP$M_KEEPALIVE 0x8
#define TCPIP$M_DONTROUTE 0x10
#define TCPIP$M_BROADCAST 0x20
#define TCPIP$M_USELOOPBACK 0x40
#define TCPIP$M_LINGER 0x80
#define TCPIP$M_OOBINLINE 0x100
#define TCPIP$M_REUSEPORT 0x200
#define TCPIP$M_DUMMYB_3 0x400
#define TCPIP$M_DUMMYB_4 0x800
#define TCPIP$M_DUMMYB_5 0x1000
#define TCPIP$M_FULL_DUPLEX_CLOSE 0x2000
#define TCPIP$M_NO_RCV_CHKSUM 0x4000
#define TCPIP$M_NO_SND_CHKSUM 0x8000
#define TCPIP$M_NO_CHKSUM 49152         /* no checksum calculation          */
#define TCPIP$C_NO_CHKSUM 49152         /* no checksum calculation          */
#define INET$M_SO_DEBUG 0x1
#define INET$M_ACCEPTCONN 0x2
#define INET$M_REUSEADDR 0x4
#define INET$M_KEEPALIVE 0x8
#define INET$M_DONTROUTE 0x10
#define INET$M_BROADCAST 0x20
#define INET$M_USELOOPBACK 0x40
#define INET$M_LINGER 0x80
#define INET$M_OOBINLINE 0x100
#define INET$M_REUSPORT 0x200
#define INET$M_DUMMYB_3 0x400
#define INET$M_DUMMYB_4 0x800
#define INET$M_DUMMYB_5 0x1000
#define INET$M_FULL_DUPLEX_CLOSE 0x2000
#define INET$M_NO_RCV_CHKSUM 0x4000
#define INET$M_NO_SND_CHKSUM 0x8000
#define INET$M_NO_CHKSUM 49152          /* no checksum calculation          */
#define INET$C_NO_CHKSUM 49152          /* no checksum calculation          */
#define SOCKOPT$C_SNDBUF 4097           /* send buffer size                 */
#define SOCKOPT$C_RCVBUF 4098           /* receive buffer size              */
#define SOCKOPT$C_SNDLOWAT 4099         /* send low-water mark              */
#define SOCKOPT$C_RCVLOWAT 4100         /* receive low-water mark           */
#define SOCKOPT$C_SNDTIMEO 4101         /* send timeout                     */
#define SOCKOPT$C_RCVTIMEO 4102         /* receive timeout                  */
#define SOCKOPT$C_ERROR 4103            /* get error status and clear       */
#define SOCKOPT$C_TYPE 4104             /* get socket type                  */
#define SOCKOPT$C_SHARE 4105            /* shared between processes         */
#define SOCKOPT$C_CCL 4106              /* carriage control added           */
#define SOCKOPT$C_STATE 4107            /* get socket state bits            */
#define SOCKOPT$C_FAMILY 4108           /* get socket address family        */
#define SOCKOPT$C_XSE 4109              /* _XOPEN_SOURCE_EXTENDED socket    */
#define SOCKOPT$M_SNDBUF 4097           /* send buffer size                 */
#define SOCKOPT$M_RCVBUF 4098           /* receive buffer size              */
#define SOCKOPT$M_SNDLOWAT 4099         /* send low-water mark              */
#define SOCKOPT$M_RCVLOWAT 4100         /* receive low-water mark           */
#define SOCKOPT$M_SNDTIMEO 4101         /* send timeout                     */
#define SOCKOPT$M_RCVTIMEO 4102         /* receive timeout                  */
#define SOCKOPT$M_ERROR 4103            /* get error status and clear       */
#define SOCKOPT$M_TYPE 4104             /* get socket type                  */
#define SOCKOPT$M_STATE 4105            /* get socket state bits            */
#define SOCKOPT$M_FAMILY 4106           /* get socket address family        */
#define SOCKOPT$M_XSE 4107              /* _XOPEN_SOURCE_EXTENDED socket    */
#define SOCKOPT$M_SHARE 4105            /* shared between processes         */
#define SOCKOPT$M_CCL 4106              /* carriage control added           */
#define TCPIP$C_SNDBUF 4097             /* send buffer size                 */
#define TCPIP$C_RCVBUF 4098             /* receive buffer size              */
#define TCPIP$C_SNDLOWAT 4099           /* send low-water mark              */
#define TCPIP$C_RCVLOWAT 4100           /* receive low-water mark           */
#define TCPIP$C_SNDTIMEO 4101           /* send timeout                     */
#define TCPIP$C_RCVTIMEO 4102           /* receive timeout                  */
#define TCPIP$C_ERROR 4103              /* get error status and clear       */
#define TCPIP$C_TYPE 4104               /* get socket type                  */
#define TCPIP$C_SHARE 4105              /* shared between processes         */
#define TCPIP$C_CCL 4106                /* carriage control added           */
#define TCPIP$C_STATE 4107              /* get socket state bits            */
#define TCPIP$C_FAMILY 4108             /* get socket address family        */
#define TCPIP$C_XSE 4109                /* _XOPEN_SOURCE_EXTENDED socket    */
#define TCPIP$M_SNDBUF 4097             /* send buffer size                 */
#define TCPIP$M_RCVBUF 4098             /* receive buffer size              */
#define TCPIP$M_SNDLOWAT 4099           /* send low-water mark              */
#define TCPIP$M_RCVLOWAT 4100           /* receive low-water mark           */
#define TCPIP$M_SNDTIMEO 4101           /* send timeout                     */
#define TCPIP$M_RCVTIMEO 4102           /* receive timeout                  */
#define TCPIP$M_ERROR 4103              /* get error status and clear       */
#define TCPIP$M_TYPE 4104               /* get socket type                  */
#define TCPIP$M_SHARE 4105              /* shared between processes         */
#define TCPIP$M_CCL 4106                /* carriage control added           */
#define TCPIP$M_STATE 4107              /* get socket state bits            */
#define TCPIP$M_FAMILY 4108             /* get socket address family        */
#define TCPIP$M_XSE 4109                /* _XOPEN_SOURCE_EXTENDED socket    */
#define INET$C_SNDBUF 4097              /* send buffer size                 */
#define INET$C_RCVBUF 4098              /* receive buffer size              */
#define INET$C_SNDLOWAT 4099            /* send low-water mark              */
#define INET$C_RCVLOWAT 4100            /* receive low-water mark           */
#define INET$C_SNDTIMEO 4101            /* send timeout                     */
#define INET$C_RCVTIMEO 4102            /* receive timeout                  */
#define INET$C_ERROR 4103               /* get error status and clear       */
#define INET$C_TYPE 4104                /* get socket type                  */
#define INET$C_SHARE 4105               /* shared between processes         */
#define INET$C_CCL 4106                 /* carriage control added           */
#define INET$C_STATE 4107               /* get socket state bits            */
#define INET$C_FAMILY 4108              /* get socket address family        */
#define INET$C_XSE 4109                 /* _XOPEN_SOURCE_EXTENDED socket    */
#define INET$M_SNDBUF 4097              /* send buffer size                 */
#define INET$M_RCVBUF 4098              /* receive buffer size              */
#define INET$M_SNDLOWAT 4099            /* send low-water mark              */
#define INET$M_RCVLOWAT 4100            /* receive low-water mark           */
#define INET$M_SNDTIMEO 4101            /* send timeout                     */
#define INET$M_RCVTIMEO 4102            /* receive timeout                  */
#define INET$M_ERROR 4103               /* get error status and clear       */
#define INET$M_TYPE 4104                /* get socket type                  */
#define INET$M_SHARE 4105               /* shared between processes         */
#define INET$M_CCL 4106                 /* carriage control added           */
#define INET$M_STATE 4107               /* get socket state bits            */
#define INET$M_FAMILY 4108              /* get socket address family        */
#define INET$M_XSE 4109                 /* _XOPEN_SOURCE_EXTENDED socket    */
#define SOCKOPT$C_SO_DEBUG 1            /* turn on event logging, not used  */
#define SOCKOPT$C_ACCEPTCONN 2          /* socket has had LISTEN            */
#define SOCKOPT$C_REUSEADDR 4           /* allow local address reuse        */
#define SOCKOPT$C_KEEPALIVE 8           /* keep connection alive            */
#define SOCKOPT$C_DONTROUTE 16          /* use only the interface addr      */
#define SOCKOPT$C_BROADCAST 32          /* allow broadcasting               */
#define SOCKOPT$C_USELOOPBACK 64        /* loopback interface, not used     */
#define SOCKOPT$C_LINGER 128            /* linger at close                  */
#define SOCKOPT$C_OOBINLINE 256         /* leave received OOB data in line  */
#define SOCKOPT$C_REUSEPORT 512         /* allow local address and port reuse */
#define SOCKOPT$C_FULL_DUPLEX_CLOSE 8192 /* full duplex close               */
#define SOCKOPT$C_NO_RCV_CHKSUM 16384   /* no receive checksum calculation  */
#define SOCKOPT$C_NO_SND_CHKSUM 32768   /* no send checksum calculation     */
#define TCPIP$C_SO_DEBUG 1              /* turn on event logging, not used  */
#define TCPIP$C_ACCEPTCONN 2            /* socket has had LISTEN            */
#define TCPIP$C_REUSEADDR 4             /* allow local address reuse        */
#define TCPIP$C_KEEPALIVE 8             /* keep connection alive            */
#define TCPIP$C_DONTROUTE 16            /* use only the interface addr      */
#define TCPIP$C_BROADCAST 32            /* allow broadcasting               */
#define TCPIP$C_USELOOPBACK 64          /* loopback interface, not used     */
#define TCPIP$C_LINGER 128              /* linger at close                  */
#define TCPIP$C_OOBINLINE 256           /* leave received OOB data in line  */
#define TCPIP$C_REUSEPORT 512           /* allow local address and port reuse */
#define TCPIP$C_FULL_DUPLEX_CLOSE 8192  /* full duplex close                */
#define TCPIP$C_NO_RCV_CHKSUM 16384     /* no receive checksum calculation  */
#define TCPIP$C_NO_SND_CHKSUM 32768     /* no send checksum calculation     */
#define INET$C_SO_DEBUG 1               /* turn on event logging, not used  */
#define INET$C_ACCEPTCONN 2             /* socket has had LISTEN            */
#define INET$C_REUSEADDR 4              /* allow local address reuse        */
#define INET$C_KEEPALIVE 8              /* keep connection alive            */
#define INET$C_DONTROUTE 16             /* use only the interface addr      */
#define INET$C_BROADCAST 32             /* allow broadcasting               */
#define INET$C_USELOOPBACK 64           /* loopback interface, not used     */
#define INET$C_LINGER 128               /* linger at close                  */
#define INET$C_OOBINLINE 256            /* leave received OOB data in line  */
#define INET$C_REUSEPORT 512            /* allow local address and port reuse */
#define INET$C_FULL_DUPLEX_CLOSE 8192   /* full duplex close                */
#define INET$C_NO_RCV_CHKSUM 16384      /* no receive checksum calculation  */
#define INET$C_NO_SND_CHKSUM 32768      /* no send checksum calculation     */
#define SOCKOPT$C_LENGTH 2
#define SOCKOPT$K_LENGTH 2
/*constant LENGTH equals . prefix TCPIP$ tag C;                             */
#define INET$C_LENGTH 2
#define INET$K_LENGTH 2
struct SOCKETOPTDEF {
    union  {                            /*                                  */
        unsigned short int SOCKOPT$W_OPTIONS; /* Socket options, see socket.h  */
        union  {
            struct  {
/*                                                                          */
/* Socket options bits.                                                     */
/*                                                                          */
                unsigned SOCKOPT$V_SO_DEBUG : 1; /* turn on event logging, not used */
                unsigned SOCKOPT$V_ACCEPTCONN : 1; /* socket has had LISTEN */
                unsigned SOCKOPT$V_REUSEADDR : 1; /* allow local address reuse */
                unsigned SOCKOPT$V_KEEPALIVE : 1; /* keep connection alive  */
                unsigned SOCKOPT$V_DONTROUTE : 1; /* use only the interface addr */
                unsigned SOCKOPT$V_BROADCAST : 1; /* allow broadcasting     */
                unsigned SOCKOPT$V_USELOOPBACK : 1; /* loopback interface, not used */
                unsigned SOCKOPT$V_LINGER : 1; /* linger at close           */
                unsigned SOCKOPT$V_OOBINLINE : 1; /* leave received OOB data in line  */
                unsigned SOCKOPT$V_REUSEPORT : 1; /* allow local addr and port reuse  */
/*                                                                          */
/*                                                                          */
                unsigned SOCKOPT$V_DUMMYB_3 : 1; /* reserve space           */
                unsigned SOCKOPT$V_DUMMYB_4 : 1; /* reserve space           */
                unsigned SOCKOPT$V_DUMMYB_5 : 1; /* reserve space           */
/*                                                                          */
                unsigned SOCKOPT$V_FULL_DUPLEX_CLOSE : 1; /* full duplex close */
                unsigned SOCKOPT$V_NO_RCV_CHKSUM : 1; /* no receive checksum calculation */
                unsigned SOCKOPT$V_NO_SND_CHKSUM : 1; /* no send checksum calculation */
                } SOCKOPT$R_SOCKOPT_OPT_BITS;
            struct  {
/*                                                                          */
/* Socket options bits.                                                     */
/*                                                                          */
                unsigned TCPIP$V_SO_DEBUG : 1; /* turn on event logging, not used */
                unsigned TCPIP$V_ACCEPTCONN : 1; /* socket has had LISTEN   */
                unsigned TCPIP$V_REUSEADDR : 1; /* allow local address reuse */
                unsigned TCPIP$V_KEEPALIVE : 1; /* keep connection alive    */
                unsigned TCPIP$V_DONTROUTE : 1; /* use only the interface addr */
                unsigned TCPIP$V_BROADCAST : 1; /* allow broadcasting       */
                unsigned TCPIP$V_USELOOPBACK : 1; /* loopback interface, not used */
                unsigned TCPIP$V_LINGER : 1; /* linger at close             */
                unsigned TCPIP$V_OOBINLINE : 1; /* leave received OOB data in line  */
                unsigned TCPIP$V_REUSEPORT : 1; /* allow local address and port reuse */
/*                                                                          */
/*                                                                          */
                unsigned TCPIP$V_DUMMYB_3 : 1; /* reserve space             */
                unsigned TCPIP$V_DUMMYB_4 : 1; /* reserve space             */
                unsigned TCPIP$V_DUMMYB_5 : 1; /* reserve space             */
/*                                                                          */
                unsigned TCPIP$V_FULL_DUPLEX_CLOSE : 1; /* full duplex close */
                unsigned TCPIP$V_NO_RCV_CHKSUM : 1; /* no receive checksum calculation */
                unsigned TCPIP$V_NO_SND_CHKSUM : 1; /* no send checksum calculation */
                } SOCKOPT$R_TCPIP_OPT_BITS;
            struct  {
/*                                                                          */
/* Socket options bits.                                                     */
/*                                                                          */
                unsigned INET$V_SO_DEBUG : 1; /* turn on event logging, not used */
                unsigned INET$V_ACCEPTCONN : 1; /* socket has had LISTEN    */
                unsigned INET$V_REUSEADDR : 1; /* allow local address reuse */
                unsigned INET$V_KEEPALIVE : 1; /* keep connection alive     */
                unsigned INET$V_DONTROUTE : 1; /* use only the interface addr */
                unsigned INET$V_BROADCAST : 1; /* allow broadcasting        */
                unsigned INET$V_USELOOPBACK : 1; /* loopback interface, not used */
                unsigned INET$V_LINGER : 1; /* linger at close              */
                unsigned INET$V_OOBINLINE : 1; /* leave received OOB data in line  */
                unsigned INET$V_REUSPORT : 1; /* allow local address and port reuse */
/*                                                                          */
/*                                                                          */
                unsigned INET$V_DUMMYB_3 : 1; /* reserve space              */
                unsigned INET$V_DUMMYB_4 : 1; /* reserve space              */
                unsigned INET$V_DUMMYB_5 : 1; /* reserve space              */
/*                                                                          */
                unsigned INET$V_FULL_DUPLEX_CLOSE : 1; /* full duplex close */
                unsigned INET$V_NO_RCV_CHKSUM : 1; /* no receive checksum calculation */
                unsigned INET$V_NO_SND_CHKSUM : 1; /* no send checksum calculation */
                } SOCKOPT$R_INET_OPT_BITS;
            } SOCKOPT$R_OPTIONS_UNION;
        } SOCKOPT$R_OPT_OVRLY;
/*                                                                          */
/* Additional options, not kept in so_options.                              */
/*                                                                          */
/*constant LENGTH equals . prefix TCPIP$ tag K;                             */
    } ;
 
#pragma __member_alignment __restore
/*** MODULE $SOCKADDRDEF ***/
#pragma __member_alignment __save
#pragma __nomember_alignment
#define AF_UNSPEC 0                     /* unspecified socket family        */
#define AF_INET 2                       /* INET socket family               */
#define SA$C_LENGTH 16
#define SA$K_LENGTH 16
struct SOCKADDR {
    union  {
        unsigned short int SA$W_FAMILY; /* address family                   */
        struct  {
            unsigned char SA$B_LEN;
            unsigned char SA$B_FAMILY;
            } SA$R_SA_STRUCT;
        } SA$R_SA_UNION;
    char SA$T_DATA [14];                /* up to 14 bytes of address        */
    } ;
 
#pragma __member_alignment __restore
/*** MODULE $SOCKADDRINDEF ***/
#pragma __member_alignment __save
#pragma __nomember_alignment
#define SIN$C_LENGTH 16
#define SIN$K_LENGTH 16
typedef struct _SOCKADDRIN {
    unsigned short int SIN$W_FAMILY;    /* address family                   */
    unsigned short int SIN$W_PORT;      /* 2 bytes specifying a port        */
    unsigned int SIN$L_ADDR;            /* 4 bytes specifying an IP address */
    char SIN$T_ZERO [8];                /* 8 bytes                          */
    } SOCKADDRIN;
#define SIN44$C_LENGTH 16
#define SIN44$K_LENGTH 16
typedef struct _SOCKADDRIN44 {
    unsigned char SIN44$B_LEN;
    unsigned char SIN44$B_FAMILY;
    unsigned short int SIN44$W_PORT;    /* 2 bytes specifying a port        */
    unsigned int SIN44$L_ADDR;          /* 4 bytes specifying an IP address */
    char SIN44$T_ZERO [8];              /* 8 bytes                          */
    } SOCKADDRIN44;
#define SIN6$K_LENGTH 28                /* Structure size                   */
#define SIN6$C_LENGTH 28                /* Structure size                   */
typedef struct _SOCKADDRIN6 {
    unsigned char SIN6$B_LEN;           /* length of this struct            */
    unsigned char SIN6$B_FAMILY;        /* AF_INET6                         */
    unsigned short int SIN6$W_PORT;     /* Transport layer port #           */
    unsigned int SIN6$L_FLOWLABEL;      /* IPv6 flow information            */
    union  {
        char SIN6$T_ADDR [16];
        unsigned char SIN6$B_SA6_ADDR [16];
        unsigned short int SIN6$W_SA6_WADDR [8];
        unsigned int SIN6$L_SA6_LADDR [4];
        unsigned int SIN6$Q_SA6_QADDR [2] [2];
        } SIN6$R_ADDR_OVERLAY;
    unsigned int SIN6$L_SCOPE_ID;       /* set of interfaces for a scope    */
    } SOCKADDRIN6;
 
#pragma __member_alignment __restore
/*** MODULE $INETACPSYMDEF ***/
#pragma __member_alignment __save
#pragma __nomember_alignment
/*+                                                                         */
/* Define ACP HOST/NET data base subroutine calls subfunction codes         */
/*                                                                          */
/*-                                                                         */
#define INETACP$C_ALIASES 1             /* aliases                          */
#define INETACP$C_TRANS 2               /* translate ASCII string in binary */
#define INETACP$C_HOSTENT 3             /* get back a HOSTENT               */
#define INETACP$C_NETENT 4              /* get back a NETENT                */
#define INETACP$C_HOSTENT_OFFSET 5      /* get back a HOSTENT               */
#define INETACP$C_NETENT_OFFSET 6       /* get back a NETENT                */
#define INETACPC$C_ALIASES 1            /* aliases                          */
#define INETACPC$C_TRANS 2              /* translate ASCII string in binary */
#define INETACPC$C_HOSTENT 3            /* get back a HOSTENT               */
#define INETACPC$C_NETENT 4             /* get back a NETENT                */
#define INETACPC$C_HOSTENT_OFFSET 5     /* get back a HOSTENT               */
#define INETACPC$C_NETENT_OFFSET 6      /* get back a NETENT                */
 
#pragma __member_alignment __restore
/*** MODULE $INETACPFSYMDEF ***/
#pragma __member_alignment __save
#pragma __nomember_alignment
/*+                                                                         */
/* Define ACP control subfunction codes                                     */
/*                                                                          */
/*-                                                                         */
#define INETACP_FUNC$C_GETHOSTBYNAME 1  /* Subroutine call of GET_HOST_BY_NAME */
#define INETACP_FUNC$C_GETHOSTBYADDR 2  /* Subroutine call of GET_HOST_BY_ADDR */
#define INETACP_FUNC$C_GETNETBYNAME 3   /* Subroutine call of GET_NET_BY_NAME */
#define INETACP_FUNC$C_GETNETBYADDR 4   /* Subroutine call of GET_NET_BY_ADDR  */
/*                                                                          */
 
#pragma __member_alignment __restore
/*** MODULE $NETENTDEF ***/
#pragma __member_alignment __save
#pragma __nomember_alignment
#define NET$C_LENGTH 16                 /*                                  */
#define NET$K_LENGTH 16                 /*                                  */
struct NETENTDEF {
    int *NET$L_N_NAME;                  /* pointer to the network name      */
    int *NET$L_N_ALIASES;               /* pointer to array of pointers to aliases */
    unsigned int NET$L_N_ADDRTYPE;      /* Network address type             */
    unsigned int NET$L_N_NET;           /* Network address                  */
    } ;
/*                                                                          */
/* Structures returned by network                                           */
/* data base library.  All addresses                                        */
/* are supplied in host order, and                                          */
/* returned in network order (suitable                                      */
/* for use in system calls).                                                */
/*                                                                          */
 
#pragma __member_alignment __restore
/*** MODULE $HOSTENTDEF ***/
#pragma __member_alignment __save
#pragma __nomember_alignment
#define HOST$L_H_ADDR 16                /*                                  */
#define HOST$C_LENGTH 20                /*                                  */
#define HOST$K_LENGTH 20                /*                                  */
struct HOSTENTDEF {
    int *HOST$L_H_NAME;                 /* pointer to the host name         */
    int *HOST$L_H_ALIASES;              /* pointer to array of pointers to aliases */
    unsigned int HOST$L_H_ADDRTYPE;     /* Host address type                */
    unsigned int HOST$L_H_LENGTH;       /* Length of address                */
    int *HOST$L_H_ADDR_LIST;            /* Pointer to array of pointers to addresses */
    } ;
 
#pragma __member_alignment __restore
/*** MODULE $$END ***/
#pragma __member_alignment __save
#pragma __nomember_alignment
#endif /* TCPIP$INETDEF_LOADED */
 
#pragma __member_alignment __restore
