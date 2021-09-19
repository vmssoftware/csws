#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <stdarg.h>
#include <assert.h>

#include <descrip.h>
#include <dvidef.h>
#include <efndef.h>
#include <lib$routines.h>
#include <starlet.h>
#include <stsdef.h>
#include <lnmdef.h>
#include <iodef.h>

#include <rmq.h>

#ifndef OKAY
#define OKAY(STATUS) (((STATUS) & 1) != 0)
#endif

#define BUFSIZE 1024

#define DEF_URI "amqp://guest:guest@127.0.0.1:5672"
#define DEF_EXCHANGE "httpd.log"
#define DEF_ROUTING_KEY "httpd"

typedef struct {
    char *exchange;
    char *routing_key;
    char *uri;
    int mode;
} args_t;

typedef struct {
    unsigned short length;
    unsigned short code;
    void *addr;
    void *rla;
} list_t;

typedef struct {
    unsigned short status;
    unsigned short bytes;
    unsigned long details;
} iosb_t;



static void usage(const char *path, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);

    fprintf(stderr,
	    "Usage: %s [-p] [-e exchange] [-r routing-key] [-u uri]\n",
	    path);
    fprintf(stderr,
	    "Options:\n"
	    "\t-p                    Publish messages with persistent delivery mode\n"
	    "\t-e exchange           Exchange to which messages will be published (%s)\n"
	    "\t-r routing-key        Routing key to use when publishing messages (%s)\n"
	    "\t-u uri                Broker URI (%s)\n", DEF_EXCHANGE,
	    DEF_ROUTING_KEY, DEF_URI);

    exit(EXIT_FAILURE);
}


int main(int argc, char **argv)
{
    RMQ_conn_t *ch;
    amqp_basic_properties_t props;
    char buf[BUFSIZE];
    char c;
    unsigned long status;
    char name[64];
    int len;
    iosb_t iosb;
    struct dsc$descriptor dsc;
    unsigned short chan;
    $DESCRIPTOR(input, "SYS$INPUT");

    args_t args = { DEF_EXCHANGE, DEF_ROUTING_KEY, DEF_URI, 1 };
    list_t list[2];

    while ((c = getopt(argc, argv, "e:r:u:p")) != EOF) {
	switch (c) {
	case 'e':
	    assert((args.exchange = strdup(optarg)));
	    break;

	case 'r':
	    assert((args.routing_key = strdup(optarg)));
	    break;

	case 'u':
	    assert((args.uri = strdup(optarg)));
	    break;

	case 'p':
	    args.mode = 2;
	    break;

	default:
	    usage(argv[0], "Invalid command line option (-%c)\n", optopt);
	    break;
	}
    }

    list[0].length = sizeof(name);
    list[0].code = DVI$_DEVNAM;
    list[0].addr = name;
    list[0].rla = &len;
    list[1].length = 0;
    list[1].code = 0;
    list[1].addr = NULL;
    list[1].rla = NULL;

    /* Get mailbox device name for sys$input */
    if (!OKAY
	((status =
	  sys$getdviw(EFN$C_ENF, 0, &input, list, &iosb, 0, 0, 0)))) {
	lib$signal(status);
    }

    dsc.dsc$w_length = len;
    dsc.dsc$b_dtype = DSC$K_DTYPE_T;
    dsc.dsc$b_class = DSC$K_CLASS_S;
    dsc.dsc$a_pointer = name;

    if (!OKAY((status = sys$assign(&dsc, &chan, 0, 0, 0)))) {
	lib$signal(status);
    }

    /* Connect to RabbitMQ */
    if ((ch = Rmq_Connect(args.uri)) == NULL) {
	exit(EXIT_FAILURE);
    }

    memset(&props, '\0', sizeof(props));
    props._flags =
	AMQP_BASIC_CONTENT_TYPE_FLAG | AMQP_BASIC_DELIVERY_MODE_FLAG;
    props.content_type = amqp_cstring_bytes("text/plain");
    props.delivery_mode = args.mode;

    for (;;) {
	memset(buf, '\0', sizeof(buf));

	if (!OKAY
	    ((status =
	      sys$qiow(EFN$C_ENF, chan, IO$_READVBLK, &iosb, 0, 0, buf,
		       sizeof(buf), 0, 0, 0, 0)))) {
	    lib$signal(status);
	}

	if (iosb.bytes == 0) {	/* Didn't actually get anything */
	    sleep(3);
	    continue;
	}

	if (Rmq_Publish
	    (ch, args.exchange, args.routing_key, 0, 0, &props, buf,
	     iosb.bytes) == -1) {
	    fprintf(stderr, "Error publishing to RabbitMQ: %s",
		    Rmq_Strerror(ch));
	    exit(EXIT_FAILURE);
	}
    }

    /* Never get here */

    Rmq_Disconnect(ch);
    return (0);
}
