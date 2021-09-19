#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stat.h>
#include <errno.h>
#include <assert.h>
#include "hash.h"

#ifndef adc_Assert
#define adc_Assert(x) \
    do { \
        if ((!(x))) { \
            fprintf (stderr, "Assertion failed: %s (%s: %d)\n", #x, __FILE__, __LINE__); \
            abort(); \
        } \
    } while (0)
#endif


typedef struct {
    char *key;
    struct stat sb;
    int count;
} sb_cache_t;


#define MULTIPLIER 31

static unsigned int _hash(const void *ent)
{
    unsigned int h = 0;
    unsigned char *p;
    char *tmp = ((sb_cache_t *) ent)->key;

    for (p = (unsigned char *) tmp; *p != '\0'; p++) {
	h = MULTIPLIER * h + *p;
    }

    return (h);
}

static int _match(const void *v1, const void *v2)
{
    return (strcmp(((sb_cache_t *) v1)->key, ((sb_cache_t *) v2)->key));
}

static void _destroy(void *ent)
{
    sb_cache_t *ep = (sb_cache_t *) ent;

    if (ep) {
	if (ep->key) {
	   free(ep->key);
	}
	free(ep);
    }
}


static adc_HT_t *sb$z_ht = NULL;
static int sb$l_init = 0;
static int sb$l_reset = 0;


#ifndef HT_SIZE
#define HT_SIZE 521
#endif


static void sb_cache_init()
{
    char *tmp = getenv("apache$stat_cache");

    if (tmp) {
	adc_Assert((sb$z_ht = adc_HT_New(HT_SIZE, _hash, _match, _destroy)));
	sb$l_reset = 0;

	if ((tmp = getenv("apache$stat_reset_after"))) {
	    if ((sb$l_reset = atoi(tmp)) < 0) {
	        sb$l_reset = 0;
	    }
	}
    }

    sb$l_init = 1;
}


int BRC_stat(const char *path, struct stat *sb)
{
    sb_cache_t ent, *tmp;
    int rv;

    if (!sb$l_init) {
	sb_cache_init();
    }

    if (! sb$z_ht) {
	return (stat(path, sb));
    }

    ent.key = (char *) path;
    tmp = &ent;

    if (adc_HT_Lookup(sb$z_ht, (void **) &tmp) == 0) {
	tmp->count++;
	memcpy(sb, &tmp->sb, sizeof(struct stat));

	if (tmp->count == sb$l_reset) {
	    adc_HT_Remove(sb$z_ht, (void **) &tmp);
	}

	return (0);
    } else {
	rv = stat(path, sb);

	if (rv != 0) {
	    return (rv);
	}

	adc_Assert((tmp = malloc(sizeof(sb_cache_t))));
	adc_Assert((tmp->key = strdup(path)));
	memcpy(&tmp->sb, sb, sizeof(struct stat));
	tmp->count = 0;
	adc_Assert((adc_HT_Insert(sb$z_ht, tmp) == 0));

	return (0);
    }

    return (-1);
}


int BRC_lstat(const char *path, struct stat *sb)
{
    sb_cache_t ent, *tmp;
    int rv;

    if (!sb$l_init) {
	sb_cache_init();
    }

    if (! sb$z_ht) {
	return (lstat(path, sb));
    }

    ent.key = (char *) path;
    tmp = &ent;

    if (adc_HT_Lookup(sb$z_ht, (void **) &tmp) == 0) {
	tmp->count++;
	memcpy(sb, &tmp->sb, sizeof(struct stat));

	if (tmp->count == sb$l_reset) {
	    adc_HT_Remove(sb$z_ht, (void **) &tmp);
	}

	return (0);
    } else {
	rv = lstat(path, sb);

	if (rv != 0) {
	    return (rv);
	}

	adc_Assert((tmp = malloc(sizeof(sb_cache_t))));
	adc_Assert((tmp->key = strdup(path)));
	memcpy(&tmp->sb, sb, sizeof(struct stat));
	tmp->count = 0;
	adc_Assert((adc_HT_Insert(sb$z_ht, tmp) == 0));

	return (0);
    }

    return (-1);
}

#if 0
#include <time.h>

void main()
{
    time_t t0, t1;
    int rv;
    int i, j;
    struct stat sb;

    char *list[] = {
	"hash.c",
	"list.c",
	"sb_cache.c",
	"hash.h",
	"list.h",
	NULL
    };

    t0 = time(NULL);

    for (i = 0; i < 100000; i++) {
	j = 0;
	while (list[j]) {
	    if ((rv = BRC_stat(list[j], &sb)) != 0) {
	        fprintf(stderr, "%s: %s\n", list[j], strerror(errno));
	        exit(0);
	    }
	    j++;
	}
    }

    t1 = time(NULL);
    fprintf(stderr, "Elapsed time = %ds\n", t1 - t0);
}
#endif

