#pragma module HASH "V1.00"

#include <stdlib.h>
#include <string.h>
#include "list.h"
#include "hash.h"


int adc_HT_Init(adc_HT_t * ht, int buckets,
		unsigned int (*hash) (const void *), int
		 (*match) (const void *, const void *),
		void (*destroy) (void *))
{
    int i;

    if ((ht->table =
	 (adc_SLL_t *) malloc(buckets * sizeof(adc_SLL_t))) == NULL) {
	return (-1);
    }

    ht->buckets = buckets;

    for (i = 0; i < ht->buckets; i++) {
	adc_SLL_Init(&ht->table[i], destroy);
    }

    ht->hash = hash;
    ht->match = match;
    ht->destroy = destroy;
    ht->size = 0;

    return (0);
}


adc_HT_t *adc_HT_New(int buckets, unsigned int (*hash) (const void *),
		     int (*match) (const void *, const void *),
		     void (*destroy) (void *))
{
    adc_HT_t *ht = NULL;

    ht = (adc_HT_t *) malloc(sizeof(adc_HT_t));

    if (ht != NULL) {
	adc_HT_Init(ht, buckets, hash, match, destroy);
    }

    return (ht);
}


void adc_HT_Destroy(adc_HT_t * ht)
{
    int i;

    for (i = 0; i < ht->buckets; i++) {
	adc_SLL_Destroy(&ht->table[i]);
    }

    free(ht->table);
    memset(ht, 0, sizeof(adc_HT_t));
}


int adc_HT_Insert(adc_HT_t * ht, const void *data)
{
    int bucket;
    int retval;
    void *tmp;

    tmp = (void *) data;

    if (adc_HT_Lookup(ht, &tmp) == 0) {
	return (1);
    }

    bucket = ht->hash(data) % ht->buckets;

    if ((retval = adc_SLL_InsertNext(&ht->table[bucket], NULL, data)) == 0) {
	ht->size++;
    }

    return (retval);
}


int adc_HT_Remove(adc_HT_t * ht, void **data)
{
    int bucket;
    adc_SLL_node_t *elem;
    adc_SLL_node_t *prev;

    bucket = ht->hash(*data) % ht->buckets;
    prev = NULL;

    for (elem = adc_SLL_Head(&ht->table[bucket]); elem != NULL;
	 elem = adc_SLL_Next(elem)) {
	if (ht->match(*data, adc_SLL_Data(elem)) == 0) {
	    if (adc_SLL_RemoveNext(&ht->table[bucket], prev, data) == 0) {
		ht->size--;
		return (0);
	    } else {
		return (-1);
	    }
	}
	prev = elem;
    }

    return (-1);
}


int adc_HT_Lookup(const adc_HT_t * ht, void **data)
{
    int bucket;
    adc_SLL_node_t *elem;

    bucket = ht->hash(*data) % ht->buckets;

    for (elem = adc_SLL_Head(&ht->table[bucket]); elem != NULL;
	 elem = adc_SLL_Next(elem)) {
	if (ht->match(*data, adc_SLL_Data(elem)) == 0) {
	    *data = adc_SLL_Data(elem);
	    return (0);
	}
    }

    return (-1);
}


int adc_HT_Exists(const adc_HT_t * ht, void *data)
{
    int bucket;
    adc_SLL_node_t *elem;

    bucket = ht->hash(data) % ht->buckets;

    for (elem = adc_SLL_Head(&ht->table[bucket]); elem != NULL;
	 elem = adc_SLL_Next(elem)) {
	if (ht->match(data, adc_SLL_Data(elem)) == 0) {
	    return (1);
	}
    }

    return (0);
}


void adc_HT_Traverse(const adc_HT_t * ht,
		     void (*func) (const void *item, void *ud), void *ud)
{
    adc_SLL_node_t *elem;
    int i;

    for (i = 0; i < ht->buckets; i++) {
	for (elem = adc_SLL_Head(&ht->table[i]); elem != NULL;
	     elem = adc_SLL_Next(elem)) {
	    (*func) (adc_SLL_Data(elem), ud);
	}
    }
}
