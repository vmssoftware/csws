#ifndef __ADCHASH_H__
#define __ADCHASH_H__

#include "list.h"

typedef struct {
    int buckets;
    unsigned int (*hash) (const void *);
    int (*match) (const void *, const void *);
    void (*destroy) (void *);
    int size;
    adc_SLL_t *table;
} adc_HT_t;

#ifdef __cplusplus
extern "C" {
#endif

    extern int adc_HT_Init(adc_HT_t *, int, unsigned int (*)(const void *),
			   int (*)(const void *, const void *),
			   void (*)(void *)
	);
    extern adc_HT_t *adc_HT_New(int, unsigned int (*)(const void *),
				int (*)(const void *, const void *),
				void (*)(void *));

    extern void adc_HT_Destroy(adc_HT_t *);
    extern int adc_HT_Insert(adc_HT_t *, const void *);
    extern int adc_HT_Remove(adc_HT_t *, void **);
    extern int adc_HT_Lookup(const adc_HT_t *, void **);
    extern int adc_HT_Exists(const adc_HT_t *, void *);
    extern void adc_HT_Traverse(const adc_HT_t *,
				void (*)(const void *, void *), void *);

#define adc_HT_Size(htbl) ((htbl)->size)

#ifdef __cplusplus
}
#endif
#endif
