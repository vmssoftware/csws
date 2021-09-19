#ifndef __ADCLIST_H__
#define __ADCLIST_H__

typedef struct SLL_node_ {
    void *data;
    struct SLL_node_ *next;
} adc_SLL_node_t;


typedef struct {
    int size;
    int (*match) (const void *, const void *);
    void (*destroy) (void *);
    adc_SLL_node_t *head;
    adc_SLL_node_t *tail;
} adc_SLL_t;


#ifdef __cplusplus
extern "C" {
#endif

    extern adc_SLL_t *adc_SLL_New(void (*)(void *));
    extern void adc_SLL_ForEach(adc_SLL_t *,
				void (*func) (const void *, const void *),
				const void *);
    extern void adc_SLL_ForEachNode(adc_SLL_t *,
				    void (*)(const adc_SLL_node_t *,
					     const void *), const void *);
    extern int adc_SLL_Append(adc_SLL_t *, const void *);
    extern int adc_SLL_Add(adc_SLL_t *, const void *);
    extern void adc_SLL_Init(adc_SLL_t *, void (*)(void *));
    extern void adc_SLL_Destroy(adc_SLL_t *);
    extern int adc_SLL_InsertNext(adc_SLL_t *, adc_SLL_node_t *,
				  const void *);
    extern int adc_SLL_RemoveNext(adc_SLL_t *, adc_SLL_node_t *, void **);
    extern void *adc_SLL_Nth(adc_SLL_t *, int);
    extern adc_SLL_node_t *adc_SLL_NthNode(adc_SLL_t *, int);

#ifdef __cplusplus
}
#endif
#define adc_SLL_Size(list) 		((list)->size)
#define adc_SLL_Head(list) 		((list)->head)
#define adc_SLL_Tail(list) 		((list)->tail)
#define adc_SLL_IsHead(list, elem) 	((elem) == (list)->head ? 1 : 0)
#define adc_SLL_IsTail(elem) 		((elem)->next == NULL ? 1 : 0)
#define adc_SLL_Data(elem) 		((elem)->data)
#define adc_SLL_Next(elem) 		((elem)->next)
#endif
