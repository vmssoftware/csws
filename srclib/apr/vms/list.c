#pragma module LIST "V1.00"

# include <stdlib.h>
# include <string.h>
# include "list.h"

void *adc_SLL_Nth(adc_SLL_t * list, int n)
{
    adc_SLL_node_t *np;
    int i;

    if (n > adc_SLL_Size(list) - 1 || n < 0) {
	return (NULL);
    }

    np = adc_SLL_Head(list);
    for (i = 0; i < n; i++)
	np = adc_SLL_Next(np);

    return (adc_SLL_Data(np));
}


adc_SLL_node_t *adc_SLL_NthNode(adc_SLL_t * list, int n)
{
    adc_SLL_node_t *np;
    int i;

    if (n > adc_SLL_Size(list) - 1 || n < 0) {
	return (NULL);
    }

    np = adc_SLL_Head(list);
    for (i = 0; i < n; i++)
	np = adc_SLL_Next(np);

    return (np);
}


adc_SLL_t *adc_SLL_New(void (*destroy) (void *data))
{
    adc_SLL_t *list = NULL;

    list = (adc_SLL_t *) malloc(sizeof(adc_SLL_t));

    if (list != NULL) {
	adc_SLL_Init(list, destroy);
    }

    return (list);
}


void adc_SLL_ForEach(adc_SLL_t * list,
		     void (*func) (const void *, const void *),
		     const void *ud)
{
    adc_SLL_node_t *elem;

    for (elem = adc_SLL_Head(list); elem != NULL;
	 elem = adc_SLL_Next(elem)) {
	(*func) (adc_SLL_Data(elem), ud);
    }
}


void adc_SLL_ForEachNode(adc_SLL_t * list,
			 void (*func) (const adc_SLL_node_t *,
				       const void *), const void *ud)
{
    adc_SLL_node_t *elem;

    for (elem = adc_SLL_Head(list); elem != NULL;
	 elem = adc_SLL_Next(elem)) {
	(*func) (elem, ud);
    }
}


int adc_SLL_Append(adc_SLL_t * list, const void *data)
{
    return (adc_SLL_InsertNext(list, adc_SLL_Tail(list), data));
}


int adc_SLL_Add(adc_SLL_t * list, const void *data)
{
    return (adc_SLL_InsertNext(list, NULL, data));
}


void adc_SLL_Init(adc_SLL_t * list, void (*destroy) (void *data))
{
    list->match = NULL;
    list->size = 0;
    list->destroy = destroy;
    list->head = NULL;
    list->tail = NULL;
}


void adc_SLL_Destroy(adc_SLL_t * list)
{
    void *data;

    while (adc_SLL_Size(list) > 0) {
	if (adc_SLL_RemoveNext(list, NULL, (void **) &data) == 0
	    && list->destroy != NULL) {
	    list->destroy(data);
	}
    }

    memset(list, 0, sizeof(adc_SLL_t));
}


int adc_SLL_InsertNext(adc_SLL_t * list, adc_SLL_node_t * elem,
		       const void *data)
{
    adc_SLL_node_t *new_elem;

    if ((new_elem =
	 (adc_SLL_node_t *) malloc(sizeof(adc_SLL_node_t))) == NULL) {
	return (-1);
    }

    new_elem->data = (void *) data;

    if (elem == NULL) {
	if (adc_SLL_Size(list) == 0) {
	    list->tail = new_elem;
	}
	new_elem->next = list->head;
	list->head = new_elem;
    } else {
	if (elem->next == NULL) {
	    list->tail = new_elem;
	}
	new_elem->next = elem->next;
	elem->next = new_elem;
    }

    list->size++;
    return (0);
}


int adc_SLL_RemoveNext(adc_SLL_t * list, adc_SLL_node_t * elem,
		       void **data)
{
    adc_SLL_node_t *old_elem;

    if (adc_SLL_Size(list) == 0) {
	return (-1);
    }

    if (elem == NULL) {
	*data = list->head->data;
	old_elem = list->head;
	list->head = list->head->next;

	if (adc_SLL_Size(list) == 1) {
	    list->tail = NULL;
	}
    } else {
	if (elem->next == NULL) {
	    return (-1);
	}

	*data = elem->next->data;
	old_elem = elem->next;
	elem->next = elem->next->next;

	if (elem->next == NULL) {
	    list->tail = elem;
	}
    }

    free(old_elem);
    list->size--;

    return (0);
}
