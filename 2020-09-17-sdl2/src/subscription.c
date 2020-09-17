#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "subscription.h"

Subscription subscription_new() {
	return (Subscription){
	    .head = NULL,
	};
}

void subscription_push(Subscription *self, const char *hint, void *ptr,
                       void (*fun)(void *)) {
	assert(self != NULL);

	SubscriptionEntry *next =
	    (SubscriptionEntry *)malloc(sizeof(SubscriptionEntry));
	*next = (SubscriptionEntry){
	    .hint = hint,
	    .ptr = ptr,
	    .fun = fun,
	    .next = self->head,
	};
	self->head = next;
}

void subscription_dispose(Subscription *self) {
	assert(self != NULL);

	SubscriptionEntry *sub = self->head;
	while (sub != 0) {
		fprintf(stderr, "[TRACE] %s\n", sub->hint);

		(*sub->fun)(sub->ptr);
		SubscriptionEntry *next = sub->next;

		free(sub);
		sub = next;
	}
	self->head = 0;
}
