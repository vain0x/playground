#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "subscription.h"

Subscription subscription_new() {
	return (Subscription){
	    .head = NULL,
	};
}

void subscription_push0(Subscription *self, const char *hint,
                        void (*fn)(void)) {
	assert(self != NULL);

	SubscriptionEntry *entry =
	    (SubscriptionEntry *)malloc(sizeof(SubscriptionEntry));
	entry->hint = hint;
	entry->arity = 0;
	entry->fn0 = fn;
	entry->next = self->head;

	self->head = entry;
}

void subscription_push(Subscription *self, const char *hint, void *ptr,
                       void (*fn)(void *)) {
	assert(self != NULL);

	SubscriptionEntry *entry =
	    (SubscriptionEntry *)malloc(sizeof(SubscriptionEntry));
	entry->hint = hint;
	entry->arity = 1;
	entry->fn1 = fn;
	entry->ptr = ptr;
	entry->next = self->head;

	self->head = entry;
}

void subscription_dispose(Subscription *self) {
	assert(self != NULL);

	SubscriptionEntry *sub = self->head;
	while (sub != 0) {
		fprintf(stderr, "[TRACE] %s\n", sub->hint);

		switch (sub->arity) {
		case 0:
			(sub->fn0)();
			break;

		case 1:
			(sub->fn1)(sub->ptr);
			break;

		default:
			assert(false && "invalid arity");
		}

		SubscriptionEntry *next = sub->next;

		free(sub);
		sub = next;
	}
	self->head = 0;
}
