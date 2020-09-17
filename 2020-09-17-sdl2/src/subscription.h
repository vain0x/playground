#pragma once

typedef struct SubscriptionEntry {
	const char *hint;
	void *ptr;
	void (*fun)(void *);
	struct SubscriptionEntry *next;
} SubscriptionEntry;

typedef struct Subscription {
	SubscriptionEntry *head;
} Subscription;

Subscription subscription_new();
void subscription_push(Subscription *self, const char *hint, void *ptr, void (*fun)(void *));
void subscription_dispose(Subscription *self);
