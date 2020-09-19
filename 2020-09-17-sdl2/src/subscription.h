#pragma once

typedef struct SubscriptionEntry {
	const char *hint;

	// 呼ばれるクロージャ
	int arity;
	union {
		void (*fn0)(void);

		struct {
			void *ptr;
			void (*fn1)(void *);
		};
	};

	struct SubscriptionEntry *next;
} SubscriptionEntry;

typedef struct Subscription {
	SubscriptionEntry *head;
} Subscription;

Subscription subscription_new();
void subscription_push0(Subscription *self, const char *hint, void (*fn)(void));
void subscription_push(Subscription *self, const char *hint, void *ptr,
                       void (*fn)(void *));
void subscription_dispose(Subscription *self);
