#ifndef ATOMIC_COUNTER_FILE_H_INCLUDED
#define ATOMIC_COUNTER_FILE_H_INCLUDED

struct ACF;

int ACF_open(struct ACF **ctx, char const *pathname);
void ACF_close(struct ACF *ctx);
int ACF_increment(struct ACF *ctx, long long *result);

void ACF_truncate(struct ACF *ctx);
void ACF_reserve(struct ACF *ctx, long long count);

#endif
