// Example

#include <stdio.h>
#include <stdlib.h>
#include "./atomic_counter_file.h"

static char const *FILENAME = "id.db";

int main(void)
{
    struct ACF *ctx;
    if (ACF_open(&ctx, FILENAME) != 0)
    {
        fprintf(stderr, "ERROR: open\n");
        exit(1);
    }

    for (int i = 0; i < 10; i++)
    {
        long long id;
        if (ACF_increment(ctx, &id) != 0)
        {
            fprintf(stderr, "ERROR: increment\n");
            exit(1);
        }

        printf("ID = %d\n", (int)id);
    }

    ACF_close(ctx);
    return 0;
}
