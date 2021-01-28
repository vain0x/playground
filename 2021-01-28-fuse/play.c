#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

int main(void) {
    FILE *out =
        popen("ls -l --color=never --directory --no-group README.md", "r");
    if (out == NULL) {
        fprintf(stderr, "[ERROR] getattr ls failed\n");
        return 1;
    }

    {
        char buf[0x1000] = {};
        while (true) {
            size_t read = fread(buf, 1, sizeof buf, out);
            if (read == 0) {
                break;
            }

            size_t written = fwrite(buf, read, 1, stdout);
			assert(written == 1);
        }

		int stat = fflush(stdout);
		assert(stat == 0);
    }

    int stat = pclose(out);
	assert(stat == 0);
    return 0;
}
