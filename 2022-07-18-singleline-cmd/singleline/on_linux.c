#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Linux only.
#include <unistd.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <sys/time.h>

static void console_print(char const *src, size_t len)
{
	fprintf(stdout, "%.*s\n", (int)len, src);
	fflush(stdout);
}

int main(void)
{
	char buf[64] = "";
	while (true)
	{
		// read
		bool ok = fgets(buf, sizeof(buf), stdin) != NULL;
		if (!ok)
		{
			break;
		}

		// write
		size_t len = strlen(buf);
		bool newline = false;

		if (len >= 1 && buf[len - 1] == '\n')
		{
			len--;
			newline = true;
		}

		console_print(buf, len);

		// skip rest of line
		if (!newline)
		{
			while (true)
			{
				ok = fgets(buf, sizeof(buf), stdin) != NULL;
				if (!ok)
				{
					goto END;
				}

				size_t len = strlen(buf);
				if (len >= 1 && buf[len - 1] == '\n')
				{
					break;
				}
			}
		}
	}
END:
	return 0;
}
