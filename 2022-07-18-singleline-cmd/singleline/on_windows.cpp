#ifdef _MSVC_VER // Windows

#include <cassert>
#include <string>

#define WIN32_LEAN_AND_MEAN 1
#include <Windows.h>

static void to_os_string(char *src, size_t src_len, wchar_t *dest, size_t *dest_len)
{
	if (src_len == 0)
	{
		*dest = wchar_t{};
		*dest_len = 0;
		return;
	}

	int len = MultiByteToWideChar(CP_UTF8, 0, src, (int)src_len, NULL, 0);
	assert(src_len > 0);

	assert(src_len <= *dest_len && "overflow");

	int n = MultiByteToWideChar(CP_UTF8, 0, src, (int)src_len, dest, (int)src_len);
	if (n == 0)
	{
		assert(false && "MultiByteToWideChar");
	}
	assert(n >= 0);
	assert(n <= src_len);
	*dest_len = (size_t)n;
}

static void run()
{
	void *console_ = GetStdHandle(STD_OUTPUT_HANDLE);

	char buf[4000] = "";
	wchar_t out_buf[4000] = L"";
	size_t out_len = 0;

	while (true)
	{
		bool ok = fgets(buf, sizeof(buf), stdin) != NULL;
		if (!ok)
		{
			break;
		}

		size_t len = strlen(buf);
		if (len >= 1 && buf[len - 1] == '\n')
		{
			len--;

			if (len >= 1 && buf[len - 1] == '\r')
			{
				len--;
			}
		}

		out_len = sizeof(out_buf) - 1;
		to_os_string(buf, len, out_buf, &out_len);

		CONSOLE_SCREEN_BUFFER_INFO csbi;
		GetConsoleScreenBufferInfo(console_, &csbi);

		DWORD w;
		COORD coord = {csbi.dwCursorPosition.X, csbi.dwCursorPosition.Y};
		for (size_t i = 0; i < out_len; i++)
		{
			WriteConsoleOutputCharacterW(console_, &out_buf[i], 1, coord, &w);
			coord.X += out_buf[i] < 0x7f ? 1 : 2;
		}

		while (coord.X < csbi.dwSize.X)
		{
			WriteConsoleOutputCharacterW(console_, L" ", 1, coord, &w);
			WriteConsoleOutputAttribute(console_, &csbi.wAttributes, 1, coord, &w);
			coord.X++;
		}
	}
}

int main(void)
{
	run();
	return 0;
}

#endif
