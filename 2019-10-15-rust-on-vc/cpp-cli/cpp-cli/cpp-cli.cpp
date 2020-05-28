#include <iostream>
#include <Windows.h>

extern "C" void rlib_initialize();

extern "C" void rlib_start();

extern "C" void rlib_drop();

extern "C" int rlib_add(int x);

int main() {
	rlib_initialize();

	rlib_start();

	int x;
	std::cout << "Input an integer:" << std::endl;
	std::cin >> x;

	for (int i = 0; i < 3; i++) {
		std::cout << rlib_add(x) << std::endl;
	}

	rlib_drop();
	return 0;
}
