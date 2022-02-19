default: run

a.out: \
	atomic_counter_file.c \
	atomic_counter_file.h \
	main.c
	gcc atomic_counter_file.c atomic_counter_file.h main.c

build: a.out

run: build
	./a.out

parallel: build
	seq 16 | xargs -P16 -I{} ./a.out {} | sort
