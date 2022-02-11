default: a.out

a.out: main.c
	gcc -static -I${PWD}/vendor/include main.c -L${PWD}/vendor/lib -l"avro" -l"jansson"
