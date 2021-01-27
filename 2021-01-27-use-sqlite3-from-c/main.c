#include <stdio.h>
#include <stdlib.h>
#include "./vendor/sqlite3/sqlite3.h"

static int callback(void *_unused, int argc, char **argv, char **column_names) {
    for (int i = 0; i < argc; i++) {
        printf("%s = %s\n", column_names[i], argv[i] ? argv[i] : "NULL");
    }
    printf("\n");
    return 0;
}

int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s DATABASE SQL-STATEMENT\n", argv[0]);
        return 1;
    }

    sqlite3 *db;
    int stat = sqlite3_open(argv[1], &db);
    if (stat) {
        fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
        sqlite3_close(db);
        return 1;
    }

    char *error_message = NULL;
    stat = sqlite3_exec(db, argv[2], callback, 0, &error_message);
    if (stat != SQLITE_OK) {
        fprintf(stderr, "SQL error: %s\n", error_message);
        sqlite3_free(error_message);
    }

    sqlite3_close(db);
    return 0;
}
