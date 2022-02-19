
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

#include "./atomic_counter_file.h"

// what: manipulate a file as atomically incremented counter
// how:
//    Initially create an 8-byte, zeroed file.
//    On open, allocate K counts and hold in memory.
//    On close, nothing to do. In-memory unused counters are just discarded.
//    On increment, atomically increment in-memory counter and return its value.
//
//    When in-memory counters run out, allocate K counts again. Re-open file, increment by K, and write, sync and then close.
//
//    As optimization for the use-case to batch increment
//    Per-T increments, it checks current time
//    If it seems incrementing counter at a speed exceeding threashold,
//    it pre-allocate more counters. That is, pre-allocate by 2^R.

// Never defined.
struct ACF_File;

struct ACF
{
    struct ACF_File *file;
    uint64_t last;
    uint64_t capacity;
    uint32_t rank;
};

#define TRACE(MSG)                               \
    do                                           \
    {                                            \
        fprintf(stderr, "TRACE: ACF:" MSG "\n"); \
    } while (0);

#ifdef _MSVC

// Windows

// Windows.h
typedef int BOOL;
typedef void *HANDLE;
BOOL FlushFileBuffers(HANDLE hFile);

// ACF_File * <-> HANDLE
#define AS_ACF_FILE(HANDLE) ((struct ACF_File *)(HANDLE))
#define OF_ACF_FILE(FILE_PTR) ((HANDLE)(FILE_PTR))

static struct ACF_File *_open(const char *pathname);
static void _close(struct ACF_File *file);
static uint64_t _read(struct ACF_File *file);
static void _write(struct ACF_File *file, uint64_t value);
static void _flush(struct ACF_File *file);

#else

// Unix

#include <unistd.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>

// ACF_File * <-> FILE *
#define AS_ACF_FILE(PTR) ((struct ACF_File *)(FILE *)(PTR))
#define OF_ACF_FILE(PTR) ((FILE *)(PTR))

struct ACF_File
{
    FILE *ptr;
};

static struct ACF_File *_open(const char *pathname);
static void _close(struct ACF_File *file);
static void _flush(struct ACF_File *file);

#endif

int ACF_open(struct ACF **ctx, char const *pathname)
{
    TRACE("open");
    assert(ctx != NULL);
    assert(pathname != NULL);

    *ctx = NULL;

    // Open or create file.
    int fd = open(pathname, O_RDWR | O_CREAT | O_DSYNC);
    if (fd == -1)
        return -1;

    FILE *file = fdopen(fd, "rb+");
    if (file == NULL)
        return -1;

    *ctx = malloc(sizeof(struct ACF));
    if (*ctx == NULL)
        return -1;

    **ctx = (struct ACF){.file = AS_ACF_FILE(file), .last = 0, .capacity = 0, .rank = 0};
    return 0;
}

void ACF_close(struct ACF *ctx)
{
    TRACE("close");
    assert(ctx != NULL);

    FILE *file = OF_ACF_FILE(ctx->file);
    if (file != NULL)
    {
        fclose(file);
    }

    ctx->file = NULL;
    ctx->last = 0;
    ctx->capacity = 0;
    ctx->rank = 0;
}

#include <errno.h>
#include <string.h>

static bool _preallocate(struct ACF *ctx);

static bool _initialize(struct ACF *ctx)
{
    FILE *file = OF_ACF_FILE(ctx->file);
    assert(file != NULL);

    if (flock(fileno(file), LOCK_EX) != 0)
        return false;

    uint64_t buf;
    fseek(file, 0, SEEK_SET);
    bool ok = fread(&buf, sizeof(uint64_t), 1, file) == 1;
    if (!ok && !feof(file))
        return false;

    if (!ok)
    {
        // In the case file was not found or empty since feof() was true.
        TRACE("initial");

        buf = 1;
        fseek(file, 0, SEEK_SET);
        if (fwrite(&buf, sizeof(uint64_t), 1, file) != 1)
            return false;

        ctx->last = 0;
        ctx->capacity = 1;
        ctx->rank = 1;

        return flock(fileno(file), LOCK_UN) == 0;
    }

    if (flock(fileno(file), LOCK_UN) != 0)
        return false;

    return _preallocate(ctx);
}

bool _preallocate(struct ACF *ctx)
{
    TRACE("preallocate");
    FILE *file = OF_ACF_FILE(ctx->file);
    assert(file != NULL);

    if (flock(fileno(file), LOCK_EX) != 0)
        return false;

    uint64_t buf;
    fseek(file, 0, SEEK_SET);
    if (fread(&buf, sizeof(uint64_t), 1, file) != 1)
        return false;

    fprintf(stderr, "read = %d\n", (int)buf);

    ctx->last = buf;
    ctx->capacity = buf + (1 << ctx->rank);
    ctx->rank++;

    return fseek(file, 0, SEEK_SET) == 0 &&
           fwrite(&ctx->capacity, sizeof(uint64_t), 1, file) == 1 && fflush(file) == 0 && flock(fileno(file), LOCK_UN) == 0;
}

int ACF_increment(struct ACF *ctx, long long *result)
{
    assert(ctx != NULL);
    assert(result != NULL);

    // FIXME: make it thread-safe

    if (ctx->rank == 0)
    {
        if (!_initialize(ctx))
            return -1;
    }

    if (ctx->last == ctx->capacity)
    {
        if (!_preallocate(ctx))
            return -1;
    }

    TRACE("fastpath");
    assert(ctx->last < ctx->capacity);
    ctx->last++;
    *result = ctx->last;
    return 0;
}
