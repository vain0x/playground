
#define FUSE_USE_VERSION 31

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <fuse.h>
#include <spawn.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

// -----------------------------------------------
// Command line options
// -----------------------------------------------

static void show_help(char *const program_name) {
    printf("usage: %s [options] <mountpoint>\n", program_name);
}

struct options {
    char const *src_dir;
    int show_help;
};

#define OPTION(t, p)                                                           \
    { t, offsetof(struct options, p), 1 }

static struct fuse_opt const option_spec[] = {
    OPTION("--src=%s", src_dir),
    OPTION("-h", show_help),
    OPTION("--help", show_help),
    FUSE_OPT_END,
};

// -----------------------------------------------
// File system
// -----------------------------------------------

static void *myfs_init(struct fuse_conn_info *conn, struct fuse_config *cfg) {
    (void)conn;
    (void)cfg;
    return NULL;
}

static int myfs_getattr(char const *path, struct stat *stbuf,
                        struct fuse_file_info *fi) {
    (void)fi;

    fprintf(stderr, "trace: getattr path='%s'\n", path);
    memset(stbuf, 0, sizeof(struct stat));

    // FILE *out = popen("ls -l --block-size=1 --color=never --directory
    // --time-style=full-iso", "r"); if (stat != 0) { 	fprintf(stderr, "[ERROR]
    // getattr ls failed %d", stat); 	return -EACCES;
    // }

    char *output;
    if (strcmp(path, "/") == 0) {
        output = strdup("drwxrwxr-x 2 john john 4096 2021-01-28 "
                        "21:30:03.387502968 +0900 /");
    } else if (strcmp(path + 1, "hello") == 0) {
        output = strdup("-rw-rw-r-- 1 john john    6 2021-01-28 "
                        "20:30:00.616157986 +0900 hello");
    } else {
        fprintf(stderr, "trace:    not found\n");
        return -ENOENT;
    }

    size_t n = strlen(output);
    assert(n >= 1);

    char *tok;
    char const *delim = " \t\r\n";
    int stat = 0;

    char const *word = strtok_r(output, delim, &tok);
    if (word == NULL) {
        stat = -ENOENT;
        goto RET;
    }
    char *perm = strdup(word);
    nlink_t nlink = (nlink_t)atol(strtok_r(NULL, delim, &tok));
    strtok_r(NULL, delim, &tok); // user
    strtok_r(NULL, delim, &tok); // group
    off_t size = (off_t)atol(strtok_r(NULL, delim, &tok));

    fprintf(stderr, "trace:   perm='%s' size='%ld'\n", perm, size);

    if (perm[0] == 'd') {
        stbuf->st_mode = S_IFDIR | 0755; // FIXME: read permission from output
        stbuf->st_nlink = nlink;
    } else if (perm[0] == '-') {
        stbuf->st_mode = S_IFREG | 0444;
        stbuf->st_nlink = 1;
        stbuf->st_size = size;
    } else {
        stat = -ENOENT;
        goto RET;
    }

    free(perm);
RET:
    free(output);
    return stat;
}

static int myfs_readdir(char const *path, void *buf, fuse_fill_dir_t filler,
                        off_t offset, struct fuse_file_info *fi,
                        enum fuse_readdir_flags flags) {
    (void)offset;
    (void)fi;
    (void)flags;
    fprintf(stderr, "trace: readdir path='%s'\n", path);

    if (strcmp(path, "/") != 0)
        return -ENOENT;

    filler(buf, ".", NULL, 0, 0);
    filler(buf, "..", NULL, 0, 0);
    filler(buf, "hello", NULL, 0, 0);

    return 0;
}

static int myfs_open(char const *path, struct fuse_file_info *fi) {
    fprintf(stderr, "trace: open path='%s'\n", path);

    if (strcmp(path + 1, "hello") != 0)
        return -ENOENT;

    if ((fi->flags & O_ACCMODE) != O_RDONLY)
        return -EACCES;

    return 0;
}

static int myfs_read(char const *path, char *buf, size_t size, off_t offset,
                     struct fuse_file_info *fi) {
    size_t len;
    (void)fi;
    if (strcmp(path + 1, "hello") != 0)
        return -ENOENT;

    char const *contents = "hello\n";
    len = strlen(contents);
    if ((size_t)offset >= len) {
        return 0;
    }

    if ((size_t)offset + size > len) {
        size = len - offset;
    }
    memcpy(buf, contents + offset, size);
    return size;
}

static struct fuse_operations const myfs_oper = {
    .init = myfs_init,
    .getattr = myfs_getattr,
    .readdir = myfs_readdir,
    .open = myfs_open,
    .read = myfs_read,
};

// -----------------------------------------------
// Entrypoint
// -----------------------------------------------

int main(int argc, char **argv) {
    struct fuse_args args = FUSE_ARGS_INIT(argc, argv);

    // 既定値の入ったオプションを作る。
    // 文字列は `fuse_opt_parse` によって free されるので strdup
    // でヒープに置く必要があるらしい。
    struct options options = {
        .src_dir = strdup("."),
        .show_help = 0,
    };

    if (fuse_opt_parse(&args, &options, option_spec, NULL) != 0) {
        return 1;
    }

    if (options.show_help) {
        show_help(argv[0]);

        int stat = fuse_opt_add_arg(&args, "--help");
        assert(stat == 0);

        // `fuse_main` が "usage: ..." を表示するのを抑制する。
        args.argv[0][0] = '\0';
    }

    {
        int code = fuse_main(args.argc, args.argv, &myfs_oper, NULL);
        fuse_opt_free_args(&args);
        return code;
    }
}
