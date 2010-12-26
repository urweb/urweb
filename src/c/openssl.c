#include "config.h"

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>

#include <openssl/sha.h>

#define PASSSIZE 4

int uw_hash_blocksize = 32;

static int password[PASSSIZE];

char *uw_sig_file = NULL;

static void random_password() {
  int i;

  for (i = 0; i < PASSSIZE; ++i)
    password[i] = rand();
}

void uw_init_crypto() {
  if (uw_sig_file) {
    int fd;

    if (access(uw_sig_file, F_OK)) {
      random_password();
      
      if ((fd = open(uw_sig_file, O_WRONLY | O_CREAT, 0700)) < 0) {
        fprintf(stderr, "Can't open signature file %s\n", uw_sig_file);
        perror("open");
        exit(1);
      }

      if (write(fd, &password, sizeof password) != sizeof password) {
        fprintf(stderr, "Error writing signature file\n");
        exit(1);
      }

      close(fd);
    } else {
      if ((fd = open(uw_sig_file, O_RDONLY)) < 0) {
        fprintf(stderr, "Can't open signature file %s\n", uw_sig_file);
        perror("open");
        exit(1);
      }

      if (read(fd, &password, sizeof password) != sizeof password) {
        fprintf(stderr, "Error reading signature file\n");
        exit(1);
      }

      close(fd);
    }
  } else
    random_password();
}

void uw_sign(const char *in, unsigned char *out) {
  SHA256_CTX c;

  SHA256_Init(&c);
  SHA256_Update(&c, password, sizeof password);
  SHA256_Update(&c, in, strlen(in));
  SHA256_Final(out, &c);
}
