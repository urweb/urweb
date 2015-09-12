#include "config.h"

#include <assert.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>

#include <openssl/crypto.h>
#include <openssl/sha.h>
#include <openssl/rand.h>

#define PASSSIZE 4

// OpenSSL locks array.  See threads(3SSL).
static pthread_mutex_t *openssl_locks;

int uw_hash_blocksize = 32;

static int password[PASSSIZE];

char *uw_sig_file = NULL;

static void random_password() {
  if (!RAND_bytes((unsigned char *)password, sizeof password)) {
    fprintf(stderr, "Error generating random password\n");
    perror("RAND_bytes");
    exit(1);
  }
}

// OpenSSL callbacks
static void thread_id(CRYPTO_THREADID *const result) {
  CRYPTO_THREADID_set_numeric(result, pthread_self());
}
static void lock_or_unlock(const int mode, const int type, const char *file,
                           const int line) {
  pthread_mutex_t *const lock = &openssl_locks[type];
  if (mode & CRYPTO_LOCK) {
    if (pthread_mutex_lock(lock)) {
      fprintf(stderr, "Can't take lock at %s:%d\n", file, line);
      exit(1);
    }
  } else {
    if (pthread_mutex_unlock(lock)) {
      fprintf(stderr, "Can't release lock at %s:%d\n", file, line);
      exit(1);
    }
  }
}

void uw_init_crypto() {
  int i;
  // Set up OpenSSL.
  assert(openssl_locks == NULL);
  openssl_locks = malloc(CRYPTO_num_locks() * sizeof(pthread_mutex_t));
  if (!openssl_locks) {
    perror("malloc");
    exit(1);
  }
  for (i = 0; i < CRYPTO_num_locks(); ++i) {
    pthread_mutex_init(&(openssl_locks[i]), NULL);
  }
  CRYPTO_THREADID_set_callback(thread_id);
  CRYPTO_set_locking_callback(lock_or_unlock);
  // Prepare signatures.
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
