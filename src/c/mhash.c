#include "config.h"

#include <mhash.h>
#include <fcntl.h>

#define KEYSIZE 16
#define PASSSIZE 4

#define HASH_ALGORITHM MHASH_SHA256
#define HASH_BLOCKSIZE 32
#define KEYGEN_ALGORITHM KEYGEN_MCRYPT

int uw_hash_blocksize = HASH_BLOCKSIZE;

static int password[PASSSIZE];
static unsigned char private_key[KEYSIZE];

char *uw_sig_file = NULL;

static void random_password() {
  int i;

  for (i = 0; i < PASSSIZE; ++i)
    password[i] = rand();
}

void uw_init_crypto() {
  KEYGEN kg = {{HASH_ALGORITHM, HASH_ALGORITHM}};

  assert(mhash_get_block_size(HASH_ALGORITHM) == HASH_BLOCKSIZE);

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

  if (mhash_keygen_ext(KEYGEN_ALGORITHM, kg,
                       private_key, sizeof(private_key),
                       (unsigned char*)password, sizeof(password)) < 0) {
    fprintf(stderr, "Key generation failed\n");
    exit(1);
  }
}

void uw_sign(const char *in, char *out) {
  MHASH td;

  td = mhash_hmac_init(HASH_ALGORITHM, private_key, sizeof(private_key),
                       mhash_get_hash_pblock(HASH_ALGORITHM));
  
  mhash(td, in, strlen(in));
  if (mhash_hmac_deinit(td, out) < 0)
    fprintf(stderr, "Signing failed\n");
}
