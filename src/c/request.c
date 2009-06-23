#define _GNU_SOURCE

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <signal.h>

#include <pthread.h>

#include <mhash.h>

#include "urweb.h"

#define MAX_RETRIES 5

static int try_rollback(uw_context ctx) {
  int r = uw_rollback(ctx);

  if (r) {
    printf("Error running SQL ROLLBACK\n");
    uw_reset(ctx);
    uw_write(ctx, "HTTP/1.1 500 Internal Server Error\n\r");
    uw_write(ctx, "Content-type: text/plain\r\n\r\n");
    uw_write(ctx, "Error running SQL ROLLBACK\n");
  }

  return r;
}

uw_context uw_request_new_context() {
  uw_context ctx = uw_init();
  int retries_left = MAX_RETRIES;

  while (1) {
    failure_kind fk = uw_begin_init(ctx);

    if (fk == SUCCESS) {
      printf("Database connection initialized.\n");
      break;
    } else if (fk == BOUNDED_RETRY) {
      if (retries_left) {
        printf("Initialization error triggers bounded retry: %s\n", uw_error_message(ctx));
        --retries_left;
      } else {
        printf("Fatal initialization error (out of retries): %s\n", uw_error_message(ctx));
        uw_free(ctx);
        return NULL;
      }
    } else if (fk == UNLIMITED_RETRY)
      printf("Initialization error triggers unlimited retry: %s\n", uw_error_message(ctx));
    else if (fk == FATAL) {
      printf("Fatal initialization error: %s\n", uw_error_message(ctx));
      uw_free(ctx);
      return NULL;
    } else {
      printf("Unknown uw_begin_init return code!\n");
      uw_free(ctx);
      return NULL;
    }
  }

  return ctx;
}

#define KEYSIZE 16
#define PASSSIZE 4

#define HASH_ALGORITHM MHASH_SHA256
#define HASH_BLOCKSIZE 32
#define KEYGEN_ALGORITHM KEYGEN_MCRYPT

int uw_hash_blocksize = HASH_BLOCKSIZE;

static int password[PASSSIZE];
static unsigned char private_key[KEYSIZE];

static void init_crypto() {
  KEYGEN kg = {{HASH_ALGORITHM, HASH_ALGORITHM}};
  int i;

  assert(mhash_get_block_size(HASH_ALGORITHM) == HASH_BLOCKSIZE);

  for (i = 0; i < PASSSIZE; ++i)
    password[i] = rand();

  if (mhash_keygen_ext(KEYGEN_ALGORITHM, kg,
                       private_key, sizeof(private_key),
                       (unsigned char*)password, sizeof(password)) < 0) {
    printf("Key generation failed\n");
    exit(1);
  }
}

void uw_request_init() {
  uw_context ctx;
  failure_kind fk;

  uw_global_init();

  ctx = uw_request_new_context();

  if (!ctx)
    exit(1);

  for (fk = uw_initialize(ctx); fk == UNLIMITED_RETRY; fk = uw_initialize(ctx)) {
    printf("Unlimited retry during init: %s\n", uw_error_message(ctx));
    uw_db_rollback(ctx);
    uw_reset(ctx);
  }

  if (fk != SUCCESS) {
    printf("Failed to initialize database!  %s\n", uw_error_message(ctx));
    uw_db_rollback(ctx);
    exit(1);
  }

  uw_free(ctx);

  init_crypto();
}

void uw_sign(const char *in, char *out) {
  MHASH td;

  td = mhash_hmac_init(HASH_ALGORITHM, private_key, sizeof(private_key),
                       mhash_get_hash_pblock(HASH_ALGORITHM));
  
  mhash(td, in, strlen(in));
  if (mhash_hmac_deinit(td, out) < 0)
    printf("Signing failed");
}

typedef struct uw_rc {
  size_t path_copy_size;
  char *path_copy;
} *uw_request_context;

uw_request_context uw_new_request_context(void) {
  uw_request_context r = malloc(sizeof(struct uw_rc));
  r->path_copy_size = 0;
  r->path_copy = malloc(0);
  return r;
}

void uw_free_request_context(uw_request_context r) {
  free(r->path_copy);
  free(r);
}

request_result uw_request(uw_request_context rc, uw_context ctx, char *request, size_t request_len, int sock) {
  int retries_left = MAX_RETRIES;
  char *s;
  failure_kind fk;
  int is_post = 0, do_normal_send = 1;
  char *boundary = NULL;
  size_t boundary_len;
  char *cmd, *path, *headers, *inputs, *after_headers;

  if (!(s = strstr(request, "\r\n\r\n"))) {
    fprintf(stderr, "No end of headers found in request\n");
    return FAILED;
  }

  s[2] = 0;
  after_headers = s + 4;

  if (!(s = strstr(request, "\r\n"))) {
    fprintf(stderr, "No newline in request\n");
    return FAILED;
  }

  *s = 0;
  headers = s + 2;
  cmd = s = request;

  if (!strsep(&s, " ")) {
    fprintf(stderr, "No first space in HTTP command\n");
    return FAILED;
  }

  uw_set_headers(ctx, headers);

  if (!strcmp(cmd, "POST")) {
    char *clen_s = uw_Basis_requestHeader(ctx, "Content-length");
    if (!clen_s) {
      fprintf(stderr, "No Content-length with POST\n");
      return FAILED;
    }
    int clen = atoi(clen_s);
    if (clen < 0) {
      fprintf(stderr, "Negative Content-length with POST\n");
      return FAILED;
    }

    if (request + request_len - after_headers < clen) {
      fprintf(stderr, "Request doesn't contain all POST data (according to Content-Length)\n");
      return FAILED;
    }

    is_post = 1;

    clen_s = uw_Basis_requestHeader(ctx, "Content-type");
    if (clen_s && !strncasecmp(clen_s, "multipart/form-data", 19)) {
      if (strncasecmp(clen_s + 19, "; boundary=", 11)) {
        fprintf(stderr, "Bad multipart boundary spec");
        return FAILED;
      }

      boundary = clen_s + 28;
      boundary[0] = '-';
      boundary[1] = '-';
      boundary_len = strlen(boundary);
    }
  } else if (strcmp(cmd, "GET")) {
    fprintf(stderr, "Not ready for non-GET/POST command: %s\n", cmd);
    return FAILED;
  }

  path = s;
  if (!strsep(&s, " ")) {
    fprintf(stderr, "No second space in HTTP command\n");
    return FAILED;
  }

  if (!strcmp(path, "/.msgs")) {
    char *id = uw_Basis_requestHeader(ctx, "UrWeb-Client");
    char *pass = uw_Basis_requestHeader(ctx, "UrWeb-Pass");

    if (sock < 0) {
      fprintf(stderr, ".msgs requested, but not socket supplied\n");
      return FAILED;
    }

    if (id && pass) {
      unsigned idn = atoi(id);
      uw_client_connect(idn, atoi(pass), sock);
      fprintf(stderr, "Processed request for messages by client %u\n\n", idn);
      return KEEP_OPEN;
    }
    else {
      fprintf(stderr, "Missing fields in .msgs request: %s, %s\n\n", id, pass);
      return FAILED;
    }
  }

  if (boundary) {
    char *part = after_headers, *after_sub_headers, *header, *after_header;
    size_t part_len;

    part = strstr(part, boundary);
    if (!part) {
      fprintf(stderr, "Missing first multipart boundary\n");
      return FAILED;
    }
    part += boundary_len;

    while (1) {
      char *name = NULL, *filename = NULL, *type = NULL;

      if (part[0] == '-' && part[1] == '-')
        break;

      if (*part != '\r') {
        fprintf(stderr, "No \\r after multipart boundary\n");
        return FAILED;
      }
      ++part;
      if (*part != '\n') {
        fprintf(stderr, "No \\n after multipart boundary\n");
        return FAILED;
      }
      ++part;
            
      if (!(after_sub_headers = strstr(part, "\r\n\r\n"))) {
        fprintf(stderr, "Missing end of headers after multipart boundary\n");
        return FAILED;
      }
      after_sub_headers[2] = 0;
      after_sub_headers += 4;

      for (header = part; after_header = strstr(header, "\r\n"); header = after_header + 2) {
        char *colon, *after_colon;

        *after_header = 0;
        if (!(colon = strchr(header, ':'))) {
          fprintf(stderr, "Missing colon in multipart sub-header\n");
          return FAILED;
        }
        *colon++ = 0;
        if (*colon++ != ' ') {
          fprintf(stderr, "No space after colon in multipart sub-header\n");
          return FAILED;
        }
        
        if (!strcasecmp(header, "Content-Disposition")) {
          if (strncmp(colon, "form-data; ", 11)) {
            fprintf(stderr, "Multipart data is not \"form-data\"\n");
            return FAILED;
          }

          for (colon += 11; after_colon = strchr(colon, '='); colon = after_colon) {
            char *data;
            after_colon[0] = 0;
            if (after_colon[1] != '"') {
              fprintf(stderr, "Disposition setting is missing initial quote\n");
              return FAILED;
            }
            data = after_colon+2;
            if (!(after_colon = strchr(data, '"'))) {
              fprintf(stderr, "Disposition setting is missing final quote\n");
              return FAILED;
            }
            after_colon[0] = 0;
            ++after_colon;
            if (after_colon[0] == ';' && after_colon[1] == ' ')
              after_colon += 2;

            if (!strcasecmp(colon, "name"))
              name = data;
            else if (!strcasecmp(colon, "filename"))
              filename = data;
          }
        } else if (!strcasecmp(header, "Content-Type")) {
          type = colon;
        }
      }

      part = memmem(after_sub_headers, request + request_len - after_sub_headers, boundary, boundary_len);
      if (!part) {
        fprintf(stderr, "Missing boundary after multipart payload\n");
        return FAILED;
      }
      part[-2] = 0;
      part_len = part - after_sub_headers - 2;
      part[0] = 0;
      part += boundary_len;

      if (filename) {
        uw_Basis_file f = {filename, type, {part_len, after_sub_headers}};

        if (uw_set_file_input(ctx, name, f)) {
          fprintf(stderr, "%s\n", uw_error_message(ctx));
          return FAILED;
        }
      } else if (uw_set_input(ctx, name, after_sub_headers)) {
        fprintf(stderr, "%s\n", uw_error_message(ctx));
        return FAILED;
      }
    }
  }
  else {
    if (is_post)
      inputs = after_headers;
    else if (inputs = strchr(path, '?'))
      *inputs++ = 0;

    if (inputs) {
      char *name, *value;

      while (*inputs) {
        name = inputs;
        if (inputs = strchr(inputs, '&'))
          *inputs++ = 0;
        else
          inputs = strchr(name, 0);

        if (value = strchr(name, '=')) {
          *value++ = 0;
          if (uw_set_input(ctx, name, value)) {
            fprintf(stderr, "%s\n", uw_error_message(ctx));
            return FAILED;
          }
        }
        else if (uw_set_input(ctx, name, "")) {
          fprintf(stderr, "%s\n", uw_error_message(ctx));
          return FAILED;
        }
      }
    }
  }

  printf("Serving URI %s....\n", path);

  while (1) {
    size_t path_len = strlen(path);

    uw_write_header(ctx, "HTTP/1.1 200 OK\r\n");

    if (path_len + 1 > rc->path_copy_size) {
      rc->path_copy_size = path_len + 1;
      rc->path_copy = realloc(rc->path_copy, rc->path_copy_size);
    }
    strcpy(rc->path_copy, path);
    fk = uw_begin(ctx, rc->path_copy);
    if (fk == SUCCESS || fk == RETURN_BLOB) {
      uw_commit(ctx);
      return SERVED;
    } else if (fk == BOUNDED_RETRY) {
      if (retries_left) {
        printf("Error triggers bounded retry: %s\n", uw_error_message(ctx));
        --retries_left;
      }
      else {
        printf("Fatal error (out of retries): %s\n", uw_error_message(ctx));

        try_rollback(ctx);

        uw_reset_keep_error_message(ctx);
        uw_write_header(ctx, "HTTP/1.1 500 Internal Server Error\n\r");
        uw_write_header(ctx, "Content-type: text/plain\r\n");
        uw_write(ctx, "Fatal error (out of retries): ");
        uw_write(ctx, uw_error_message(ctx));
        uw_write(ctx, "\n");

        return FAILED;
      }
    } else if (fk == UNLIMITED_RETRY)
      printf("Error triggers unlimited retry: %s\n", uw_error_message(ctx));
    else if (fk == FATAL) {
      printf("Fatal error: %s\n", uw_error_message(ctx));

      try_rollback(ctx);

      uw_reset_keep_error_message(ctx);
      uw_write_header(ctx, "HTTP/1.1 500 Internal Server Error\r\n");
      uw_write_header(ctx, "Content-type: text/html\r\n");
      uw_write(ctx, "<html><head><title>Fatal Error</title></head><body>");
      uw_write(ctx, "Fatal error: ");
      uw_write(ctx, uw_error_message(ctx));
      uw_write(ctx, "\n</body></html>");

      return FAILED;
    } else {
      printf("Unknown uw_handle return code!\n");

      try_rollback(ctx);

      uw_reset_keep_request(ctx);
      uw_write_header(ctx, "HTTP/1.1 500 Internal Server Error\n\r");
      uw_write_header(ctx, "Content-type: text/plain\r\n");
      uw_write(ctx, "Unknown uw_handle return code!\n");

      return FAILED;
    }

    if (try_rollback(ctx))
      return FAILED;

    uw_reset_keep_request(ctx);
  }
}

void *client_pruner(void *data) {
  uw_context ctx = uw_request_new_context();

  if (!ctx)
    exit(1);

  while (1) {
    uw_prune_clients(ctx);
    sleep(5);
  }
}
