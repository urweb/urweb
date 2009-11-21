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

static int try_rollback(uw_context ctx, void *logger_data, uw_logger log_error) {
  int r = uw_rollback(ctx);

  if (r) {
    log_error(logger_data, "Error running SQL ROLLBACK\n");
    uw_reset(ctx);
    uw_write(ctx, "HTTP/1.1 500 Internal Server Error\n\r");
    uw_write(ctx, "Content-type: text/plain\r\n\r\n");
    uw_write(ctx, "Error running SQL ROLLBACK\n");
  }

  return r;
}

uw_context uw_request_new_context(void *logger_data, uw_logger log_error, uw_logger log_debug) {
  uw_context ctx = uw_init();
  int retries_left = MAX_RETRIES;

  while (1) {
    failure_kind fk = uw_begin_init(ctx);

    if (fk == SUCCESS) {
      log_debug(logger_data, "Database connection initialized.\n");
      break;
    } else if (fk == BOUNDED_RETRY) {
      if (retries_left) {
        log_debug(logger_data, "Initialization error triggers bounded retry: %s\n", uw_error_message(ctx));
        --retries_left;
      } else {
        log_error(logger_data, "Fatal initialization error (out of retries): %s\n", uw_error_message(ctx));
        uw_free(ctx);
        return NULL;
      }
    } else if (fk == UNLIMITED_RETRY)
      log_debug(logger_data, "Initialization error triggers unlimited retry: %s\n", uw_error_message(ctx));
    else if (fk == FATAL) {
      log_error(logger_data, "Fatal initialization error: %s\n", uw_error_message(ctx));
      uw_free(ctx);
      return NULL;
    } else {
      log_error(logger_data, "Unknown uw_begin_init return code!\n");
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

static void init_crypto(void *logger_data, uw_logger log_error) {
  KEYGEN kg = {{HASH_ALGORITHM, HASH_ALGORITHM}};
  int i;

  assert(mhash_get_block_size(HASH_ALGORITHM) == HASH_BLOCKSIZE);

  for (i = 0; i < PASSSIZE; ++i)
    password[i] = rand();

  if (mhash_keygen_ext(KEYGEN_ALGORITHM, kg,
                       private_key, sizeof(private_key),
                       (unsigned char*)password, sizeof(password)) < 0) {
    log_error(logger_data, "Key generation failed\n");
    exit(1);
  }
}

void uw_request_init(void *logger_data, uw_logger log_error, uw_logger log_debug) {
  uw_context ctx;
  failure_kind fk;

  uw_global_init();

  ctx = uw_request_new_context(logger_data, log_error, log_debug);

  if (!ctx)
    exit(1);

  for (fk = uw_initialize(ctx); fk == UNLIMITED_RETRY; fk = uw_initialize(ctx)) {
    log_debug(logger_data, "Unlimited retry during init: %s\n", uw_error_message(ctx));
    uw_db_rollback(ctx);
    uw_reset(ctx);
  }

  if (fk != SUCCESS) {
    log_error(logger_data, "Failed to initialize database!  %s\n", uw_error_message(ctx));
    uw_db_rollback(ctx);
    exit(1);
  }

  uw_free(ctx);

  init_crypto(logger_data, log_error);
}

void uw_sign(const char *in, char *out) {
  MHASH td;

  td = mhash_hmac_init(HASH_ALGORITHM, private_key, sizeof(private_key),
                       mhash_get_hash_pblock(HASH_ALGORITHM));
  
  mhash(td, in, strlen(in));
  if (mhash_hmac_deinit(td, out) < 0)
    fprintf(stderr, "Signing failed\n");
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

extern char *uw_url_prefix;

request_result uw_request(uw_request_context rc, uw_context ctx,
                          char *method, char *path, char *query_string,
                          char *body, size_t body_len,
                          void (*on_success)(uw_context), void (*on_failure)(uw_context),
                          void *logger_data, uw_logger log_error, uw_logger log_debug,
                          int sock,
                          int (*send)(int sockfd, const void *buf, size_t len),
                          int (*close)(int fd)) {
  int retries_left = MAX_RETRIES;
  char *s;
  failure_kind fk;
  int is_post = 0, do_normal_send = 1;
  char *boundary = NULL;
  size_t boundary_len;
  char *inputs;

  if (!strcmp(method, "POST")) {
    char *clen_s = uw_Basis_requestHeader(ctx, "Content-length");
    if (!clen_s) {
      clen_s = "0";
      /*log_error(logger_data, "No Content-length with POST\n");
      return FAILED;*/
    }
    int clen = atoi(clen_s);
    if (clen < 0) {
      log_error(logger_data, "Negative Content-length with POST\n");
      return FAILED;
    }

    if (body_len < clen) {
      log_error(logger_data, "Request doesn't contain all POST data (according to Content-Length)\n");
      return FAILED;
    }

    is_post = 1;

    clen_s = uw_Basis_requestHeader(ctx, "Content-type");
    if (clen_s && !strncasecmp(clen_s, "multipart/form-data", 19)) {
      if (strncasecmp(clen_s + 19, "; boundary=", 11)) {
        log_error(logger_data, "Bad multipart boundary spec");
        return FAILED;
      }

      boundary = clen_s + 28;
      boundary[0] = '-';
      boundary[1] = '-';
      boundary_len = strlen(boundary);
    }
  } else if (strcmp(method, "GET")) {
    log_error(logger_data, "Not ready for non-GET/POST command: %s\n", method);
    return FAILED;
  }

  if (!strncmp(path, uw_url_prefix, strlen(uw_url_prefix))
      && !strcmp(path + strlen(uw_url_prefix), ".msgs")) {
    char *id = uw_Basis_requestHeader(ctx, "UrWeb-Client");
    char *pass = uw_Basis_requestHeader(ctx, "UrWeb-Pass");

    if (sock < 0) {
      log_error(logger_data, ".msgs requested, but not socket supplied\n");
      return FAILED;
    }

    if (id && pass) {
      unsigned idn = atoi(id);
      uw_client_connect(idn, atoi(pass), sock, send, close, logger_data, log_error);
      log_error(logger_data, "Processed request for messages by client %u\n\n", idn);
      return KEEP_OPEN;
    }
    else {
      log_error(logger_data, "Missing fields in .msgs request: %s, %s\n\n", id, pass);
      return FAILED;
    }
  }

  if (boundary) {
    char *part = body, *after_sub_headers, *header, *after_header;
    size_t part_len;

    part = strstr(part, boundary);
    if (!part) {
      log_error(logger_data, "Missing first multipart boundary\n");
      return FAILED;
    }
    part += boundary_len;

    while (1) {
      char *name = NULL, *filename = NULL, *type = NULL;

      if (part[0] == '-' && part[1] == '-')
        break;

      if (*part != '\r') {
        log_error(logger_data, "No \\r after multipart boundary\n");
        return FAILED;
      }
      ++part;
      if (*part != '\n') {
        log_error(logger_data, "No \\n after multipart boundary\n");
        return FAILED;
      }
      ++part;
            
      if (!(after_sub_headers = strstr(part, "\r\n\r\n"))) {
        log_error(logger_data, "Missing end of headers after multipart boundary\n");
        return FAILED;
      }
      after_sub_headers[2] = 0;
      after_sub_headers += 4;

      for (header = part; after_header = strstr(header, "\r\n"); header = after_header + 2) {
        char *colon, *after_colon;

        *after_header = 0;
        if (!(colon = strchr(header, ':'))) {
          log_error(logger_data, "Missing colon in multipart sub-header\n");
          return FAILED;
        }
        *colon++ = 0;
        if (*colon++ != ' ') {
          log_error(logger_data, "No space after colon in multipart sub-header\n");
          return FAILED;
        }
        
        if (!strcasecmp(header, "Content-Disposition")) {
          if (strncmp(colon, "form-data; ", 11)) {
            log_error(logger_data, "Multipart data is not \"form-data\"\n");
            return FAILED;
          }

          for (colon += 11; after_colon = strchr(colon, '='); colon = after_colon) {
            char *data;
            after_colon[0] = 0;
            if (after_colon[1] != '"') {
              log_error(logger_data, "Disposition setting is missing initial quote\n");
              return FAILED;
            }
            data = after_colon+2;
            if (!(after_colon = strchr(data, '"'))) {
              log_error(logger_data, "Disposition setting is missing final quote\n");
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

      part = memmem(after_sub_headers, body + body_len - after_sub_headers, boundary, boundary_len);
      if (!part) {
        log_error(logger_data, "Missing boundary after multipart payload\n");
        return FAILED;
      }
      part[-2] = 0;
      part_len = part - after_sub_headers - 2;
      part[0] = 0;
      part += boundary_len;

      if (filename) {
        uw_Basis_file f = {filename, type, {part_len, after_sub_headers}};

        if (uw_set_file_input(ctx, name, f)) {
          log_error(logger_data, "%s\n", uw_error_message(ctx));
          return FAILED;
        }
      } else if (uw_set_input(ctx, name, after_sub_headers)) {
        log_error(logger_data, "%s\n", uw_error_message(ctx));
        return FAILED;
      }
    }
  }
  else {
    inputs = is_post ? body : query_string;

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
            log_error(logger_data, "%s\n", uw_error_message(ctx));
            return FAILED;
          }
        }
        else if (uw_set_input(ctx, name, "")) {
          log_error(logger_data, "%s\n", uw_error_message(ctx));
          return FAILED;
        }
      }
    }
  }

  log_debug(logger_data, "Serving URI %s....\n", path);

  while (1) {
    size_t path_len = strlen(path);

    on_success(ctx);

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
        log_debug(logger_data, "Error triggers bounded retry: %s\n", uw_error_message(ctx));
        --retries_left;
      }
      else {
        log_error(logger_data, "Fatal error (out of retries): %s\n", uw_error_message(ctx));

        try_rollback(ctx, logger_data, log_error);

        uw_reset_keep_error_message(ctx);
        on_failure(ctx);
        uw_write_header(ctx, "Content-type: text/plain\r\n");
        uw_write(ctx, "Fatal error (out of retries): ");
        uw_write(ctx, uw_error_message(ctx));
        uw_write(ctx, "\n");

        return FAILED;
      }
    } else if (fk == UNLIMITED_RETRY)
      log_debug(logger_data, "Error triggers unlimited retry: %s\n", uw_error_message(ctx));
    else if (fk == FATAL) {
      log_error(logger_data, "Fatal error: %s\n", uw_error_message(ctx));

      try_rollback(ctx, logger_data, log_error);

      uw_reset_keep_error_message(ctx);
      on_failure(ctx);
      uw_write_header(ctx, "Content-type: text/html\r\n");
      uw_write(ctx, "<html><head><title>Fatal Error</title></head><body>");
      uw_write(ctx, "Fatal error: ");
      uw_write(ctx, uw_error_message(ctx));
      uw_write(ctx, "\n</body></html>");

      return FAILED;
    } else {
      log_error(logger_data, "Unknown uw_handle return code!\n");

      try_rollback(ctx, logger_data, log_error);

      uw_reset_keep_request(ctx);
      on_failure(ctx);
      uw_write_header(ctx, "Content-type: text/plain\r\n");
      uw_write(ctx, "Unknown uw_handle return code!\n");

      return FAILED;
    }

    if (try_rollback(ctx, logger_data, log_error))
      return FAILED;

    uw_reset_keep_request(ctx);
  }
}

typedef struct {
  void *logger_data;
  uw_logger log_error, log_debug;
} loggers;

void *client_pruner(void *data) {
  loggers *ls = (loggers *)data;
  uw_context ctx = uw_request_new_context(ls->logger_data, ls->log_error, ls->log_debug);

  if (!ctx)
    exit(1);

  while (1) {
    uw_prune_clients(ctx);
    sleep(5);
  }
}
