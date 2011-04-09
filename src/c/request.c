#include "config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <signal.h>

#include <pthread.h>

#include "urweb.h"

#define MAX_RETRIES 5

void *memmem(const void *b1, size_t len1, const void *b2, size_t len2);

static int try_rollback(uw_context ctx, int will_retry, void *logger_data, uw_logger log_error) {
  int r = uw_rollback(ctx, will_retry);

  if (r) {
    log_error(logger_data, "Error running SQL ROLLBACK\n");
    uw_reset(ctx);
    uw_write(ctx, "HTTP/1.1 500 Internal Server Error\r\n");
    uw_write(ctx, "Content-type: text/plain\r\n\r\n");
    uw_write(ctx, "Error running SQL ROLLBACK\n");
    uw_set_error_message(ctx, "Database error; you are probably out of storage space.");
  }

  return r;
}

uw_context uw_request_new_context(int id, uw_app *app, void *logger_data, uw_logger log_error, uw_logger log_debug) {
  uw_context ctx = uw_init(id, logger_data, log_debug);
  int retries_left = MAX_RETRIES;
  uw_set_app(ctx, app);

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

static void *ticker(void *data) {
  while (1) {
    usleep(100000);
    ++uw_time;
  }

  return NULL;
}

typedef struct {
  uw_app *app;
  void *logger_data;
  uw_logger log_error, log_debug;
} loggers;

typedef struct {
  int id;
  loggers *ls;
  uw_periodic pdic;
} periodic;

static void *periodic_loop(void *data) {
  periodic *p = (periodic *)data;
  uw_context ctx = uw_request_new_context(p->id, p->ls->app, p->ls->logger_data, p->ls->log_error, p->ls->log_debug);

  if (!ctx)
    exit(1);

  while (1) {
    int retries_left = MAX_RETRIES;

    failure_kind r;
    do {
      uw_reset(ctx);
      r = uw_runCallback(ctx, p->pdic.callback);
      if (r == BOUNDED_RETRY)
        --retries_left;
      else if (r == UNLIMITED_RETRY)
        p->ls->log_debug(p->ls->logger_data, "Error triggers unlimited retry in periodic: %s\n", uw_error_message(ctx));
      else if (r == BOUNDED_RETRY)
        p->ls->log_debug(p->ls->logger_data, "Error triggers bounded retry in periodic: %s\n", uw_error_message(ctx));
      else if (r == FATAL)
        p->ls->log_error(p->ls->logger_data, "Fatal error: %s\n", uw_error_message(ctx));
      if (r == FATAL || r == BOUNDED_RETRY || r == UNLIMITED_RETRY)
        if (try_rollback(ctx, 0, p->ls->logger_data, p->ls->log_error))
          return NULL;
    } while (r == UNLIMITED_RETRY || (r == BOUNDED_RETRY && retries_left > 0));

    if (r != FATAL && r != BOUNDED_RETRY)
      uw_commit(ctx);

    sleep(p->pdic.period);
  };
}

void uw_request_init(uw_app *app, void *logger_data, uw_logger log_error, uw_logger log_debug) {
  uw_context ctx;
  failure_kind fk;
  uw_periodic *ps;
  loggers *ls = malloc(sizeof(loggers));
  int id;

  ls->app = app;
  ls->logger_data = logger_data;
  ls->log_error = log_error;
  ls->log_debug = log_debug;

  uw_global_init();
  uw_app_init(app);

  {
    pthread_t thread;
    
    if (uw_time_max && pthread_create(&thread, NULL, ticker, NULL)) {
      fprintf(stderr, "Error creating ticker thread\n");
      exit(1);
    }
  }

  ctx = uw_request_new_context(0, app, logger_data, log_error, log_debug);

  if (!ctx)
    exit(1);

  for (fk = uw_initialize(ctx); fk == UNLIMITED_RETRY; fk = uw_initialize(ctx)) {
    log_debug(logger_data, "Unlimited retry during init: %s\n", uw_error_message(ctx));
    uw_rollback(ctx, 1);
    uw_reset(ctx);
  }

  if (fk != SUCCESS) {
    log_error(logger_data, "Failed to initialize database!  %s\n", uw_error_message(ctx));
    uw_rollback(ctx, 0);
    exit(1);
  }

  uw_free(ctx);

  id = 1;
  for (ps = app->periodics; ps->callback; ++ps) {
    pthread_t thread;
    periodic *arg = malloc(sizeof(periodic));
    arg->id = id++;
    arg->ls = ls;
    arg->pdic = *ps;
    
    if (pthread_create(&thread, NULL, periodic_loop, arg)) {
      fprintf(stderr, "Error creating periodic thread\n");
      exit(1);
    }
  }  
}


typedef struct uw_rc {
  size_t path_copy_size, queryString_size;
  char *path_copy, *queryString;
} *uw_request_context;

uw_request_context uw_new_request_context(void) {
  uw_request_context r = malloc(sizeof(struct uw_rc));
  r->path_copy_size = 0;
  r->queryString_size = 1;
  r->path_copy = malloc(0);
  r->queryString = malloc(1);
  return r;
}

void uw_free_request_context(uw_request_context r) {
  free(r->path_copy);
  free(r->queryString);
  free(r);
}

request_result uw_request(uw_request_context rc, uw_context ctx,
                          char *method, char *path, char *query_string,
                          char *body, size_t body_len,
                          void (*on_success)(uw_context), void (*on_failure)(uw_context),
                          void *logger_data, uw_logger log_error, uw_logger log_debug,
                          int sock,
                          int (*send)(int sockfd, const void *buf, size_t len),
                          int (*close)(int fd)) {
  int retries_left = MAX_RETRIES;
  failure_kind fk;
  int is_post = 0;
  char *boundary = NULL;
  size_t boundary_len = 0;
  char *inputs;
  const char *prefix = uw_get_url_prefix(ctx);
  char *s;
  int had_error = 0;
  char errmsg[ERROR_BUF_LEN];

  uw_reset(ctx);

  rc->queryString[0] = 0;

  for (s = path; *s; ++s) {
    if (s[0] == '%' && s[1] == '2' && s[2] == '7') {
      s[0] = '\'';
      memmove(s+1, s+3, strlen(s+3)+1);
    }
  }

  uw_set_currentUrl(ctx, path);

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
    uw_isPost(ctx);

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
    } else if (clen_s && strcasecmp(clen_s, "application/x-www-form-urlencoded")) {
      uw_Basis_postBody pb = {clen_s, body};
      uw_postBody(ctx, pb);
    }
  } else if (strcmp(method, "GET")) {
    log_error(logger_data, "Not ready for non-GET/POST command: %s\n", method);
    return FAILED;
  }

  if (!strncmp(path, prefix, strlen(prefix))
      && !strcmp(path + strlen(prefix), ".msgs")) {
    char *id = uw_Basis_requestHeader(ctx, "UrWeb-Client");
    char *pass = uw_Basis_requestHeader(ctx, "UrWeb-Pass");

    if (sock < 0) {
      log_error(logger_data, ".msgs requested, but not socket supplied\n");
      return FAILED;
    }

    if (id && pass) {
      unsigned idn = atoi(id);
      uw_client_connect(idn, atoi(pass), sock, send, close, logger_data, log_error);
      log_debug(logger_data, "Processed request for messages by client %u\n\n", idn);
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

      for (header = part; (after_header = strstr(header, "\r\n")); header = after_header + 2) {
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

          for (colon += 11; (after_colon = strchr(colon, '=')); colon = after_colon) {
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
  else if (!uw_hasPostBody(ctx)) {
    inputs = is_post ? body : query_string;

    if (inputs) {
      char *name, *value;
      int len = strlen(inputs);

      if (len+1 > rc->queryString_size) {
        rc->queryString_size = len+1;
        rc->queryString = realloc(rc->queryString, len+1);
      }
      strcpy(rc->queryString, inputs);

      while (*inputs) {
        name = inputs;
        if ((inputs = strchr(inputs, '&')))
          *inputs++ = 0;
        else
          inputs = strchr(name, 0);

        if ((value = strchr(name, '='))) {
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

  while (1) {
    uw_setQueryString(ctx, rc->queryString);

    if (!had_error) {
      size_t path_len = strlen(path);

      on_success(ctx);

      if (path_len + 1 > rc->path_copy_size) {
        rc->path_copy_size = path_len + 1;
        rc->path_copy = realloc(rc->path_copy, rc->path_copy_size);
      }
      strcpy(rc->path_copy, path);

      uw_set_deadline(ctx, uw_time + uw_time_max);
      fk = uw_begin(ctx, rc->path_copy);
    } else {
      uw_set_deadline(ctx, uw_time + uw_time_max);
      fk = uw_begin_onError(ctx, errmsg);
    }

    if (fk == SUCCESS || fk == RETURN_INDIRECTLY) {
      uw_commit(ctx);
      if (uw_has_error(ctx) && !had_error) {
        log_error(logger_data, "Fatal error: %s\n", uw_error_message(ctx));
        uw_reset_keep_error_message(ctx);
        on_failure(ctx);

        if (uw_get_app(ctx)->on_error) {
          had_error = 1;
          strcpy(errmsg, uw_error_message(ctx));
        } else {
          uw_write_header(ctx, "Content-type: text/html\r\n");
          uw_write(ctx, "<html><head><title>Fatal Error</title></head><body>");
          uw_write(ctx, "Fatal error: ");
          uw_write(ctx, uw_error_message(ctx));
          uw_write(ctx, "\n</body></html>");
        
          return FAILED;
        }
      } else
        return had_error ? FAILED : SERVED;
    } else if (fk == BOUNDED_RETRY) {
      if (retries_left) {
        log_debug(logger_data, "Error triggers bounded retry: %s\n", uw_error_message(ctx));
        --retries_left;
      }
      else {
        log_error(logger_data, "Fatal error (out of retries): %s\n", uw_error_message(ctx));

        try_rollback(ctx, 0, logger_data, log_error);

        if (!had_error && uw_get_app(ctx)->on_error) {
          had_error = 1;
          strcpy(errmsg, uw_error_message(ctx));
        } else {
          uw_reset_keep_error_message(ctx);
          on_failure(ctx);
          uw_write_header(ctx, "Content-type: text/plain\r\n");
          uw_write(ctx, "Fatal error (out of retries): ");
          uw_write(ctx, uw_error_message(ctx));
          uw_write(ctx, "\n");
          
          return FAILED;
        }
      }
    } else if (fk == UNLIMITED_RETRY)
      log_debug(logger_data, "Error triggers unlimited retry: %s\n", uw_error_message(ctx));
    else if (fk == FATAL) {
      log_error(logger_data, "Fatal error: %s\n", uw_error_message(ctx));

      try_rollback(ctx, 0, logger_data, log_error);

      if (uw_get_app(ctx)->on_error && !had_error) {
        had_error = 1;
        strcpy(errmsg, uw_error_message(ctx));
      } else {
        uw_reset_keep_error_message(ctx);
        on_failure(ctx);
        uw_write_header(ctx, "Content-type: text/html\r\n");
        uw_write(ctx, "<html><head><title>Fatal Error</title></head><body>");
        uw_write(ctx, "Fatal error: ");
        uw_write(ctx, uw_error_message(ctx));
        uw_write(ctx, "\n</body></html>");

        return FAILED;
      }
    } else {
      log_error(logger_data, "Unknown uw_handle return code!\n");

      try_rollback(ctx, 0, logger_data, log_error);

      if (uw_get_app(ctx)->on_error && !had_error) {
        had_error = 1;
        strcpy(errmsg, "Unknown uw_handle return code");
      } else {
        uw_reset_keep_request(ctx);
        on_failure(ctx);
        uw_write_header(ctx, "Content-type: text/plain\r\n");
        uw_write(ctx, "Unknown uw_handle return code!\n");

        return FAILED;
      }
    }

    if (try_rollback(ctx, 1, logger_data, log_error))
      return FAILED;

    uw_reset_keep_request(ctx);
  }
}

void *client_pruner(void *data) {
  loggers *ls = (loggers *)data;
  uw_context ctx = uw_request_new_context(0, ls->app, ls->logger_data, ls->log_error, ls->log_debug);

  if (!ctx)
    exit(1);

  while (1) {
    uw_prune_clients(ctx);
    sleep(5);
  }
}
