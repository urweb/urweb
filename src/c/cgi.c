#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include "urweb.h"
#include "request.h"

#if defined(_WIN32) || defined(__WIN32__) || defined(__WINDOWS__)
#include "winshim.h"
#else
#include <unistd.h>
#endif

extern uw_app uw_application;

static char *uppercased;
static size_t uppercased_len;

static char *get_header(void *data, const char *h) {
  (void)data;

  size_t len = strlen(h);
  char *s, *r;
  const char *saved_h = h;

  if (len > uppercased_len) {
    uppercased_len = len;
    uppercased = realloc(uppercased, len + 6);
  }

  strcpy(uppercased, "HTTP_");
  for (s = uppercased+5; *h; ++h)
    *s++ = *h == '-' ? '_' : toupper((int)*h);
  *s = 0;

  if ((r = getenv(uppercased)))
    return r;
  else if (!strcasecmp(saved_h, "Content-length")
           || !strcasecmp(saved_h, "Content-type"))
    return getenv(uppercased + 5);
  else
    return NULL;
}

static char *get_env(void *data, const char *name) {
  (void)data;
  return getenv(name);
}

static void on_success(uw_context ctx) {
  (void)ctx;
}

static void on_failure(uw_context ctx) {
  uw_write_header(ctx, "Status: 500 Internal Server Error\r\n");
}

static void log_error(void *data, const char *fmt, ...) {
  (void)data;

  va_list ap;
  va_start(ap, fmt);

  vfprintf(stderr, fmt, ap);
}

static void log_debug(void *data, const char *fmt, ...) {
  (void)data;
  (void)fmt;
}

static uw_loggers ls = {NULL, log_error, log_debug};

int main(int argc, char *argv[]) {
  (void)argc;
  (void)argv;

  uw_context ctx = uw_request_new_context(0, &uw_application, &ls);
  uw_request_context rc = uw_new_request_context();
  request_result rr;
  char *method = getenv("REQUEST_METHOD"),
    *path = getenv("SCRIPT_NAME"), *path_info = getenv("PATH_INFO"),
    *query_string = getenv("QUERY_STRING");
  char *body = malloc(1);
  ssize_t body_len = 1, body_pos = 0, res;

  uppercased = malloc(6);

  if (!method) {
    log_error(NULL, "REQUEST_METHOD not set\n");
    exit(1);
  }

  if (!path) {
    log_error(NULL, "SCRIPT_NAME not set\n");
    exit(1);
  }

  if (path_info) {
    char *new_path = malloc(strlen(path) + strlen(path_info) + 1);
    sprintf(new_path, "%s%s", path, path_info);
    path = new_path;
  }

  if (!query_string)
    query_string = "";

  while ((res = read(0, body + body_pos, body_len - body_pos)) > 0) {
    body_pos += res;

    if (body_pos == body_len) {
      body_len *= 2;
      body = realloc(body, body_len);
    }
  }

  if (res < 0) {
    log_error(NULL, "Error reading stdin\n");
    exit(1);
  }

  uw_set_on_success("");
  uw_set_headers(ctx, get_header, NULL);
  uw_set_env(ctx, get_env, NULL);
  uw_request_init(&uw_application, &ls);

  body[body_pos] = 0;
  rr = uw_request(rc, ctx, method, path, query_string, body, body_pos,
                  on_success, on_failure,
                  NULL, log_error, log_debug,
                  -1, NULL, NULL);
  uw_print(ctx, 1);

  if (rr == SERVED)
    return 0;
  else
    return 1;
}

void *uw_init_client_data() {
  return NULL;
}

void uw_free_client_data(void *data) {
  (void)data;
}

void uw_copy_client_data(void *dst, void *src) {
  (void)dst;
  (void)src;
}

void uw_do_expunge(uw_context ctx, uw_Basis_client cli, void *data) {
  (void)data;

  uw_ensure_transaction(ctx);
  uw_get_app(ctx)->expunger(ctx, cli);

  if (uw_commit(ctx))
    uw_error(ctx, UNLIMITED_RETRY, "Rerunning expunge transaction");
}

void uw_post_expunge(uw_context ctx, void *data) {
  (void)ctx;
  (void)data;
}

int uw_supports_direct_status = 0;
