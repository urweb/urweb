#include "config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <unistd.h>
#include <signal.h>
#include <stdarg.h>
#include <ctype.h>

#include <pthread.h>

#include "urweb.h"
#include "request.h"
#include "queue.h"

#include "fastcgi.h"

extern uw_app uw_application;

typedef struct {
  unsigned char version;
  unsigned char type;
  unsigned char requestIdB1;
  unsigned char requestIdB0;
  unsigned char contentLengthB1;
  unsigned char contentLengthB0;
  unsigned char paddingLength;
  unsigned char reserved;
  unsigned char contentData[65535];
} FCGI_Record;

typedef struct {
  FCGI_Record r;
  int sock;
} FCGI_Output;

typedef struct {
  char buf[sizeof(FCGI_Record) + 255];
  int available, used, sock;
} FCGI_Input;

static FCGI_Output *fastcgi_output() {
  FCGI_Output *o = malloc(sizeof(FCGI_Output));

  o->r.version = FCGI_VERSION_1;
  o->r.paddingLength = 0;
  o->r.reserved = 0;

  return o;
}

static FCGI_Input *fastcgi_input() {
  FCGI_Input *i = malloc(sizeof(FCGI_Input));

  i->available = i->used = 0;

  return i;
}

static void fastcgi_input_reset(FCGI_Input *i) {
  i->available = i->used = 0;
}

static int fastcgi_send(FCGI_Output *o,
                        unsigned char type,
                        unsigned short contentLength) {
  o->r.type = type;
  o->r.requestIdB1 = o->r.requestIdB0 = 0;
  o->r.contentLengthB1 = contentLength >> 8;
  o->r.contentLengthB0 = contentLength & 255;
  return uw_really_send(o->sock, &o->r, sizeof(o->r) - 65535 + contentLength);
}

#define LATEST(i) ((FCGI_Record *)(i->buf + i->used))

static FCGI_Record *fastcgi_recv(FCGI_Input *i) {
  while (1) {
    ssize_t n;

    if (i->available >= i->used + sizeof(FCGI_Record) - 65535
        && i->available >= i->used + sizeof(FCGI_Record) - 65535
        + ((LATEST(i)->contentLengthB1 << 8) | LATEST(i)->contentLengthB0)
        + LATEST(i)->paddingLength) {
      FCGI_Record *r = LATEST(i);

      i->used += sizeof(FCGI_Record) - 65535
        + ((LATEST(i)->contentLengthB1 << 8) | LATEST(i)->contentLengthB0)
        + LATEST(i)->paddingLength;
      
      return r;
    }

    if (i->used > 0) {
      memmove(i->buf, i->buf + i->used, i->available - i->used);
      i->available -= i->used;
      i->used = 0;
    }

    n = recv(i->sock, i->buf + i->available, sizeof(i->buf) - i->available, 0);

    if (n <= 0)
      return NULL;

    i->available += n;
  }
}

static void on_success(uw_context ctx) { }

static void on_failure(uw_context ctx) {
  uw_write_header(ctx, "Status: 500 Internal Server Error\r\n");
}

static int write_stdout(void *data, const char *buf, size_t len) {
  FCGI_Output *o = (FCGI_Output *)data;
  while (len > 0) {
    size_t len2 = len;
    if (len2 > 65535)
      len2 = 65535;
    memcpy(o->r.contentData, buf, len2);
    if (fastcgi_send(o, FCGI_STDOUT, len2)) {
      fprintf(stderr, "fastcgi_send() failed in write_stdout().\n");
      return -1;
    }
    buf += len2;
    len -= len2;
  }

  return 0;
}

#include <errno.h>

static void write_stderr(FCGI_Output *o, const char *fmt, ...) {
  int len;
  va_list ap;
  va_start(ap, fmt);

  len = vsnprintf((char *)o->r.contentData, 65535, fmt, ap);
  if (len < 0)
    fprintf(stderr, "vsnprintf() failed in write_stderr().\n");
  else if (fastcgi_send(o, FCGI_STDERR, len))
    fprintf(stderr, "fastcgi_send() failed in write_stderr().\n");
}

static void close_stream(FCGI_Output *o, unsigned char type) {
  if (fastcgi_send(o, type, 0))
    fprintf(stderr, "fastcgi_send() failed in close_stream().\n");
}

static void log_error(void *data, const char *fmt, ...) {
  FCGI_Output *o = (FCGI_Output *)data;
  va_list ap;
  va_start(ap, fmt);

  if (o) {
    int len = vsnprintf((char *)o->r.contentData, 65535, fmt, ap);
    if (len < 0)
      fprintf(stderr, "vsnprintf() failed in log_error().\n");
    else if (fastcgi_send(o, FCGI_STDERR, len))
      fprintf(stderr, "fastcgi_send() failed in log_error().\n");
  } else
    vfprintf(stderr, fmt, ap);
}

static void log_debug(void *data, const char *fmt, ...) {
  FCGI_Output *o = (FCGI_Output *)data;
  va_list ap;
  va_start(ap, fmt);

  if (o) {
    strcpy((char *)o->r.contentData, "DEBUG: ");
    int len = vsnprintf((char *)o->r.contentData + 7, 65535 - 7, fmt, ap);
    if (len < 0)
      fprintf(stderr, "vsnprintf() failed in log_debug().\n");
    else if (fastcgi_send(o, FCGI_STDERR, len + 7)) {
      len += 7;
      if (len >= 65535) len = 65534;
      o->r.contentData[len] = 0;
      fputs((char *)o->r.contentData, stderr);
      fflush(stderr);
    }
  } else
    vfprintf(stderr, fmt, ap);
}

typedef struct {
  char *name, *value;
  unsigned name_len, value_len;
} nvp;

static char *search_nvps(nvp *nvps, const char *h) {
  for (; nvps->name[0]; ++nvps)
    if (!strcmp(h, nvps->name))
      return nvps->value;

  return NULL;
}

typedef struct {
  nvp *nvps;
  char *uppercased;
  int n_nvps, uppercased_len;
} headers;

static char *get_header(void *data, const char *h) {
  headers *hs = (headers *)data;
  size_t len = strlen(h);
  char *s;
  const char *saved_h = h;

  if (len > hs->uppercased_len) {
    hs->uppercased_len = len;
    hs->uppercased = realloc(hs->uppercased, len + 6);
  }

  strcpy(hs->uppercased, "HTTP_");
  for (s = hs->uppercased+5; *h; ++h)
    *s++ = *h == '-' ? '_' : toupper((int)*h);
  *s = 0;

  if (!strcasecmp(saved_h, "Content-length")
      || !strcasecmp(saved_h, "Content-type")) {
    if ((s = search_nvps(hs->nvps, hs->uppercased + 5)))
      return s;
  }
  
  return search_nvps(hs->nvps, hs->uppercased);
}

static int read_funny_len(unsigned char **buf, int *len) {
  if (*len <= 0)
    return -1;

  if ((*buf)[0] >> 7 == 0) {
    int r = (*buf)[0];
    ++*buf;
    --*len;
    return r;
  }
  else if (*len < 4)
    return -1;
  else {
    int r = (((*buf)[0] & 0x7f) << 24) + ((*buf)[1] << 16) + ((*buf)[2] << 8) + (*buf)[3];
    *buf += 4;
    *len -= 4;
    return r;
  }
}

static int read_nvp(unsigned char **buf, int len, nvp *nv) {
  int nameLength, valueLength;

  if ((nameLength = read_funny_len(buf, &len)) < 0)
    return -1;
  if ((valueLength = read_funny_len(buf, &len)) < 0)
    return -2;
  if (len < nameLength + valueLength)
    return -3;

  if (nameLength+1 > nv->name_len) {
    nv->name_len = nameLength+1;
    nv->name = realloc(nv->name, nv->name_len);
  }
  if (valueLength+1 > nv->value_len) {
    nv->value_len = valueLength+1;
    nv->value = realloc(nv->value, nv->value_len);
  }

  memcpy(nv->name, *buf, nameLength);
  nv->name[nameLength] = 0;

  memcpy(nv->value, *buf + nameLength, valueLength);
  nv->value[valueLength] = 0;

  *buf += nameLength + valueLength;

  return 0;
}

static int fastcgi_close_with(FCGI_Output *out, request_result rr) {
  FCGI_EndRequestBody *erb = (FCGI_EndRequestBody *)out->r.contentData;

  close_stream(out, FCGI_STDOUT);
  close_stream(out, FCGI_STDERR);

  if (rr == SERVED)
    erb->appStatusB3 = erb->appStatusB2 = erb->appStatusB1 = erb->appStatusB0 = 0;
  else
    erb->appStatusB3 = erb->appStatusB2 = erb->appStatusB1 = erb->appStatusB0 = 0xFF;

  erb->protocolStatus = FCGI_REQUEST_COMPLETE;
  fastcgi_send(out, FCGI_END_REQUEST, sizeof(FCGI_EndRequestBody));
  return close(out->sock);
}

static int fastcgi_close(int sock) {
  FCGI_Output out;
  out.sock = sock;
  out.r.version = FCGI_VERSION_1;
  out.r.paddingLength = 0;
  out.r.reserved = 0;

  return fastcgi_close_with(&out, SERVED);
}

int fastcgi_send_normal(int sock, const void *buf, ssize_t len) {
  FCGI_Output out;
  out.sock = sock;
  out.r.version = FCGI_VERSION_1;
  out.r.paddingLength = 0;
  out.r.reserved = 0;

  return write_stdout(&out, buf, len);
}

static void *worker(void *data) {
  FCGI_Input *in = fastcgi_input();
  FCGI_Output *out = fastcgi_output();
  uw_context ctx = uw_request_new_context(*(int *)data, &uw_application, out, log_error, log_debug);
  uw_request_context rc = uw_new_request_context();
  headers hs;
  size_t body_size = 0;
  char *body = malloc(0);
  size_t path_size = 0;
  char *path_buf = malloc(0);

  hs.uppercased = malloc(0);
  hs.uppercased_len = 0;
  hs.nvps = malloc(sizeof(nvp));
  hs.n_nvps = 1;
  hs.nvps[0].name = malloc(1);
  hs.nvps[0].name_len = 1;
  hs.nvps[0].value = malloc(0);
  hs.nvps[0].value_len = 0;

  while (1) {
    FCGI_Record *r;
    size_t used_nvps = 0;
    int body_len, body_read;
    char *s;
    char *method, *path, *path_info, *query_string;

    in->sock = out->sock = uw_dequeue();

    if (!(r = fastcgi_recv(in))) {
      fprintf(stderr, "Error receiving initial message\n");
      goto done;
    }

    if (r->type != FCGI_BEGIN_REQUEST) {
      write_stderr(out, "First message is not BEGIN_REQUEST\n");
      goto done;
    } else if (r->contentData[1] != FCGI_RESPONDER) {
      write_stderr(out, "Request is for a role besides RESPONDER\n");
      goto done;
    }

    while (1) {
      unsigned char *buf;
      int len;

      if (!(r = fastcgi_recv(in))) {
        write_stderr(out, "Error receiving environment variables\n");
        goto done;
      }

      if (r->type != FCGI_PARAMS) {
        write_stderr(out, "Expected FCGI_PARAMS but got %d\n", r->type);
        goto done;
      }

      if (r->contentLengthB1 == 0 && r->contentLengthB0 == 0)
        break;

      len = (r->contentLengthB1 << 8) | r->contentLengthB0;

      for (buf = r->contentData; buf < r->contentData + len; ) {
        if (used_nvps == hs.n_nvps-1) {
          ++hs.n_nvps;
          hs.nvps = realloc(hs.nvps, hs.n_nvps * sizeof(nvp));
          hs.nvps[hs.n_nvps-1].name = malloc(1);
          hs.nvps[hs.n_nvps-1].value = malloc(0);
          hs.nvps[hs.n_nvps-1].name_len = 1;
          hs.nvps[hs.n_nvps-1].value_len = 0;
        }
        
        if (read_nvp(&buf, len - (buf - r->contentData), &hs.nvps[used_nvps]) < 0) {
          write_stderr(out, "Error reading FCGI_PARAMS name-value pair\n");
          goto done;
        }

        //write_stderr(out, "PARAM: %s -> %s\n", hs.nvps[used_nvps].name, hs.nvps[used_nvps].value);

        ++used_nvps;
      }
    }

    hs.nvps[used_nvps].name[0] = 0;

    if ((s = get_header(&hs, "Content-Length"))) {
      body_len = atoi(s);
      if (body_len < 0) {
        write_stderr(out, "Invalid Content-Length\n");
        goto done;
      }
    } else
      body_len = 0;

    if (body_len+1 > body_size) {
      body_size = body_len+1;
      body = realloc(body, body_size);
    }

    for (body_read = 0; body_read < body_len; ) {
      int this_len;

      if (!(r = fastcgi_recv(in))) {
        write_stderr(out, "Error receiving STDIN\n");
        goto done;
      }

      if (r->type != FCGI_STDIN) {
        write_stderr(out, "Expected FCGI_STDIN but got %d\n", r->type);
        goto done;
      }

      if (r->contentLengthB1 == 0 && r->contentLengthB0 == 0) {
        write_stderr(out, "End of STDIN\n");
        break;
      }

      this_len = (r->contentLengthB1 << 8) | r->contentLengthB0;

      if (body_read + this_len > body_len) {
        write_stderr(out, "Too much STDIN\n");
        goto done;
      }

      memcpy(&body[body_read], r->contentData, this_len);
      body_read += this_len;
    }

    body[body_read] = 0;

    if (!(method = search_nvps(hs.nvps, "REQUEST_METHOD"))) {
      write_stderr(out, "REQUEST_METHOD not set\n");
      goto done;
    }

    if (!(path = search_nvps(hs.nvps, "SCRIPT_NAME"))) {
      write_stderr(out, "SCRIPT_NAME not set\n");
      goto done;
    }

    if ((path_info = search_nvps(hs.nvps, "PATH_INFO"))) {
      int len1 = strlen(path), len2 = strlen(path_info);
      int len = len1 + len2 + 1;

      if (len > path_size) {
        path_size = len;
        path_buf = realloc(path_buf, path_size);
      }

      sprintf(path_buf, "%s%s", path, path_info);
      path = path_buf;
    }

    if (!(query_string = search_nvps(hs.nvps, "QUERY_STRING")))
      query_string = "";

    uw_set_headers(ctx, get_header, &hs);

    {
      request_result rr;

      rr = uw_request(rc, ctx, method, path, query_string, body, body_read,
                      on_success, on_failure,
                      out, log_error, log_debug,
                      in->sock, fastcgi_send_normal, fastcgi_close);

      if (rr == KEEP_OPEN)
        goto done2;

      uw_output(ctx, write_stdout, out);
      fastcgi_close_with(out, rr);
      goto done2;
    }

  done:
    close(in->sock);
  done2:
    fastcgi_input_reset(in);
    uw_reset(ctx);
  }

  return NULL;
}

static void help(char *cmd) {
  printf("Usage: %s [-t <thread-count>]\n", cmd);
}

static void sigint(int signum) {
  printf("Exiting....\n");
  exit(0);
}

static loggers ls = {&uw_application, NULL, log_error, log_debug};

int main(int argc, char *argv[]) {
  // The skeleton for this function comes from Beej's sockets tutorial.
  struct sockaddr_in their_addr; // connector's address information
  socklen_t sin_size;
  int nthreads = 1, i, *names, opt;
  char *fwsa = getenv("FCGI_WEB_SERVER_ADDRS"), *nthreads_s = getenv("URWEB_NUM_THREADS");
 
  if (nthreads_s) {
    nthreads = atoi(nthreads_s);
    if (nthreads <= 0) {
      fprintf(stderr, "Bad URWEB_NUM_THREADS value\n");
      return 1;
    }
  }

  signal(SIGINT, sigint);
  signal(SIGPIPE, SIG_IGN);
  signal(SIGUSR1, sigint);
  signal(SIGTERM, sigint);

  while ((opt = getopt(argc, argv, "ht:")) != -1) {
    switch (opt) {
    case '?':
      fprintf(stderr, "Unknown command-line option");
      help(argv[0]);
      return 1;

    case 'h':
      help(argv[0]);
      return 0;

    case 't':
      nthreads = atoi(optarg);
      if (nthreads <= 0) {
        fprintf(stderr, "Invalid thread count\n");
        help(argv[0]);
        return 1;
      }
      break;

    default:
      fprintf(stderr, "Unexpected getopt() behavior\n");
      return 1;
    }
  }

  uw_set_on_success("");
  uw_request_init(&uw_application, NULL, log_error, log_debug);

  names = calloc(nthreads, sizeof(int));

  sin_size = sizeof their_addr;

  {
    pthread_t thread;

    if (pthread_create(&thread, NULL, client_pruner, &ls)) {
      fprintf(stderr, "Error creating pruner thread\n");
      return 1;
    }
  }

  for (i = 0; i < nthreads; ++i) {
    pthread_t thread;    
    names[i] = i;
    if (pthread_create(&thread, NULL, worker, &names[i])) {
      fprintf(stderr, "Error creating worker thread #%d\n", i);
      return 1;
    }
  }

  while (1) {
    int new_fd = accept(FCGI_LISTENSOCK_FILENO, (struct sockaddr *)&their_addr, &sin_size);

    if (new_fd < 0) {
      fprintf(stderr, "Socket accept failed\n");
      return 1;
    }

    if (fwsa) {
      char host[100], matched = 0;
      char *ips, *sep;

      if (getnameinfo((struct sockaddr *)&their_addr, sin_size, host, sizeof host, NULL, 0, NI_NUMERICHOST)) {
        fprintf(stderr, "Remote IP determination failed\n");
        return 1;
      }

      for (ips = fwsa; (sep = strchr(ips, ',')); ips = sep+1) {
        if (!strncmp(ips, host, sep - ips)) {
          matched = 1;
          break;
        }
      }

      if (!matched && strcmp(ips, host)) {
        fprintf(stderr, "Remote address is not in FCGI_WEB_SERVER_ADDRS");
        return 1;
      }
    }

    uw_enqueue(new_fd);
  }
}

void *uw_init_client_data() {
  return NULL;
}

void uw_free_client_data(void *data) {
}

void uw_copy_client_data(void *dst, void *src) {
}

void uw_do_expunge(uw_context ctx, uw_Basis_client cli, void *data) {
  if (uw_get_app(ctx)->db_begin(ctx))
    uw_error(ctx, FATAL, "Error running SQL BEGIN");
  uw_get_app(ctx)->expunger(ctx, cli);
  if (uw_get_app(ctx)->db_commit(ctx))
    uw_error(ctx, FATAL, "Error running SQL COMMIT");
}

void uw_post_expunge(uw_context ctx, void *data) {
}

int uw_supports_direct_status = 0;
