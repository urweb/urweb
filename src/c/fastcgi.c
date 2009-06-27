#define _GNU_SOURCE

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

#include <pthread.h>

#include "urweb.h"
#include "request.h"
#include "queue.h"

#include "fastcgi.h"

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
                        unsigned short requestId,
                        unsigned short contentLength) {
  o->r.type = type;
  o->r.requestIdB1 = requestId >> 8;
  o->r.requestIdB0 = requestId & 255;
  o->r.contentLengthB1 = contentLength >> 8;
  o->r.contentLengthB0 = contentLength & 255;
  return uw_really_send(o->sock, &o->r, sizeof(o->r) - (65535 - contentLength));
}

#define LATEST(i) ((FCGI_Record *)(i->buf + i->used))

static FCGI_Record *fastcgi_recv(FCGI_Input *i) {
  while (1) {
    ssize_t n;

    if (i->available >= i->used + sizeof(FCGI_Record) - 65535
        && i->available >= i->used + sizeof(FCGI_Record) - 65535
        + ((LATEST(i)->contentLengthB1 << 8) & LATEST(i)->contentLengthB0)
        + LATEST(i)->paddingLength) {
      FCGI_Record *r = LATEST(i);

      i->used += sizeof(FCGI_Record) - 65535
        + ((LATEST(i)->contentLengthB1 << 8) & LATEST(i)->contentLengthB0)
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

static char *get_header(void *data, const char *h) {
  return NULL;
}

static void on_success(uw_context ctx) { }

static void on_failure(uw_context ctx) {
  uw_write_header(ctx, "Status: 500 Internal Server Error\r\n");
}

static void write_stderr(FCGI_Output *o, const char *fmt, ...) {
  int len;
  va_list ap;
  va_start(ap, fmt);

  len = vsnprintf(o->r.contentData, 65535, fmt, ap);
  if (len < 0)
    fprintf(stderr, "vsnprintf() failed in log_error().\n");
  else if (fastcgi_send(o, FCGI_STDERR, FCGI_NULL_REQUEST_ID, len))
    fprintf(stderr, "fastcgi_send() failed in log_error().\n");
}

static void log_error(void *data, const char *fmt, ...) {
  FCGI_Output *o = (FCGI_Output *)data;
  va_list ap;
  va_start(ap, fmt);

  if (o) {
    int len = vsnprintf(o->r.contentData, 65535, fmt, ap);
    if (len < 0)
      fprintf(stderr, "vsnprintf() failed in log_error().\n");
    else if (fastcgi_send(o, FCGI_STDERR, FCGI_NULL_REQUEST_ID, len))
      fprintf(stderr, "fastcgi_send() failed in log_error().\n");
  } else
    vfprintf(stderr, fmt, ap);
}

static void log_debug(void *data, const char *fmt, ...) {
}

static void *worker(void *data) {
  int me = *(int *)data;
  FCGI_Input *in = fastcgi_input();
  FCGI_Output *out = fastcgi_output();
  uw_context ctx = uw_request_new_context(out, log_error, log_debug);
  uw_request_context rc = uw_new_request_context();

  while (1) {
    FCGI_Record *r;

    in->sock = out->sock = uw_dequeue();

    if (!(r = fastcgi_recv(in))) {
      fprintf(stderr, "Error receiving initial message\n");
      return NULL;
    }
    
    if (r->type != FCGI_BEGIN_REQUEST) {
      write_stderr(out, "First message is not BEGIN_REQUEST\n");
      goto done;
    } else if (((FCGI_BeginRequestBody *)&r->contentData)->roleB0 != FCGI_RESPONDER) {
      write_stderr(out, "First message is not BEGIN_REQUEST\n");
      goto done;
    }

    if (!(r = fastcgi_recv(in))) {
      fprintf(stderr, "Error receiving second message\n");
      return NULL;
    }
    write_stderr(out, "Next message code: %d\n", r->type);

  done:
    close(in->sock);
    fastcgi_input_reset(in);
    uw_reset(ctx);
  }
}

static void help(char *cmd) {
  printf("Usage: %s [-t <thread-count>]\n", cmd);
}

static void sigint(int signum) {
  printf("Exiting....\n");
  exit(0);
}

static loggers ls = {NULL, log_error, log_debug};

int main(int argc, char *argv[]) {
  // The skeleton for this function comes from Beej's sockets tutorial.
  struct sockaddr_in their_addr; // connector's address information
  int sin_size, yes = 1;
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

  uw_request_init(NULL, log_error, log_debug);

  names = calloc(nthreads, sizeof(int));

  sin_size = sizeof their_addr;

  {
    pthread_t thread;
    int name;

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

      for (ips = fwsa; sep = strchr(ips, ','); ips = sep+1) {
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
