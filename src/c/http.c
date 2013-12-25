#include "config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <signal.h>
#include <stdarg.h>

#include <pthread.h>

#include "urweb.h"
#include "request.h"
#include "queue.h"

extern uw_app uw_application;

int uw_backlog = SOMAXCONN;
static int keepalive = 0, quiet = 0;

static char *get_header(void *data, const char *h) {
  char *s = data;
  int len = strlen(h);
  char *p;

  while ((p = strchr(s, ':'))) {
    if (p - s == len && !strncasecmp(s, h, len)) {
      return p + 2;
    } else {
      if ((s = strchr(p, 0)) && s[1] != 0)
        s += 2;
      else
        return NULL;
    }
  }
  
  return NULL;
}

static char *get_env(void *data, const char *name) {
  return getenv(name);
}

static void on_success(uw_context ctx) {
  uw_write_header(ctx, "HTTP/1.1 200 OK\r\n");
}

static void on_failure(uw_context ctx) {
  uw_write_header(ctx, "HTTP/1.1 500 Internal Server Error\r\n");
}

static void log_error(void *data, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);

  vfprintf(stderr, fmt, ap);
}

static void log_debug(void *data, const char *fmt, ...) {
  if (!quiet) {
    va_list ap;
    va_start(ap, fmt);

    vprintf(fmt, ap);
  }
}

static void *worker(void *data) {
  int me = *(int *)data;
  uw_context ctx = uw_request_new_context(me, &uw_application, NULL, log_error, log_debug);
  size_t buf_size = 1024;
  char *buf = malloc(buf_size), *back = buf;
  uw_request_context rc = uw_new_request_context();
  int sock = 0;

  while (1) {
    if (sock == 0) {
      back = buf;
      sock = uw_dequeue();
    }

    if (!quiet)
      printf("Handling connection with thread #%d.\n", me);

    while (1) {
      int r;
      char *method, *path, *query_string, *headers, *body, *after, *s, *s2;

      if (back - buf == buf_size - 1) {
        char *new_buf;
        buf_size *= 2;
        new_buf = realloc(buf, buf_size);
        back = new_buf + (back - buf);
        buf = new_buf;
      }

      *back = 0;
      body = strstr(buf, "\r\n\r\n");
      if (body == NULL) {
        r = recv(sock, back, buf_size - 1 - (back - buf), 0);

        if (r < 0) {
          if (!quiet)
            fprintf(stderr, "Recv failed\n");
          close(sock);
          sock = 0;
          break;
        }

        if (r == 0) {
          if (!quiet)
            printf("Connection closed.\n");
          close(sock);
          sock = 0;
          break;
        }

        back += r;
        *back = 0;
      }

      if (body != NULL || (body = strstr(buf, "\r\n\r\n"))) {
        request_result rr;
        int should_keepalive = 0;

        body[0] = body[1] = 0;
        body += 4;

        if ((s = strcasestr(buf, "\r\nContent-Length: ")) && s < body) {
          int clen;

          if (sscanf(s + 18, "%d\r\n", &clen) != 1) {
            fprintf(stderr, "Malformed Content-Length header\n");
            close(sock);
            sock = 0;
            break;
          }

          while (back - body < clen) {
            if (back - buf == buf_size - 1) {
              char *new_buf;
              buf_size *= 2;
              new_buf = realloc(buf, buf_size);

              back = new_buf + (back - buf);
              body = new_buf + (body - buf);
              s = new_buf + (s - buf);

              buf = new_buf;
            }

            r = recv(sock, back, buf_size - 1 - (back - buf), 0);

            if (r < 0) {
              if (!quiet)
                fprintf(stderr, "Recv failed\n");
              close(sock);
              sock = 0;
              goto done;
            }

            if (r == 0) {
              if (!quiet)
                fprintf(stderr, "Connection closed.\n");
              close(sock);
              sock = 0;
              goto done;
            }

            back += r;
            *back = 0;      
          }

          after = body + clen;
        } else
          after = body;

        body[-4] = '\r';
        body[-3] = '\n';

        if (!(s = strstr(buf, "\r\n"))) {
          fprintf(stderr, "No newline in request\n");
          close(sock);
          sock = 0;
          goto done;
        }

        body[-4] = body[-3] = 0;

        *s = 0;
        headers = s + 2;
        method = s = buf;

        strsep(&s, " ");
        if (!s) {
          fprintf(stderr, "No first space in HTTP command\n");
          close(sock);
          sock = 0;
          goto done;
        }
        path = s;

        if ((s = strchr(path, ' ')))
          *s = 0;

        if ((s = strchr(path, '?'))) {
          *s = 0;
          query_string = s+1;
        }
        else
          query_string = NULL;

        s = headers;
        while ((s2 = strchr(s, '\r'))) {
          if (s2 == s) {
            *s = 0;
            break;
          }

          s = s2;

          if (s[1] == 0)
            break;

          *s = 0;
          s += 2;
        }

        uw_set_headers(ctx, get_header, headers);
        uw_set_env(ctx, get_env, NULL);

        if (!quiet)
          printf("Serving URI %s....\n", path);
        rr = uw_request(rc, ctx, method, path, query_string, body, back - body,
                        on_success, on_failure,
                        NULL, log_error, log_debug,
                        sock, uw_really_send, close);

        if (rr != KEEP_OPEN) {
          if (keepalive) {
            char *connection = uw_Basis_requestHeader(ctx, "Connection");

            should_keepalive = !(connection && !strcmp(connection, "close"));
          }

          if (!should_keepalive)
            uw_write_header(ctx, "Connection: close\r\n");

          if (!uw_has_contentLength(ctx)) {
            char clen[100];

            sprintf(clen, "Content-length: %d\r\n", uw_pagelen(ctx));
            uw_write_header(ctx, clen);
          }

          uw_send(ctx, sock);
        }

        if (rr == SERVED || rr == FAILED) {
          if (should_keepalive) {
            // In case any other requests are queued up, shift
            // unprocessed part of buffer to front.
            int kept = back - after;
            memmove(buf, after, kept);
            back = buf + kept;
          } else {
            close(sock);
            sock = 0;
          }
        } else if (rr == KEEP_OPEN)
          sock = 0;
        else
          fprintf(stderr, "Illegal uw_request return code: %d\n", rr);

        break;
      }
    }

  done:
    uw_reset(ctx);
  }

  return NULL;
}

static void help(char *cmd) {
  printf("Usage: %s [-p <port>] [-a <IP address>] [-t <thread count>] [-k] [-q]\nThe '-k' option turns on HTTP keepalive.\nThe '-q' option turns off some chatter on stdout.\n", cmd);
}

static void sigint(int signum) {
  printf("Exiting....\n");
  exit(0);
}

static loggers ls = {&uw_application, NULL, log_error, log_debug};

int main(int argc, char *argv[]) {
  // The skeleton for this function comes from Beej's sockets tutorial.
  int sockfd;  // listen on sock_fd
  struct sockaddr_in my_addr;
  struct sockaddr_in their_addr; // connector's address information
  socklen_t sin_size;
  int yes = 1, uw_port = 8080, nthreads = 1, i, *names, opt;
 
  signal(SIGINT, sigint);
  signal(SIGPIPE, SIG_IGN); 

  my_addr.sin_addr.s_addr = INADDR_ANY; // auto-fill with my IP
  memset(my_addr.sin_zero, '\0', sizeof my_addr.sin_zero);

  while ((opt = getopt(argc, argv, "hp:a:t:kq")) != -1) {
    switch (opt) {
    case '?':
      fprintf(stderr, "Unknown command-line option\n");
      help(argv[0]);
      return 1;

    case 'h':
      help(argv[0]);
      return 0;

    case 'p':
      uw_port = atoi(optarg);
      if (uw_port <= 0) {
        fprintf(stderr, "Invalid port number\n");
        help(argv[0]);
        return 1;
      }
      break;

    case 'a':
      if (!inet_pton(AF_INET, optarg, &my_addr.sin_addr)) {
        fprintf(stderr, "Invalid IP address\n");
        help(argv[0]);
        return 1;
      }
      break;

    case 't':
      nthreads = atoi(optarg);
      if (nthreads <= 0) {
        fprintf(stderr, "Invalid thread count\n");
        help(argv[0]);
        return 1;
      }
      break;

    case 'k':
      keepalive = 1;
      break;

    case 'q':
      quiet = 1;
      break;

    default:
      fprintf(stderr, "Unexpected getopt() behavior\n");
      return 1;
    }
  }

  uw_request_init(&uw_application, NULL, log_error, log_debug);

  names = calloc(nthreads, sizeof(int));

  sockfd = socket(PF_INET, SOCK_STREAM, 0); // do some error checking!

  if (sockfd < 0) {
    fprintf(stderr, "Listener socket creation failed\n");
    return 1;
  }

  if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) < 0) {
    fprintf(stderr, "Listener socket option setting failed\n");
    return 1;
  }

  my_addr.sin_family = AF_INET;         // host byte order
  my_addr.sin_port = htons(uw_port);    // short, network byte order

  if (bind(sockfd, (struct sockaddr *)&my_addr, sizeof my_addr) < 0) {
    fprintf(stderr, "Listener socket bind failed\n");
    return 1;
  }

  if (listen(sockfd, uw_backlog) < 0) {
    fprintf(stderr, "Socket listen failed\n");
    return 1;
  }

  sin_size = sizeof their_addr;

  if (!quiet)
    printf("Listening on port %d....\n", uw_port);

  {
    pthread_t thread;

    if (pthread_create_big(&thread, NULL, client_pruner, &ls)) {
      fprintf(stderr, "Error creating pruner thread\n");
      return 1;
    }
  }

  for (i = 0; i < nthreads; ++i) {
    pthread_t thread;    
    names[i] = i;
    if (pthread_create_big(&thread, NULL, worker, &names[i])) {
      fprintf(stderr, "Error creating worker thread #%d\n", i);
      return 1;
    }
  }

  while (1) {
    int new_fd = accept(sockfd, (struct sockaddr *)&their_addr, &sin_size);

    if (new_fd < 0) {
      fprintf(stderr, "Socket accept failed\n");
      return 1;
    }

    if (!quiet)
      printf("Accepted connection.\n");

    if (keepalive) {
      int flag = 1; 
      setsockopt(new_fd, IPPROTO_TCP, TCP_NODELAY, (char *) &flag, sizeof(int));
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
  uw_ensure_transaction(ctx);
  uw_get_app(ctx)->expunger(ctx, cli);

  if (uw_commit(ctx))
    uw_error(ctx, UNLIMITED_RETRY, "Rerunning expunge transaction");
}

void uw_post_expunge(uw_context ctx, void *data) {
}

int uw_supports_direct_status = 1;
