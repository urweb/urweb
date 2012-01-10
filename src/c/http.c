#include "config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <signal.h>
#include <stdarg.h>

#include <pthread.h>

#include "urweb.h"
#include "request.h"
#include "queue.h"

extern uw_app uw_application;

int uw_backlog = 10;

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
  va_list ap;
  va_start(ap, fmt);

  vprintf(fmt, ap);
}

static void *worker(void *data) {
  int me = *(int *)data;
  uw_context ctx = uw_request_new_context(me, &uw_application, NULL, log_error, log_debug);
  size_t buf_size = 2;
  char *buf = malloc(buf_size);
  uw_request_context rc = uw_new_request_context();

  while (1) {
    char *back = buf;
    int sock = uw_dequeue();

    printf("Handling connection with thread #%d.\n", me);

    while (1) {
      int r;
      char *method, *path, *query_string, *headers, *body, *s, *s2;

      if (back - buf == buf_size - 1) {
        char *new_buf;
        buf_size *= 2;
        new_buf = realloc(buf, buf_size);
        back = new_buf + (back - buf);
        buf = new_buf;
      }

      r = recv(sock, back, buf_size - 1 - (back - buf), 0);

      if (r < 0) {
        fprintf(stderr, "Recv failed\n");
        break;
      }

      if (r == 0) {
        printf("Connection closed.\n");
        break;
      }

      back += r;
      *back = 0;

      if ((body = strstr(buf, "\r\n\r\n"))) {
        request_result rr;

        body[0] = body[1] = 0;
        body += 4;

        if ((s = strcasestr(buf, "\r\nContent-Length: ")) && s < body) {
          int clen;

          if (sscanf(s + 18, "%d\r\n", &clen) != 1) {
            fprintf(stderr, "Malformed Content-Length header\n");
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
              fprintf(stderr, "Recv failed\n");
              close(sock);
              goto done;
            }

            if (r == 0) {
              fprintf(stderr, "Connection closed.\n");
              close(sock);
              goto done;
            }

            back += r;
            *back = 0;      
          }
        }

        body[-4] = '\r';
        body[-3] = '\n';

        if (!(s = strstr(buf, "\r\n"))) {
          fprintf(stderr, "No newline in request\n");
          close(sock);
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
          s = s2;

          if (s[1] == 0)
            break;

          *s = 0;
          s += 2;
        }

        uw_set_headers(ctx, get_header, headers);

        printf("Serving URI %s....\n", path);
        rr = uw_request(rc, ctx, method, path, query_string, body, back - body,
                        on_success, on_failure,
                        NULL, log_error, log_debug,
                        sock, uw_really_send, close);
        if (rr != KEEP_OPEN) uw_send(ctx, sock);

        if (rr == SERVED || rr == FAILED)
          close(sock);
        else if (rr != KEEP_OPEN)
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
  printf("Usage: %s [-p <port>] [-t <thread-count>]\n", cmd);
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

  while ((opt = getopt(argc, argv, "hp:t:")) != -1) {
    switch (opt) {
    case '?':
      fprintf(stderr, "Unknown command-line option");
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
  my_addr.sin_addr.s_addr = INADDR_ANY; // auto-fill with my IP
  memset(my_addr.sin_zero, '\0', sizeof my_addr.sin_zero);

  if (bind(sockfd, (struct sockaddr *)&my_addr, sizeof my_addr) < 0) {
    fprintf(stderr, "Listener socket bind failed\n");
    return 1;
  }

  if (listen(sockfd, uw_backlog) < 0) {
    fprintf(stderr, "Socket listen failed\n");
    return 1;
  }

  sin_size = sizeof their_addr;

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

    printf("Accepted connection.\n");

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
  uw_commit(ctx);
}

void uw_post_expunge(uw_context ctx, void *data) {
}

int uw_supports_direct_status = 1;
