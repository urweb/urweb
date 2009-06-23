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
#include "request.h"

int uw_backlog = 10;

typedef struct node {
  int fd;
  struct node *next;
} *node;

static node front = NULL, back = NULL;

static int empty() {
  return front == NULL;
}

static void enqueue(int fd) {
  node n = malloc(sizeof(struct node));

  n->fd = fd;
  n->next = NULL;
  if (back)
    back->next = n;
  else
    front = n;
  back = n;
}

static int dequeue() {
  int ret = front->fd;

  front = front->next;
  if (!front)
    back = NULL;

  return ret;
}

static pthread_mutex_t queue_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t queue_cond = PTHREAD_COND_INITIALIZER;

static void *worker(void *data) {
  int me = *(int *)data;
  uw_context ctx = uw_request_new_context();
  size_t buf_size = 2;
  char *buf = malloc(buf_size);
  uw_request_context rc = uw_new_request_context();

  while (1) {
    char *back = buf;
    int sock;

    pthread_mutex_lock(&queue_mutex);
    while (empty())
      pthread_cond_wait(&queue_cond, &queue_mutex);
    sock = dequeue();
    pthread_mutex_unlock(&queue_mutex);

    printf("Handling connection with thread #%d.\n", me);

    while (1) {
      int r;
      char *s1, *s2;

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

      if ((s1 = strstr(buf, "\r\n\r\n"))) {
        request_result rr;

        if ((s2 = strcasestr(buf, "\r\nContent-Length: ")) && s2 < s1) {
          int clen;

          if (sscanf(s2 + 18, "%d\r\n", &clen) != 1) {
            fprintf(stderr, "Malformed Content-Length header\n");
            break;
          }

          if (s1 + 4 + clen > back)
            continue;
        }

        rr = uw_request(rc, ctx, buf, back - buf, sock);
        uw_send(ctx, sock);

        if (rr == SERVED || rr == FAILED)
          close(sock);
        else if (rr != KEEP_OPEN)
          fprintf(stderr, "Illegal uw_request return code: %d\n", rr);

        break;
      }
    }

    uw_reset(ctx);
  }
}

static void help(char *cmd) {
  printf("Usage: %s [-p <port>] [-t <thread-count>]\n", cmd);
}

static void sigint(int signum) {
  printf("Exiting....\n");
  exit(0);
}

int main(int argc, char *argv[]) {
  // The skeleton for this function comes from Beej's sockets tutorial.
  int sockfd;  // listen on sock_fd
  struct sockaddr_in my_addr;
  struct sockaddr_in their_addr; // connector's address information
  int sin_size, yes = 1;
  int uw_port = 8080, nthreads = 1, i, *names, opt;
 
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

  uw_request_init();

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
    int name;

    if (pthread_create(&thread, NULL, client_pruner, &name)) {
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
    int new_fd = accept(sockfd, (struct sockaddr *)&their_addr, &sin_size);

    if (new_fd < 0) {
      fprintf(stderr, "Socket accept failed\n");
      return 1;
    }

    printf("Accepted connection.\n");

    pthread_mutex_lock(&queue_mutex);
    enqueue(new_fd);
    pthread_cond_broadcast(&queue_cond);
    pthread_mutex_unlock(&queue_mutex);
  }
}
