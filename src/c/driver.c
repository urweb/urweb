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

int uw_backlog = 10;
int uw_bufsize = 1024;

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

static uw_context new_context() {
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

void uw_sign(const char *in, char *out) {
  MHASH td;

  td = mhash_hmac_init(HASH_ALGORITHM, private_key, sizeof(private_key),
                       mhash_get_hash_pblock(HASH_ALGORITHM));
  
  mhash(td, in, strlen(in));
  if (mhash_hmac_deinit(td, out) < 0)
    printf("Signing failed");
}

static void *worker(void *data) {
  int me = *(int *)data, retries_left = MAX_RETRIES;
  uw_context ctx = new_context();
  size_t buf_size = 2;
  char *buf = malloc(buf_size);
  size_t path_copy_size = 0;
  char *path_copy = malloc(path_copy_size);

  while (1) {
    char *back = buf, *s, *post;
    int sock, dont_close = 0;

    pthread_mutex_lock(&queue_mutex);
    while (empty())
      pthread_cond_wait(&queue_cond, &queue_mutex);
    sock = dequeue();
    pthread_mutex_unlock(&queue_mutex);

    printf("Handling connection with thread #%d.\n", me);

    while (1) {
      unsigned retries_left = MAX_RETRIES;
      int r;

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

      //printf("Received %d bytes.\n", r);

      back += r;
      *back = 0;

      if (s = strstr(buf, "\r\n\r\n")) {
        failure_kind fk;
        int is_post = 0, do_normal_send = 1;
        char *boundary = NULL;
        size_t boundary_len;
        char *cmd, *path, *headers, *inputs, *after_headers;

        //printf("All: %s\n", buf);

        s[2] = 0;
        after_headers = s + 4;

        if (!(s = strstr(buf, "\r\n"))) {
          fprintf(stderr, "No newline in buf\n");
          break;
        }

        *s = 0;
        headers = s + 2;
        cmd = s = buf;

        //printf("Read: %s\n", buf);
      
        if (!strsep(&s, " ")) {
          fprintf(stderr, "No first space in HTTP command\n");
          break;
        }

        uw_set_headers(ctx, headers);

        if (!strcmp(cmd, "POST")) {
          char *clen_s = uw_Basis_requestHeader(ctx, "Content-length");
          if (!clen_s) {
            fprintf(stderr, "No Content-length with POST\n");
            goto done;
          }
          int clen = atoi(clen_s);
          if (clen < 0) {
            fprintf(stderr, "Negative Content-length with POST\n");
            goto done;
          }

          while (back - after_headers < clen) {
            if (back - buf == buf_size - 1) {
              char *new_buf;
              buf_size *= 2;
              new_buf = realloc(buf, buf_size);

              back = new_buf + (back - buf);
              headers = new_buf + (headers - buf);
              uw_headers_moved(ctx, headers);
              after_headers = new_buf + (after_headers - buf);
              s = new_buf + (s - buf);

              buf = new_buf;
            }

            r = recv(sock, back, buf_size - 1 - (back - buf), 0);

            if (r < 0) {
              fprintf(stderr, "Recv failed\n");
              goto done;
            }

            if (r == 0) {
              printf("Connection closed.\n");
              goto done;
            }

            back += r;
            *back = 0;      
          }

          is_post = 1;

          clen_s = uw_Basis_requestHeader(ctx, "Content-type");
          if (clen_s && !strncasecmp(clen_s, "multipart/form-data", 19)) {
            if (strncasecmp(clen_s + 19, "; boundary=", 11)) {
              fprintf(stderr, "Bad multipart boundary spec");
              break;
            }

            boundary = clen_s + 28;
            boundary[0] = '-';
            boundary[1] = '-';
            boundary_len = strlen(boundary);
          }
        } else if (strcmp(cmd, "GET")) {
          fprintf(stderr, "Not ready for non-GET/POST command: %s\n", cmd);
          break;
        }

        path = s;
        if (!strsep(&s, " ")) {
          fprintf(stderr, "No second space in HTTP command\n");
          break;
        }

        if (!strcmp(path, "/.msgs")) {
          char *id = uw_Basis_requestHeader(ctx, "UrWeb-Client");
          char *pass = uw_Basis_requestHeader(ctx, "UrWeb-Pass");

          if (id && pass) {
            unsigned idn = atoi(id);
            uw_client_connect(idn, atoi(pass), sock);
            dont_close = 1;
            fprintf(stderr, "Processed request for messages by client %u\n\n", idn);
          }
          else {
            fprintf(stderr, "Missing fields in .msgs request: %s, %s\n\n", id, pass);
          }
          break;
        }

        if (boundary) {
          char *part = after_headers, *after_sub_headers, *header, *after_header;
          size_t part_len;

          part = strstr(part, boundary);
          if (!part) {
            fprintf(stderr, "Missing first multipart boundary\n");
            break;
          }
          part += boundary_len;

          while (1) {
            char *name = NULL, *filename = NULL, *type = NULL;

            if (part[0] == '-' && part[1] == '-')
              break;

            if (*part != '\r') {
              fprintf(stderr, "No \\r after multipart boundary\n");
              goto done;
            }
            ++part;
            if (*part != '\n') {
              fprintf(stderr, "No \\n after multipart boundary\n");
              goto done;
            }
            ++part;
            
            if (!(after_sub_headers = strstr(part, "\r\n\r\n"))) {
              fprintf(stderr, "Missing end of headers after multipart boundary\n");
              goto done;
            }
            after_sub_headers[2] = 0;
            after_sub_headers += 4;

            for (header = part; after_header = strstr(header, "\r\n"); header = after_header + 2) {
              char *colon, *after_colon;

              *after_header = 0;
              if (!(colon = strchr(header, ':'))) {
                fprintf(stderr, "Missing colon in multipart sub-header\n");
                goto done;
              }
              *colon++ = 0;
              if (*colon++ != ' ') {
                fprintf(stderr, "No space after colon in multipart sub-header\n");
                goto done;
              }

              if (!strcasecmp(header, "Content-Disposition")) {
                if (strncmp(colon, "form-data; ", 11)) {
                  fprintf(stderr, "Multipart data is not \"form-data\"\n");
                  goto done;
                }

                for (colon += 11; after_colon = strchr(colon, '='); colon = after_colon) {
                  char *data;
                  after_colon[0] = 0;
                  if (after_colon[1] != '"') {
                    fprintf(stderr, "Disposition setting is missing initial quote\n");
                    goto done;
                  }
                  data = after_colon+2;
                  if (!(after_colon = strchr(data, '"'))) {
                    fprintf(stderr, "Disposition setting is missing final quote\n");
                    goto done;
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

            part = memmem(after_sub_headers, back - after_sub_headers, boundary, boundary_len);
            if (!part) {
              fprintf(stderr, "Missing boundary after multipart payload\n");
              goto done;
            }
            part[-2] = 0;
            part_len = part - after_sub_headers - 2;
            part[0] = 0;
            part += boundary_len;

            if (filename) {
              uw_Basis_file f = {filename, type, {part_len, after_sub_headers}};

              uw_set_file_input(ctx, name, f);
            } else
              uw_set_input(ctx, name, after_sub_headers);
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
                uw_set_input(ctx, name, value);
              }
              else
                uw_set_input(ctx, name, "");
            }
          }
        }

        printf("Serving URI %s....\n", path);

        while (1) {
          size_t path_len = strlen(path);

          uw_write_header(ctx, "HTTP/1.1 200 OK\r\n");

          if (path_len + 1 > path_copy_size) {
            path_copy_size = path_len + 1;
            path_copy = realloc(path_copy, path_copy_size);
          }
          strcpy(path_copy, path);
          fk = uw_begin(ctx, path_copy);
          if (fk == SUCCESS || fk == RETURN_BLOB) {
            uw_commit(ctx);
            break;
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

              break;
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

            break;
          } else {
            printf("Unknown uw_handle return code!\n");

            try_rollback(ctx);

            uw_reset_keep_request(ctx);
            uw_write_header(ctx, "HTTP/1.1 500 Internal Server Error\n\r");
            uw_write_header(ctx, "Content-type: text/plain\r\n");
            uw_write(ctx, "Unknown uw_handle return code!\n");

            break;
          }

          if (try_rollback(ctx))
            break;

          uw_reset_keep_request(ctx);
        }

        uw_send(ctx, sock);

        printf("Done with client.\n\n");
        uw_memstats(ctx);
        break;
      }
    }

  done:
    if (!dont_close)
      close(sock);
    uw_reset(ctx);
  }
}

static void *client_pruner(void *data) {
  uw_context ctx = new_context();

  if (!ctx)
    exit(1);

  while (1) {
    uw_prune_clients(ctx);
    sleep(5);
  }
}

static void help(char *cmd) {
  printf("Usage: %s [-p <port>] [-t <thread-count>]\n", cmd);
}

static void sigint(int signum) {
  printf("Exiting....\n");
  exit(0);
}

static void initialize() {
  uw_context ctx;
  failure_kind fk;

  init_crypto();

  ctx = new_context();

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

  uw_global_init();
  initialize();

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
