#include "config.h"

#include <stdlib.h>

#include <pthread.h>

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
  node n = front->next;
  free(front);

  front = n;

  if (!front)
    back = NULL;

  return ret;
}

static pthread_mutex_t queue_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t queue_cond = PTHREAD_COND_INITIALIZER;

int uw_dequeue() {
  int sock;

  pthread_mutex_lock(&queue_mutex);
  while (empty())
    pthread_cond_wait(&queue_cond, &queue_mutex);
  sock = dequeue();
  pthread_mutex_unlock(&queue_mutex);

  return sock;
}

void uw_enqueue(int new_fd) {
  pthread_mutex_lock(&queue_mutex);
  enqueue(new_fd);
  pthread_cond_broadcast(&queue_cond);
  pthread_mutex_unlock(&queue_mutex);
}
