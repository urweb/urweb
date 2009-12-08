#define _XOPEN_SOURCE 500

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <setjmp.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>

#include <pthread.h>

#include "types.h"

uw_unit uw_unit_v = {};


// Socket extras

int uw_really_send(int sock, const void *buf, ssize_t len) {
  while (len > 0) {
    ssize_t n = send(sock, buf, len, 0);

    if (n < 0)
      return n;

    buf += n;
    len -= n;
  }

  return 0;
}

int uw_really_write(int fd, const void *buf, size_t len) {
  while (len > 0) {
    ssize_t n = write(fd, buf, len);

    if (n < 0)
      return n;

    buf += n;
    len -= n;
  }

  return 0;
}


// Buffers

typedef struct {
  char *start, *front, *back;
} buf;

static void buf_init(buf *b, size_t s) {
  b->front = b->start = malloc(s);
  b->back = b->front + s;
}

static void buf_free(buf *b) {
  free(b->start);
}

static void buf_reset(buf *b) {
  b->front = b->start;
}

static void buf_check(buf *b, size_t extra) {
  if (b->back - b->front < extra) {
    size_t desired = b->front - b->start + extra, next;
    char *new_heap;

    next = b->back - b->start;
    if (next == 0)
      next = 1;
    for (; next < desired; next *= 2);

    new_heap = realloc(b->start, next);
    b->front = new_heap + (b->front - b->start);
    b->back = new_heap + next;
    b->start = new_heap;
  }
}

static size_t buf_used(buf *b) {
  return b->front - b->start;
}

static size_t buf_avail(buf *b) {
  return b->back - b->start;
}

static void buf_append(buf *b, const char *s, size_t len) {
  buf_check(b, len+1);
  memcpy(b->front, s, len);
  b->front += len;
  *b->front = 0;
}


// Persistent state types

typedef enum { UNUSED, USED } usage;

typedef struct client {
  unsigned id;
  usage mode;
  int pass;
  struct client *next;
  pthread_mutex_t lock, pull_lock;
  buf msgs;
  int sock;
  int (*send)(int sockfd, const void *buf, ssize_t len);
  int (*close)(int fd);
  time_t last_contact;
  unsigned n_channels;
  unsigned refcount;
} client;


// Persistent client state

static client **clients, *clients_free, *clients_used;
static unsigned n_clients;

static pthread_mutex_t clients_mutex = PTHREAD_MUTEX_INITIALIZER;

static client *new_client() {
  client *c;

  pthread_mutex_lock(&clients_mutex);

  if (clients_free) {
    c = clients_free;
    clients_free = clients_free->next;
  }
  else {
    ++n_clients;
    clients = realloc(clients, sizeof(client) * n_clients);
    c = malloc(sizeof(client));
    c->id = n_clients-1;
    pthread_mutex_init(&c->lock, NULL);
    pthread_mutex_init(&c->pull_lock, NULL);
    buf_init(&c->msgs, 0);
    clients[n_clients-1] = c;
  }

  pthread_mutex_lock(&c->lock);
  c->mode = USED;
  c->pass = rand();
  c->sock = -1;
  c->last_contact = time(NULL);
  buf_reset(&c->msgs);
  c->n_channels = 0;
  c->refcount = 0;
  pthread_mutex_unlock(&c->lock);

  c->next = clients_used;
  clients_used = c;

  pthread_mutex_unlock(&clients_mutex);

  return c;
}

static void use_client(client *c) {
  pthread_mutex_lock(&c->lock);
  ++c->refcount;
  pthread_mutex_unlock(&c->lock);
  pthread_mutex_lock(&c->pull_lock);
}

static void release_client(client *c) {
  pthread_mutex_unlock(&c->pull_lock);
  pthread_mutex_lock(&c->lock);
  --c->refcount;
  pthread_mutex_unlock(&c->lock);

}

static const char begin_msgs[] = "Content-type: text/plain\r\n\r\n";

static client *find_client(unsigned id) {
  client *c;

  pthread_mutex_lock(&clients_mutex);

  if (id >= n_clients) {
    pthread_mutex_unlock(&clients_mutex);
    return NULL;
  }

  c = clients[id];

  pthread_mutex_unlock(&clients_mutex);
  return c;
}

static char *on_success = "HTTP/1.1 200 OK\r\n";

void uw_set_on_success(char *s) {
  on_success = s;
}

void uw_client_connect(unsigned id, int pass, int sock,
                       int (*send)(int sockfd, const void *buf, ssize_t len),
                       int (*close)(int fd),
                       void *logger_data, uw_logger log_error) {
  client *c = find_client(id);

  if (c == NULL) {
    close(sock);
    log_error(logger_data, "Out-of-bounds client request (%u)\n", id);
    return;
  }

  pthread_mutex_lock(&c->lock);

  if (c->mode != USED) {
    pthread_mutex_unlock(&c->lock);
    close(sock);
    log_error(logger_data, "Client request for unused slot (%u)\n", id);
    return;
  }

  if (pass != c->pass) {
    pthread_mutex_unlock(&c->lock);
    close(sock);
    log_error(logger_data, "Wrong client password (%u, %d)\n", id, pass);
    return;
  }

  if (c->sock != -1) {
    c->close(c->sock);
    c->sock = -1;
  }

  c->last_contact = time(NULL);

  if (buf_used(&c->msgs) > 0) {
    send(sock, on_success, strlen(on_success));
    send(sock, begin_msgs, sizeof(begin_msgs) - 1);
    send(sock, c->msgs.start, buf_used(&c->msgs));
    buf_reset(&c->msgs);
    close(sock);
  }
  else {
    c->sock = sock;
    c->send = send;
    c->close = close;
  }

  pthread_mutex_unlock(&c->lock);
}

static void free_client(client *c) {
  c->mode = UNUSED;
  c->pass = -1;

  c->next = clients_free;
  clients_free = c;
}

static uw_Basis_channel new_channel(client *c) {
  uw_Basis_channel ch = {c->id, c->n_channels++};
  return ch;
}

static void client_send(client *c, buf *msg) {
  pthread_mutex_lock(&c->lock);

  if (c->sock != -1) {
    c->send(c->sock, on_success, strlen(on_success));
    c->send(c->sock, begin_msgs, sizeof(begin_msgs) - 1);
    c->send(c->sock, msg->start, buf_used(msg));
    c->close(c->sock);
    c->sock = -1;
  } else
    buf_append(&c->msgs, msg->start, buf_used(msg));

  pthread_mutex_unlock(&c->lock);
}


// Global entry points

extern void uw_client_init();

void uw_global_init() {
  srand(time(NULL) ^ getpid());

  clients = malloc(0);

  uw_client_init();
}


// Single-request state

#define ERROR_BUF_LEN 1024

typedef struct regions {
  struct regions *next;
} regions;

typedef struct {
  void (*func)(void*);
  void *arg;
} cleanup;

typedef struct {
  unsigned client;
  buf msgs;
} delta;

typedef enum {
  UNSET, NORMAL, FIL, SUBFORM, SUBFORMS, ENTRY
} input_kind;

typedef struct input {
  input_kind kind;
  union {
    char *normal;
    uw_Basis_file file;
    struct {
      struct input *fields, *parent;
    } subform;
    struct {
      struct input *entries, *parent;
    } subforms;
    struct {
      struct input *fields, *next, *parent;
    } entry;
  } data;
} input;

typedef struct {
  void *data;
  uw_callback commit, rollback, free;
} transactional;

struct uw_context {
  char *(*get_header)(void *, const char *);
  void *get_header_data;

  buf outHeaders, page, heap, script;
  int returning_blob;
  input *inputs, *subinputs, *cur_container;
  size_t n_subinputs, used_subinputs;

  int source_count;

  void *db;

  jmp_buf jmp_buf;

  regions *regions;

  cleanup *cleanup, *cleanup_front, *cleanup_back;

  const char *script_header, *url_prefix;

  int needs_push, needs_sig;

  size_t n_deltas, used_deltas;
  delta *deltas;

  int timeout;

  client *client;

  transactional *transactionals;
  size_t n_transactionals, used_transactionals;

  char error_message[ERROR_BUF_LEN];
};

extern int uw_inputs_len, uw_timeout;

uw_context uw_init() {
  uw_context ctx = malloc(sizeof(struct uw_context));

  ctx->get_header = NULL;
  ctx->get_header_data = NULL;

  buf_init(&ctx->outHeaders, 0);
  buf_init(&ctx->page, 0);
  ctx->returning_blob = 0;
  buf_init(&ctx->heap, 0);
  buf_init(&ctx->script, 1);
  ctx->script.start[0] = 0;

  ctx->inputs = calloc(uw_inputs_len, sizeof(input));
  ctx->cur_container = NULL;
  ctx->subinputs = malloc(0);
  ctx->n_subinputs = ctx->used_subinputs = 0;

  ctx->db = NULL;

  ctx->regions = NULL;

  ctx->cleanup_front = ctx->cleanup_back = ctx->cleanup = malloc(0);

  ctx->script_header = "";
  ctx->url_prefix = "/";
  ctx->needs_push = 0;
  ctx->needs_sig = 0;
  
  ctx->error_message[0] = 0;

  ctx->source_count = 0;

  ctx->n_deltas = ctx->used_deltas = 0;
  ctx->deltas = malloc(0);

  ctx->timeout = uw_timeout;

  ctx->client = NULL;

  ctx->error_message[0] = 0;

  ctx->transactionals = malloc(0);
  ctx->n_transactionals = ctx->used_transactionals = 0;

  return ctx;
}

void uw_set_db(uw_context ctx, void *db) {
  ctx->db = db;
}

void *uw_get_db(uw_context ctx) {
  return ctx->db;
}

void uw_free(uw_context ctx) {
  size_t i;

  buf_free(&ctx->outHeaders);
  buf_free(&ctx->script);
  buf_free(&ctx->page);
  buf_free(&ctx->heap);
  free(ctx->inputs);
  free(ctx->subinputs);
  free(ctx->cleanup);
  free(ctx->transactionals);

  for (i = 0; i < ctx->n_deltas; ++i)
    buf_free(&ctx->deltas[i].msgs);

  free(ctx);
}

void uw_reset_keep_error_message(uw_context ctx) {
  size_t i;

  buf_reset(&ctx->outHeaders);
  buf_reset(&ctx->script);
  ctx->script.start[0] = 0;
  buf_reset(&ctx->page);
  ctx->returning_blob = 0;
  buf_reset(&ctx->heap);
  ctx->regions = NULL;
  ctx->cleanup_front = ctx->cleanup;
  ctx->source_count = 0;
  ctx->used_deltas = 0;
  ctx->client = NULL;
  ctx->cur_container = NULL;
  ctx->used_transactionals = 0;
}

void uw_reset_keep_request(uw_context ctx) {
  uw_reset_keep_error_message(ctx);
  ctx->error_message[0] = 0;
}

void uw_reset(uw_context ctx) {
  uw_reset_keep_request(ctx);
  memset(ctx->inputs, 0, uw_inputs_len * sizeof(input));
  memset(ctx->subinputs, 0, ctx->n_subinputs * sizeof(input));
  ctx->used_subinputs = 0;
}

void uw_db_init(uw_context);
void uw_handle(uw_context, char *);

failure_kind uw_begin_init(uw_context ctx) {
  int r = setjmp(ctx->jmp_buf);

  if (r == 0)
    uw_db_init(ctx);

  return r;
}

void uw_set_headers(uw_context ctx, char *(*get_header)(void *, const char *), void *get_header_data) {
  ctx->get_header = get_header;
  ctx->get_header_data = get_header_data;
}

int uw_db_begin(uw_context);

static void uw_set_error(uw_context ctx, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);

  vsnprintf(ctx->error_message, ERROR_BUF_LEN, fmt, ap);
}

__attribute__((noreturn)) void uw_error(uw_context ctx, failure_kind fk, const char *fmt, ...) {
  cleanup *cl;

  va_list ap;
  va_start(ap, fmt);

  vsnprintf(ctx->error_message, ERROR_BUF_LEN, fmt, ap);

  for (cl = ctx->cleanup; cl < ctx->cleanup_front; ++cl)
    cl->func(cl->arg);

  ctx->cleanup_front = ctx->cleanup;

  longjmp(ctx->jmp_buf, fk);
}

void uw_push_cleanup(uw_context ctx, void (*func)(void *), void *arg) {
  if (ctx->cleanup_front >= ctx->cleanup_back) {
    int len = ctx->cleanup_back - ctx->cleanup, newLen;
    if (len == 0)
      newLen = 1;
    else
      newLen = len * 2;
    ctx->cleanup = realloc(ctx->cleanup, newLen * sizeof(cleanup));
    ctx->cleanup_front = ctx->cleanup + len;
    ctx->cleanup_back = ctx->cleanup + newLen;
  }

  ctx->cleanup_front->func = func;
  ctx->cleanup_front->arg = arg;
  ++ctx->cleanup_front;
}

uw_Basis_string uw_Basis_requestHeader(uw_context ctx, uw_Basis_string h) {
  return ctx->get_header(ctx->get_header_data, h);
}

void uw_login(uw_context ctx) {
  if (ctx->needs_push) {
    char *id_s, *pass_s;

    if ((id_s = uw_Basis_requestHeader(ctx, "UrWeb-Client"))
        && (pass_s = uw_Basis_requestHeader(ctx, "UrWeb-Pass"))) {
      unsigned id = atoi(id_s);
      int pass = atoi(pass_s);
      client *c = find_client(id);

      if (c == NULL)
        uw_error(ctx, FATAL, "Unknown client ID in HTTP headers (%s, %s)", id_s, pass_s);
      else {
        use_client(c);
        ctx->client = c;

        if (c->mode != USED)
          uw_error(ctx, FATAL, "Stale client ID (%u) in subscription request", id);
        if (c->pass != pass)
          uw_error(ctx, FATAL, "Wrong client password (%u, %d) in subscription request", id, pass);
      }
    } else {
      client *c = new_client();
      use_client(c);
      ctx->client = c;
    }
  }
}

failure_kind uw_begin(uw_context ctx, char *path) {
  int r = setjmp(ctx->jmp_buf);

  if (r == 0) {
    if (uw_db_begin(ctx))
      uw_error(ctx, BOUNDED_RETRY, "Error running SQL BEGIN");

    uw_handle(ctx, path);
  }

  return r;
}

uw_Basis_client uw_Basis_self(uw_context ctx) {
  if (ctx->client == NULL)
    uw_error(ctx, FATAL, "Call to Basis.self() from page that has only server-side code");

  return ctx->client->id;
}

void uw_pop_cleanup(uw_context ctx) {
  if (ctx->cleanup_front == ctx->cleanup)
    uw_error(ctx, FATAL, "Attempt to pop from empty cleanup action stack");

  --ctx->cleanup_front;
  ctx->cleanup_front->func(ctx->cleanup_front->arg);
}

char *uw_error_message(uw_context ctx) {
  return ctx->error_message;
}

extern int uw_input_num(const char*);

static input *INP(uw_context ctx) {
  if (ctx->cur_container == NULL)
    return ctx->inputs;
  else if (ctx->cur_container->kind == SUBFORM)
    return ctx->cur_container->data.subform.fields;
  else if (ctx->cur_container->kind == ENTRY)
    return ctx->cur_container->data.entry.fields;
  else
    uw_error(ctx, FATAL, "INP: Wrong kind (%d, %p)", ctx->cur_container->kind, ctx->cur_container);
}

static void adjust_pointer(input **ptr, input *old_start, input *new_start, size_t len) {
  if (*ptr != NULL && *ptr >= old_start && *ptr < old_start + len)
    *ptr += new_start - old_start;
}

static void adjust_input(input *x, input *old_start, input *new_start, size_t len) {
  switch (x->kind) {
  case SUBFORM:
    adjust_pointer(&x->data.subform.fields, old_start, new_start, len);
    adjust_pointer(&x->data.subform.parent, old_start, new_start, len);
    break;
  case SUBFORMS:
    adjust_pointer(&x->data.subforms.entries, old_start, new_start, len);
    adjust_pointer(&x->data.subforms.parent, old_start, new_start, len);
    break;
  case ENTRY:
    adjust_pointer(&x->data.entry.fields, old_start, new_start, len);
    adjust_pointer(&x->data.entry.next, old_start, new_start, len);
    adjust_pointer(&x->data.entry.parent, old_start, new_start, len);
  }  
}

static input *check_input_space(uw_context ctx, size_t len) {
  size_t i;
  input *r;

  if (ctx->used_subinputs + len >= ctx->n_subinputs) {
    input *new_subinputs = realloc(ctx->subinputs, sizeof(input) * (ctx->used_subinputs + len));
    size_t offset = new_subinputs - ctx->subinputs;

    if (ctx->subinputs != new_subinputs) {
      for (i = 0; i < ctx->used_subinputs; ++i)
        adjust_input(&new_subinputs[i], ctx->subinputs, new_subinputs, ctx->used_subinputs);
      for (i = 0; i < uw_inputs_len; ++i)
        adjust_input(&ctx->inputs[i], ctx->subinputs, new_subinputs, ctx->used_subinputs);

      adjust_pointer(&ctx->cur_container, ctx->subinputs, new_subinputs, ctx->used_subinputs);

      ctx->n_subinputs = ctx->used_subinputs + len;
      ctx->subinputs = new_subinputs;
    }
  }

  r = &ctx->subinputs[ctx->used_subinputs];

  for (i = 0; i < len; ++i)
    ctx->subinputs[ctx->used_subinputs++].kind = UNUSED;

  return r;
}

int uw_set_input(uw_context ctx, const char *name, char *value) {
  //printf("Input name %s\n", name);

  if (!strcasecmp(name, ".b")) {
    int n = uw_input_num(value);
    input *inps;

    if (n < 0) {
      uw_set_error(ctx, "Bad subform name %s", value);
      return -1;
    }

    if (n >= uw_inputs_len) {
      uw_set_error(ctx, "For subform name %s, index %d is out of range", value, n);
      return -1;
    }

    inps = check_input_space(ctx, uw_inputs_len);

    INP(ctx)[n].kind = SUBFORM;
    INP(ctx)[n].data.subform.parent = ctx->cur_container;
    INP(ctx)[n].data.subform.fields = inps;
    ctx->cur_container = &INP(ctx)[n];
  } else if (!strcasecmp(name, ".e")) {
    input *tmp;

    if (ctx->cur_container == NULL) {
      uw_set_error(ctx, "Unmatched subform closer");
      return -1;
    }

    tmp = ctx->cur_container;
    switch (tmp->kind) {
    case SUBFORM:
      ctx->cur_container = tmp->data.subform.parent;
      tmp->data.subform.parent = NULL;
      break;
    case SUBFORMS:
      ctx->cur_container = tmp->data.subforms.parent;
      tmp->data.subforms.parent = NULL;
      break;
    case ENTRY:
      ctx->cur_container = tmp->data.entry.parent;
      break;
    default:
      uw_set_error(ctx, "uw_set_input: Wrong kind");
      return -1;
    }
  } else if (!strcasecmp(name, ".s")) {
    int n = uw_input_num(value);

    if (n < 0) {
      uw_set_error(ctx, "Bad subforms name %s", value);
      return -1;
    }

    if (n >= uw_inputs_len) {
      uw_set_error(ctx, "For subforms name %s, index %d is out of range", value, n);
      return -1;
    }

    INP(ctx)[n].kind = SUBFORMS;
    INP(ctx)[n].data.subforms.parent = ctx->cur_container;
    INP(ctx)[n].data.subforms.entries = NULL;
    ctx->cur_container = &INP(ctx)[n];
  } else if (!strcasecmp(name, ".i")) {
    input *inps;

    if (!ctx->cur_container) {
      uw_set_error(ctx, "New entry without container");
      return -1;
    }

    if (ctx->cur_container->kind != SUBFORMS) {
      uw_set_error(ctx, "Bad kind for entry parent");
      return -1;
    }

    inps = check_input_space(ctx, uw_inputs_len + 1);

    inps->kind = ENTRY;
    inps->data.entry.parent = ctx->cur_container;
    inps->data.entry.next = ctx->cur_container->data.subforms.entries;
    ctx->cur_container->data.subforms.entries = inps;

    inps->data.entry.fields = inps+1;
    ctx->cur_container = inps;
  } else {
    int n = uw_input_num(name);

    if (n < 0) {
      if (!strcmp(name, "null"))
        return 0;
      uw_set_error(ctx, "Bad input name %s", name);
      return -1;
    }

    if (n >= uw_inputs_len) {
      uw_set_error(ctx, "For input name %s, index %d is out of range", name, n);
      return -1;
    }

    INP(ctx)[n].kind = NORMAL;
    INP(ctx)[n].data.normal = value;
  }

  return 0;
}

char *uw_get_input(uw_context ctx, int n) {
  if (n < 0)
    uw_error(ctx, FATAL, "Negative input index %d", n);
  if (n >= uw_inputs_len)
    uw_error(ctx, FATAL, "Out-of-bounds input index %d", n);

  switch (INP(ctx)[n].kind) {
  case UNSET:
    return NULL;
  case FIL:
    uw_error(ctx, FATAL, "Tried to read a file form input as normal");
  case SUBFORM:
    uw_error(ctx, FATAL, "Tried to read a subform form input as normal");
  case SUBFORMS:
    uw_error(ctx, FATAL, "Tried to read a subforms form input as normal");
  case ENTRY:
    uw_error(ctx, FATAL, "Tried to read an entry form input as normal");
  case NORMAL:
    return INP(ctx)[n].data.normal;
  default:
    uw_error(ctx, FATAL, "Impossible input kind");
  }
}

char *uw_get_optional_input(uw_context ctx, int n) {
  if (n < 0)
    uw_error(ctx, FATAL, "Negative input index %d", n);
  if (n >= uw_inputs_len)
    uw_error(ctx, FATAL, "Out-of-bounds input index %d", n);

  switch (INP(ctx)[n].kind) {
  case UNSET:
    return "";
  case FIL:
    uw_error(ctx, FATAL, "Tried to read a file form input as normal");
  case SUBFORM:
    uw_error(ctx, FATAL, "Tried to read a subform form input as normal");
  case SUBFORMS:
    uw_error(ctx, FATAL, "Tried to read a subforms form input as normal");
  case ENTRY:
    uw_error(ctx, FATAL, "Tried to read an entry form input as normal");
  case NORMAL:
    return INP(ctx)[n].data.normal;
  default:
    uw_error(ctx, FATAL, "Impossible input kind");
  }
}

int uw_set_file_input(uw_context ctx, const char *name, uw_Basis_file f) {
  int n = uw_input_num(name);

  if (n < 0) {
    uw_set_error(ctx, "Bad file input name %s", name);
    return -1;
  }

  if (n >= uw_inputs_len) {
    uw_set_error(ctx, "For file input name %s, index %d is out of range", name, n);
    return -1;
  }

  ctx->inputs[n].kind = FIL;
  ctx->inputs[n].data.file = f;

  return 0;
}

void *uw_malloc(uw_context ctx, size_t len);


static void parents(input *inp) {
  printf("Stack: %p\n", inp);
  while (inp) {
    switch (inp->kind) {
    case NORMAL:
      printf("Normal(%p)\n", inp);
      break;
    case FIL:
      printf("File(%p)\n", inp);
      break;
    case SUBFORM:
      printf("Subform; fields = %p\n", inp->data.subform.fields);
      inp = inp->data.subform.parent;
      break;
    case SUBFORMS:
      printf("Subforms; entries = %p\n", inp->data.subforms.entries);
      inp = inp->data.subforms.parent;
      break;
    case ENTRY:
      printf("Entry; fields = %p; next = %p\n", inp->data.entry.fields, inp->data.entry.next);
      inp = inp->data.entry.parent;
      break;
    default:
      inp = NULL;
    }
  }
}

uw_Basis_file uw_get_file_input(uw_context ctx, int n) {
  if (n < 0)
    uw_error(ctx, FATAL, "Negative file input index %d", n);
  if (n >= uw_inputs_len)
    uw_error(ctx, FATAL, "Out-of-bounds file input index %d", n);

  switch (INP(ctx)[n].kind) {
  case UNSET:
    {
      char *data = uw_malloc(ctx, 0);
      uw_Basis_file f = {NULL, "", {0, data}};
      return f;
    }
  case FIL:
    return INP(ctx)[n].data.file;
  case NORMAL:
    uw_error(ctx, FATAL, "Tried to read a normal form input as files");
  case SUBFORM:
    uw_error(ctx, FATAL, "Tried to read a subform form input as files");
  case SUBFORMS:
    uw_error(ctx, FATAL, "Tried to read a subforms form input as files");
  case ENTRY:
    uw_error(ctx, FATAL, "Tried to read an entry form input as files");
  default:
    uw_error(ctx, FATAL, "Impossible input kind");
  }
}

void uw_enter_subform(uw_context ctx, int n) {
  if (n < 0)
    uw_error(ctx, FATAL, "Negative subform index %d", n);
  if (n >= uw_inputs_len)
    uw_error(ctx, FATAL, "Out-of-bounds subform index %d", n);

  switch (INP(ctx)[n].kind) {
  case UNSET:
    uw_error(ctx, FATAL, "Missing subform");
  case FIL:
    uw_error(ctx, FATAL, "Tried to read a file form input as subform");
  case NORMAL:
    uw_error(ctx, FATAL, "Tried to read a normal form input as subform");
  case SUBFORMS:
    uw_error(ctx, FATAL, "Tried to read a subforms form input as subform");
  case ENTRY:
    uw_error(ctx, FATAL, "Tried to read an entry form input as subform");
  case SUBFORM:
    INP(ctx)[n].data.subform.parent = ctx->cur_container;
    ctx->cur_container = &INP(ctx)[n];
    return;
  default:
    uw_error(ctx, FATAL, "Impossible input kind");
  }
}

void uw_leave_subform(uw_context ctx) {
  input *tmp;

  if (ctx->cur_container == NULL)
    uw_error(ctx, FATAL, "Unmatched uw_leave_subform");

  tmp = ctx->cur_container;
  ctx->cur_container = tmp->data.subform.parent;
  tmp->data.subform.parent = NULL;
}

int uw_enter_subforms(uw_context ctx, int n) {
  input *inps;

  if (n < 0)
    uw_error(ctx, FATAL, "Negative subforms index %d", n);
  if (n >= uw_inputs_len)
    uw_error(ctx, FATAL, "Out-of-bounds subforms index %d", n);

  switch (INP(ctx)[n].kind) {
  case UNSET:
    uw_error(ctx, FATAL, "Missing subforms");
  case FIL:
    uw_error(ctx, FATAL, "Tried to read a file form input as subforms");
  case NORMAL:
    uw_error(ctx, FATAL, "Tried to read a normal form input %p as subforms", &INP(ctx)[n]);
  case SUBFORM:
    uw_error(ctx, FATAL, "Tried to read a subform form input as subforms");
  case ENTRY:
    uw_error(ctx, FATAL, "Tried to read an entry form input as subforms");
  case SUBFORMS:
    inps = INP(ctx)[n].data.subforms.entries;
    if (inps) {
      INP(ctx)[n].data.subforms.parent = ctx->cur_container;
      ctx->cur_container = INP(ctx)[n].data.subforms.entries;
      return 1;
    } else
      return 0;
  default:
    uw_error(ctx, FATAL, "Impossible input kind");
  }
}

int uw_next_entry(uw_context ctx) {
  if (ctx->cur_container == NULL)
    uw_error(ctx, FATAL, "uw_next_entry(NULL)");

  switch (ctx->cur_container->kind) {
  case UNSET:
    uw_error(ctx, FATAL, "Missing entry");
  case FIL:
    uw_error(ctx, FATAL, "Tried to read a file form input as entry");
  case NORMAL:
    uw_error(ctx, FATAL, "Tried to read a normal form input as entry");
  case SUBFORM:
    uw_error(ctx, FATAL, "Tried to read a subform form input as entry");
  case SUBFORMS:
    uw_error(ctx, FATAL, "Tried to read a subforms form input as entry");
  case ENTRY:
    if (ctx->cur_container->data.entry.next) {
      ctx->cur_container = ctx->cur_container->data.entry.next;
      return 1;
    } else {
      ctx->cur_container = ctx->cur_container->data.entry.parent->data.subforms.parent;
      return 0;
    }
  default:
    uw_error(ctx, FATAL, "Impossible input kind");
  }
}

void uw_set_script_header(uw_context ctx, const char *s) {
  ctx->script_header = s;
}

void uw_set_url_prefix(uw_context ctx, const char *s) {
  ctx->url_prefix = s;
}

void uw_set_needs_push(uw_context ctx, int n) {
  ctx->needs_push = n;
}

void uw_set_needs_sig(uw_context ctx, int n) {
  ctx->needs_sig = n;
}


static void buf_check_ctx(uw_context ctx, buf *b, size_t extra, const char *desc) {
  if (b->back - b->front < extra) {
    size_t desired = b->front - b->start + extra, next;
    char *new_heap;

    next = b->back - b->start;
    if (next == 0)
      next = 1;
    for (; next < desired; next *= 2);

    new_heap = realloc(b->start, next);
    b->front = new_heap + (b->front - b->start);
    b->back = new_heap + next;

    if (new_heap != b->start) {
      b->start = new_heap;
      uw_error(ctx, UNLIMITED_RETRY, "Couldn't allocate new %s contiguously", desc);
    }

    b->start = new_heap;
  }
}

void uw_check_heap(uw_context ctx, size_t extra) {
  buf_check_ctx(ctx, &ctx->heap, extra, "heap chunk");
}

char *uw_heap_front(uw_context ctx) {
  return ctx->heap.front;
}

void uw_set_heap_front(uw_context ctx, char *fr) {
  ctx->heap.front = fr;
}

void *uw_malloc(uw_context ctx, size_t len) {
  void *result;

  uw_check_heap(ctx, len);

  result = ctx->heap.front;
  ctx->heap.front += len;
  return result;
}

void uw_begin_region(uw_context ctx) {
  regions *r = (regions *) ctx->heap.front;

  uw_check_heap(ctx, sizeof(regions));

  ctx->heap.front += sizeof(regions);

  r->next = ctx->regions;
  ctx->regions = r;
}

void uw_end_region(uw_context ctx) {
  regions *r = ctx->regions;

  if (r == NULL)
    uw_error(ctx, FATAL, "Region stack underflow");

  ctx->heap.front = (char *) r;
  ctx->regions = r->next;
}

void uw_memstats(uw_context ctx) {
  printf("Headers: %d/%d\n", buf_used(&ctx->outHeaders), buf_avail(&ctx->outHeaders));
  printf("Script: %d/%d\n", buf_used(&ctx->script), buf_avail(&ctx->script));
  printf("Page: %d/%d\n", buf_used(&ctx->page), buf_avail(&ctx->page));
  printf("Heap: %d/%d\n", buf_used(&ctx->heap), buf_avail(&ctx->heap));
}

int uw_send(uw_context ctx, int sock) {
  int n = uw_really_send(sock, ctx->outHeaders.start, ctx->outHeaders.front - ctx->outHeaders.start);

  if (n < 0)
    return n;

  n = uw_really_send(sock, "\r\n", 2);

  if (n < 0)
    return n;

  return uw_really_send(sock, ctx->page.start, ctx->page.front - ctx->page.start);
}

int uw_print(uw_context ctx, int fd) {
  int n = uw_really_write(fd, ctx->outHeaders.start, ctx->outHeaders.front - ctx->outHeaders.start);

  if (n < 0)
    return n;

  n = uw_really_write(fd, "\r\n", 2);

  if (n < 0)
    return n;

  return uw_really_write(fd, ctx->page.start, ctx->page.front - ctx->page.start);
}

int uw_output(uw_context ctx, int (*output)(void *data, char *buf, size_t len), void *data) {
  int n = output(data, ctx->outHeaders.start, ctx->outHeaders.front - ctx->outHeaders.start);

  if (n < 0)
    return n;

  n = output(data, "\r\n", 2);

  if (n < 0)
    return n;

  return output(data, ctx->page.start, ctx->page.front - ctx->page.start);
}

static void uw_check_headers(uw_context ctx, size_t extra) {
  buf_check(&ctx->outHeaders, extra);
}

void uw_write_header(uw_context ctx, uw_Basis_string s) {
  int len = strlen(s);

  uw_check_headers(ctx, len + 1);
  strcpy(ctx->outHeaders.front, s);
  ctx->outHeaders.front += len;
}

static void uw_check_script(uw_context ctx, size_t extra) {
  buf_check(&ctx->script, extra);
}

void uw_write_script(uw_context ctx, uw_Basis_string s) {
  int len = strlen(s);

  uw_check_script(ctx, len + 1);
  strcpy(ctx->script.front, s);
  ctx->script.front += len;
}

const char *uw_Basis_get_script(uw_context ctx, uw_unit u) {
  return "<sc>";
}

uw_Basis_string uw_Basis_maybe_onload(uw_context ctx, uw_Basis_string s) {
  if (s[0] == 0)
    return "";
  else {
    char *r = uw_malloc(ctx, 11 + strlen(s));
    sprintf(r, " onload='%s'", s);
    return r;
  }
}

uw_Basis_string uw_Basis_maybe_onunload(uw_context ctx, uw_Basis_string s) {
  if (ctx->script_header[0] == 0)
    return "";
  else {
    char *r = uw_malloc(ctx, 22 + strlen(s));
    sprintf(r, " onunload='unload();%s'", s);
    return r;
  }
}

extern uw_Basis_string uw_cookie_sig(uw_context);

const char *uw_Basis_get_settings(uw_context ctx, uw_unit u) {
  if (ctx->client == NULL) {
    if (ctx->needs_sig) {
      char *sig = uw_cookie_sig(ctx);
      char *r = uw_malloc(ctx, strlen(sig) + 8);
      sprintf(r, "sig=\"%s\";", sig);
      return r;
    }
    else
      return "";
  } else {
    char *sig = ctx->needs_sig ? uw_cookie_sig(ctx) : "";
    char *r = uw_malloc(ctx, 59 + 3 * INTS_MAX + strlen(ctx->url_prefix)
                        + (ctx->needs_sig ? strlen(sig) + 7 : 0));
    sprintf(r, "client_id=%u;client_pass=%d;url_prefix=\"%s\";timeout=%d;%s%s%slistener();",
            ctx->client->id,
            ctx->client->pass,
            ctx->url_prefix,
            ctx->timeout,
            ctx->needs_sig ? "sig=\"" : "",
            sig,
            ctx->needs_sig ? "\";" : "");
    return r;
  }
}

uw_Basis_string uw_Basis_jsifyString(uw_context ctx, uw_Basis_string s) {
  char *r, *s2;

  uw_check_heap(ctx, strlen(s) * 4 + 2);

  r = s2 = ctx->heap.front;
  *s2++ = '"';

  for (; *s; s++) {
    char c = *s;

    switch (c) {
    case '"':
      strcpy(s2, "\\\"");
      s2 += 2;
      break;
    case '\'':
      strcpy(s2, "\\047");
      s2 += 4;
      break;
    case '\\':
      strcpy(s2, "\\\\");
      s2 += 2;
      break;
    default:
      if (isprint(c))
        *s2++ = c;
      else {
        sprintf(s2, "\\%3o", c);
        s2 += 4;
      }
    }
  }

  strcpy(s2, "\"");
  ctx->heap.front = s2 + 2;
  return r;
}

uw_Basis_string uw_Basis_jsifyChar(uw_context ctx, uw_Basis_char c) {
  char *r, *s2;

  uw_check_heap(ctx, 6);

  r = s2 = ctx->heap.front;
  *s2++ = '"';

  switch (c) {
  case '"':
    strcpy(s2, "\\\"");
    s2 += 2;
    break;
  case '\'':
    strcpy(s2, "\\047");
    s2 += 4;
    break;
  case '\\':
    strcpy(s2, "\\\\");
    s2 += 2;
    break;
  default:
    if (isprint(c))
      *s2++ = c;
    else {
      sprintf(s2, "\\%3o", c);
      s2 += 4;
    }
  }

  strcpy(s2, "\"");
  ctx->heap.front = s2 + 2;
  return r;
}

uw_Basis_string uw_Basis_jsifyString_ws(uw_context ctx, uw_Basis_string s) {
  char *r, *s2;

  uw_check_script(ctx, strlen(s) * 4 + 2);

  r = s2 = ctx->script.front;
  *s2++ = '"';

  for (; *s; s++) {
    char c = *s;

    switch (c) {
    case '\'':
      strcpy(s2, "\\");
      s2 += 2;
      break;
    case '\\':
      strcpy(s2, "\\\\");
      s2 += 2;
      break;
    default:
      if (isprint(c))
        *s2++ = c;
      else {
        sprintf(s2, "\\%3o", c);
        s2 += 4;
      }
    }
  }

  strcpy(s2, "\"");
  ctx->script.front = s2 + 1;
  return r;
}

char *uw_Basis_jsifyChannel(uw_context ctx, uw_Basis_channel chn) {
  if (ctx->client == NULL || chn.cli != ctx->client->id)
    return "null";
  else {
    int len;
    char *r;

    uw_check_heap(ctx, INTS_MAX + 1);
    r = ctx->heap.front;
    sprintf(r, "%u%n", chn.chn, &len);
    ctx->heap.front += len+1;
    return r;
  }
}

uw_Basis_int uw_Basis_new_client_source(uw_context ctx, uw_Basis_string s) {
  int len;
  size_t s_len = strlen(s);

  uw_check_script(ctx, 18 + INTS_MAX + s_len);
  sprintf(ctx->script.front, "var s%d=sc(exec(%n", ctx->source_count, &len);
  ctx->script.front += len;
  strcpy(ctx->script.front, s);
  ctx->script.front += s_len;
  strcpy(ctx->script.front, "));");
  ctx->script.front += 3;

  return ctx->source_count++;
}

uw_unit uw_Basis_set_client_source(uw_context ctx, uw_Basis_int n, uw_Basis_string s) {
  int len;
  size_t s_len = strlen(s);

  uw_check_script(ctx, 12 + INTS_MAX + s_len);
  sprintf(ctx->script.front, "sv(s%d,exec(%n", (int)n, &len);
  ctx->script.front += len;
  strcpy(ctx->script.front, s);
  ctx->script.front += s_len;
  strcpy(ctx->script.front, "));");
  ctx->script.front += 3;

  return uw_unit_v;
}

static void uw_check(uw_context ctx, size_t extra) {
  buf_check(&ctx->page, extra);
}

static void uw_writec_unsafe(uw_context ctx, char c) {
  *(ctx->page.front)++ = c;
}

void uw_writec(uw_context ctx, char c) {
  uw_check(ctx, 1);
  uw_writec_unsafe(ctx, c);
}

static void uw_write_unsafe(uw_context ctx, const char* s) {
  int len = strlen(s);
  memcpy(ctx->page.front, s, len);
  ctx->page.front += len;
}

void uw_write(uw_context ctx, const char* s) {
  uw_check(ctx, strlen(s) + 1);
  uw_write_unsafe(ctx, s);
  *ctx->page.front = 0;
}

char *uw_Basis_attrifyInt(uw_context ctx, uw_Basis_int n) {
  char *result;
  int len;
  uw_check_heap(ctx, INTS_MAX);
  result = ctx->heap.front;
  sprintf(result, "%lld%n", n, &len);
  ctx->heap.front += len+1;
  return result;
}

char *uw_Basis_attrifyFloat(uw_context ctx, uw_Basis_float n) {
  char *result;
  int len;
  uw_check_heap(ctx, FLOATS_MAX);
  result = ctx->heap.front;
  sprintf(result, "%g%n", n, &len);
  ctx->heap.front += len+1;
  return result;
}

static int isCont(unsigned char ch) {
  return ch / 64 == 2;
}

char *uw_Basis_attrifyString(uw_context ctx, uw_Basis_string s) {
  int len = strlen(s);
  char *result, *p;
  uw_check_heap(ctx, len * 6 + 1);

  result = p = ctx->heap.front;

  for (; *s; s++) {
    unsigned char c = *s;

    if (c == '"') {
      strcpy(p, "&quot;");
      p += 6;
    } else if (c == '&') {
      strcpy(p, "&amp;");
      p += 5;
    }
    else
      *p++ = c;
  }

  *p++ = 0;
  ctx->heap.front = p;
  return result;
}

char *uw_Basis_attrifyChar(uw_context ctx, uw_Basis_char c) {
  char *result, *p;
  uw_check_heap(ctx, 7);

  result = p = ctx->heap.front;

  if (c == '"') {
    strcpy(p, "&quot;");
    p += 6;
  } else if (c == '&') {
    strcpy(p, "&amp;");
    p += 5;
  }
  else
    *p++ = c;

  *p++ = 0;
  ctx->heap.front = p;
  return result;
}

char *uw_Basis_attrifyCss_class(uw_context ctx, uw_Basis_css_class s) {
  return s;
}

static void uw_Basis_attrifyInt_w_unsafe(uw_context ctx, uw_Basis_int n) {
  int len;

  sprintf(ctx->page.front, "%lld%n", n, &len);
  ctx->page.front += len;
}

uw_unit uw_Basis_attrifyInt_w(uw_context ctx, uw_Basis_int n) {
  uw_check(ctx, INTS_MAX);
  uw_Basis_attrifyInt_w_unsafe(ctx, n);

  return uw_unit_v;
}

uw_unit uw_Basis_attrifyFloat_w(uw_context ctx, uw_Basis_float n) {
  int len;

  uw_check(ctx, FLOATS_MAX);
  sprintf(ctx->page.front, "%g%n", n, &len);
  ctx->page.front += len;

  return uw_unit_v;
}

uw_unit uw_Basis_attrifyString_w(uw_context ctx, uw_Basis_string s) {
  uw_check(ctx, strlen(s) * 6);

  for (; *s; s++) {
    unsigned char c = *s;

    if (c == '"')
      uw_write_unsafe(ctx, "&quot;");
    else if (c == '&')
      uw_write_unsafe(ctx, "&amp;");
    else
      uw_writec_unsafe(ctx, c);
  }

  return uw_unit_v;
}

uw_unit uw_Basis_attrifyChar_w(uw_context ctx, uw_Basis_char c) {
  uw_check(ctx, 6);

  if (c == '"')
    uw_write_unsafe(ctx, "&quot;");
  else if (c == '&')
    uw_write_unsafe(ctx, "&amp;");
  else
    uw_writec_unsafe(ctx, c);

  return uw_unit_v;
}


char *uw_Basis_urlifyInt(uw_context ctx, uw_Basis_int n) {
  int len;
  char *r;

  uw_check_heap(ctx, INTS_MAX);
  r = ctx->heap.front;
  sprintf(r, "%lld%n", n, &len);
  ctx->heap.front += len+1;
  return r;
}

char *uw_Basis_urlifyChannel(uw_context ctx, uw_Basis_channel chn) {
  if (ctx->client == NULL || chn.cli != ctx->client->id)
    return "";
  else {
    int len;
    char *r;

    uw_check_heap(ctx, INTS_MAX + 1);
    r = ctx->heap.front;
    sprintf(r, "%u%n", chn.chn, &len);
    ctx->heap.front += len+1;
    return r;
  }
}

char *uw_Basis_urlifyFloat(uw_context ctx, uw_Basis_float n) {
  int len;
  char *r;

  uw_check_heap(ctx, FLOATS_MAX);
  r = ctx->heap.front;
  sprintf(r, "%g%n", n, &len);
  ctx->heap.front += len+1;
  return r;
}

char *uw_Basis_urlifyString(uw_context ctx, uw_Basis_string s) {
  char *r, *p;

  if (s[0] == '\0')
    return "_";

  uw_check_heap(ctx, strlen(s) * 3 + 1 + !!(s[0] == '_'));

  r = p = ctx->heap.front;
  if (s[0] == '_')
    *p++ = '_';

  for (; *s; s++) {
    char c = *s;

    if (c == ' ')
      *p++ = '+';
    else if (isalnum(c))
      *p++ = c;
    else {
      sprintf(p, "%%%02X", c);
      p += 3;
    }
  }

  *p++ = 0;
  ctx->heap.front = p;
  return r;
}

char *uw_Basis_urlifyBool(uw_context ctx, uw_Basis_bool b) {
  if (b == uw_Basis_False)
    return "0";
  else
    return "1";
}

static void uw_Basis_urlifyInt_w_unsafe(uw_context ctx, uw_Basis_int n) {
  int len;

  sprintf(ctx->page.front, "%lld%n", n, &len);
  ctx->page.front += len;
}

uw_unit uw_Basis_urlifyInt_w(uw_context ctx, uw_Basis_int n) {
  uw_check(ctx, INTS_MAX);
  uw_Basis_urlifyInt_w_unsafe(ctx, n);

  return uw_unit_v;
}

uw_unit uw_Basis_urlifyChannel_w(uw_context ctx, uw_Basis_channel chn) {
  if (ctx->client != NULL && chn.cli == ctx->client->id) {
    int len;
    
    uw_check(ctx, INTS_MAX + 1);
    sprintf(ctx->page.front, "%u%n", chn.chn, &len);
    ctx->page.front += len;
  }
    
  return uw_unit_v;
}

uw_unit uw_Basis_urlifyFloat_w(uw_context ctx, uw_Basis_float n) {
  int len;

  uw_check(ctx, FLOATS_MAX);
  sprintf(ctx->page.front, "%g%n", n, &len);
  ctx->page.front += len;

  return uw_unit_v;
}

uw_Basis_string uw_Basis_urlifyTime(uw_context ctx, uw_Basis_time t) {
  return uw_Basis_urlifyInt(ctx, t);
}

uw_unit uw_Basis_urlifyString_w(uw_context ctx, uw_Basis_string s) {
  if (s[0] == '\0') {
    uw_check(ctx, 1);
    uw_writec_unsafe(ctx, '_');
    return uw_unit_v;
  }

  uw_check(ctx, strlen(s) * 3 + !!(s[0] == '_'));

  if (s[0] == '_')
    uw_writec_unsafe(ctx, '_');

  for (; *s; s++) {
    char c = *s;

    if (c == ' ')
      uw_writec_unsafe(ctx, '+');
    else if (isalnum(c))
      uw_writec_unsafe(ctx, c);
    else {
      sprintf(ctx->page.front, "%%%02X", c);
      ctx->page.front += 3;
    }
  }

  return uw_unit_v;
}

uw_unit uw_Basis_urlifyBool_w(uw_context ctx, uw_Basis_bool b) {
  if (b == uw_Basis_False)
    uw_writec(ctx, '0');
  else
    uw_writec(ctx, '1');

  return uw_unit_v;
}


static char *uw_unurlify_advance(char *s) {
  char *new_s = strchr(s, '/');

  if (new_s)
    *new_s++ = 0;
  else
    new_s = strchr(s, 0);

  return new_s;
}

uw_Basis_int uw_Basis_unurlifyInt(uw_context ctx, char **s) {
  char *new_s = uw_unurlify_advance(*s);
  uw_Basis_int r;

  r = atoll(*s);
  *s = new_s;
  return r;
}

uw_Basis_float uw_Basis_unurlifyFloat(uw_context ctx, char **s) {
  char *new_s = uw_unurlify_advance(*s);
  uw_Basis_float r;

  r = atof(*s);
  *s = new_s;
  return r;
}

uw_Basis_time uw_Basis_unurlifyTime(uw_context ctx, char **s) {
  return uw_Basis_unurlifyInt(ctx, s);
}

static uw_Basis_string uw_unurlifyString_to(int fromClient, uw_context ctx, char *r, char *s) {
  char *s1, *s2 = s;
  int n;

  if (!fromClient) {
    if (*s2 == '_')
      ++s2;
    else if (s2[0] == '%' && s2[1] == '5' && (s2[2] == 'f' || s2[2] == 'F'))
      s2 += 3;
  }

  for (s1 = r; *s2; ++s1, ++s2) {
    char c = *s2;

    switch (c) {
    case '+':
      *s1 = ' ';
      break;
    case '%':
      if (s2[1] == 0)
        uw_error(ctx, FATAL, "Missing first character of escaped URL byte");
      if (s2[2] == 0)
        uw_error(ctx, FATAL, "Missing second character of escaped URL byte");
      if (sscanf(s2+1, "%02X", &n) != 1)
        uw_error(ctx, FATAL, "Invalid escaped URL byte starting at: %s", s2);
      *s1 = n;
      s2 += 2;
      break;
    default:
      *s1 = c;
    }
  }
  *s1++ = 0;
  return s1;
}

uw_Basis_bool uw_Basis_unurlifyBool(uw_context ctx, char **s) {
  char *new_s = uw_unurlify_advance(*s);
  uw_Basis_bool r;
  
  if (*s[0] == 0 || !strcmp(*s, "0") || !strcmp(*s, "off"))
    r = uw_Basis_False;
  else
    r = uw_Basis_True;

  *s = new_s;
  return r;
}

uw_Basis_string uw_Basis_unurlifyString(uw_context ctx, char **s) {
  char *new_s = uw_unurlify_advance(*s);
  char *r, *s1, *s2;
  int len, n;

  len = strlen(*s);
  uw_check_heap(ctx, len + 1);

  r = ctx->heap.front;
  ctx->heap.front = uw_unurlifyString_to(0, ctx, ctx->heap.front, *s);
  *s = new_s;
  return r;
}

uw_Basis_string uw_Basis_unurlifyString_fromClient(uw_context ctx, char **s) {
  char *new_s = uw_unurlify_advance(*s);
  char *r, *s1, *s2;
  int len, n;

  len = strlen(*s);
  uw_check_heap(ctx, len + 1);

  r = ctx->heap.front;
  ctx->heap.front = uw_unurlifyString_to(1, ctx, ctx->heap.front, *s);
  *s = new_s;
  return r;
}


char *uw_Basis_htmlifyInt(uw_context ctx, uw_Basis_int n) {
  int len;
  char *r;

  uw_check_heap(ctx, INTS_MAX);
  r = ctx->heap.front;
  sprintf(r, "%lld%n", n, &len);
  ctx->heap.front += len+1;
  return r;
}

uw_unit uw_Basis_htmlifyInt_w(uw_context ctx, uw_Basis_int n) {
  int len;

  uw_check(ctx, INTS_MAX);
  sprintf(ctx->page.front, "%lld%n", n, &len);
  ctx->page.front += len;
  
  return uw_unit_v;
}

char *uw_Basis_htmlifyFloat(uw_context ctx, uw_Basis_float n) {
  int len;
  char *r;

  uw_check_heap(ctx, FLOATS_MAX);
  r = ctx->heap.front;
  sprintf(r, "%g%n", n, &len);
  ctx->heap.front += len+1;
  return r;
}

uw_unit uw_Basis_htmlifyFloat_w(uw_context ctx, uw_Basis_float n) {
  int len;

  uw_check(ctx, FLOATS_MAX);
  sprintf(ctx->page.front, "%g%n", n, &len);
  ctx->page.front += len;

  return uw_unit_v;
}

char *uw_Basis_htmlifyString(uw_context ctx, uw_Basis_string s) {
  char *r, *s2;

  uw_check_heap(ctx, strlen(s) * 5 + 1);

  for (r = s2 = ctx->heap.front; *s; s++) {
    unsigned char c = *s;

    switch (c) {
    case '<':
      strcpy(s2, "&lt;");
      s2 += 4;
      break;
    case '&':
      strcpy(s2, "&amp;");
      s2 += 5;
      break;
    default:
      *s2++ = c;
    }
  }

  *s2++ = 0;
  ctx->heap.front = s2;
  return r;
}

uw_unit uw_Basis_htmlifyString_w(uw_context ctx, uw_Basis_string s) {
  uw_check(ctx, strlen(s) * 6);

  for (; *s; s++) {
    unsigned char c = *s;

    switch (c) {
    case '<':
      uw_write_unsafe(ctx, "&lt;");
      break;
    case '&':
      uw_write_unsafe(ctx, "&amp;");
      break;
    default:
      uw_writec_unsafe(ctx, c);
    }
  }

  return uw_unit_v;
}

uw_Basis_string uw_Basis_htmlifyBool(uw_context ctx, uw_Basis_bool b) {
  if (b == uw_Basis_False)
    return "False";
  else
    return "True";
}

uw_unit uw_Basis_htmlifyBool_w(uw_context ctx, uw_Basis_bool b) {
  if (b == uw_Basis_False) {
    uw_check(ctx, 6);
    strcpy(ctx->page.front, "False");
    ctx->page.front += 5;
  } else {
    uw_check(ctx, 5);
    strcpy(ctx->page.front, "True");
    ctx->page.front += 4;
  }

  return uw_unit_v;
}

#define TIME_FMT "%x %X"
#define TIME_FMT_PG "%Y-%m-%d %T"

uw_Basis_string uw_Basis_htmlifyTime(uw_context ctx, uw_Basis_time t) {
  size_t len;
  char *r;
  struct tm stm;

  if (localtime_r(&t, &stm)) {
    uw_check_heap(ctx, TIMES_MAX);
    r = ctx->heap.front;
    len = strftime(r, TIMES_MAX, TIME_FMT, &stm);
    ctx->heap.front += len+1;
    return r;
  } else
    return "<i>Invalid time</i>";
}

uw_unit uw_Basis_htmlifyTime_w(uw_context ctx, uw_Basis_time t) {
  size_t len;
  char *r;
  struct tm stm;

  if (localtime_r(&t, &stm)) {
    uw_check(ctx, TIMES_MAX);
    r = ctx->page.front;
    len = strftime(r, TIMES_MAX, TIME_FMT, &stm);
    ctx->page.front += len;
  } else {
    uw_check(ctx, 20);
    strcpy(ctx->page.front, "<i>Invalid time</i>");
    ctx->page.front += 19;
  }

  return uw_unit_v;
}

uw_Basis_char uw_Basis_strsub(uw_context ctx, uw_Basis_string s, uw_Basis_int n) {
  if (n >= 0 && n < strlen(s))
    return s[n];
  else
    uw_error(ctx, FATAL, "Out-of-bounds strsub");
}

uw_Basis_string uw_Basis_strsuffix(uw_context ctx, uw_Basis_string s, uw_Basis_int n) {
  if (n >= 0 && n < strlen(s))
    return &s[n];
  else
    uw_error(ctx, FATAL, "Out-of-bounds strsuffix");
}

uw_Basis_int uw_Basis_strlen(uw_context ctx, uw_Basis_string s) {
  return strlen(s);
}

uw_Basis_string uw_Basis_strchr(uw_context ctx, uw_Basis_string s, uw_Basis_char ch) {
  return strchr(s, ch);
}

uw_Basis_int uw_Basis_strcspn(uw_context ctx, uw_Basis_string s, uw_Basis_string chs) {
  return strcspn(s, chs);
}

uw_Basis_int *uw_Basis_strindex(uw_context ctx, uw_Basis_string s, uw_Basis_char ch) {
  uw_Basis_string r = strchr(s, ch);
  if (r == NULL)
    return NULL;
  else {
    uw_Basis_int *nr = uw_malloc(ctx, sizeof(uw_Basis_int));
    *nr = r - s;
    return nr;
  }
}

uw_Basis_string uw_Basis_strcat(uw_context ctx, uw_Basis_string s1, uw_Basis_string s2) {
  int len = uw_Basis_strlen(ctx, s1) + uw_Basis_strlen(ctx, s2) + 1;
  char *s;

  uw_check_heap(ctx, len);

  s = ctx->heap.front;

  strcpy(s, s1);
  strcat(s, s2);
  ctx->heap.front += len;

  return s;
}

uw_Basis_string uw_Basis_substring(uw_context ctx, uw_Basis_string s, uw_Basis_int start, uw_Basis_int len) {
  size_t full_len = uw_Basis_strlen(ctx, s);

  if (start < 0)
    uw_error(ctx, FATAL, "substring: Negative start index");
  if (len < 0)
    uw_error(ctx, FATAL, "substring: Negative length");
  if (start + len > full_len)
    uw_error(ctx, FATAL, "substring: Start index plus length is too large");

  if (start + len == full_len)
    return &s[start];
  else {
    uw_Basis_string r = uw_malloc(ctx, len+1);
    memcpy(r, s+start, len);
    r[len] = 0;
    return r;
  }
    
}

uw_Basis_string uw_Basis_str1(uw_context ctx, uw_Basis_char ch) {
  char *r;

  uw_check_heap(ctx, 2);
  r = ctx->heap.front;
  r[0] = ch;
  r[1] = 0;

  ctx->heap.front += 2;

  return r;
}

uw_Basis_string uw_strdup(uw_context ctx, uw_Basis_string s1) {
  int len = uw_Basis_strlen(ctx, s1) + 1;
  char *s;

  uw_check_heap(ctx, len);

  s = ctx->heap.front;

  strcpy(s, s1);
  ctx->heap.front += len;

  return s;
}

uw_Basis_string uw_maybe_strdup(uw_context ctx, uw_Basis_string s1) {
  if (s1)
    return uw_strdup(ctx, s1);
  else
    return NULL;
}

char *uw_memdup(uw_context ctx, const char *p, size_t len) {
  char *r = uw_malloc(ctx, len);
  memcpy(r, p, len);
  return r;
}

char *uw_sqlfmtInt = "%lld::int8%n";

char *uw_Basis_sqlifyInt(uw_context ctx, uw_Basis_int n) {
  int len;
  char *r;

  uw_check_heap(ctx, INTS_MAX + 6);
  r = ctx->heap.front;
  sprintf(r, uw_sqlfmtInt, n, &len);
  ctx->heap.front += len+1;
  return r;
}

char *uw_Basis_sqlifyIntN(uw_context ctx, uw_Basis_int *n) {
  if (n == NULL)
    return "NULL";
  else
    return uw_Basis_sqlifyInt(ctx, *n);
}

char *uw_sqlfmtFloat = "%g::float8%n";

char *uw_Basis_sqlifyFloat(uw_context ctx, uw_Basis_float n) {
  int len;
  char *r;

  uw_check_heap(ctx, FLOATS_MAX + 8);
  r = ctx->heap.front;
  sprintf(r, uw_sqlfmtFloat, n, &len);
  ctx->heap.front += len+1;
  return r;
}

char *uw_Basis_sqlifyFloatN(uw_context ctx, uw_Basis_float *n) {
  if (n == NULL)
    return "NULL";
  else
    return uw_Basis_sqlifyFloat(ctx, *n);
}

int uw_Estrings = 1;
char *uw_sqlsuffixString = "::text";
char *uw_sqlsuffixChar = "::char";

uw_Basis_string uw_Basis_sqlifyString(uw_context ctx, uw_Basis_string s) {
  char *r, *s2;

  uw_check_heap(ctx, strlen(s) * 2 + 3 + uw_Estrings + strlen(uw_sqlsuffixString));

  r = s2 = ctx->heap.front;
  if (uw_Estrings)
    *s2++ = 'E';
  *s2++ = '\'';

  for (; *s; s++) {
    char c = *s;

    switch (c) {
    case '\'':
      if (uw_Estrings)
        strcpy(s2, "\\'");
      else
        strcpy(s2, "''");
      s2 += 2;
      break;
    case '\\':
      if (uw_Estrings) {
        strcpy(s2, "\\\\");
        s2 += 2;
      } else
        *s2++ = '\\';
      break;
    default:
      if (isprint(c))
        *s2++ = c;
      else if (uw_Estrings) {
        sprintf(s2, "\\%03o", c);
        s2 += 4;
      }
      else
        uw_error(ctx, FATAL, "Non-printable character %u in string to SQLify", c);
    }
  }

  *s2++ = '\'';
  strcpy(s2, uw_sqlsuffixString);
  ctx->heap.front = s2 + 1 + strlen(uw_sqlsuffixString);
  return r;
}

uw_Basis_string uw_Basis_sqlifyChar(uw_context ctx, uw_Basis_char c) {
  char *r, *s2;

  uw_check_heap(ctx, 5 + uw_Estrings + strlen(uw_sqlsuffixChar));

  r = s2 = ctx->heap.front;
  if (uw_Estrings)
    *s2++ = 'E';
  *s2++ = '\'';

  switch (c) {
  case '\'':
    if (uw_Estrings)
      strcpy(s2, "\\'");
    else
      strcpy(s2, "''");
    s2 += 2;
    break;
  case '\\':
    if (uw_Estrings) {
      strcpy(s2, "\\\\");
      s2 += 2;
    } else
      *s2++ = '\\';
    break;
  default:
    if (isprint(c))
      *s2++ = c;
    else if (uw_Estrings) {
      sprintf(s2, "\\%03o", c);
      s2 += 4;
    }
    else
      uw_error(ctx, FATAL, "Non-printable character %u in char to SQLify", c);
  }

  *s2++ = '\'';
  strcpy(s2, uw_sqlsuffixChar);
  ctx->heap.front = s2 + 1 + strlen(uw_sqlsuffixChar);
  return r;
}

char *uw_sqlsuffixBlob = "::bytea";

uw_Basis_string uw_Basis_sqlifyBlob(uw_context ctx, uw_Basis_blob b) {
  char *r, *s2;
  size_t i;

  uw_check_heap(ctx, b.size * 5 + 3 + uw_Estrings + strlen(uw_sqlsuffixBlob));

  r = s2 = ctx->heap.front;
  if (uw_Estrings)
    *s2++ = 'E';
  *s2++ = '\'';

  for (i = 0; i < b.size; ++i) {
    char c = b.data[i];

    switch (c) {
    case '\'':
      if (uw_Estrings)
        strcpy(s2, "\\'");
      else
        strcpy(s2, "''");
      s2 += 2;
      break;
    case '\\':
      if (uw_Estrings) {
        strcpy(s2, "\\\\\\\\");
        s2 += 4;
      } else
        *s2++ = '\\';
      break;
    default:
      if (isprint(c))
        *s2++ = c;
      else if (uw_Estrings) {
        sprintf(s2, "\\\\%03o", c);
        s2 += 5;
      }
      else
        uw_error(ctx, FATAL, "Non-printable character %u in blob to SQLify", c);
    }
  }

  *s2++ = '\'';
  strcpy(s2, uw_sqlsuffixBlob);
  ctx->heap.front = s2 + 1 + strlen(uw_sqlsuffixBlob);
  return r;
}

char *uw_Basis_sqlifyChannel(uw_context ctx, uw_Basis_channel chn) {
  int len;
  char *r;
  unsigned long long combo = ((unsigned long long)chn.cli << 32) | chn.chn;

  uw_check_heap(ctx, INTS_MAX + 7);
  r = ctx->heap.front;
  sprintf(r, uw_sqlfmtInt, combo, &len);
  ctx->heap.front += len+1;
  return r;
}

char *uw_Basis_attrifyChannel(uw_context ctx, uw_Basis_channel chn) {
  int len;
  char *r;
  unsigned long long combo = ((unsigned long long)chn.cli << 32) | chn.chn;

  uw_check_heap(ctx, INTS_MAX + 1);
  r = ctx->heap.front;
  sprintf(r, "%lld%n", combo, &len);
  ctx->heap.front += len+1;
  return r;
}

char *uw_sqlfmtUint4 = "%u::int4%n";

char *uw_Basis_sqlifyClient(uw_context ctx, uw_Basis_client cli) {
  int len;
  char *r;

  uw_check_heap(ctx, INTS_MAX + 7);
  r = ctx->heap.front;
  sprintf(r, uw_sqlfmtUint4, cli, &len);
  ctx->heap.front += len+1;
  return r;
}

char *uw_Basis_attrifyClient(uw_context ctx, uw_Basis_client cli) {
  int len;
  char *r;

  uw_check_heap(ctx, INTS_MAX + 1);
  r = ctx->heap.front;
  sprintf(r, "%u%n", cli, &len);
  ctx->heap.front += len+1;
  return r;
}

uw_Basis_string uw_Basis_sqlifyStringN(uw_context ctx, uw_Basis_string s) {
  if (s == NULL)
    return "NULL";
  else
    return uw_Basis_sqlifyString(ctx, s);
}

char *uw_Basis_sqlifyBool(uw_context ctx, uw_Basis_bool b) {
  if (b == uw_Basis_False)
    return "FALSE";
  else
    return "TRUE";
}

char *uw_Basis_sqlifyBoolN(uw_context ctx, uw_Basis_bool *b) {
  if (b == NULL)
    return "NULL";
  else
    return uw_Basis_sqlifyBool(ctx, *b);
}

char *uw_Basis_sqlifyTime(uw_context ctx, uw_Basis_time t) {
  size_t len;
  char *r, *s;
  struct tm stm;

  if (localtime_r(&t, &stm)) {
    s = uw_malloc(ctx, TIMES_MAX);
    --stm.tm_hour;
    len = strftime(s, TIMES_MAX, TIME_FMT, &stm);
    r = uw_malloc(ctx, len + 14);
    sprintf(r, "'%s'::timestamp", s);
    return r;
  } else
    return "<Invalid time>";
}

char *uw_Basis_attrifyTime(uw_context ctx, uw_Basis_time t) {
  size_t len;
  char *r;
  struct tm stm;

  if (localtime_r(&t, &stm)) {
    uw_check_heap(ctx, TIMES_MAX);
    r = ctx->heap.front;
    len = strftime(r, TIMES_MAX, TIME_FMT, &stm);
    ctx->heap.front += len+1;
    return r;
  } else
    return "<Invalid time>";
}

char *uw_Basis_sqlifyTimeN(uw_context ctx, uw_Basis_time *t) {
  if (t == NULL)
    return "NULL";
  else
    return uw_Basis_sqlifyTime(ctx, *t);
}

char *uw_Basis_ensqlBool(uw_Basis_bool b) {
  static uw_Basis_int true = 1;
  static uw_Basis_int false = 0;

  if (b == uw_Basis_False)
    return (char *)&false;
  else
    return (char *)&true;
}

uw_Basis_string uw_Basis_intToString(uw_context ctx, uw_Basis_int n) {
  int len;
  char *r;

  uw_check_heap(ctx, INTS_MAX);
  r = ctx->heap.front;
  sprintf(r, "%lld%n", n, &len);
  ctx->heap.front += len+1;
  return r;
}

uw_Basis_string uw_Basis_floatToString(uw_context ctx, uw_Basis_float n) {
  int len;
  char *r;

  uw_check_heap(ctx, FLOATS_MAX);
  r = ctx->heap.front;
  sprintf(r, "%g%n", n, &len);
  ctx->heap.front += len+1;
  return r;
}

uw_Basis_string uw_Basis_charToString(uw_context ctx, uw_Basis_char ch) {
  char *r = uw_malloc(ctx, 2);
  r[0] = ch;
  r[1] = 0;
  return r;
}

uw_Basis_string uw_Basis_boolToString(uw_context ctx, uw_Basis_bool b) {
  if (b == uw_Basis_False)
    return "False";
  else
    return "True";
}

uw_Basis_string uw_Basis_timeToString(uw_context ctx, uw_Basis_time t) {
  size_t len;
  char *r;
  struct tm stm;

  if (localtime_r(&t, &stm)) {
    uw_check_heap(ctx, TIMES_MAX);
    r = ctx->heap.front;
    len = strftime(r, TIMES_MAX, TIME_FMT, &stm);
    ctx->heap.front += len+1;
    return r;
  } else
    return "<Invalid time>";
}

uw_Basis_int *uw_Basis_stringToInt(uw_context ctx, uw_Basis_string s) {
  char *endptr;
  uw_Basis_int n = strtoll(s, &endptr, 10);

  if (*s != '\0' && *endptr == '\0') {
    uw_Basis_int *r = uw_malloc(ctx, sizeof(uw_Basis_int));
    *r = n;
    return r;
  } else
    return NULL;
}

uw_Basis_float *uw_Basis_stringToFloat(uw_context ctx, uw_Basis_string s) {
  char *endptr;
  uw_Basis_float n = strtod(s, &endptr);

  if (*s != '\0' && *endptr == '\0') {
    uw_Basis_float *r = uw_malloc(ctx, sizeof(uw_Basis_float));
    *r = n;
    return r;
  } else
    return NULL;
}

uw_Basis_char *uw_Basis_stringToChar(uw_context ctx, uw_Basis_string s) {
  if (s[0] == 0) {
    uw_Basis_char *r = uw_malloc(ctx, 1);
    r[0] = 0;
    return r;
  } else if (s[1] != 0)
    return NULL;
  else {
    uw_Basis_char *r = uw_malloc(ctx, 1);
    r[0] = s[0];
    return r;
  }
}

uw_Basis_bool *uw_Basis_stringToBool(uw_context ctx, uw_Basis_string s) {
  static uw_Basis_bool true = uw_Basis_True;
  static uw_Basis_bool false = uw_Basis_False;

  if (!strcasecmp (s, "True"))
    return &true;
  else if (!strcasecmp (s, "False"))
    return &false;
  else
    return NULL;
}

uw_Basis_time *uw_Basis_stringToTime(uw_context ctx, uw_Basis_string s) {
  char *dot = strchr(s, '.'), *end = strchr(s, 0);
  struct tm stm;

  if (dot) {
    *dot = 0;
    if (strptime(s, TIME_FMT_PG, &stm) == end) {
      *dot = '.';
      uw_Basis_time *r = uw_malloc(ctx, sizeof(uw_Basis_time));
      *r = mktime(&stm);
      return r;
    }
    else {
      *dot = '.';
      return NULL;
    }
  }
  else {
    if (strptime(s, TIME_FMT_PG, &stm) == end) {
      uw_Basis_time *r = uw_malloc(ctx, sizeof(uw_Basis_time));
      *r = mktime(&stm);
      return r;
    }
    else if (strptime(s, TIME_FMT, &stm) == end) {
      uw_Basis_time *r = uw_malloc(ctx, sizeof(uw_Basis_time));
      *r = mktime(&stm);
      return r;
    }
    else
      return NULL;
  }
}

uw_Basis_int uw_Basis_stringToInt_error(uw_context ctx, uw_Basis_string s) {
  char *endptr;
  uw_Basis_int n = strtoll(s, &endptr, 10);

  if (*s != '\0' && *endptr == '\0')
    return n;
  else
    uw_error(ctx, FATAL, "Can't parse int: %s", s);
}

#include <errno.h>

uw_Basis_channel uw_Basis_stringToChannel_error(uw_context ctx, uw_Basis_string s) {
  unsigned long long n;

  if (sscanf(s, "%llu", &n) < 1)
    uw_error(ctx, FATAL, "Can't parse channel: %s", s);
  else {
    uw_Basis_channel ch = {n >> 32, n & ((1ull << 32) - 1)};
    return ch;
  }
}

uw_Basis_client uw_Basis_stringToClient_error(uw_context ctx, uw_Basis_string s) {
  char *endptr;
  unsigned long n = strtoul(s, &endptr, 10);

  if (*s != '\0' && *endptr == '\0')
    return n;
  else
    uw_error(ctx, FATAL, "Can't parse client: %s", s);
}

uw_Basis_float uw_Basis_stringToFloat_error(uw_context ctx, uw_Basis_string s) {
  char *endptr;
  uw_Basis_float n = strtod(s, &endptr);

  if (*s != '\0' && *endptr == '\0')
    return n;
  else
    uw_error(ctx, FATAL, "Can't parse float: %s", s);
}

uw_Basis_char uw_Basis_stringToChar_error(uw_context ctx, uw_Basis_string s) {
  if (s[0] == 0)
    return 0;
  else if (s[1] != 0)
    uw_error(ctx, FATAL, "Can't parse char: %s", s);
  else
    return s[0];
}

uw_Basis_bool uw_Basis_stringToBool_error(uw_context ctx, uw_Basis_string s) {
  if (!strcasecmp(s, "T") || !strcasecmp (s, "True"))
    return uw_Basis_True;
  else if (!strcasecmp(s, "F") || !strcasecmp (s, "False"))
    return uw_Basis_False;
  else
    uw_error(ctx, FATAL, "Can't parse bool: %s", s);
}

uw_Basis_time uw_Basis_stringToTime_error(uw_context ctx, uw_Basis_string s) {
  char *dot = strchr(s, '.'), *end = strchr(s, 0);
  struct tm stm = {};

  if (dot) {
    *dot = 0;
    if (strptime(s, TIME_FMT_PG, &stm)) {
      *dot = '.';
      return mktime(&stm);
    }
    else {
      *dot = '.';
      uw_error(ctx, FATAL, "Can't parse time: %s", s);
    }
  }
  else {
    if (strptime(s, TIME_FMT_PG, &stm) == end)
      return mktime(&stm);
    else if (strptime(s, TIME_FMT, &stm) == end)
      return mktime(&stm);
    else
      uw_error(ctx, FATAL, "Can't parse time: %s", s);
  }
}

uw_Basis_time uw_Basis_unsqlTime(uw_context ctx, uw_Basis_string s) {
  char *dot = strchr(s, '.'), *end = strchr(s, 0);
  struct tm stm = {};

  if (dot) {
    *dot = 0;
    if (strptime(s, TIME_FMT_PG, &stm)) {
      *dot = '.';
      return mktime(&stm);
    }
    else {
      *dot = '.';
      uw_error(ctx, FATAL, "Can't parse time: %s", s);
    }
  }
  else {
    if (strptime(s, TIME_FMT_PG, &stm) == end) {
      return mktime(&stm);
    } else if (strptime(s, TIME_FMT, &stm) == end) {
      return mktime(&stm);
    } else
      uw_error(ctx, FATAL, "Can't parse time: %s", s);
  }
}

uw_Basis_blob uw_Basis_stringToBlob_error(uw_context ctx, uw_Basis_string s, size_t len) {
  char *r = ctx->heap.front;
  uw_Basis_blob b = {len, r};

  uw_check_heap(ctx, len);

  while (*s) {
    if (s[0] == '\\') {
      if (s[1] == '\\') {
        *r++ = '\\';
        s += 2;
      } else if (isdigit(s[1]) && isdigit(s[2]) && isdigit(s[3])) {
        *r++ = (s[1] - '0') * 8 * 8 + ((s[2] - '0') * 8) + (s[3] - '0');
        s += 4;
      }
      else {
        *r++ = '\\';
        ++s;
      }
    } else {
      *r++ = s[0];
      ++s;
    }
  }

  b.size = r - ctx->heap.front;
  ctx->heap.front = r;
  return b;
}

#define THE_PAST "expires=Mon, 01-01-1970 00:00:00 GMT"

uw_Basis_string uw_Basis_get_cookie(uw_context ctx, uw_Basis_string c) {
  int len = strlen(c);
  char *p = ctx->outHeaders.start;

  while (p = strstr(p, "\nSet-Cookie: ")) {
    char *p2;
    p += 13;
    p2 = strchr(p, '=');

    if (p2) {
      size_t sz = strcspn(p2+1, ";\r\n");

      if (!strncasecmp(p, c, p2 - p)) {
        if (sz == 0 && strstr(p2+2, THE_PAST))
          return NULL;
        else {
          char *ret = uw_malloc(ctx, sz + 1);
          memcpy(ret, p2+1, sz);
          ret[sz] = 0;
          return ret;
        }
      }
    }
  }

  if (p = uw_Basis_requestHeader(ctx, "Cookie")) {
    char *p2;

    while (1) {
      if (!strncmp(p, c, len) && p[len] == '=') {
        if (p2 = strchr(p, ';')) {
          size_t n = p2 - (p + len);
          char *r = uw_malloc(ctx, n);
          memcpy(r, p + 1 + len, n-1);
          r[n-1] = 0;
          return r;
        } else
          return p + 1 + len;
      } else if (p = strchr(p, ';'))
        p += 2;
      else
        return NULL;
    }
  }

  return NULL;
}

uw_unit uw_Basis_set_cookie(uw_context ctx, uw_Basis_string prefix, uw_Basis_string c, uw_Basis_string v, uw_Basis_time *expires, uw_Basis_bool secure) {
  uw_write_header(ctx, "Set-Cookie: ");
  uw_write_header(ctx, c);
  uw_write_header(ctx, "=");
  uw_write_header(ctx, v);
  uw_write_header(ctx, "; path=");
  uw_write_header(ctx, prefix);
  if (expires) {
    char formatted[30];
    struct tm tm;

    gmtime_r(expires, &tm);

    strftime(formatted, sizeof formatted, "%a, %d-%b-%Y %T GMT", &tm);

    uw_write_header(ctx, "; expires=");
    uw_write_header(ctx, formatted);
  }
  if (secure)
    uw_write_header(ctx, "; secure");
  uw_write_header(ctx, "\r\n");

  return uw_unit_v;
}

uw_unit uw_Basis_clear_cookie(uw_context ctx, uw_Basis_string prefix, uw_Basis_string c) {
  uw_write_header(ctx, "Set-Cookie: ");
  uw_write_header(ctx, c);
  uw_write_header(ctx, "=; path=");
  uw_write_header(ctx, prefix);
  uw_write_header(ctx, "; " THE_PAST "\r\n");

  return uw_unit_v;
}

static delta *allocate_delta(uw_context ctx, unsigned client) {
  unsigned i;
  delta *d;

  for (i = 0; i < ctx->used_deltas; ++i)
    if (ctx->deltas[i].client == client)
      return &ctx->deltas[i];

  if (ctx->used_deltas >= ctx->n_deltas) {
    ctx->deltas = realloc(ctx->deltas, sizeof(delta) * ++ctx->n_deltas);
    buf_init(&ctx->deltas[ctx->n_deltas-1].msgs, 0);
  }

  d = &ctx->deltas[ctx->used_deltas++];
  d->client = client;
  buf_reset(&d->msgs);
  return d;
}

uw_Basis_channel uw_Basis_new_channel(uw_context ctx, uw_unit u) {
  if (ctx->client == NULL)
    uw_error(ctx, FATAL, "Attempt to create channel on request not associated with a persistent connection");

  return new_channel(ctx->client);
}

uw_unit uw_Basis_send(uw_context ctx, uw_Basis_channel chn, uw_Basis_string msg) {
  delta *d = allocate_delta(ctx, chn.cli);
  size_t len;
  int preLen;
  char pre[INTS_MAX + 2];

  len = strlen(msg);

  sprintf(pre, "%u\n%n", chn.chn, &preLen);

  buf_append(&d->msgs, pre, preLen);
  buf_append(&d->msgs, msg, len);
  buf_append(&d->msgs, "\n", 1);

  return uw_unit_v;
}

int uw_db_commit(uw_context);
int uw_db_rollback(uw_context);

void uw_commit(uw_context ctx) {
  unsigned i;

  for (i = 0; i < ctx->used_transactionals; ++i)
    if (ctx->transactionals[i].rollback != NULL)
      ctx->transactionals[i].commit(ctx->transactionals[i].data);

  for (i = 0; i < ctx->used_transactionals; ++i)
    if (ctx->transactionals[i].rollback == NULL)
      ctx->transactionals[i].commit(ctx->transactionals[i].data);

  if (uw_db_commit(ctx))
    uw_error(ctx, FATAL, "Error running SQL COMMIT");

  for (i = 0; i < ctx->used_deltas; ++i) {
    delta *d = &ctx->deltas[i];
    client *c = find_client(d->client);

    assert (c != NULL && c->mode == USED);

    client_send(c, &d->msgs);
  }

  if (ctx->client)
    release_client(ctx->client);

  for (i = 0; i < ctx->used_transactionals; ++i)
    ctx->transactionals[i].free(ctx->transactionals[i].data);

  // Splice script data into appropriate part of page
  if (ctx->returning_blob || ctx->script_header[0] == 0) {
    char *start = strstr(ctx->page.start, "<sc>");
    if (start) {
      memmove(start, start + 4, buf_used(&ctx->page) - (start - ctx->page.start) - 4);
      ctx->page.front -= 4;
    }
  } else if (buf_used(&ctx->script) == 0) {
    size_t len = strlen(ctx->script_header);
    char *start = strstr(ctx->page.start, "<sc>");
    if (start) {
      buf_check(&ctx->page, buf_used(&ctx->page) - 4 + len);
      start = strstr(ctx->page.start, "<sc>");
      memmove(start + len, start + 4, buf_used(&ctx->page) - (start - ctx->page.start) - 3);
      ctx->page.front += len - 4;
      memcpy(start, ctx->script_header, len);
    }
  } else {
    size_t lenH = strlen(ctx->script_header), len = buf_used(&ctx->script);
    size_t lenP = lenH + 40 + len;
    char *start = strstr(ctx->page.start, "<sc>");
    if (start) {
      buf_check(&ctx->page, buf_used(&ctx->page) - 4 + lenP);
      start = strstr(ctx->page.start, "<sc>");
      memmove(start + lenP, start + 4, buf_used(&ctx->page) - (start - ctx->page.start) - 3);
      ctx->page.front += lenP - 4;
      memcpy(start, ctx->script_header, lenH);
      memcpy(start + lenH, "<script type=\"text/javascript\">", 31);
      memcpy(start + lenH + 31, ctx->script.start, len);
      memcpy(start + lenH + 31 + len, "</script>", 9);
    }
  }
}

int uw_rollback(uw_context ctx) {
  size_t i;
  cleanup *cl;

  if (ctx->client)
    release_client(ctx->client);

  for (cl = ctx->cleanup; cl < ctx->cleanup_front; ++cl)
    cl->func(cl->arg);

  ctx->cleanup_front = ctx->cleanup;

  for (i = 0; i < ctx->used_transactionals; ++i)
    if (ctx->transactionals[i].rollback != NULL)
      ctx->transactionals[i].rollback(ctx->transactionals[i].data);

  for (i = 0; i < ctx->used_transactionals; ++i)
    ctx->transactionals[i].free(ctx->transactionals[i].data);

  return uw_db_rollback(ctx);
}

void uw_register_transactional(uw_context ctx, void *data, uw_callback commit, uw_callback rollback,
                               uw_callback free) {
  if (commit == NULL)
    uw_error(ctx, FATAL, "uw_register_transactional: NULL commit callback");

  if (ctx->used_transactionals >= ctx->n_transactionals) {
    ctx->transactionals = realloc(ctx->transactionals, ctx->used_transactionals+1);
    ++ctx->n_transactionals;
  }

  ctx->transactionals[ctx->used_transactionals].data = data;
  ctx->transactionals[ctx->used_transactionals].commit = commit;
  ctx->transactionals[ctx->used_transactionals].rollback = rollback;
  ctx->transactionals[ctx->used_transactionals++].free = free;
}


// "Garbage collection"

void uw_expunger(uw_context ctx, uw_Basis_client cli);

static failure_kind uw_expunge(uw_context ctx, uw_Basis_client cli) {
  int r = setjmp(ctx->jmp_buf);

  if (r == 0) {
    if (uw_db_begin(ctx))
      uw_error(ctx, FATAL, "Error running SQL BEGIN");
    uw_expunger(ctx, cli);
    if (uw_db_commit(ctx))
      uw_error(ctx, FATAL, "Error running SQL COMMIT");
  }

  return r;
}

void uw_prune_clients(uw_context ctx) {
  client *c, *next, *prev = NULL;
  time_t cutoff;

  cutoff = time(NULL) - uw_timeout;

  pthread_mutex_lock(&clients_mutex);

  for (c = clients_used; c; c = next) {
    next = c->next;
    pthread_mutex_lock(&c->lock);
    if (c->last_contact < cutoff && c->refcount == 0) {
      failure_kind fk = UNLIMITED_RETRY;
      if (prev)
        prev->next = next;
      else
        clients_used = next;
      uw_reset(ctx);
      while (fk == UNLIMITED_RETRY) {
        fk = uw_expunge(ctx, c->id);
        if (fk == UNLIMITED_RETRY) {
          uw_db_rollback(ctx);
          printf("Unlimited retry during expunge: %s\n", uw_error_message(ctx));
        }
      }
      if (fk == SUCCESS)
        free_client(c);
      else {
        uw_db_rollback(ctx);
        fprintf(stderr, "Expunge blocked by error: %s\n", uw_error_message(ctx));
      }
    }
    else
      prev = c;
    pthread_mutex_unlock(&c->lock);
  }

  pthread_mutex_unlock(&clients_mutex);
}

void uw_initializer(uw_context ctx);

failure_kind uw_initialize(uw_context ctx) {
  int r = setjmp(ctx->jmp_buf);

  if (r == 0) {
    if (uw_db_begin(ctx))
      uw_error(ctx, FATAL, "Error running SQL BEGIN");
    uw_initializer(ctx);
    if (uw_db_commit(ctx))
      uw_error(ctx, FATAL, "Error running SQL COMMIT");
  }

  return r;
}

extern int uw_check_url(const char *);
extern int uw_check_mime(const char *);

uw_Basis_string uw_Basis_bless(uw_context ctx, uw_Basis_string s) {
  if (uw_check_url(s))
    return s;
  else
    uw_error(ctx, FATAL, "Disallowed URL %s", uw_Basis_htmlifyString(ctx, s));
}

uw_Basis_string uw_Basis_checkUrl(uw_context ctx, uw_Basis_string s) {
  if (uw_check_url(s))
    return s;
  else
    return NULL;
}

int mime_format(const char *s) {
  for (; *s; ++s)
    if (!isalnum(*s) && *s != '/' && *s != '-' && *s != '.')
      return 0;

  return 1;
}

uw_Basis_string uw_Basis_blessMime(uw_context ctx, uw_Basis_string s) {
  if (!mime_format(s))
    uw_error(ctx, FATAL, "MIME type \"%s\" contains invalid character", uw_Basis_htmlifyString(ctx, s));

  if (uw_check_mime(s))
    return s;
  else
    uw_error(ctx, FATAL, "Disallowed MIME type %s", uw_Basis_htmlifyString(ctx, s));
}

uw_Basis_string uw_Basis_checkMime(uw_context ctx, uw_Basis_string s) {
  if (!mime_format(s))
    return NULL;

  if (uw_check_mime(s))
    return s;
  else
    return NULL;
}

uw_Basis_string uw_unnull(uw_Basis_string s) {
  return s ? s : "";
}

extern int uw_hash_blocksize;

uw_Basis_string uw_Basis_makeSigString(uw_context ctx, uw_Basis_string sig) {
  uw_Basis_string r = uw_malloc(ctx, 2 * uw_hash_blocksize + 1);
  int i;
  
  for (i = 0; i < uw_hash_blocksize; ++i)
    sprintf(&r[2*i], "%.02X", ((unsigned char *)sig)[i]);

  return r;
}

uw_Basis_string uw_Basis_sigString(uw_context ctx, uw_unit u) {
  return uw_cookie_sig(ctx);
}

uw_Basis_string uw_Basis_fileName(uw_context ctx, uw_Basis_file f) {
  return f.name;
}

uw_Basis_string uw_Basis_fileMimeType(uw_context ctx, uw_Basis_file f) {
  return f.type;
}

uw_Basis_int uw_Basis_blobSize(uw_context ctx, uw_Basis_blob b) {
  return b.size;
}

uw_Basis_blob uw_Basis_fileData(uw_context ctx, uw_Basis_file f) {
  return f.data;
}

__attribute__((noreturn)) void uw_return_blob(uw_context ctx, uw_Basis_blob b, uw_Basis_string mimeType) {
  cleanup *cl;
  int len;

  ctx->returning_blob = 1;
  buf_reset(&ctx->outHeaders);
  buf_reset(&ctx->page);

  uw_write_header(ctx, on_success);
  uw_write_header(ctx, "Content-Type: ");
  uw_write_header(ctx, mimeType);
  uw_write_header(ctx, "\r\nContent-Length: ");
  buf_check(&ctx->outHeaders, INTS_MAX);
  sprintf(ctx->outHeaders.front, "%d%n", b.size, &len);
  ctx->outHeaders.front += len;
  uw_write_header(ctx, "\r\n");  

  buf_append(&ctx->page, b.data, b.size);

  for (cl = ctx->cleanup; cl < ctx->cleanup_front; ++cl)
    cl->func(cl->arg);

  ctx->cleanup_front = ctx->cleanup;

  longjmp(ctx->jmp_buf, RETURN_BLOB);
}

uw_Basis_string uw_Basis_unAs(uw_context ctx, uw_Basis_string s) {
  uw_Basis_string r = uw_malloc(ctx, strlen(s) + 1);

  for (; *s; ++s) {
    if (s[0] == '\'') {
      *r++ = '\'';
      for (++s; *s; ++s) {
        if (s[0] == '\'') {
          *r++ = '\'';
          break;
        } else if (s[0] == '\\') {
          if (s[1] == '\\') {
            *r++ = '\\';
            *r++ = '\\';
            ++s;
          } else if (s[1] == '\'') {
            *r++ = '\\';
            *r++ = '\'';
            ++s;
          } else
            *r++ = '\'';
        } else
          *r++ = s[0];
      }
      if (*s == 0) break;
    } else if (s[0] == 'T' && s[1] == '.')
      ++s;
    else
      *r++ = s[0];
  }

  return r;
}

uw_Basis_string uw_Basis_mstrcat(uw_context ctx, ...) {
  va_list ap;
  size_t len = 1;
  char *s, *r, *s2;
  
  va_start(ap, ctx);
  for (s = va_arg(ap, char*); s; s = va_arg(ap, char*))
    len += strlen(s);
  va_end(ap);

  r = uw_malloc(ctx, len);
  va_start(ap, ctx);
  for (s2 = r, s = va_arg(ap, char*); s; s = va_arg(ap, char*))
    while (*s)
      *s2++ = *s++;
  va_end(ap);
  *s2 = 0;

  return r;
}

const uw_Basis_time minTime = 0;

uw_Basis_time uw_Basis_now(uw_context ctx) {
  return time(NULL);
}
