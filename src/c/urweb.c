#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <setjmp.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <openssl/rand.h>
#include <openssl/sha.h>
#include <time.h>
#include <math.h>

#include <pthread.h>

#include <unicode/utf8.h>
#include <unicode/uchar.h>

#include "types.h"

#include "uthash.h"

uw_unit uw_unit_v = 0;


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

void uw_buffer_init(size_t max, uw_buffer *b, size_t s) {
  b->max = max;
  b->front = b->start = malloc(s);
  b->back = b->front + s;
}

void uw_buffer_free(uw_buffer *b) {
  free(b->start);
}

void uw_buffer_reset(uw_buffer *b) {
  b->front = b->start;
  if (b->front != b->back) {
    *b->front = 0;
  }
}

int uw_buffer_check(uw_buffer *b, size_t extra) {
  if (b->back - b->front < extra) {
    size_t desired = b->front - b->start + extra, next;
    char *new_heap;

    next = b->back - b->start;
    if (next == 0)
      next = 1;
    for (; next < desired; next *= 2);

    if (next > b->max) {
      if (desired <= b->max)
        next = desired;
      else
        return 1;
    }

    new_heap = realloc(b->start, next);
    b->front = new_heap + (b->front - b->start);
    b->back = new_heap + next;
    b->start = new_heap;
  }

  return 0;
}

__attribute__((noreturn)) void uw_error(uw_context, failure_kind, const char *, ...);

static void ctx_uw_buffer_check(uw_context ctx, const char *kind, uw_buffer *b, size_t extra) {
  if (uw_buffer_check(b, extra))
    uw_error(ctx, FATAL, "Memory limit exceeded (%s)", kind);
}

size_t uw_buffer_used(uw_buffer *b) {
  return b->front - b->start;
}

size_t uw_buffer_avail(uw_buffer *b) {
  return b->back - b->start;
}

int uw_buffer_append(uw_buffer *b, const char *s, size_t len) {
  if (uw_buffer_check(b, len+1))
    return 1;

  memcpy(b->front, s, len);
  b->front += len;
  *b->front = 0;

  return 0;
}

static void ctx_uw_buffer_append(uw_context ctx, const char *kind, uw_buffer *b, const char *s, size_t len) {
  ctx_uw_buffer_check(ctx, kind, b, len+1);

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
  uw_buffer msgs;
  int sock;
  int (*send)(int sockfd, const void *buf, ssize_t len);
  int (*close)(int fd);
  time_t last_contact;
  unsigned n_channels;
  unsigned refcount;
  void *data;
} client;


// Persistent client state

static client **clients, *clients_free, *clients_used;
static unsigned n_clients;

static pthread_mutex_t clients_mutex = PTHREAD_MUTEX_INITIALIZER;
size_t uw_messages_max = SIZE_MAX;
size_t uw_clients_max = SIZE_MAX;

void *uw_init_client_data();
void uw_free_client_data(void *);
void uw_copy_client_data(void *dst, void *src);

static int my_rand(uw_context ctx) {
  unsigned int ret;
  if (RAND_bytes((unsigned char *)&ret, sizeof ret)) {
    ret >>= 1; // clear top bit
    return ret;
  } else
    uw_error(ctx, FATAL, "Random number generation failed");
}

static client *new_client(uw_context ctx) {
  client *c;
  int pass = my_rand(ctx);

  pthread_mutex_lock(&clients_mutex);

  if (clients_free) {
    c = clients_free;
    clients_free = clients_free->next;
  }
  else if (n_clients >= uw_clients_max) {
    pthread_mutex_unlock(&clients_mutex);
    return NULL;
  } else {
    ++n_clients;
    clients = realloc(clients, sizeof(client) * n_clients);
    c = malloc(sizeof(client));
    c->id = n_clients-1;
    pthread_mutex_init(&c->lock, NULL);
    pthread_mutex_init(&c->pull_lock, NULL);
    uw_buffer_init(uw_messages_max, &c->msgs, 0);
    clients[n_clients-1] = c;
  }

  pthread_mutex_lock(&c->lock);
  c->mode = USED;
  c->pass = pass;
  c->sock = -1;
  c->last_contact = time(NULL);
  uw_buffer_reset(&c->msgs);
  c->n_channels = 0;
  c->refcount = 0;
  c->data = uw_init_client_data();
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
static pthread_t pruning_thread;
static int pruning_thread_initialized = 0;

static client *find_client(unsigned id) {
  client *c;
  int i_am_pruner = pruning_thread_initialized && pthread_equal(pruning_thread, pthread_self());

  if (!i_am_pruner) pthread_mutex_lock(&clients_mutex);

  if (id >= n_clients) {
    if (!i_am_pruner) pthread_mutex_unlock(&clients_mutex);
    return NULL;
  }

  c = clients[id];

  if (!i_am_pruner) pthread_mutex_unlock(&clients_mutex);
  return c;
}

static char *on_success = "HTTP/1.1 200 OK\r\n";
static char *on_redirect = "HTTP/1.1 303 See Other\r\n";

void uw_set_on_success(char *s) {
  on_success = s;
}

static void chastise(int (*send)(int sockfd, const void *buf, ssize_t len), int sock) {
  send(sock, on_success, strlen(on_success));
  send(sock, begin_msgs, sizeof(begin_msgs) - 1);
  send(sock, "R", 1);
  close(sock);
}

void uw_client_connect(unsigned id, int pass, int sock,
                       int (*send)(int sockfd, const void *buf, ssize_t len),
                       int (*close)(int fd),
                       void *logger_data, uw_logger log_error) {
  client *c = find_client(id);

  if (c == NULL) {
    chastise(send, sock);
    log_error(logger_data, "Out-of-bounds client request (%u)\n", id);
    return;
  }

  pthread_mutex_lock(&c->lock);

  if (c->mode != USED) {
    pthread_mutex_unlock(&c->lock);
    chastise(send, sock);
    log_error(logger_data, "Client request for unused slot (%u)\n", id);
    return;
  }

  if (pass != c->pass) {
    pthread_mutex_unlock(&c->lock);
    chastise(send, sock);
    log_error(logger_data, "Wrong client password (%u, %d)\n", id, pass);
    return;
  }

  if (c->sock != -1) {
    c->close(c->sock);
    c->sock = -1;
  }

  c->last_contact = time(NULL);

  if (uw_buffer_used(&c->msgs) > 0) {
    send(sock, on_success, strlen(on_success));
    send(sock, begin_msgs, sizeof(begin_msgs) - 1);
    send(sock, c->msgs.start, uw_buffer_used(&c->msgs));
    uw_buffer_reset(&c->msgs);
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

static void client_send(client *c, uw_buffer *msg, const char *script, int script_len) {
  pthread_mutex_lock(&c->lock);

  if (c->mode != USED) { }
  else if (c->sock != -1) {
    c->send(c->sock, on_success, strlen(on_success));
    c->send(c->sock, begin_msgs, sizeof(begin_msgs) - 1);
    if (script_len > 0) {
      c->send(c->sock, "E\n", 2);
      c->send(c->sock, script, script_len);
      c->send(c->sock, "\n", 1);
    }
    c->send(c->sock, msg->start, uw_buffer_used(msg));
    c->close(c->sock);
    c->sock = -1;
  } else if (uw_buffer_append(&c->msgs, msg->start, uw_buffer_used(msg)))
    fprintf(stderr, "Client message buffer size exceeded");

  pthread_mutex_unlock(&c->lock);
}


// Global entry points

extern void uw_global_custom();
extern void uw_init_crypto();

void uw_global_init() {
  clients = malloc(0);

  uw_global_custom();
  uw_init_crypto();

  // Fast non-cryptographic strength randomness for Sqlcache.
  srandom(clock());
}

void uw_app_init(uw_app *app) {
  app->client_init();
}

int uw_time = 0, uw_time_max = 0, uw_min_heap = 0;


// Single-request state

typedef struct regions {
  struct regions *next;
} regions;

typedef struct {
  void (*func)(void*);
  void *arg;
} cleanup;

typedef struct {
  unsigned client;
  uw_buffer msgs;
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
  uw_callback commit, rollback;
  uw_callback_with_retry free;
} transactional;

typedef struct {
  char *name;
  void *data;
  void (*free)(void*);
} global;

typedef struct uw_Sqlcache_Update {
  uw_Sqlcache_Cache *cache;
  char **keys;
  uw_Sqlcache_Value *value;
  struct uw_Sqlcache_Update *next;
} uw_Sqlcache_Update;

typedef struct uw_Sqlcache_Unlock {
  pthread_rwlock_t *lock;
  struct uw_Sqlcache_Unlock *next;
} uw_Sqlcache_Unlock;

struct uw_context {
  uw_app *app;
  int id;

  char *(*get_header)(void *, const char *);
  void *get_header_data;

  char *(*get_env)(void *, const char *);
  void *get_env_data;

  uw_buffer outHeaders, page, heap, script;
  int allowed_to_return_indirectly, returning_indirectly;
  input *inputs, *subinputs, *cur_container;
  size_t sz_inputs, n_subinputs, used_subinputs;

  unsigned long long source_count;

  void *db;
  int transaction_started;

  jmp_buf jmp_buf;

  regions *regions;

  cleanup *cleanup, *cleanup_front, *cleanup_back;

  const char *script_header;

  int needs_push, needs_sig, could_write_db, at_most_one_query;

  size_t n_deltas, used_deltas;
  delta *deltas;

  client *client;

  transactional *transactionals;
  size_t n_transactionals, used_transactionals;

  global *globals;
  size_t n_globals;

  char *current_url;

  int deadline;

  void *client_data;

  uw_loggers *loggers;

  int isPost, hasPostBody;
  uw_Basis_postBody postBody;
  uw_Basis_string queryString;

  unsigned nextId;

  int amInitializing;

  char error_message[ERROR_BUF_LEN];

  int usedSig, needsResig;

  char *output_buffer;
  size_t output_buffer_size;

  // Sqlcache.
  int numRecording, recordingCapacity;
  int *recordingOffsets, *scriptRecordingOffsets;
  uw_Sqlcache_Update *cacheUpdate;
  uw_Sqlcache_Update *cacheUpdateTail;
  uw_Sqlcache_Unlock *cacheUnlock;

  int remoteSock;

  int file_cache_missed;
  // Set if we are recovering from a miss in the file cache in handling an SQL
  // query that only returns hashes of files.  If so, this time around we will
  // run queries to return actual file contents instead.
};

size_t uw_headers_max = SIZE_MAX;
size_t uw_page_max = SIZE_MAX;
size_t uw_heap_max = SIZE_MAX;
size_t uw_script_max = SIZE_MAX;

uw_context uw_init(int id, uw_loggers *lg) {
  uw_context ctx = malloc(sizeof(struct uw_context));

  ctx->app = NULL;
  ctx->id = id;

  ctx->get_header = NULL;
  ctx->get_header_data = NULL;

  ctx->get_env = NULL;
  ctx->get_env_data = NULL;

  uw_buffer_init(uw_headers_max, &ctx->outHeaders, 1);
  ctx->outHeaders.start[0] = 0;
  uw_buffer_init(uw_page_max, &ctx->page, 1);
  ctx->page.start[0] = 0;
  ctx->allowed_to_return_indirectly = ctx->returning_indirectly = 0;
  uw_buffer_init(uw_heap_max, &ctx->heap, uw_min_heap);
  uw_buffer_init(uw_script_max, &ctx->script, 1);
  ctx->script.start[0] = 0;

  ctx->inputs = malloc(0);
  ctx->cur_container = NULL;
  ctx->subinputs = malloc(0);
  ctx->sz_inputs = ctx->n_subinputs = ctx->used_subinputs = 0;

  ctx->db = NULL;
  ctx->transaction_started = 0;

  ctx->regions = NULL;

  ctx->cleanup_front = ctx->cleanup_back = ctx->cleanup = malloc(0);

  ctx->script_header = "";
  ctx->needs_push = 0;
  ctx->needs_sig = 0;
  ctx->could_write_db = 1;
  ctx->at_most_one_query = 0;

  ctx->source_count = 0;

  ctx->n_deltas = ctx->used_deltas = 0;
  ctx->deltas = malloc(0);

  ctx->client = NULL;

  ctx->error_message[0] = 0;

  ctx->transactionals = malloc(0);
  ctx->n_transactionals = ctx->used_transactionals = 0;

  ctx->globals = malloc(0);
  ctx->n_globals = 0;

  ctx->current_url = "";

  ctx->deadline = INT_MAX;

  ctx->client_data = uw_init_client_data();

  ctx->loggers = lg;

  ctx->isPost = ctx->hasPostBody = 0;

  ctx->queryString = NULL;

  ctx->nextId = 0;

  ctx->amInitializing = 0;

  ctx->usedSig = 0;
  ctx->needsResig = 0;

  ctx->output_buffer = malloc(1);
  ctx->output_buffer_size = 1;

  ctx->numRecording = 0;
  ctx->recordingCapacity = 0;
  ctx->recordingOffsets = malloc(0);
  ctx->scriptRecordingOffsets = malloc(0);
  ctx->cacheUpdate = NULL;
  ctx->cacheUpdateTail = NULL;

  ctx->remoteSock = -1;

  ctx->cacheUnlock = NULL;

  ctx->file_cache_missed = 0;

  return ctx;
}

size_t uw_inputs_max = SIZE_MAX;

uw_app *uw_get_app(uw_context ctx) {
  return ctx->app;
}

int uw_set_app(uw_context ctx, uw_app *app) {
  ctx->app = app;

  if (app && app->inputs_len > ctx->sz_inputs) {
    if (app->inputs_len > uw_inputs_max)
      return 1;

    ctx->sz_inputs = app->inputs_len;
    ctx->inputs = realloc(ctx->inputs, ctx->sz_inputs * sizeof(input));
    memset(ctx->inputs, 0, ctx->sz_inputs * sizeof(input));
  }

  return 0;
}

void uw_set_client_data(uw_context ctx, void *data) {
  uw_copy_client_data(ctx->client_data, data);
}

void uw_set_db(uw_context ctx, void *db) {
  ctx->db = db;
}

void *uw_get_db(uw_context ctx) {
  return ctx->db;
}


uw_loggers* uw_get_loggers(struct uw_context *ctx) {
  return ctx->loggers;
}

void uw_free(uw_context ctx) {
  size_t i;

  uw_buffer_free(&ctx->outHeaders);
  uw_buffer_free(&ctx->script);
  uw_buffer_free(&ctx->page);
  uw_buffer_free(&ctx->heap);
  free(ctx->inputs);
  free(ctx->subinputs);
  free(ctx->cleanup);
  free(ctx->transactionals);
  uw_free_client_data(ctx->client_data);

  for (i = 0; i < ctx->n_deltas; ++i)
    uw_buffer_free(&ctx->deltas[i].msgs);
  free(ctx->deltas);

  for (i = 0; i < ctx->n_globals; ++i)
    if (ctx->globals[i].free)
      ctx->globals[i].free(ctx->globals[i].data);
  free(ctx->globals);

  free(ctx->output_buffer);

  free(ctx->recordingOffsets);
  free(ctx->scriptRecordingOffsets);

  free(ctx);
}

void uw_reset_keep_error_message(uw_context ctx) {
  uw_buffer_reset(&ctx->outHeaders);
  uw_buffer_reset(&ctx->script);
  ctx->script.start[0] = 0;
  uw_buffer_reset(&ctx->page);
  ctx->allowed_to_return_indirectly = ctx->returning_indirectly = 0;
  uw_buffer_reset(&ctx->heap);
  ctx->regions = NULL;
  ctx->cleanup_front = ctx->cleanup;
  ctx->used_deltas = 0;
  ctx->client = NULL;
  ctx->cur_container = NULL;
  ctx->used_transactionals = 0;
  ctx->script_header = "";
  ctx->queryString = NULL;
  ctx->nextId = 0;
  ctx->amInitializing = 0;
  ctx->usedSig = 0;
  ctx->needsResig = 0;
  ctx->remoteSock = -1;
  ctx->numRecording = 0;
}

void uw_reset_keep_request(uw_context ctx) {
  uw_reset_keep_error_message(ctx);
  ctx->error_message[0] = 0;
}

void uw_reset(uw_context ctx) {
  uw_reset_keep_request(ctx);
  if (ctx->app)
    memset(ctx->inputs, 0, ctx->app->inputs_len * sizeof(input));
  memset(ctx->subinputs, 0, ctx->n_subinputs * sizeof(input));
  ctx->used_subinputs = ctx->hasPostBody = ctx->isPost = 0;
  ctx->transaction_started = 0;
}

failure_kind uw_begin_init(uw_context ctx) {
  int r = setjmp(ctx->jmp_buf);

  if (r == 0 && ctx->app)
    ctx->app->db_init(ctx);

  return r;
}

void uw_close(uw_context ctx) {
  ctx->app->db_close(ctx);
}

uw_Basis_string uw_Basis_requestHeader(uw_context ctx, uw_Basis_string h) {
  if (ctx->get_header)
    return ctx->get_header(ctx->get_header_data, h);
  else
    return NULL;
}

void uw_set_headers(uw_context ctx, char *(*get_header)(void *, const char *), void *get_header_data) {
  ctx->get_header = get_header;
  ctx->get_header_data = get_header_data;
}

void uw_set_env(uw_context ctx, char *(*get_env)(void *, const char *), void *get_env_data) {
  ctx->get_env = get_env;
  ctx->get_env_data = get_env_data;
}

static void uw_set_error(uw_context ctx, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);

  vsnprintf(ctx->error_message, ERROR_BUF_LEN, fmt, ap);
}

int uw_has_error(uw_context ctx) {
  return ctx->error_message[0] != 0;
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

size_t uw_cleanup_max = SIZE_MAX;

void uw_push_cleanup(uw_context ctx, void (*func)(void *), void *arg) {
  if (ctx->cleanup_front >= ctx->cleanup_back) {
    int len = ctx->cleanup_back - ctx->cleanup, newLen;
    if (len == 0)
      newLen = 1;
    else
      newLen = len * 2;

    if (newLen > uw_cleanup_max) {
      if (len+1 <= uw_cleanup_max)
        newLen = uw_cleanup_max;
      else
        uw_error(ctx, FATAL, "Exceeded limit on number of cleanup handlers");
    }

    ctx->cleanup = realloc(ctx->cleanup, newLen * sizeof(cleanup));
    ctx->cleanup_front = ctx->cleanup + len;
    ctx->cleanup_back = ctx->cleanup + newLen;
  }

  ctx->cleanup_front->func = func;
  ctx->cleanup_front->arg = arg;
  ++ctx->cleanup_front;
}

char *uw_Basis_htmlifyString(uw_context, const char *);

void uw_login(uw_context ctx) {
  char *id_s, *pass_s;

  if ((id_s = uw_Basis_requestHeader(ctx, "UrWeb-Client"))
      && (pass_s = uw_Basis_requestHeader(ctx, "UrWeb-Pass"))) {
    unsigned id = atoi(id_s);
    int pass = atoi(pass_s);
    client *c = find_client(id);

    if (c == NULL)
      uw_error(ctx, FATAL, "Unknown client ID in HTTP headers (%s, %s)", uw_Basis_htmlifyString(ctx, id_s), uw_Basis_htmlifyString(ctx, pass_s));
    else {
      use_client(c);
      ctx->client = c;

      if (c->mode != USED)
        uw_error(ctx, FATAL, "Stale client ID (%u) in subscription request", id);
      if (c->pass != pass)
        uw_error(ctx, FATAL, "Wrong client password (%u, %d) in subscription request", id, pass);
    }
  } else if (ctx->needs_push) {
    client *c = new_client(ctx);

    if (c == NULL)
      uw_error(ctx, FATAL, "Limit exceeded on number of message-passing clients");

    use_client(c);
    uw_copy_client_data(c->data, ctx->client_data);
    ctx->client = c;
  }
}

// It gets particularly hard to avoid concurrency anomalies in connection with
// the expunger thread, which garbage-collects unused clients, so that their IDs
// can be reused.  To save ourselves from needing to deal with those anomalies,
// we enforce that the expunger never runs simultaneously with other transactions.

static pthread_rwlock_t expunge_lock = PTHREAD_RWLOCK_INITIALIZER;

void uw_transaction_arrives() {
  pthread_rwlock_rdlock(&expunge_lock);
}

void uw_transaction_departs() {
  pthread_rwlock_unlock(&expunge_lock);
}

static void uw_expunger_arrives() {
  pthread_rwlock_wrlock(&expunge_lock);
}

static void uw_expunger_departs() {
  pthread_rwlock_unlock(&expunge_lock);
}

failure_kind uw_begin(uw_context ctx, char *path) {
  int r = setjmp(ctx->jmp_buf);

  if (r == 0)
    ctx->app->handle(ctx, path);

  return r;
}

static void uw_try_reconnecting(uw_context ctx) {
  // Hm, error starting transaction.
  // Maybe the database server died but has since come back up.
  // Let's try starting from scratch.
  if (ctx->db) {
    ctx->app->db_close(ctx);
    ctx->db = NULL;
  }
  ctx->app->db_init(ctx);
}

void uw_try_reconnecting_and_restarting(uw_context ctx) {
  uw_try_reconnecting(ctx);
  uw_error(ctx, BOUNDED_RETRY, "Restarting transaction after fixing database connection");
}

void uw_ensure_transaction(uw_context ctx) {
  if (!ctx->transaction_started && !ctx->at_most_one_query) {
    if (!ctx->db || ctx->app->db_begin(ctx, ctx->could_write_db)) {
      uw_try_reconnecting(ctx);

      if (ctx->app->db_begin(ctx, ctx->could_write_db))
        uw_error(ctx, FATAL, "Error running SQL BEGIN");
    }

    ctx->transaction_started = 1;
  } else if (ctx->at_most_one_query && !ctx->db)
    uw_try_reconnecting(ctx);
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

void uw_set_error_message(uw_context ctx, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  
  vsnprintf(ctx->error_message, ERROR_BUF_LEN, fmt, ap);
  ctx->error_message[ERROR_BUF_LEN-1] = 0;
}

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
    break;
  default:
    break;
  }
}

size_t uw_subinputs_max = SIZE_MAX;

static input *check_input_space(uw_context ctx, size_t len) {
  size_t i;
  input *r;

  if (ctx->used_subinputs + len >= ctx->n_subinputs) {
    if (ctx->used_subinputs + len > uw_subinputs_max)
      uw_error(ctx, FATAL, "Exceeded limit on number of subinputs");

    input *new_subinputs = realloc(ctx->subinputs, sizeof(input) * (ctx->used_subinputs + len));

    if (ctx->subinputs != new_subinputs) {
      for (i = 0; i < ctx->used_subinputs; ++i)
        adjust_input(&new_subinputs[i], ctx->subinputs, new_subinputs, ctx->used_subinputs);
      for (i = 0; i < ctx->app->inputs_len; ++i)
        adjust_input(&ctx->inputs[i], ctx->subinputs, new_subinputs, ctx->used_subinputs);

      adjust_pointer(&ctx->cur_container, ctx->subinputs, new_subinputs, ctx->used_subinputs);

      ctx->n_subinputs = ctx->used_subinputs + len;
      ctx->subinputs = new_subinputs;
    }
  }

  r = &ctx->subinputs[ctx->used_subinputs];

  for (i = 0; i < len; ++i)
    ctx->subinputs[ctx->used_subinputs++].kind = UNSET;

  return r;
}

int uw_set_input(uw_context ctx, const char *name, char *value) {
  //printf("Input name %s\n", name);

  if (!strcasecmp(name, ".b")) {
    int n = ctx->app->input_num(value);
    input *inps;

    if (n < 0) {
      uw_set_error(ctx, "Bad subform name %s", uw_Basis_htmlifyString(ctx, value));
      return -1;
    }

    if (n >= ctx->app->inputs_len) {
      uw_set_error(ctx, "For subform name %s, index %d is out of range", uw_Basis_htmlifyString(ctx, value), n);
      return -1;
    }

    inps = check_input_space(ctx, ctx->app->inputs_len);

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
    int n = ctx->app->input_num(value);

    if (n < 0) {
      uw_set_error(ctx, "Bad subforms name %s", uw_Basis_htmlifyString(ctx, value));
      return -1;
    }

    if (n >= ctx->app->inputs_len) {
      uw_set_error(ctx, "For subforms name %s, index %d is out of range", uw_Basis_htmlifyString(ctx, value), n);
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

    inps = check_input_space(ctx, ctx->app->inputs_len + 1);

    inps->kind = ENTRY;
    inps->data.entry.parent = ctx->cur_container;
    inps->data.entry.next = ctx->cur_container->data.subforms.entries;
    ctx->cur_container->data.subforms.entries = inps;

    inps->data.entry.fields = inps+1;
    ctx->cur_container = inps;
  } else {
    int n = ctx->app->input_num(name);

    if (n < 0)
      return 0;

    if (n >= ctx->app->inputs_len) {
      uw_set_error(ctx, "For input name %s, index %d is out of range", uw_Basis_htmlifyString(ctx, name), n);
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
  if (n >= ctx->app->inputs_len)
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
  if (n >= ctx->app->inputs_len)
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
  int n = ctx->app->input_num(name);

  if (n < 0) {
    uw_set_error(ctx, "Bad file input name");
    return -1;
  }

  if (n >= ctx->app->inputs_len) {
    uw_set_error(ctx, "For file input name, index %d is out of range", n);
    return -1;
  }

  ctx->inputs[n].kind = FIL;
  ctx->inputs[n].data.file = f;

  return 0;
}

void *uw_malloc(uw_context ctx, size_t len);

uw_Basis_file uw_get_file_input(uw_context ctx, int n) {
  if (n < 0)
    uw_error(ctx, FATAL, "Negative file input index %d", n);
  if (n >= ctx->app->inputs_len)
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
  if (n >= ctx->app->inputs_len)
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
  if (n >= ctx->app->inputs_len)
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

const char *uw_get_url_prefix(uw_context ctx) {
  return ctx->app->url_prefix;
}

void uw_set_needs_push(uw_context ctx, int n) {
  ctx->needs_push = n;
}

void uw_set_needs_sig(uw_context ctx, int n) {
  ctx->needs_sig = n;
}

void uw_set_could_write_db(uw_context ctx, int n) {
  ctx->could_write_db = n;
}

void uw_set_at_most_one_query(uw_context ctx, int n) {
  ctx->at_most_one_query = n;
}


static void uw_buffer_check_ctx(uw_context ctx, const char *kind, uw_buffer *b, size_t extra, const char *desc) {
  if (b->back - b->front < extra) {
    size_t desired = b->front - b->start + extra, next;
    char *new_heap;

    next = b->back - b->start;
    if (next == 0)
      next = 1;
    for (; next < desired; next *= 2);

    if (next > b->max) {
      if (desired <= b->max)
        next = desired;
      else
        uw_error(ctx, FATAL, "Memory limit exceeded (%s)", kind);
    }

    new_heap = realloc(b->start, next);
    b->front = new_heap + (b->front - b->start);
    b->back = new_heap + next;

    if (new_heap != b->start) {
      b->start = new_heap;
      uw_error(ctx, UNLIMITED_RETRY, "Couldn't allocate new %s contiguously; increasing size to %llu", desc, (unsigned long long)next);
    }

    b->start = new_heap;
  }
}

void uw_check_heap(uw_context ctx, size_t extra) {
  uw_buffer_check_ctx(ctx, "heap", &ctx->heap, extra, "heap chunk");
}

char *uw_heap_front(uw_context ctx) {
  return ctx->heap.front;
}

void uw_set_heap_front(uw_context ctx, char *fr) {
  ctx->heap.front = fr;
}

void uw_begin_initializing(uw_context ctx) {
  ctx->amInitializing = 1;
}

void uw_end_initializing(uw_context ctx) {
  ctx->amInitializing = 0;
}

static void align_heap(uw_context ctx) {
  size_t posn = ctx->heap.front - ctx->heap.start;

  if (posn % sizeof(void *) != 0) {
    size_t bump = sizeof(void *) - posn % sizeof(void *);
    uw_check_heap(ctx, bump);
    ctx->heap.front += bump;
  }
}

void *uw_malloc(uw_context ctx, size_t len) {
  // On some architectures, it's important that all word-sized memory accesses
  // be to word-aligned addresses, so we'll do a little bit of extra work here
  // in anticipation of a possible word-aligned access to the address we'll
  // return.

  void *result;

  if (ctx->amInitializing) {
    int error = posix_memalign(&result, sizeof(void *), len);

    if (!error)
      return result;
    else
      uw_error(ctx, FATAL, "uw_malloc: posix_memalign() returns %d", error);
  } else {
    align_heap(ctx);

    uw_check_heap(ctx, len);

    result = ctx->heap.front;
    ctx->heap.front += len;
    return result;
  }
}

void uw_begin_region(uw_context ctx) {
  align_heap(ctx);

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
  printf("Headers: %lu/%lu\n", (unsigned long)uw_buffer_used(&ctx->outHeaders), (unsigned long)uw_buffer_avail(&ctx->outHeaders));
  printf("Script: %lu/%lu\n", (unsigned long)uw_buffer_used(&ctx->script), (unsigned long)uw_buffer_avail(&ctx->script));
  printf("Page: %lu/%lu\n", (unsigned long)uw_buffer_used(&ctx->page), (unsigned long)uw_buffer_avail(&ctx->page));
  printf("Heap: %lu/%lu\n", (unsigned long)uw_buffer_used(&ctx->heap), (unsigned long)uw_buffer_avail(&ctx->heap));
}

int uw_pagelen(uw_context ctx) {
  return ctx->page.front - ctx->page.start;
}

int uw_send(uw_context ctx, int sock) {
  size_t target_length = (ctx->outHeaders.front - ctx->outHeaders.start) + 2 + (ctx->page.front - ctx->page.start);

  if (ctx->output_buffer_size < target_length) {
    do {
      ctx->output_buffer_size *= 2;
    } while (ctx->output_buffer_size < target_length);
    ctx->output_buffer = realloc(ctx->output_buffer, ctx->output_buffer_size);
  }

  memcpy(ctx->output_buffer, ctx->outHeaders.start, ctx->outHeaders.front - ctx->outHeaders.start);
  memcpy(ctx->output_buffer + (ctx->outHeaders.front - ctx->outHeaders.start), "\r\n", 2);
  memcpy(ctx->output_buffer + (ctx->outHeaders.front - ctx->outHeaders.start) + 2, ctx->page.start, ctx->page.front - ctx->page.start);

  return uw_really_send(sock, ctx->output_buffer, target_length);
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
  ctx_uw_buffer_check(ctx, "headers", &ctx->outHeaders, extra);
}

void uw_write_header(uw_context ctx, uw_Basis_string s) {
  int len = strlen(s);

  uw_check_headers(ctx, len + 1);
  strcpy(ctx->outHeaders.front, s);
  ctx->outHeaders.front += len;
}

int uw_has_contentLength(uw_context ctx) {
  return strstr(ctx->outHeaders.start, "Content-length: ") != NULL;
}

void uw_clear_headers(uw_context ctx) {
  uw_buffer_reset(&ctx->outHeaders);
}

void uw_Basis_clear_page(uw_context ctx) {
  uw_buffer_reset(&ctx->page);
}

static void uw_check_script(uw_context ctx, size_t extra) {
  ctx_uw_buffer_check(ctx, "script", &ctx->script, extra);
}

void uw_write_script(uw_context ctx, uw_Basis_string s) {
  int len = strlen(s);

  uw_check_script(ctx, len + 1);
  strcpy(ctx->script.front, s);
  ctx->script.front += len;
}

const char *uw_get_real_script(uw_context ctx) {
  if (strstr(ctx->outHeaders.start, "Set-Cookie: ")) {
    uw_write_script(ctx, "sig=\"");
    uw_write_script(ctx, ctx->app->cookie_sig(ctx));
    uw_write_script(ctx, "\";");
  }

  return ctx->script.start;
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
    char *r = uw_malloc(ctx, 37 + strlen(s));
    sprintf(r, " onunload='unloading=true;%s;unload()'", s);
    return r;
  }
}

const char *uw_Basis_get_settings(uw_context ctx, uw_unit u) {
  (void)u;
  if (ctx->client == NULL) {
    if (ctx->needs_sig) {
      char *sig = ctx->app->cookie_sig(ctx);
      char *r = uw_malloc(ctx, strlen(sig) + 8);
      sprintf(r, "sig=\"%s\";", sig);
      return r;
    }
    else
      return "";
  } else {
    char *sig = ctx->needs_sig ? ctx->app->cookie_sig(ctx) : "";
    char *r = uw_malloc(ctx, 59 + 3 * INTS_MAX + strlen(ctx->app->url_prefix)
                        + (ctx->needs_sig ? strlen(sig) + 7 : 0));
    sprintf(r, "isPost=%s;client_id=%u;client_pass=%d;url_prefix=\"%s\";timeout=%d;%s%s%slistener();",
            (ctx->isPost ? "true" : "false"),
            ctx->client->id,
            ctx->client->pass,
            ctx->app->url_prefix,
            ctx->app->timeout,
            ctx->needs_sig ? "sig=\"" : "",
            sig,
            ctx->needs_sig ? "\";" : "");
    return r;
  }
}

uw_Basis_bool uw_Basis_isprint(uw_context ctx, uw_Basis_char ch);

static void jsifyChar(char **buffer_ptr, uw_context ctx, uw_Basis_char c1) {
  char* buffer = *buffer_ptr;
  
  switch (c1) {
  case '"':
    strcpy(buffer, "\\\"");
    buffer += 2;
    break;
  case '\'':
    strcpy(buffer, "\\047");
    buffer += 4;
    break;
  case '\\':
    strcpy(buffer, "\\\\");
    buffer += 2;
    break;
  case '<':
    strcpy(buffer, "\\074");
    buffer += 4;
    break;
  case '&':
    strcpy(buffer, "\\046");
    buffer += 4;
    break;
  default:
    if (uw_Basis_isprint(ctx, c1)) {
      int offset = 0;
      U8_APPEND_UNSAFE(buffer, offset, c1);
      buffer += offset;
    } else {
      if(65536 > c1) {
	sprintf(buffer, "\\u%04x", c1);
	buffer += 6;
      } else {
	sprintf(buffer, "\\u{%06x}", c1);
	buffer += 10;
      }
    }
  }

  *buffer_ptr = buffer;
}

uw_Basis_string uw_Basis_jsifyString(uw_context ctx, uw_Basis_string s) {
  char *r, *s2;
  uw_Basis_char c;

  uw_check_heap(ctx, strlen(s) * 10 + 3);

  r = s2 = ctx->heap.front;
  *s2++ = '"';

  int offset = 0;
  while(s[offset] != 0)
    {
      U8_NEXT(s, offset, -1, c);
      
      jsifyChar(&s2, ctx, c);      
    }

  strcpy(s2, "\"");
  ctx->heap.front = s2 + 2;

  return r;
}

uw_Basis_int uw_Basis_ord(uw_context ctx, uw_Basis_char c);

uw_Basis_string uw_Basis_jsifyChar(uw_context ctx, uw_Basis_char c1) {
  char *r, *s2;

  uw_check_heap(ctx, 10);

  r = s2 = ctx->heap.front;
  
  *s2++ = '"';
  
  jsifyChar(&s2, ctx, c1);

  strcpy(s2, "\"");
  ctx->heap.front = s2 + 2;

  return r;
}

uw_Basis_string uw_Basis_jsifyString_ws(uw_context ctx, uw_Basis_string s) {
  char *r, *s2;

  uw_check_script(ctx, strlen(s) * 4 + 3);

  r = s2 = ctx->script.front;
  *s2++ = '"';

  for (; *s; s++) {
    unsigned char c = *s;

    switch (c) {
    case '\'':
      strcpy(s2, "\\");
      s2 += 2;
      break;
    case '\\':
      strcpy(s2, "\\\\");
      s2 += 2;
      break;
    case '<':
      strcpy(s2, "\\074");
      s2 += 4;
      break;
    case '&':
      strcpy(s2, "\\046");
      s2 += 4;
      break;
    default:
      if (isprint((int)c) || c >= 128)
        *s2++ = c;
      else {
        sprintf(s2, "\\%03o", c);
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

uw_Basis_source uw_Basis_new_client_source(uw_context ctx, uw_Basis_string s) {
  int len;
  size_t s_len = strlen(s);

  if(ctx->id < 0)
    uw_error(ctx, FATAL, "Attempt to create client source using inappropriate context");

  uw_check_script(ctx, 15 + 2 * INTS_MAX + s_len);
  sprintf(ctx->script.front, "s%d_%llu=sc(exec(%n", ctx->id, ctx->source_count, &len);
  ctx->script.front += len;
  strcpy(ctx->script.front, s);
  ctx->script.front += s_len;
  strcpy(ctx->script.front, "));");
  ctx->script.front += 3;

  uw_Basis_source r = {ctx->id, ctx->source_count++};
  return r;
}

uw_unit uw_Basis_set_client_source(uw_context ctx, uw_Basis_source src, uw_Basis_string s) {
  int len;
  size_t s_len = strlen(s);

  uw_check_script(ctx, 15 + 2 * INTS_MAX + s_len);
  sprintf(ctx->script.front, "sv(s%d_%llu,exec(%n", src.context, src.source, &len);
  ctx->script.front += len;
  strcpy(ctx->script.front, s);
  ctx->script.front += s_len;
  strcpy(ctx->script.front, "));");
  ctx->script.front += 3;

  return uw_unit_v;
}

static void uw_check(uw_context ctx, size_t extra) {
  ctx_uw_buffer_check(ctx, "page", &ctx->page, extra);
}

static void uw_writec_unsafe(uw_context ctx, char c) {
  *(ctx->page.front)++ = c;
  *ctx->page.front = 0;
}

void uw_writec(uw_context ctx, char c) {
  uw_check(ctx, 2);
  uw_writec_unsafe(ctx, c);
}

void uw_Basis_writec(uw_context ctx, char c) {
  uw_writec(ctx, c);
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

void uw_recordingStart(uw_context ctx) {
  if (ctx->numRecording == ctx->recordingCapacity) {
    ++ctx->recordingCapacity;
    ctx->recordingOffsets = realloc(ctx->recordingOffsets, sizeof(int) * ctx->recordingCapacity);
    ctx->scriptRecordingOffsets = realloc(ctx->scriptRecordingOffsets, sizeof(int) * ctx->recordingCapacity);
  }
  ctx->recordingOffsets[ctx->numRecording] = ctx->page.front - ctx->page.start;
  ctx->scriptRecordingOffsets[ctx->numRecording] = ctx->script.front - ctx->script.start;
  ++ctx->numRecording;
}

char *uw_recordingRead(uw_context ctx) {
  char *recording = ctx->page.start + ctx->recordingOffsets[ctx->numRecording-1];
  return strdup(recording);
}

char *uw_recordingReadScript(uw_context ctx) {
  char *recording = ctx->script.start + ctx->scriptRecordingOffsets[--ctx->numRecording];
  return strdup(recording);
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
  sprintf(result, "%.16g%n", n, &len);
  ctx->heap.front += len+1;
  return result;
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
  (void)ctx;
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

static void aux_urlifyChar(char** ptr, uw_Basis_char c) {
  char* p = *ptr;
  
  if((uint32_t)(c) <= 0x7f) {
    sprintf(p, ".%02X", (uint8_t)(c));
    p += 3;
  } else {
    if((uint32_t)(c) <= 0x7ff) {
      sprintf(p, ".%02X", (uint8_t)(((c)>>6)|0xc0));
      p += 3;
    } else {
      if((uint32_t)(c) <= 0xffff) { 
	sprintf(p, ".%02X", (uint8_t)(((c)>>12)|0xe0));
	p += 3;
      } else { 
	sprintf(p, ".%02X", (uint8_t)(((c)>>18)|0xf0));
	p += 3;
	sprintf(p, ".%02X", (uint8_t)((((c)>>12)&0x3f)|0x80));
	p += 3;
      } 
      sprintf(p, ".%02X", (uint8_t)((((c)>>6)&0x3f)|0x80));
      p += 3;
    } 
    sprintf(p, ".%02X", (uint8_t)(((c)&0x3f)|0x80));
    p += 3;
  }

  *ptr = p;
}

char *uw_Basis_urlifyString(uw_context ctx, uw_Basis_string s) {
  char *r, *p;

  if (s[0] == '\0')
    return "_";

  uw_check_heap(ctx, strlen(s) * 12 + 1 + !!(s[0] == '_'));

  r = p = ctx->heap.front;
  if (s[0] == '_')
    *p++ = '_';

  uw_Basis_char c;
  int offset = 0, curr = 0;
  while (s[offset] != 0) {
    U8_NEXT(s, offset, -1, c);
  
    if (U8_IS_SINGLE(s[curr]) && s[curr] == ' ')
      *p++ = '+';
    else if (U8_IS_SINGLE(s[curr]) && isalnum(s[curr]))
      *p++ = s[curr];
    else {
      aux_urlifyChar(&p, c);
    }
    curr = offset;
  }

  *p++ = 0;
  ctx->heap.front = p;
  return r;
}

char *uw_Basis_urlifyBool(uw_context ctx, uw_Basis_bool b) {
  (void)ctx;
  if (!b)
    return "0";
  else
    return "1";
}

char *uw_Basis_urlifySource(uw_context ctx, uw_Basis_source src) {
  char *r;
  int len;
  uw_check_heap(ctx, 2 * INTS_MAX + 2);
  r = ctx->heap.front;
  sprintf(r, "%d/%llu%n", src.context, src.source, &len);
  ctx->heap.front += len+1;
  return r;
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
  return uw_Basis_urlifyInt(ctx, (uw_Basis_int)t.seconds * 1000000 + t.microseconds);
}

uw_unit uw_Basis_urlifyTime_w(uw_context ctx, uw_Basis_time t) {
  return uw_Basis_urlifyInt_w(ctx, (uw_Basis_int)t.seconds * 1000000 + t.microseconds);
}

uw_unit uw_Basis_urlifyChar_w(uw_context ctx, uw_Basis_char c) {
  if (c == '\0') {
    uw_check(ctx, 1);
    uw_writec_unsafe(ctx, '_');
    return uw_unit_v;
  }

  uw_check(ctx, 12 + !!(c == '_'));

  if (c == '_')
    uw_writec_unsafe(ctx, '_');
  
  if (c == ' ')
    uw_writec_unsafe(ctx, '+');
  else if (isalnum(c) && c <= 0x7f)
    uw_writec_unsafe(ctx, c);
  else {
    aux_urlifyChar(&(ctx->page.front), c);
  }
  
  return uw_unit_v;
}

uw_unit uw_Basis_urlifyString_w(uw_context ctx, uw_Basis_string s) {
  if (s[0] == '\0') {
    uw_check(ctx, 1);
    uw_writec_unsafe(ctx, '_');
    return uw_unit_v;
  }

  uw_check(ctx, strlen(s) * 12 + !!(s[0] == '_'));

  if (s[0] == '_')
    uw_writec_unsafe(ctx, '_');

  uw_Basis_char c;
  int offset = 0, curr = 0;
  while (s[offset] != 0) {
    U8_NEXT(s, offset, -1, c);   
    
    if (U8_IS_SINGLE(s[curr]) && s[curr] == ' ')
      uw_writec_unsafe(ctx, '+');
    else if (U8_IS_SINGLE(s[curr]) && isalnum(s[curr]))
      uw_writec_unsafe(ctx, s[curr]);
    else {      
      aux_urlifyChar(&(ctx->page.front),  c);
    }
    curr = offset;
  }

  return uw_unit_v;
}

uw_unit uw_Basis_urlifyBool_w(uw_context ctx, uw_Basis_bool b) {
  if (!b)
    uw_writec(ctx, '0');
  else
    uw_writec(ctx, '1');

  return uw_unit_v;
}

uw_unit uw_Basis_urlifySource_w(uw_context ctx, uw_Basis_source src) {
  int len;

  uw_check(ctx, 2 * INTS_MAX + 2);
  sprintf(ctx->page.front, "%d/%llu%n", src.context, src.source, &len);
  ctx->page.front += len;

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
  (void)ctx;

  char *new_s = uw_unurlify_advance(*s);
  uw_Basis_int r;

  r = atoll(*s);
  *s = new_s;
  return r;
}

uw_Basis_float uw_Basis_unurlifyFloat(uw_context ctx, char **s) {
  (void)ctx;

  char *new_s = uw_unurlify_advance(*s);
  uw_Basis_float r;

  r = atof(*s);
  *s = new_s;
  return r;
}

uw_Basis_time uw_Basis_unurlifyTime(uw_context ctx, char **s) {
  uw_Basis_int n = uw_Basis_unurlifyInt(ctx, s);
  uw_Basis_time r = {n / 1000000, n % 1000000};
  return r;
}

static uw_Basis_string uw_unurlifyString_to(int fromClient, uw_context ctx, char *r, char *s) {
  char *s1, *s2 = s;
  int n;

  if (!fromClient) {
    if (*s2 == '_')
      ++s2;
    else if ((s2[0] == '%' || s2[0] == '.') && s2[1] == '5' && (s2[2] == 'f' || s2[2] == 'F'))
      s2 += 3;
  }

  for (s1 = r; *s2; ++s1, ++s2) {
    unsigned char c = *s2;

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
        uw_error(ctx, FATAL, "Invalid escaped URL byte starting at: %s", uw_Basis_htmlifyString(ctx, s2));
      *s1 = n;
      s2 += 2;
      break;
    case '.':
      if (!fromClient) {
        if (s2[1] == 0)
          uw_error(ctx, FATAL, "Missing first character of escaped URL byte");
        if (s2[2] == 0)
          uw_error(ctx, FATAL, "Missing second character of escaped URL byte");
        if (sscanf(s2+1, "%02X", &n) != 1)
          uw_error(ctx, FATAL, "Invalid escaped URL byte starting at: %s", uw_Basis_htmlifyString(ctx, s2));
        *s1 = n;
        s2 += 2;
        break;
      }
    default:
      *s1 = c;
    }
  }
  *s1++ = 0;
  return s1;
}

uw_Basis_bool uw_Basis_unurlifyBool(uw_context ctx, char **s) {
  (void)ctx;

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
  char *r;
  int len;

  len = strlen(*s);
  uw_check_heap(ctx, len + 1);

  r = ctx->heap.front;
  ctx->heap.front = uw_unurlifyString_to(0, ctx, ctx->heap.front, *s);
  *s = new_s;
  return r;
}

uw_Basis_char uw_Basis_unurlifyChar(uw_context ctx, char **s) {
  char *new_s = uw_unurlify_advance(*s);
  char *r;
  int len;

  len = strlen(*s);
  uw_check_heap(ctx, len + 1);

  r = ctx->heap.front;
  ctx->heap.front = uw_unurlifyString_to(0, ctx, ctx->heap.front, *s);
  *s = new_s;
  if (strlen(r) == 1)
    return r[0];
  else
    uw_error(ctx, FATAL, "Unurlified character is multiple characters long");
}

uw_Basis_unit uw_Basis_unurlifyUnit(uw_context ctx, char **s) {
  (void)ctx;
  *s = uw_unurlify_advance(*s);
  return uw_unit_v;
}

uw_Basis_string uw_Basis_unurlifyString_fromClient(uw_context ctx, char **s) {
  char *new_s = uw_unurlify_advance(*s);
  char *r;
  int len;

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

char *uw_Basis_htmlifySpecialChar(uw_context ctx, uw_Basis_char ch) {
  unsigned int n = ch;
  int len;
  char *r;

  uw_check_heap(ctx, INTS_MAX+3 + 1);
  r = ctx->heap.front;
  len = sprintf(r, "&#%u;", n);
  ctx->heap.front += len+1;

  return r;
}

uw_unit uw_Basis_htmlifySpecialChar_w(uw_context ctx, uw_Basis_char ch) {
  unsigned int n = ch;
  int len = 0;

  uw_check(ctx, INTS_MAX+3);

  if(uw_Basis_isprint(ctx, ch)) {
    U8_APPEND_UNSAFE(ctx->page.front, len, ch);
  }

  // either it's a non-printable character, or we failed to convert to UTF-8
  if(len == 0) {
    len = sprintf(ctx->page.front, "&#%u;", n);
  }
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

char *uw_Basis_jsifyTime(uw_context ctx, uw_Basis_time t) {
  int len;
  char *r;

  uw_check_heap(ctx, INTS_MAX);
  r = ctx->heap.front;
  sprintf(r, "%lld%n", (uw_Basis_int)t.seconds * 1000000 + t.microseconds, &len);
  ctx->heap.front += len+1;
  return r;
}

uw_unit uw_Basis_jsifyInt_w(uw_context ctx, uw_Basis_int n) {
  int len;

  uw_check(ctx, INTS_MAX);
  sprintf(ctx->page.front, "%lld%n", (uw_Basis_int)n, &len);
  ctx->page.front += len;

  return uw_unit_v;
}

char *uw_Basis_htmlifyString(uw_context ctx, const char *s) {
  char *r, *s2;
  uw_Basis_char c1;
  int oldoffset = 0, offset = 0, offset2 = 0, len = 0;
  
  uw_check_heap(ctx, strlen(s) * (INTS_MAX + 3) + 1);

  r = s2 = ctx->heap.front;
  
  while (s[offset] != 0) {
    oldoffset = offset;
    U8_NEXT(s, offset, -1, c1);

    if ((offset - oldoffset == 1) && uw_Basis_isprint(ctx, c1)) {
      switch (c1) {
      case '<':
	strcpy(s2, "&lt;");
	s2 += 4;
	break;
      case '&':
	strcpy(s2, "&amp;");
	s2 += 5;
	break;
      default:
	offset2 = 0;
	U8_APPEND_UNSAFE(s2, offset2, c1);
	s2 += offset2;
      }      
    } else {
      len = sprintf(s2, "&#%u;", c1);
      s2 += len;
    }
  }

  *s2++ = 0;
  ctx->heap.front = s2;
  return r;
}

uw_unit uw_Basis_htmlifyString_w(uw_context ctx, uw_Basis_string s) {
  uw_check(ctx, strlen(s) * 6);
  int offset = 0, oldoffset = 0;
  uw_Basis_char c1;
  
  while(s[offset] != 0){
    oldoffset = offset;
    U8_NEXT(s, offset, -1, c1);
 
    if ((offset - oldoffset == 1) && uw_Basis_isprint(ctx, c1)) {
	
      switch (c1) {
      case '<':
	uw_write_unsafe(ctx, "&lt;");
	break;
      case '&':
	uw_write_unsafe(ctx, "&amp;");
	break;
      default:
	uw_writec_unsafe(ctx, c1);
      }
    }
    else {
      uw_Basis_htmlifySpecialChar_w(ctx, c1);
    }    
  }

  return uw_unit_v;
}

uw_Basis_string uw_Basis_htmlifyBool(uw_context ctx, uw_Basis_bool b) {
  (void)ctx;
  if (!b)
    return "False";
  else
    return "True";
}

uw_unit uw_Basis_htmlifyBool_w(uw_context ctx, uw_Basis_bool b) {
  if (!b) {
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
#define TIME_FMT_JS "%Y/%m/%d %T"

uw_Basis_string uw_Basis_timeToString(uw_context, uw_Basis_time);

uw_Basis_string uw_Basis_htmlifyTime(uw_context ctx, uw_Basis_time t) {
  return uw_Basis_htmlifyString(ctx, uw_Basis_timeToString(ctx, t));
}

uw_unit uw_Basis_htmlifyTime_w(uw_context ctx, uw_Basis_time t) {
  return uw_Basis_htmlifyString_w(ctx, uw_Basis_timeToString(ctx, t));
}

char *uw_Basis_htmlifySource(uw_context ctx, uw_Basis_source src) {
  int len;
  char *r;

  uw_check_heap(ctx, 2 * INTS_MAX + 2);
  r = ctx->heap.front;
  sprintf(r, "s%d_%llu%n", src.context, src.source, &len);
  ctx->heap.front += len+1;
  return r;
}

uw_unit uw_Basis_htmlifySource_w(uw_context ctx, uw_Basis_source src) {
  int len;

  uw_check(ctx, 2 * INTS_MAX + 1);
  sprintf(ctx->page.front, "s%d_%llu%n", src.context, src.source, &len);
  ctx->page.front += len;

  return uw_unit_v;
}

uw_Basis_char uw_Basis_strsubUtf8(uw_context ctx, uw_Basis_string s, uw_Basis_int n) {
  if (n < 0 || n >= strlen(s))
    uw_error(ctx, FATAL, "Out-of-bounds strsubUtf8");

  return s[n];
}

uw_Basis_char uw_Basis_strsub(uw_context ctx, uw_Basis_string s, uw_Basis_int n) {
  uw_Basis_char c;
  int offset = 0;
  
  while (n >= 0) {
    
    if (s[offset] == 0)
      uw_error(ctx, FATAL, "Out-of-bounds strsub");

    U8_NEXT(s, offset, -1, c);
    
    if (n == 0)
      return c;

    --n;
  }

  uw_error(ctx, FATAL, "Negative strsub bound");
}

uw_Basis_string uw_Basis_strsuffixUtf8(uw_context ctx, uw_Basis_string s, uw_Basis_int n) {
  int offset = 0;
  while (n >= 0) {
    if (s[offset] == 0 || n == 0)
      return s + offset;

    ++s;
    --n;
  }

  uw_error(ctx, FATAL, "Negative strsuffixUtf8 bound");
}

uw_Basis_string uw_Basis_strsuffix(uw_context ctx, uw_Basis_string s, uw_Basis_int n) {
  int offset = 0;
  while (n >= 0) {
    if (s[offset] == 0 || n == 0)
      return s + offset;

    U8_FWD_1(s, offset, -1);
    --n;
  }

  uw_error(ctx, FATAL, "Negative strsuffix bound");
}

uw_Basis_int uw_Basis_strlenUtf8(uw_context ctx, uw_Basis_string s) {
  (void)ctx;
  return strlen(s);
}

uw_Basis_int uw_Basis_strlen(uw_context ctx, uw_Basis_string s) {
  (void)ctx;
  int offset = 0, iterations = 0;
  while (s[offset] != 0) {
    U8_FWD_1(s, offset, -1);
    ++iterations;
  }
  return iterations;
}

uw_Basis_bool uw_Basis_strlenGe(uw_context ctx, uw_Basis_string s, uw_Basis_int n) {
  (void)ctx;
  int offset = 0;
  while (n > 0) {
    if (s[offset] == 0)
      return uw_Basis_False;
        
    U8_FWD_1(s, offset, -1);
    --n;
  }

  return uw_Basis_True;
}

static int aux_strchr(uw_Basis_string s, uw_Basis_char ch, int *o_offset) {
  int u8idx = 0, offset = 0, offsetpr = 0;
  uw_Basis_char c;
    
  while (s[offset] != 0) {
    U8_NEXT(s, offset, -1, c);
    if (c == ch) {
      *o_offset = offsetpr;
      return u8idx;
    }

    offsetpr = offset;
    ++u8idx;
  }

  *o_offset = -1;
  return -1;
}

uw_Basis_string uw_Basis_strchr(uw_context ctx, uw_Basis_string s, uw_Basis_char ch) {
  (void)ctx;
  int offset = -1;
  if (aux_strchr(s, ch, &offset) > -1) {
    return s + offset;
  }
  return NULL;  
}

uw_Basis_int uw_Basis_strcspn(uw_context ctx, uw_Basis_string s, uw_Basis_string chs) {
  (void)ctx;
  int offset = 0, u8idx = 0, offsetChs = 0;
  uw_Basis_char c;
  
  while (s[offset] != 0) {
    U8_NEXT(s, offset, -1, c);
    if (aux_strchr(chs, c, &offsetChs) > -1) {
      return u8idx;
    }
    ++u8idx;
  }

  return u8idx;
}

uw_Basis_int *uw_Basis_strindex(uw_context ctx, uw_Basis_string s, uw_Basis_char ch) {
  (void)ctx;
  int offset = -1;
  int r = aux_strchr(s, ch, &offset);
  if (r == -1)
    return NULL;
  else {
    uw_Basis_int *nr = uw_malloc(ctx, sizeof(uw_Basis_int));
    *nr = r;
    return nr;
  }
}

uw_Basis_int *uw_Basis_strsindex(uw_context ctx, const char *haystack, const char *needle) {
  uw_Basis_string r = strstr(haystack, needle);
  if (r == NULL)
    return NULL;
  else {
    uw_Basis_int *nr = uw_malloc(ctx, sizeof(uw_Basis_int));
    int src = r - haystack, offset = 0, utf8idx = 0;
    while (offset < src) {
      U8_FWD_1(haystack, offset, -1);
      ++utf8idx;
    }
    
    *nr = utf8idx;
    return nr;
  }
}

uw_Basis_string uw_Basis_strcat(uw_context ctx, uw_Basis_string s1, uw_Basis_string s2) {
  int len = strlen(s1) + strlen(s2) + 1;
  char *s;

  uw_check_heap(ctx, len);

  s = ctx->heap.front;

  strcpy(s, s1);
  strcat(s, s2);
  ctx->heap.front += len;

  return s;
}

uw_Basis_string uw_Basis_substring(uw_context ctx, uw_Basis_string s, uw_Basis_int start, uw_Basis_int len) {
  int full_len = uw_Basis_strlen(ctx, s);
  
  if (start < 0)
    uw_error(ctx, FATAL, "substring: Negative start index");
  if (len < 0)
    uw_error(ctx, FATAL, "substring: Negative length");
  if (start + len > full_len)
    uw_error(ctx, FATAL, "substring: Start index plus length is too large");

  int offset = 0;
  U8_FWD_N(s, offset, -1, start);
  
  if (start + len == full_len) {
    return s + offset;
  } else {
    int end = offset;
    U8_FWD_N(s, end, -1, len);

    int actual_len = end - offset;

    uw_Basis_string r = uw_malloc(ctx, actual_len + 1);
    memcpy(r, s + offset, actual_len);
    r[actual_len] = 0;
    return r;
  }
}

uw_Basis_string uw_Basis_str1(uw_context ctx, uw_Basis_char ch) {
  char *r;
  int req = U8_LENGTH(ch);
  int offset = 0;
  
  uw_check_heap(ctx, req + 1);
  r = ctx->heap.front;

  U8_APPEND_UNSAFE(r, offset, ch);  
  r[req] = 0;

  ctx->heap.front += req + 1;
  return r; 
}

uw_Basis_string uw_strdup(uw_context ctx, uw_Basis_string s1) {
  int len = strlen(s1) + 1;
  char *s;

  uw_check_heap(ctx, len);

  s = ctx->heap.front;

  strcpy(s, s1);
  ctx->heap.front += len;

  return s;
}

uw_Basis_string uw_dup_and_clear_error_message(uw_context ctx) {
  if (ctx->error_message[0]) {
    char *s = uw_strdup(ctx, ctx->error_message);
    ctx->error_message[0] = 0;
    return s;
  } else
    return NULL;
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

char *uw_sqlfmtFloat = "%.16g::float8%n";

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

int uw_Estrings = 1, uw_sql_type_annotations = 1;
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
      if (isprint((int)c))
        *s2++ = c;
      else if (uw_Estrings) {
        sprintf(s2, "\\%03o", (unsigned char)c);
        s2 += 4;
      }
      else
        *s2++ = c; // I hope this is safe to do... don't know how to support UTF-8 outside Postgres otherwise!
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
    if (isprint((int)c))
      *s2++ = c;
    else if (uw_Estrings) {
      sprintf(s2, "\\%03o", (unsigned char)c);
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

  uw_check_heap(ctx, b.size * 5 + 4 + strlen(uw_sqlsuffixBlob));

  r = s2 = ctx->heap.front;
  if (uw_Estrings)
    *s2++ = 'E';
  else
    *s2++ = 'X';
  *s2++ = '\'';

  for (i = 0; i < b.size; ++i) {
    unsigned char c = b.data[i];

    if (uw_Estrings) {
      switch (c) {
      case '\'':
        strcpy(s2, "\\'");
        s2 += 2;
        break;
      case '\\':
        strcpy(s2, "\\\\\\\\");
        s2 += 4;
        break;
      default:
        if (isprint((int)c))
          *s2++ = c;
        else {
          sprintf(s2, "\\\\%03o", c);
          s2 += 5;
        }
      }
    } else {
      sprintf(s2, "%02X", c);
      s2 += 2;
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
  (void)ctx;
  if (!b)
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
  struct tm stm = {};
  stm.tm_isdst = -1;

  if (localtime_r(&t.seconds, &stm)) {
    s = uw_malloc(ctx, TIMES_MAX);
    len = strftime(s, TIMES_MAX, TIME_FMT_PG, &stm);
    if (uw_sql_type_annotations) {
      if (t.microseconds) {
        r = uw_malloc(ctx, len + 21);
        sprintf(r, "'%s.%06u'::timestamp", s, t.microseconds);
      } else {
        r = uw_malloc(ctx, len + 14);
        sprintf(r, "'%s'::timestamp", s);
      }
    } else {
      r = uw_malloc(ctx, len + 3);
      sprintf(r, "'%s'", s);
    }
    return r;
  } else
    return "<Invalid time>";
}

char *uw_Basis_attrifyTime(uw_context ctx, uw_Basis_time t) {
  size_t len;
  char *r;
  struct tm stm = {};
  stm.tm_isdst = -1;

  if (localtime_r(&t.seconds, &stm)) {
    uw_check_heap(ctx, TIMES_MAX);
    r = ctx->heap.front;
    len = strftime(r, TIMES_MAX, TIME_FMT, &stm);
    ctx->heap.front += len+1;
    return r;
  } else
    return "<Invalid time>";
}

char *uw_Basis_ensqlTime(uw_context ctx, uw_Basis_time t) {
  size_t len;
  char *r;
  struct tm stm = {};
  stm.tm_isdst = -1;

  if (localtime_r(&t.seconds, &stm)) {
    uw_check_heap(ctx, TIMES_MAX);
    r = ctx->heap.front;
    len = strftime(r, TIMES_MAX-7, TIME_FMT_PG, &stm);
    ctx->heap.front += len;
    sprintf(ctx->heap.front, ".%06u", t.microseconds);
    ctx->heap.front += 8;
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
  static uw_Basis_int tru = 1;
  static uw_Basis_int fals = 0;

  if (!b)
    return (char *)&fals;
  else
    return (char *)&tru;
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
  return uw_Basis_str1(ctx, ch);
}

uw_Basis_string uw_Basis_boolToString(uw_context ctx, uw_Basis_bool b) {
  (void)ctx;
  if (!b)
    return "False";
  else
    return "True";
}

uw_Basis_string uw_Basis_timef(uw_context ctx, const char *fmt, uw_Basis_time t) {
  size_t len;
  char *r;
  struct tm stm = {};
  stm.tm_isdst = -1;

  if (localtime_r(&t.seconds, &stm)) {
    uw_check_heap(ctx, TIMES_MAX);
    r = ctx->heap.front;
    len = strftime(r, TIMES_MAX, fmt, &stm);
    ctx->heap.front += len+1;
    return r;
  } else
    return "<Invalid time>";
}

uw_Basis_string uw_Basis_timeToString(uw_context ctx, uw_Basis_time t) {
  return uw_Basis_timef(ctx, ctx->app->time_format, t);
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
  } else if (uw_Basis_strlenGe(ctx, s, 2))
    return NULL;
  else {
    uw_Basis_char *r = uw_malloc(ctx, 1);
    int offset = 0;
    U8_NEXT(s, offset, -1, *r);
    return r;
  }
}

uw_Basis_bool *uw_Basis_stringToBool(uw_context ctx, uw_Basis_string s) {
  (void)ctx;
  static uw_Basis_bool tru = uw_Basis_True;
  static uw_Basis_bool fals = uw_Basis_False;

  if (!strcasecmp (s, "True"))
    return &tru;
  else if (!strcasecmp (s, "False"))
    return &fals;
  else
    return NULL;
}

uw_Basis_time *uw_Basis_stringToTime(uw_context ctx, uw_Basis_string s) {
  char *dot = strchr(s, '.'), *end = strchr(s, 0);
  struct tm stm = {};
  stm.tm_isdst = -1;

  if (dot) {
    *dot = 0;
    if (strptime(s, TIME_FMT_PG, &stm) == end) {
      *dot = '.';
      uw_Basis_time *r = uw_malloc(ctx, sizeof(uw_Basis_time));
      r->seconds = mktime(&stm);
      r->microseconds = 0;
      return r;
    }
    else {
      *dot = '.';
      return NULL;
    }
  }
  else {
    if (strptime(s, ctx->app->time_format, &stm) == end) {
      uw_Basis_time *r = uw_malloc(ctx, sizeof(uw_Basis_time));
      r->seconds = mktime(&stm);
      r->microseconds = 0;
      return r;
    } else if (strptime(s, TIME_FMT_PG, &stm) == end) {
      uw_Basis_time *r = uw_malloc(ctx, sizeof(uw_Basis_time));
      r->seconds = mktime(&stm);
      r->microseconds = 0;
      return r;
    } else if (strptime(s, TIME_FMT, &stm) == end) {
      uw_Basis_time *r = uw_malloc(ctx, sizeof(uw_Basis_time));
      r->seconds = mktime(&stm);
      r->microseconds = 0;
      return r;
    } else if (strptime(s, TIME_FMT_JS, &stm) == end) {
      uw_Basis_time *r = uw_malloc(ctx, sizeof(uw_Basis_time));
      r->seconds = mktime(&stm);
      r->microseconds = 0;
      return r;
    }
    else
      return NULL;
  }
}

uw_Basis_time *uw_Basis_stringToTimef(uw_context ctx, const char *fmt, uw_Basis_string s) {
  char *end = strchr(s, 0);
  struct tm stm = {};
  stm.tm_isdst = -1;

  if (strptime(s, fmt, &stm) == end) {
    uw_Basis_time *r = uw_malloc(ctx, sizeof(uw_Basis_time));
    r->seconds = mktime(&stm);
    r->microseconds = 0;
    return r;
  }
  else
    return NULL;
}

uw_Basis_int uw_Basis_stringToInt_error(uw_context ctx, uw_Basis_string s) {
  char *endptr;
  uw_Basis_int n = strtoll(s, &endptr, 10);

  if (*s != '\0' && *endptr == '\0')
    return n;
  else
    uw_error(ctx, FATAL, "Can't parse int: %s", uw_Basis_htmlifyString(ctx, s));
}

#include <errno.h>

uw_Basis_channel uw_Basis_stringToChannel_error(uw_context ctx, uw_Basis_string s) {
  unsigned long long n;

  if (sscanf(s, "%llu", &n) < 1)
    uw_error(ctx, FATAL, "Can't parse channel: %s", uw_Basis_htmlifyString(ctx, s));
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
    uw_error(ctx, FATAL, "Can't parse client: %s", uw_Basis_htmlifyString(ctx, s));
}

uw_Basis_float uw_Basis_stringToFloat_error(uw_context ctx, uw_Basis_string s) {
  char *endptr;
  uw_Basis_float n = strtod(s, &endptr);

  if (*s != '\0' && *endptr == '\0')
    return n;
  else
    uw_error(ctx, FATAL, "Can't parse float: %s", uw_Basis_htmlifyString(ctx, s));
}

uw_Basis_char uw_Basis_stringToChar_error(uw_context ctx, uw_Basis_string s) {
  if (s[0] == 0)
    return 0;
  else if (uw_Basis_strlenGe(ctx, s, 2))
    uw_error(ctx, FATAL, "Can't parse char: %s", uw_Basis_htmlifyString(ctx, s));
  else {
    uw_Basis_char c;
    int offset = 0;
    U8_NEXT(s, offset, -1, c);
    return c;
  }
}

uw_Basis_bool uw_Basis_stringToBool_error(uw_context ctx, uw_Basis_string s) {
  if (!strcasecmp(s, "T") || !strcasecmp (s, "True"))
    return uw_Basis_True;
  else if (!strcasecmp(s, "F") || !strcasecmp (s, "False"))
    return uw_Basis_False;
  else
    uw_error(ctx, FATAL, "Can't parse bool: %s", uw_Basis_htmlifyString(ctx, s));
}

uw_Basis_time uw_Basis_unsqlTime(uw_context ctx, uw_Basis_string s) {
  char *dot = strchr(s, '.'), *end = strchr(s, 0);
  int dot_is_not_really_dot = 0;
  struct tm stm = {};
  stm.tm_isdst = -1;

  // Extra logic to skip Postgres content that we want to ignore
  if (!dot) {
    dot = strchr(s, ' ');
    if (dot) {
      dot = strchr(dot, '-');
      if (dot)
        dot_is_not_really_dot = 1;
    }
  }

  if (dot) {
    *dot = 0;
    if (strptime(s, TIME_FMT_PG, &stm)) {
      *dot = dot_is_not_really_dot ? '-' : '.';
      char usec[] = "000000";
      if (!dot_is_not_really_dot) {
        int len = strlen(dot+1);
        memcpy(usec, dot+1, len < 6 ? len : 6);
      }
      uw_Basis_time r = { mktime(&stm), atoi(usec) };
      return r;
    }
    else {
      *dot = '.';
      uw_error(ctx, FATAL, "Can't parse time: %s", uw_Basis_htmlifyString(ctx, s));
    }
  }
  else {
    if (strptime(s, TIME_FMT_PG, &stm) == end) {
      uw_Basis_time r = { mktime(&stm) };
      return r;
    } else if (strptime(s, TIME_FMT, &stm) == end) {
      uw_Basis_time r = { mktime(&stm) };
      return r;
    } else
      uw_error(ctx, FATAL, "Can't parse time: %s", uw_Basis_htmlifyString(ctx, s));
  }
}

uw_Basis_time uw_Basis_stringToTime_error(uw_context ctx, uw_Basis_string s) {
  char *dot = strchr(s, '.'), *end = strchr(s, 0);
  struct tm stm = {};
  stm.tm_isdst = -1;

  if (dot) {
    *dot = 0;
    if (strptime(s, TIME_FMT_PG, &stm)) {
      *dot = '.';
      {
        uw_Basis_time r = { mktime(&stm) };
        return r;
      }
    }
    else {
      *dot = '.';
      uw_error(ctx, FATAL, "Can't parse time: %s", uw_Basis_htmlifyString(ctx, s));
    }
  }
  else {
    if (strptime(s, ctx->app->time_format, &stm) == end) {
      uw_Basis_time r = { mktime(&stm) };
      return r;
    } else if (strptime(s, TIME_FMT_PG, &stm) == end) {
      uw_Basis_time r = { mktime(&stm) };
      return r;
    } else if (strptime(s, TIME_FMT, &stm) == end) {
      uw_Basis_time r = { mktime(&stm) };
      return r;
    } else if (strptime(s, TIME_FMT_JS, &stm) == end) {
      uw_Basis_time r = { mktime(&stm) };
      return r;
    } else
      uw_error(ctx, FATAL, "Can't parse time: %s", uw_Basis_htmlifyString(ctx, s));
  }
}

uw_Basis_time uw_Basis_stringToTimef_error(uw_context ctx, const char *fmt, uw_Basis_string s) {
  char *end = strchr(s, 0);
  struct tm stm = {};
  stm.tm_isdst = -1;

  if (strptime(s, fmt, &stm) == end) {
    uw_Basis_time r = { mktime(&stm) };
    return r;
  } else
    uw_error(ctx, FATAL, "Can't parse time: %s", uw_Basis_htmlifyString(ctx, s));
}

uw_Basis_blob uw_Basis_stringToBlob_error(uw_context ctx, uw_Basis_string s, size_t len) {
  char *r = ctx->heap.front;
  uw_Basis_blob b = {len, r};

  uw_check_heap(ctx, len);

  if (s[0] == '\\' && s[1] == 'x') {
    s += 2;

    while (*s) {
      char a = s[0];
      s += 1;
      char b;
      if (*s){
        b = s[0];
      } else {
        b = 0;
      }
      int n;
      char buf[3] = {a, b, 0};
      n = strtol(buf, NULL, 16);
      *r++ = n;
      s += 1;
    }
  } else {
    while (*s) {
      if (s[0] == '\\') {
        if (s[1] == '\\') {
          *r++ = '\\';
          s += 2;
        } else if (isdigit((int)s[1]) && isdigit((int)s[2]) && isdigit((int)s[3])) {
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
  }

  b.size = r - ctx->heap.front;
  ctx->heap.front = r;

  return b;
}

#define THE_PAST "expires=Sat, 01-Jan-2011 00:00:00 GMT"

uw_Basis_string uw_Basis_get_cookie(uw_context ctx, uw_Basis_string c) {
  int len = strlen(c);
  char *p = ctx->outHeaders.start;

  while ((p = strstr(p, "\nSet-Cookie: "))) {
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

  if ((p = uw_Basis_requestHeader(ctx, "Cookie"))) {
    char *p2;

    while (1) {
      if (!strncmp(p, c, len) && p[len] == '=') {
        if ((p2 = strchr(p, ';'))) {
          size_t n = p2 - (p + len);
          char *r = uw_malloc(ctx, n);
          memcpy(r, p + 1 + len, n-1);
          r[n-1] = 0;
          return r;
        } else
          return p + 1 + len;
      } else if ((p = strchr(p, ';')))
        p += 2;
      else
        return NULL;
    }
  }

  return NULL;
}

static void set_cookie(uw_context ctx) {
  if (ctx->usedSig)
    ctx->needsResig = 1;
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
    struct tm tm = {};
    tm.tm_isdst = -1;

    gmtime_r(&expires->seconds, &tm);

    strftime(formatted, sizeof formatted, "%a, %d-%b-%Y %T GMT", &tm);

    uw_write_header(ctx, "; expires=");
    uw_write_header(ctx, formatted);
  }
  if (secure)
    uw_write_header(ctx, "; secure");
  uw_write_header(ctx, "\r\n");
  set_cookie(ctx);

  return uw_unit_v;
}

uw_unit uw_Basis_clear_cookie(uw_context ctx, uw_Basis_string prefix, uw_Basis_string c) {
  uw_write_header(ctx, "Set-Cookie: ");
  uw_write_header(ctx, c);
  uw_write_header(ctx, "=; path=");
  uw_write_header(ctx, prefix);
  uw_write_header(ctx, "; " THE_PAST "\r\n");
  set_cookie(ctx);

  return uw_unit_v;
}

size_t uw_deltas_max = SIZE_MAX;

static delta *allocate_delta(uw_context ctx, unsigned client) {
  unsigned i;
  delta *d;

  for (i = 0; i < ctx->used_deltas; ++i)
    if (ctx->deltas[i].client == client)
      return &ctx->deltas[i];

  if (ctx->used_deltas >= ctx->n_deltas) {
    if (ctx->n_deltas + 1 > uw_deltas_max)
      uw_error(ctx, FATAL, "Exceeded limit on number of deltas");

    ctx->deltas = realloc(ctx->deltas, sizeof(delta) * ++ctx->n_deltas);
    uw_buffer_init(uw_messages_max, &ctx->deltas[ctx->n_deltas-1].msgs, 0);
  }

  d = &ctx->deltas[ctx->used_deltas++];
  d->client = client;
  uw_buffer_reset(&d->msgs);
  return d;
}

uw_Basis_channel uw_Basis_new_channel(uw_context ctx, uw_unit u) {
  (void)u;

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

  ctx_uw_buffer_append(ctx, "messages", &d->msgs, pre, preLen);
  ctx_uw_buffer_append(ctx, "messages", &d->msgs, msg, len);
  ctx_uw_buffer_append(ctx, "messages", &d->msgs, "\n", 1);

  return uw_unit_v;
}

int uw_rollback(uw_context ctx, int will_retry) {
  int i;
  cleanup *cl;

  if (ctx->client)
    release_client(ctx->client);

  for (cl = ctx->cleanup; cl < ctx->cleanup_front; ++cl)
    cl->func(cl->arg);

  ctx->cleanup_front = ctx->cleanup;

  for (i = ctx->used_transactionals-1; i >= 0; --i)
    if (ctx->transactionals[i].rollback != NULL)
      ctx->transactionals[i].rollback(ctx->transactionals[i].data);

  for (i = ctx->used_transactionals-1; i >= 0; --i)
    if (ctx->transactionals[i].free)
      ctx->transactionals[i].free(ctx->transactionals[i].data, will_retry);

  if (ctx->app && ctx->transaction_started) {
    ctx->transaction_started = 0;
    return ctx->app->db_rollback(ctx);
  } else
    return 0;
}

const char uw_begin_xhtml[] = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">";
const char uw_begin_html5[] = "<!DOCTYPE html><html>";

extern int uw_hash_blocksize;

static const char sig_intro[] = "<input type=\"hidden\" name=\"Sig\" value=\"";

static char *find_sig(char *haystack) {
  int i;
  char *s = strstr(haystack, sig_intro);

  if (!s || strlen(haystack) - (s - haystack) - (sizeof sig_intro - 1) < uw_hash_blocksize*2+1)
    return NULL;

  s += sizeof sig_intro - 1;

  for (i = 0; i < uw_hash_blocksize*2; ++i)
    if (!isxdigit((int)s[i]))
      return NULL;

  if (s[i] != '"')
    return NULL;

  return s;
}

static pthread_mutex_t message_send_mutex = PTHREAD_MUTEX_INITIALIZER;

int uw_commit(uw_context ctx) {
  int i;
  char *sig;

  if (uw_has_error(ctx)) {
    uw_rollback(ctx, 0);
    return 0;
  }

  for (i = ctx->used_transactionals-1; i >= 0; --i)
    if (ctx->transactionals[i].rollback != NULL)
      if (ctx->transactionals[i].commit) {
        ctx->transactionals[i].commit(ctx->transactionals[i].data);
        if (uw_has_error(ctx)) {
          uw_rollback(ctx, 0);
          return 0;
        }
      }

  // Here's an important lock to provide the abstraction that all messages from one transaction are sent as an atomic unit.
  if (ctx->used_deltas > 0)
    pthread_mutex_lock(&message_send_mutex);

  if (ctx->transaction_started) {
    int code = ctx->app->db_commit(ctx);

    if (code) {
      if (ctx->used_deltas > 0)
        pthread_mutex_unlock(&message_send_mutex);

      if (ctx->client)
        release_client(ctx->client);

      if (code == -1) {
        // This case is for a serialization failure, which is not really an "error."
        // The transaction will restart, so we should rollback any transactionals
        // that triggered above.

        for (i = ctx->used_transactionals-1; i >= 0; --i)
          if (ctx->transactionals[i].rollback != NULL)
            ctx->transactionals[i].rollback(ctx->transactionals[i].data);

        for (i = ctx->used_transactionals-1; i >= 0; --i)
          if (ctx->transactionals[i].free)
            ctx->transactionals[i].free(ctx->transactionals[i].data, 1);

        return 1;
      }

      for (i = ctx->used_transactionals-1; i >= 0; --i)
        if (ctx->transactionals[i].free)
          ctx->transactionals[i].free(ctx->transactionals[i].data, 0);

      uw_set_error_message(ctx, "Error running SQL COMMIT");
      return 0;
    }
  }

  for (i = ctx->used_transactionals-1; i >= 0; --i)
    if (ctx->transactionals[i].rollback == NULL)
      if (ctx->transactionals[i].commit) {
        ctx->transactionals[i].commit(ctx->transactionals[i].data);
        if (uw_has_error(ctx)) {
          if (ctx->used_deltas > 0)
            pthread_mutex_unlock(&message_send_mutex);

          if (ctx->client)
            release_client(ctx->client);

          for (i = ctx->used_transactionals-1; i >= 0; --i)
            if (ctx->transactionals[i].rollback != NULL)
              ctx->transactionals[i].rollback(ctx->transactionals[i].data);

          for (i = ctx->used_transactionals-1; i >= 0; --i)
            if (ctx->transactionals[i].free)
              ctx->transactionals[i].free(ctx->transactionals[i].data, 0);

          return 0;
        }
      }

  for (i = 0; i < ctx->used_deltas; ++i) {
    delta *d = &ctx->deltas[i];
    client *c = find_client(d->client);

    assert (c != NULL);

    client_send(c, &d->msgs, ctx->script.start, uw_buffer_used(&ctx->script));
  }

  if (ctx->used_deltas > 0)
    pthread_mutex_unlock(&message_send_mutex);

  if (ctx->client)
    release_client(ctx->client);

  for (i = ctx->used_transactionals-1; i >= 0; --i)
    if (ctx->transactionals[i].free)
      ctx->transactionals[i].free(ctx->transactionals[i].data, 0);

  uw_check(ctx, 1);
  *ctx->page.front = 0;

  if (!ctx->returning_indirectly
      && (ctx->app->is_html5
          ? !strncmp(ctx->page.start, uw_begin_html5, sizeof uw_begin_html5 - 1)
          : !strncmp(ctx->page.start, uw_begin_xhtml, sizeof uw_begin_xhtml - 1))) {
    char *s;

    // Splice script data into appropriate part of page, also adding <head> if needed.
    s = ctx->page.start + (ctx->app->is_html5 ? sizeof uw_begin_html5 - 1 : sizeof uw_begin_xhtml - 1);
    s = strchr(s, '<');
    if (s == NULL) {
      // Weird.  Document has no tags!

      uw_write(ctx, "<head></head><body></body>");
      uw_check(ctx, 1);
      *ctx->page.front = 0;
    } else if (!strncmp(s, "<head>", 6)) {
      // <head> is present.  Let's add the <script> tags immediately after it.

      // Any freeform JavaScript to include?
      if (uw_buffer_used(&ctx->script) > 0) {
        size_t lenH = strlen(ctx->script_header), len = uw_buffer_used(&ctx->script);
        size_t lenP = lenH + 40 + len;
        char *start = s + 6, *oldPage = ctx->page.start;

        ctx_uw_buffer_check(ctx, "page", &ctx->page, uw_buffer_used(&ctx->page) + lenP);
        start += ctx->page.start - oldPage;
        memmove(start + lenP, start, uw_buffer_used(&ctx->page) - (start - ctx->page.start) + 1);
        ctx->page.front += lenP;
        memcpy(start, ctx->script_header, lenH);
        memcpy(start + lenH, "<script type=\"text/javascript\">", 31);
        memcpy(start + lenH + 31, ctx->script.start, len);
        memcpy(start + lenH + 31 + len, "</script>", 9);
      } else {
        size_t lenH = strlen(ctx->script_header);
        char *start = s + 6, *oldPage = ctx->page.start;

        ctx_uw_buffer_check(ctx, "page", &ctx->page, uw_buffer_used(&ctx->page) + lenH);
        start += ctx->page.start - oldPage;
        memmove(start + lenH, start, uw_buffer_used(&ctx->page) - (start - ctx->page.start) + 1);
        ctx->page.front += lenH;
        memcpy(start, ctx->script_header, lenH);
      }
    } else {
      // No <head>.  At this point, add it, with <script> tags inside.

      if (uw_buffer_used(&ctx->script) > 0) {
        size_t lenH = strlen(ctx->script_header), len = uw_buffer_used(&ctx->script);
        size_t lenP = lenH + 53 + len;
        char *start = s, *oldPage = ctx->page.start;

        ctx_uw_buffer_check(ctx, "page", &ctx->page, uw_buffer_used(&ctx->page) + lenP);
        start += ctx->page.start - oldPage;
        memmove(start + lenP, start, uw_buffer_used(&ctx->page) - (start - ctx->page.start) + 1);
        ctx->page.front += lenP;
        memcpy(start, "<head>", 6);
        memcpy(start + 6, ctx->script_header, lenH);
        memcpy(start + 6 + lenH, "<script type=\"text/javascript\">", 31);
        memcpy(start + 6 + lenH + 31, ctx->script.start, len);
        memcpy(start + 6 + lenH + 31 + len, "</script></head>", 16);
      } else {
        size_t lenH = strlen(ctx->script_header);
        size_t lenP = lenH + 13;
        char *start = s, *oldPage = ctx->page.start;

        ctx_uw_buffer_check(ctx, "page", &ctx->page, uw_buffer_used(&ctx->page) + lenP);
        start += ctx->page.start - oldPage;
        memmove(start + lenP, start, uw_buffer_used(&ctx->page) - (start - ctx->page.start) + 1);
        ctx->page.front += lenP;
        memcpy(start, "<head>", 6);
        memcpy(start + 6, ctx->script_header, lenH);
        memcpy(start + 6 + lenH, "</head>", 7);
      }
    }
  }

  if (ctx->needsResig) {
    sig = find_sig(ctx->page.start);
    if (sig) {
      char *realsig = ctx->app->cookie_sig(ctx);

      do {
        memcpy(sig, realsig, 2*uw_hash_blocksize);
        sig = find_sig(sig);
      } while (sig);
    }
  }

  ctx->file_cache_missed = 0;

  return 0;
}


size_t uw_transactionals_max = SIZE_MAX;

int uw_register_transactional(uw_context ctx, void *data, uw_callback commit, uw_callback rollback,
                               uw_callback_with_retry free) {
  if (ctx->used_transactionals >= ctx->n_transactionals) {
    if (ctx->used_transactionals+1 > uw_transactionals_max)
      // Exceeded limit on number of transactionals.
      return -1;
    ctx->transactionals = realloc(ctx->transactionals, sizeof(transactional) * (ctx->used_transactionals+1));
    ++ctx->n_transactionals;
  }

  ctx->transactionals[ctx->used_transactionals].data = data;
  ctx->transactionals[ctx->used_transactionals].commit = commit;
  ctx->transactionals[ctx->used_transactionals].rollback = rollback;
  ctx->transactionals[ctx->used_transactionals++].free = free;

  return 0;
}


// "Garbage collection"

void uw_do_expunge(uw_context ctx, uw_Basis_client cli, void *data);
void uw_post_expunge(uw_context ctx, void *data);

static failure_kind uw_expunge(uw_context ctx, uw_Basis_client cli, void *data) {
  int r = setjmp(ctx->jmp_buf);

  if (r == 0)
    uw_do_expunge(ctx, cli, data);
  else
    ctx->app->db_rollback(ctx);

  uw_post_expunge(ctx, data);

  return r;
}

void uw_prune_clients(uw_context ctx) {
  client *c, *next, *prev = NULL;
  time_t cutoff;

  cutoff = time(NULL) - ctx->app->timeout;

  uw_expunger_arrives();
  
  pthread_mutex_lock(&clients_mutex);
  pruning_thread = pthread_self();
  pruning_thread_initialized = 1;

  for (c = clients_used; c; c = next) {
    next = c->next;
    pthread_mutex_lock(&c->lock);
    if (c->last_contact < cutoff && c->refcount == 0) {
      failure_kind fk = UNLIMITED_RETRY;
      if (prev)
        prev->next = next;
      else
        clients_used = next;
      while (fk == UNLIMITED_RETRY) {
        uw_reset(ctx);
        fk = uw_expunge(ctx, c->id, c->data);
        if (fk == UNLIMITED_RETRY)
          printf("Unlimited retry during expunge: %s\n", uw_error_message(ctx));
      }
      if (fk == SUCCESS)
        free_client(c);
      else
        fprintf(stderr, "Expunge blocked by error: %s\n", uw_error_message(ctx));
    }
    else
      prev = c;
    pthread_mutex_unlock(&c->lock);
  }

  pthread_mutex_unlock(&clients_mutex);

  uw_expunger_departs();
}

failure_kind uw_initialize(uw_context ctx) {
  int r = setjmp(ctx->jmp_buf);

  if (r == 0) {
    uw_ensure_transaction(ctx);
    ctx->app->initializer(ctx);
    if (uw_commit(ctx))
      uw_error(ctx, FATAL, "Error running SQL COMMIT");
  }

  return r;
}

static int url_bad(uw_Basis_string s) {
  for (; *s; ++s)
    if (!isgraph((int)*s))
      return 1;

  return 0;
}

uw_Basis_string uw_Basis_bless(uw_context ctx, uw_Basis_string s) {
  if (url_bad(s))
    uw_error(ctx, FATAL, "Invalid URL %s", uw_Basis_htmlifyString(ctx, s));
  if (ctx->app->check_url(s))
    return s;
  else
    uw_error(ctx, FATAL, "Disallowed URL %s", uw_Basis_htmlifyString(ctx, s));
}

uw_Basis_string uw_Basis_checkUrl(uw_context ctx, uw_Basis_string s) {
  if (url_bad(s))
    return NULL;
  if (ctx->app->check_url(s))
    return s;
  else
    return NULL;
}

static int mime_format(const char *s) {
  for (; *s; ++s)
    if (!isalnum((int)*s) && *s != '/' && *s != '-' && *s != '.' && *s != '+')
      return 0;

  return 1;
}

uw_Basis_string uw_Basis_blessMime(uw_context ctx, uw_Basis_string s) {
  if (!mime_format(s))
    uw_error(ctx, FATAL, "MIME type \"%s\" contains invalid character", uw_Basis_htmlifyString(ctx, s));

  if (ctx->app->check_mime(s))
    return s;
  else
    uw_error(ctx, FATAL, "Disallowed MIME type %s", uw_Basis_htmlifyString(ctx, s));
}

uw_Basis_string uw_Basis_checkMime(uw_context ctx, uw_Basis_string s) {
  if (!mime_format(s))
    return NULL;

  if (ctx->app->check_mime(s))
    return s;
  else
    return NULL;
}

uw_Basis_string uw_Basis_blessRequestHeader(uw_context ctx, uw_Basis_string s) {
  if (!mime_format(s))
    uw_error(ctx, FATAL, "Request header \"%s\" contains invalid character", uw_Basis_htmlifyString(ctx, s));

  if (ctx->app->check_requestHeader(s))
    return s;
  else
    uw_error(ctx, FATAL, "Disallowed request header %s", uw_Basis_htmlifyString(ctx, s));
}

uw_Basis_string uw_Basis_checkRequestHeader(uw_context ctx, uw_Basis_string s) {
  if (!mime_format(s))
    return NULL;

  if (ctx->app->check_requestHeader(s))
    return s;
  else
    return NULL;
}

uw_Basis_string uw_Basis_blessResponseHeader(uw_context ctx, uw_Basis_string s) {
  if (!mime_format(s))
    uw_error(ctx, FATAL, "Response header \"%s\" contains invalid character", uw_Basis_htmlifyString(ctx, s));

  if (ctx->app->check_responseHeader(s))
    return s;
  else
    uw_error(ctx, FATAL, "Disallowed response header %s", uw_Basis_htmlifyString(ctx, s));
}

static int envVar_format(const char *s) {
  for (; *s; ++s)
    if (!isalnum((int)*s) && *s != '_' && *s != '.')
      return 0;

  return 1;
}

uw_Basis_string uw_Basis_checkResponseHeader(uw_context ctx, uw_Basis_string s) {
  if (!envVar_format(s))
    return NULL;

  if (ctx->app->check_responseHeader(s))
    return s;
  else
    return NULL;
}

uw_Basis_string uw_Basis_blessEnvVar(uw_context ctx, uw_Basis_string s) {
  if (!envVar_format(s))
    uw_error(ctx, FATAL, "Environment variable \"%s\" contains invalid character", uw_Basis_htmlifyString(ctx, s));

  if (ctx->app->check_envVar(s))
    return s;
  else
    uw_error(ctx, FATAL, "Disallowed environment variable %s", uw_Basis_htmlifyString(ctx, s));
}

uw_Basis_string uw_Basis_checkEnvVar(uw_context ctx, uw_Basis_string s) {
  if (!mime_format(s))
    return NULL;

  if (ctx->app->check_envVar(s))
    return s;
  else
    return NULL;
}

static int meta_format(const char *s) {
  for (; *s; ++s)
    if (!isalpha((int)*s) && *s != '-')
      return 0;

  return 1;
}

uw_Basis_string uw_Basis_blessMeta(uw_context ctx, uw_Basis_string s) {
  if (!meta_format(s))
    uw_error(ctx, FATAL, "Meta name \"%s\" contains invalid character", uw_Basis_htmlifyString(ctx, s));

  if (ctx->app->check_meta(s))
    return s;
  else
    uw_error(ctx, FATAL, "Disallowed meta name %s", uw_Basis_htmlifyString(ctx, s));
}

uw_Basis_string uw_Basis_checkMeta(uw_context ctx, uw_Basis_string s) {
  if (!meta_format(s))
    return NULL;

  if (ctx->app->check_meta(s))
    return s;
  else
    return NULL;
}

uw_Basis_string uw_Basis_getHeader(uw_context ctx, uw_Basis_string name) {
  return uw_Basis_requestHeader(ctx, name);
}

static int mime_value_format(const char *s) {
  for (; *s; ++s)
    if (*s == '\r' || *s == '\n')
      return 0;

  return 1;
}

uw_unit uw_Basis_setHeader(uw_context ctx, uw_Basis_string name, uw_Basis_string value) {
  if (!mime_value_format(value))
    uw_error(ctx, FATAL, "Invalid value for HTTP response header");

  uw_write_header(ctx, name);
  uw_write_header(ctx, ": ");
  uw_write_header(ctx, value);
  uw_write_header(ctx, "\r\n");

  return uw_unit_v;
}

uw_Basis_string uw_Basis_getenv(uw_context ctx, uw_Basis_string name) {
  if (ctx->get_env)
    return ctx->get_env(ctx->get_env_data, name);
  else
    return getenv(name);
}

uw_Basis_string uw_unnull(uw_Basis_string s) {
  return s ? s : "";
}

uw_Basis_string uw_Basis_makeSigString(uw_context ctx, uw_Basis_string sig) {
  uw_Basis_string r = uw_malloc(ctx, 2 * uw_hash_blocksize + 1);
  int i;

  for (i = 0; i < uw_hash_blocksize; ++i)
    sprintf(&r[2*i], "%.02X", ((unsigned char *)sig)[i]);

  return r;
}

/* This bit of crafty code is intended to prevent GCC from performing
 * optimizations that would enable timing attacks.  See:
 *   http://www.impredicative.com/pipermail/ur/2011-July/000659.html
 */
int uw_streq(uw_Basis_string s1, uw_Basis_string s2) {
  int i, x = 0, len1 = strlen(s1);

  if (len1 != strlen(s2)) return 0;

  for (i = 0; i < len1; ++i) {
        __asm__ __volatile__ ("");
        x |= s1[i] ^ s2[i];
  }

  return x == 0;
}

uw_Basis_string uw_Basis_sigString(uw_context ctx, uw_unit u) {
  (void)u;
  ctx->usedSig = 1;
  return ctx->app->cookie_sig(ctx);
}

uw_Basis_string uw_Basis_fileName(uw_context ctx, uw_Basis_file f) {
  (void)ctx;
  return f.name;
}

uw_Basis_string uw_Basis_fileMimeType(uw_context ctx, uw_Basis_file f) {
  (void)ctx;
  return f.type;
}

uw_Basis_int uw_Basis_blobSize(uw_context ctx, uw_Basis_blob b) {
  (void)ctx;
  return b.size;
}

uw_Basis_blob uw_Basis_textBlob(uw_context ctx, uw_Basis_string s) {
  (void)ctx;
  uw_Basis_blob b = {strlen(s), s};

  return b;
}

uw_Basis_string uw_Basis_textOfBlob(uw_context ctx, uw_Basis_blob b) {
  size_t i;
  uw_Basis_string r;

  for (i = 0; i < b.size; ++i)
    if (b.data[i] == 0)
      return NULL;

  r = uw_malloc(ctx, b.size + 1);
  memcpy(r, b.data, b.size);
  r[b.size] = 0;
  return r;
}

uw_Basis_blob uw_Basis_fileData(uw_context ctx, uw_Basis_file f) {
  (void)ctx;
  return f.data;
}

uw_Basis_string uw_Basis_postType(uw_context ctx, uw_Basis_postBody pb) {
  (void)ctx;
  return pb.type;
}

uw_Basis_string uw_Basis_postData(uw_context ctx, uw_Basis_postBody pb) {
  (void)ctx;
  return pb.data;
}

static char *old_headers(uw_context ctx) {
  if (uw_buffer_used(&ctx->outHeaders) == 0)
    return NULL;
  else {
    char *s;
    int is_good;

    if (strncasecmp(ctx->outHeaders.start, "Content-type: ", 14)) {
      s = strchr(ctx->outHeaders.start, '\n');
      is_good = !strncasecmp(s+1, "Content-type: ", 14);
    } else {
      s = ctx->outHeaders.start;
      is_good = 1;
    }

    if (!is_good)
      return NULL;
    else {
      s = strchr(s+15, '\n');
      if (s == NULL)
        return NULL;
      else
        return uw_strdup(ctx, s+1);
    }
  }
}

__attribute__((noreturn)) void uw_return_blob(uw_context ctx, uw_Basis_blob b, uw_Basis_string mimeType) {
  cleanup *cl;
  int len;
  char *oldh;

  if (!ctx->allowed_to_return_indirectly)
    uw_error(ctx, FATAL, "Tried to return a blob from an RPC");

  ctx->returning_indirectly = 1;
  oldh = old_headers(ctx);
  uw_buffer_reset(&ctx->outHeaders);
  uw_buffer_reset(&ctx->page);

  uw_write_header(ctx, on_success);
  uw_write_header(ctx, "Content-Type: ");
  uw_write_header(ctx, mimeType);
  uw_write_header(ctx, "\r\nContent-length: ");
  ctx_uw_buffer_check(ctx, "headers", &ctx->outHeaders, INTS_MAX);
  sprintf(ctx->outHeaders.front, "%lu%n", (unsigned long)b.size, &len);
  ctx->outHeaders.front += len;
  uw_write_header(ctx, "\r\n");
  if (oldh) uw_write_header(ctx, oldh);

  ctx_uw_buffer_append(ctx, "page", &ctx->page, b.data, b.size);

  for (cl = ctx->cleanup; cl < ctx->cleanup_front; ++cl)
    cl->func(cl->arg);

  ctx->cleanup_front = ctx->cleanup;

  longjmp(ctx->jmp_buf, RETURN_INDIRECTLY);
}

void uw_replace_page(uw_context ctx, const char *data, size_t size) {
  uw_buffer_reset(&ctx->page);
  ctx_uw_buffer_append(ctx, "page", &ctx->page, data, size);
}

__attribute__((noreturn)) void uw_return_blob_from_page(uw_context ctx, uw_Basis_string mimeType) {
  cleanup *cl;
  int len;
  char *oldh;

  if (!ctx->allowed_to_return_indirectly)
    uw_error(ctx, FATAL, "Tried to return a blob from an RPC");

  ctx->returning_indirectly = 1;
  oldh = old_headers(ctx);
  uw_buffer_reset(&ctx->outHeaders);

  uw_write_header(ctx, on_success);
  uw_write_header(ctx, "Content-Type: ");
  uw_write_header(ctx, mimeType);
  uw_write_header(ctx, "\r\nContent-length: ");
  ctx_uw_buffer_check(ctx, "headers", &ctx->outHeaders, INTS_MAX);
  sprintf(ctx->outHeaders.front, "%lu%n", (unsigned long)uw_buffer_used(&ctx->page), &len);
  ctx->outHeaders.front += len;
  uw_write_header(ctx, "\r\n");
  if (oldh) uw_write_header(ctx, oldh);

  for (cl = ctx->cleanup; cl < ctx->cleanup_front; ++cl)
    cl->func(cl->arg);

  ctx->cleanup_front = ctx->cleanup;

  longjmp(ctx->jmp_buf, RETURN_INDIRECTLY);
}

__attribute__((noreturn)) void uw_redirect(uw_context ctx, uw_Basis_string url) {
  cleanup *cl;
  char *s;
  char *oldh;

  if (!ctx->allowed_to_return_indirectly)
    uw_error(ctx, FATAL, "Tried to redirect from an RPC");

  ctx->returning_indirectly = 1;
  oldh = old_headers(ctx);
  uw_buffer_reset(&ctx->page);
  ctx_uw_buffer_check(ctx, "page", &ctx->page, uw_buffer_used(&ctx->outHeaders)+1);
  memcpy(ctx->page.start, ctx->outHeaders.start, uw_buffer_used(&ctx->outHeaders));
  ctx->page.start[uw_buffer_used(&ctx->outHeaders)] = 0;
  uw_buffer_reset(&ctx->outHeaders);

  if (on_success[0])
    uw_write_header(ctx, on_redirect);
  else
    uw_write_header(ctx, "Status: 303 See Other\r\n");
  s = on_success[0] ? strchr(ctx->page.start, '\n') : ctx->page.start;
  if (s) {
    char *s2;
    if (s[0] == '\n') ++s;
    for (; (s2 = strchr(s, '\n')); s = s2+1) {
      *s2 = 0;
      if (!strncmp(s, "Set-Cookie: ", 12)) {
        uw_write_header(ctx, s);
        uw_write_header(ctx, "\n");
      }
    }
  }

  uw_write_header(ctx, "Location: ");
  uw_write_header(ctx, url);
  uw_write_header(ctx, "\r\n\r\n");
  if (oldh) uw_write_header(ctx, oldh);

  for (cl = ctx->cleanup; cl < ctx->cleanup_front; ++cl)
    cl->func(cl->arg);

  ctx->cleanup_front = ctx->cleanup;

  longjmp(ctx->jmp_buf, RETURN_INDIRECTLY);
}

uw_Basis_string uw_Basis_unAs(uw_context ctx, uw_Basis_string s) {
  uw_Basis_string ret = uw_malloc(ctx, strlen(s) + 1), r = ret;

  for (; *s; ++s) {
    if (s[0] == '\'') {
      *r++ = '\'';
      for (++s; *s; ++s) {
        if (s[0] == '\'') {
          *r++ = '\'';
          break;
        } else if (s[0] == '\\') {
          *r++ = '\\';
          *r++ = s[1];
          ++s;
        } else
          *r++ = s[0];
      }
      if (*s == 0) break;
    } else if (s[0] == 'T' && s[1] == '_' && s[2] == 'T' && s[3] == '.')
      s += 3;
    else
      *r++ = s[0];
  }

  *r = 0;
  return ret;
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

const uw_Basis_time uw_Basis_minTime = {};

uw_Basis_time uw_Basis_now(uw_context ctx) {
  (void)ctx;
  uw_Basis_time r = { time(NULL) };
  return r;
}

uw_Basis_time uw_Basis_addSeconds(uw_context ctx, uw_Basis_time tm, uw_Basis_int n) {
  (void)ctx;
  tm.seconds += n;
  return tm;
}

uw_Basis_int uw_Basis_diffInSeconds(uw_context ctx, uw_Basis_time tm1, uw_Basis_time tm2) {
  (void)ctx;
  return difftime(tm2.seconds, tm1.seconds);
}

uw_Basis_int uw_Basis_toMilliseconds(uw_context ctx, uw_Basis_time tm) {
  (void)ctx;
  return tm.seconds * 1000 + tm.microseconds / 1000;
}

uw_Basis_time uw_Basis_fromMilliseconds(uw_context ctx, uw_Basis_int n) {
  (void)ctx;
  uw_Basis_time tm = {n / 1000, n % 1000 * 1000};
  return tm;
}

uw_Basis_int uw_Basis_diffInMilliseconds(uw_context ctx, uw_Basis_time tm1, uw_Basis_time tm2) {
  return uw_Basis_toMilliseconds(ctx, tm2) - uw_Basis_toMilliseconds(ctx, tm1);
}

uw_Basis_int uw_Basis_toSeconds(uw_context ctx, uw_Basis_time tm) {
  (void)ctx;
  return tm.seconds;
}

uw_Basis_time uw_Basis_fromDatetime(uw_context ctx, uw_Basis_int year, uw_Basis_int month, uw_Basis_int day, uw_Basis_int hour, uw_Basis_int minute, uw_Basis_int second) {
  (void)ctx;
  struct tm tm = { .tm_year = year - 1900, .tm_mon = month, .tm_mday = day,
                   .tm_hour = hour, .tm_min = minute, .tm_sec = second,
                   .tm_isdst = -1 };
  uw_Basis_time r = { mktime(&tm) };
  return r;
}

uw_Basis_int uw_Basis_datetimeYear(uw_context ctx, uw_Basis_time time) {
  (void)ctx;
  struct tm tm;
  localtime_r(&time.seconds, &tm);
  return tm.tm_year + 1900;
}

uw_Basis_int uw_Basis_datetimeMonth(uw_context ctx, uw_Basis_time time) {
  (void)ctx;
  struct tm tm;
  localtime_r(&time.seconds, &tm);
  return tm.tm_mon;
}

uw_Basis_int uw_Basis_datetimeDay(uw_context ctx, uw_Basis_time time) {
  (void)ctx;
  struct tm tm;
  localtime_r(&time.seconds, &tm);
  return tm.tm_mday;
}

uw_Basis_int uw_Basis_datetimeHour(uw_context ctx, uw_Basis_time time) {
  (void)ctx;
  struct tm tm;
  localtime_r(&time.seconds, &tm);
  return tm.tm_hour;
}

uw_Basis_int uw_Basis_datetimeMinute(uw_context ctx, uw_Basis_time time) {
  (void)ctx;
  struct tm tm;
  localtime_r(&time.seconds, &tm);
  return tm.tm_min;
}

uw_Basis_int uw_Basis_datetimeSecond(uw_context ctx, uw_Basis_time time) {
  (void)ctx;
  struct tm tm;
  localtime_r(&time.seconds, &tm);
  return tm.tm_sec;
}

uw_Basis_int uw_Basis_datetimeDayOfWeek(uw_context ctx, uw_Basis_time time) {
  (void)ctx;
  struct tm tm;
  localtime_r(&time.seconds, &tm);
  return tm.tm_wday;
}


void *uw_get_global(uw_context ctx, char *name) {
  int i;

  for (i = 0; i < ctx->n_globals; ++i)
    if (!strcmp(name, ctx->globals[i].name))
      return ctx->globals[i].data;

  return NULL;
}

size_t uw_globals_max = SIZE_MAX;

void uw_set_global(uw_context ctx, char *name, void *data, void (*free)(void*)) {
  int i;

  for (i = 0; i < ctx->n_globals; ++i)
    if (!strcmp(name, ctx->globals[i].name)) {
      if (ctx->globals[i].free)
        ctx->globals[i].free(ctx->globals[i].data);
      ctx->globals[i].data = data;
      ctx->globals[i].free = free;
      return;
    }

  if (ctx->n_globals+1 > uw_globals_max)
    uw_error(ctx, FATAL, "Exceeded limit on number of globals");

  ++ctx->n_globals;
  ctx->globals = realloc(ctx->globals, ctx->n_globals * sizeof(global));
  ctx->globals[ctx->n_globals-1].name = name;
  ctx->globals[ctx->n_globals-1].data = data;
  ctx->globals[ctx->n_globals-1].free = free;
}

uw_Basis_bool uw_Basis_isalnum(uw_context ctx, uw_Basis_char c) {
  (void)ctx;
  return !!u_hasBinaryProperty(c, UCHAR_POSIX_ALNUM);
}

uw_Basis_bool uw_Basis_isalpha(uw_context ctx, uw_Basis_char c) {
  (void)ctx;
  return !!u_hasBinaryProperty(c, UCHAR_ALPHABETIC);
}

uw_Basis_bool uw_Basis_isblank(uw_context ctx, uw_Basis_char c) {
  (void)ctx;
  return !!u_hasBinaryProperty(c, UCHAR_POSIX_BLANK);
}

uw_Basis_bool uw_Basis_iscntrl(uw_context ctx, uw_Basis_char c) {
  (void)ctx;
  return !!(u_charType(c)==U_CONTROL_CHAR);
}

uw_Basis_bool uw_Basis_isdigit(uw_context ctx, uw_Basis_char c) {
  (void)ctx;
  return !!u_isdigit(c);
}

uw_Basis_bool uw_Basis_isgraph(uw_context ctx, uw_Basis_char c) {
  (void)ctx;
  return !!u_hasBinaryProperty(c, UCHAR_POSIX_GRAPH);
}

uw_Basis_bool uw_Basis_islower(uw_context ctx, uw_Basis_char c) {
  (void)ctx;
  return !!u_hasBinaryProperty(c, UCHAR_LOWERCASE);
}

uw_Basis_bool uw_Basis_isprint(uw_context ctx, uw_Basis_char c) {
  (void)ctx;
  return !!u_hasBinaryProperty(c, UCHAR_POSIX_PRINT);
}

uw_Basis_bool uw_Basis_ispunct(uw_context ctx, uw_Basis_char c) {
  (void)ctx;
  return !!u_ispunct(c);
}

uw_Basis_bool uw_Basis_isspace(uw_context ctx, uw_Basis_char c) {
  (void)ctx;
  return !!u_hasBinaryProperty(c, UCHAR_WHITE_SPACE);
}

uw_Basis_bool uw_Basis_isupper(uw_context ctx, uw_Basis_char c) {
  (void)ctx;
  return !!u_hasBinaryProperty(c, UCHAR_UPPERCASE);
}

uw_Basis_bool uw_Basis_isxdigit(uw_context ctx, uw_Basis_char c) {
  (void)ctx;
  return !!(c <= 0x7f && u_isxdigit(c));
}

uw_Basis_char uw_Basis_tolower(uw_context ctx, uw_Basis_char c) {
  (void)ctx;
  return u_tolower(c);
}

uw_Basis_char uw_Basis_toupper(uw_context ctx, uw_Basis_char c) {
  (void)ctx;
  return u_toupper(c);
}

uw_Basis_int uw_Basis_ord(uw_context ctx, uw_Basis_char c) {
  (void)ctx;
  return (uw_Basis_int)c;
}

uw_Basis_bool uw_Basis_iscodepoint(uw_context ctx, uw_Basis_int n) {
  (void)ctx;
  return !!(0 <= n && n <= 0x10FFFF);
}

uw_Basis_bool uw_Basis_issingle(uw_context ctx, uw_Basis_char c) {
  (void)ctx;
  return !!(c < 128);
}

uw_Basis_char uw_Basis_chr(uw_context ctx, uw_Basis_int n) {
  if (!uw_Basis_iscodepoint(ctx, n)) {
    uw_error(ctx, FATAL, "The integer %lld is not a valid char codepoint", n);
  }
  return (uw_Basis_char)n;
}

uw_Basis_string uw_Basis_currentUrl(uw_context ctx) {
  return ctx->current_url;
}

uw_Basis_string uw_Basis_anchorUrl(uw_context ctx, uw_Basis_string s) {
  return uw_Basis_strcat(ctx, uw_Basis_strcat(ctx, ctx->current_url, "#"), s);
}

void uw_set_currentUrl(uw_context ctx, char *s) {
  ctx->current_url = s;
}

void uw_set_deadline(uw_context ctx, int n) {
  ctx->deadline = n;
}

void uw_check_deadline(uw_context ctx) {
  if (uw_time > ctx->deadline)
    uw_error(ctx, FATAL, "Maximum running time exceeded");
}

size_t uw_database_max = SIZE_MAX;

uw_Basis_int uw_Basis_naughtyDebug(uw_context ctx, uw_Basis_string s) {
  if (ctx->loggers->log_debug)
    ctx->loggers->log_debug(ctx->loggers->logger_data, "%s\n", s);
  else
    fprintf(stderr, "%s\n", s);
  return 0;
}

uw_Basis_unit uw_Basis_debug(uw_context ctx, uw_Basis_string s) {
  if (ctx->loggers->log_debug)
    ctx->loggers->log_debug(ctx->loggers->logger_data, "%s\n", s);
  else
    fprintf(stderr, "%s\n", s);
  return uw_unit_v;
}

uw_Basis_int uw_Basis_rand(uw_context ctx) {
  return my_rand(ctx);
}

void uw_noPostBody(uw_context ctx) {
  ctx->hasPostBody = 0;
}

void uw_postBody(uw_context ctx, uw_Basis_postBody pb) {
  ctx->hasPostBody = 1;
  ctx->postBody = pb;
}

int uw_hasPostBody(uw_context ctx) {
  return ctx->hasPostBody;
}

void uw_isPost(uw_context ctx) {
  ctx->isPost = 1;
}

uw_Basis_bool uw_Basis_currentUrlHasPost(uw_context ctx) {
  return ctx->isPost;
}

uw_Basis_bool uw_Basis_currentUrlHasQueryString(uw_context ctx) {
  return ctx->queryString != NULL && ctx->queryString[0] != 0;
}

void uw_setQueryString(uw_context ctx, uw_Basis_string s) {
  ctx->queryString = s;
}

uw_Basis_string uw_queryString(uw_context ctx) {
  return ctx->queryString;
}

uw_Basis_postBody uw_getPostBody(uw_context ctx) {
  if (ctx->hasPostBody)
    return ctx->postBody;
  else
    uw_error(ctx, FATAL, "Asked for POST body when none exists");
}

failure_kind uw_runCallback(uw_context ctx, void (*callback)(uw_context)) {
  int r = setjmp(ctx->jmp_buf);

  if (r == 0) {
    uw_ensure_transaction(ctx);

    callback(ctx);
  }

  return r;
}

uw_Basis_bool uw_Basis_eq_time(uw_context ctx, uw_Basis_time t1, uw_Basis_time t2) {
  (void)ctx;
  return !!(t1.seconds == t2.seconds && t1.microseconds == t2.microseconds);
}

uw_Basis_bool uw_Basis_lt_time(uw_context ctx, uw_Basis_time t1, uw_Basis_time t2) {
  (void)ctx;
  return !!(t1.seconds < t2.seconds || (t1.seconds == t2.seconds && t1.microseconds < t2.microseconds));
}

uw_Basis_bool uw_Basis_le_time(uw_context ctx, uw_Basis_time t1, uw_Basis_time t2) {
  return !!(uw_Basis_eq_time(ctx, t1, t2) || uw_Basis_lt_time(ctx, t1, t2));
}

uw_Basis_time *uw_Basis_readUtc(uw_context ctx, uw_Basis_string s) {
  struct tm stm = {};
  char *end = strchr(s, 0);
  stm.tm_isdst = -1;

  if (strptime(s, TIME_FMT_PG, &stm) == end || strptime(s, TIME_FMT, &stm) == end || strptime(s, TIME_FMT_JS, &stm) == end) {
    uw_Basis_time *r = uw_malloc(ctx, sizeof(uw_Basis_time));

    r->seconds = timegm(&stm);
    r->microseconds = 0;

    return r;
  }
  else
    return NULL;
}

failure_kind uw_begin_onError(uw_context ctx, char *msg) {
  int r = setjmp(ctx->jmp_buf);

  if (ctx->app->on_error) {
    if (r == 0) {
      uw_ensure_transaction(ctx);

      uw_buffer_reset(&ctx->outHeaders);
      if (on_success[0])
        uw_write_header(ctx, "HTTP/1.1 ");
      else
        uw_write_header(ctx, "Status: ");
      uw_write_header(ctx, "500 Internal Server Error\r\n");
      uw_write_header(ctx, "Content-type: text/html\r\n");
      uw_write(ctx, ctx->app->is_html5 ? uw_begin_html5 : uw_begin_xhtml);
      ctx->app->on_error(ctx, msg);
      uw_write(ctx, "</html>");
    }

    return r;
  } else
    uw_error(ctx, FATAL, "Tried to run nonexistent onError handler");
}

void uw_mayReturnIndirectly(uw_context ctx) {
  ctx->allowed_to_return_indirectly = 1;
}

uw_Basis_string uw_Basis_fresh(uw_context ctx) {
  int len;
  char *r;

  uw_check_heap(ctx, 2+INTS_MAX);
  r = ctx->heap.front;
  sprintf(r, "uw%u%n", ctx->nextId++, &len);
  ctx->heap.front += len+1;
  return r;
}

uw_Basis_float uw_Basis_floatFromInt(uw_context ctx, uw_Basis_int n) {
  (void)ctx;
  return n;
}

uw_Basis_int uw_Basis_ceil(uw_context ctx, uw_Basis_float n) {
  (void)ctx;
  return ceil(n);
}

uw_Basis_int uw_Basis_trunc(uw_context ctx, uw_Basis_float n) {
  (void)ctx;
  return trunc(n);
}

uw_Basis_int uw_Basis_round(uw_context ctx, uw_Basis_float n) {
  (void)ctx;
  return round(n);
}

uw_Basis_int uw_Basis_floor(uw_context ctx, uw_Basis_float n) {
  (void)ctx;
  return floor(n);
}

uw_Basis_float uw_Basis_pow(uw_context ctx, uw_Basis_float n, uw_Basis_float m) {
  (void)ctx;
  return pow(n,m);
}

uw_Basis_float uw_Basis_sqrt(uw_context ctx, uw_Basis_float n) {
  (void)ctx;
  return sqrt(n);
}

uw_Basis_float uw_Basis_sin(uw_context ctx, uw_Basis_float n) {
  (void)ctx;
  return sin(n);
}

uw_Basis_float uw_Basis_cos(uw_context ctx, uw_Basis_float n) {
  (void)ctx;
  return cos(n);
}

uw_Basis_float uw_Basis_log(uw_context ctx, uw_Basis_float n) {
  (void)ctx;
  return log(n);
}

uw_Basis_float uw_Basis_exp(uw_context ctx, uw_Basis_float n) {
  (void)ctx;
  return exp(n);
}

uw_Basis_float uw_Basis_asin(uw_context ctx, uw_Basis_float n) {
  (void)ctx;
  return asin(n);
}

uw_Basis_float uw_Basis_acos(uw_context ctx, uw_Basis_float n) {
  (void)ctx;
  return acos(n);
}

uw_Basis_float uw_Basis_atan(uw_context ctx, uw_Basis_float n) {
  (void)ctx;
  return atan(n);
}

uw_Basis_float uw_Basis_atan2(uw_context ctx, uw_Basis_float n, uw_Basis_float m) {
  (void)ctx;
  return atan2(n, m);
}

uw_Basis_float uw_Basis_abs(uw_context ctx, uw_Basis_float n) {
  (void)ctx;
  return fabs(n);
}

uw_Basis_string uw_Basis_atom(uw_context ctx, uw_Basis_string s) {
  char *p;

  for (p = s; *p; ++p) {
    char c = *p;
    if (!U8_IS_SINGLE(c) || (!isalnum((int)c) && c != '+' && c != '-' && c != '.' && c != '%' && c != '#'))
      uw_error(ctx, FATAL, "Disallowed character in CSS atom");
  }

  return s;
}

uw_Basis_string uw_Basis_css_url(uw_context ctx, uw_Basis_string s) {
  char *p;

  for (p = s; *p; ++p) {
    char c = *p;
    if (!U8_IS_SINGLE(c) || (!isalnum((int)c) && c != ':' && c != '/' && c != '.' && c != '_' && c != '+'
			     && c != '-' && c != '%' && c != '?' && c != '&' && c != '=' && c != '#'))
      uw_error(ctx, FATAL, "Disallowed character in CSS URL");
  }

  return s;
}

uw_Basis_string uw_Basis_property(uw_context ctx, uw_Basis_string s) {
  char *p;

  if (!*s)
    uw_error(ctx, FATAL, "Empty CSS property");

  if (!U8_IS_SINGLE(s[0]) || (!islower((int)s[0]) && s[0] != '_'))
    uw_error(ctx, FATAL, "Bad initial character in CSS property");

  for (p = s; *p; ++p) {
    char c = *p;
    if (!U8_IS_SINGLE(c) || (!islower((int)c) && !isdigit((int)c) && c != '_' && c != '-'))
      uw_error(ctx, FATAL, "Disallowed character in CSS property");
  }

  return s;
}

uw_Basis_string uw_Basis_fieldName(uw_context ctx, uw_Basis_postField f) {
  (void)ctx;
  return f.name;
}

uw_Basis_string uw_Basis_fieldValue(uw_context ctx, uw_Basis_postField f) {
  (void)ctx;
  return f.value;
}

uw_Basis_string uw_Basis_remainingFields(uw_context ctx, uw_Basis_postField f) {
  (void)ctx;
  return f.remaining;
}

uw_Basis_postField *uw_Basis_firstFormField(uw_context ctx, uw_Basis_string s) {
  char *unurl;
  uw_Basis_postField *f;

  if (!ctx->hasPostBody)
    uw_error(ctx, FATAL, "firstFormField called when there is no POST body");

  if (s < ctx->postBody.data || s >= ctx->postBody.data + ctx->postBody.len)
    return NULL;

  f = uw_malloc(ctx, sizeof(uw_Basis_postField));
  unurl = s;
  f->name = uw_Basis_unurlifyString_fromClient(ctx, &unurl);
  s = strchr(s, 0);
  if (!s)
    uw_error(ctx, FATAL, "firstFormField: Missing null terminator");
  ++s;
  unurl = s;
  f->value = uw_Basis_unurlifyString_fromClient(ctx, &unurl);
  s = strchr(s, 0);
  if (!s)
    uw_error(ctx, FATAL, "firstFormField: Missing null terminator");
  f->remaining = s+1;

  return f;
}

uw_Basis_string uw_Basis_blessData(uw_context ctx, uw_Basis_string s) {
  char *p = s;

  for (; *p; ++p)
    if (!U8_IS_SINGLE(*p) || (!isalnum(*p) && *p != '-' && *p != '_'))
      uw_error(ctx, FATAL, "Illegal HTML5 data-* attribute: %s", s);

  return s;
}

int uw_remoteSock(uw_context ctx) {
  return ctx->remoteSock;
}

void uw_set_remoteSock(uw_context ctx, int sock) {
  ctx->remoteSock = sock;
}


// Sqlcache

static void uw_Sqlcache_freeValue(uw_Sqlcache_Value *value) {
  if (value) {
    free(value->result);
    free(value->output);
    free(value->scriptOutput);
    free(value);
  }
}

static void uw_Sqlcache_freeEntry(uw_Sqlcache_Entry* entry) {
  if (entry) {
    free(entry->key);
    uw_Sqlcache_freeValue(entry->value);
    free(entry);
  }
}

// TODO: pick a number.
static unsigned int uw_Sqlcache_maxSize = 1234567890;

static void uw_Sqlcache_delete(uw_Sqlcache_Cache *cache, uw_Sqlcache_Entry *entry) {
  if (entry) {
    HASH_DEL(cache->table, entry);
    uw_Sqlcache_freeEntry(entry);
  }
}

static uw_Sqlcache_Entry *uw_Sqlcache_find(uw_Sqlcache_Cache *cache, char *key, size_t len, int bump) {
  uw_Sqlcache_Entry *entry = NULL;
  HASH_FIND(hh, cache->table, key, len, entry);
  if (entry && bump) {
    // Bump for LRU purposes.
    HASH_DEL(cache->table, entry);
    // Important that we use [entry->key], because [key] might be ephemeral.
    HASH_ADD_KEYPTR(hh, cache->table, entry->key, len, entry);
  }
  return entry;
}

static void uw_Sqlcache_add(uw_Sqlcache_Cache *cache, uw_Sqlcache_Entry *entry, size_t len) {
  HASH_ADD_KEYPTR(hh, cache->table, entry->key, len, entry);
  if (HASH_COUNT(cache->table) > uw_Sqlcache_maxSize) {
    // Deletes the first element of the cache.
    uw_Sqlcache_delete(cache, cache->table);
  }
}

static unsigned long uw_Sqlcache_getTimeNow(uw_Sqlcache_Cache *cache) {
  // TODO: verify that this makes time comparisons do the Right Thing.
  return cache->timeNow++;
}

static unsigned long uw_Sqlcache_timeMax(unsigned long x, unsigned long y) {
  return x > y ? x : y;
}

static char uw_Sqlcache_keySep = '_';

static char *uw_Sqlcache_allocKeyBuffer(char **keys, size_t numKeys) {
  size_t len = 0;
  while (numKeys-- > 0) {
    char* k = keys[numKeys];
    if (!k) {
      // Can only happen when flushing, in which case we don't need anything past the null key.
      break;
    }
    // Leave room for separator.
    len += 1 + strlen(k);
  }
  char *buf = malloc(len+1);
  // If nothing is copied into the buffer, it should look like it has length 0.
  buf[0] = 0;
  return buf;
}

static char *uw_Sqlcache_keyCopy(char *buf, char *key) {
  *buf++ = uw_Sqlcache_keySep;
  return stpcpy(buf, key);
}

// The NUL-terminated prefix of [key] below always looks something like "_k1_k2_k3..._kn".

uw_Sqlcache_Value *uw_Sqlcache_check(uw_context ctx, uw_Sqlcache_Cache *cache, char **keys) {
  (void)ctx;
  int doBump = random() % 1024 == 0;
  if (doBump) {
    pthread_rwlock_wrlock(&cache->lockIn);
  } else {
    pthread_rwlock_rdlock(&cache->lockIn);
  }
  size_t numKeys = cache->numKeys;
  char *key = uw_Sqlcache_allocKeyBuffer(keys, numKeys);
  char *buf = key;
  time_t timeInvalid = cache->timeInvalid;
  uw_Sqlcache_Entry *entry = NULL;
  if (numKeys == 0) {
    entry = cache->table;
    if (!entry) {
      free(key);
      pthread_rwlock_unlock(&cache->lockIn);
      return NULL;
    }
  } else {
    while (numKeys-- > 0) {
      buf = uw_Sqlcache_keyCopy(buf, keys[numKeys]);
      size_t len = buf - key;
      entry = uw_Sqlcache_find(cache, key, len, doBump);
      if (!entry) {
        free(key);
        pthread_rwlock_unlock(&cache->lockIn);
        return NULL;
      }
      timeInvalid = uw_Sqlcache_timeMax(timeInvalid, entry->timeInvalid);
    }
    free(key);
  }
  uw_Sqlcache_Value *value = entry->value;
  pthread_rwlock_unlock(&cache->lockIn);
  // ASK: though the argument isn't trivial, this is safe, right?
  // Returning outside the lock is safe because updates happen at commit time.
  // Those are the only times the returned value or its strings can get freed.
  // Handler output is a new string, so it's safe to free this at commit time.
  return value && timeInvalid < value->timeValid ? value : NULL;
}

static void uw_Sqlcache_storeCommitOne(uw_Sqlcache_Cache *cache, char **keys, uw_Sqlcache_Value *value) {
  pthread_rwlock_wrlock(&cache->lockIn);
  size_t numKeys = cache->numKeys;
  time_t timeNow = uw_Sqlcache_getTimeNow(cache);
  uw_Sqlcache_Entry *entry = NULL;
  if (numKeys == 0) {
    entry = cache->table;
    if (!entry) {
      entry = calloc(1, sizeof(uw_Sqlcache_Entry));
      entry->key = NULL;
      entry->value = NULL;
      entry->timeInvalid = 0;
      cache->table = entry;
    }
  } else {
    char *key = uw_Sqlcache_allocKeyBuffer(keys, numKeys);
    char *buf = key;
    while (numKeys-- > 0) {
      buf = uw_Sqlcache_keyCopy(buf, keys[numKeys]);
      size_t len = buf - key;

      entry = uw_Sqlcache_find(cache, key, len, 1);
      if (!entry) {
        entry = calloc(1, sizeof(uw_Sqlcache_Entry));
        entry->key = strdup(key);
        entry->value = NULL;
        entry->timeInvalid = 0;
        uw_Sqlcache_add(cache, entry, len);
      }
    }
    free(key);
  }
  if (!entry->value || entry->value->timeValid < value->timeValid) {
    uw_Sqlcache_freeValue(entry->value);
    entry->value = value;
    entry->value->timeValid = timeNow;
  }
  pthread_rwlock_unlock(&cache->lockIn);
}

static void uw_Sqlcache_flushCommitOne(uw_Sqlcache_Cache *cache, char **keys) {
  (void)cache;
  (void)keys;
}

static void uw_Sqlcache_commit(void *data) {
  uw_context ctx = (uw_context)data;
  uw_Sqlcache_Update *update = ctx->cacheUpdate;
  while (update) {
    uw_Sqlcache_Cache *cache = update->cache;
    char **keys = update->keys;
    if (update->value) {
      uw_Sqlcache_storeCommitOne(cache, keys, update->value);
    } else {
      uw_Sqlcache_flushCommitOne(cache, keys);
    }
    update = update->next;
  }
}

static void uw_Sqlcache_free(void *data, int dontCare) {
  (void)dontCare;
  uw_context ctx = (uw_context)data;
  uw_Sqlcache_Update *update = ctx->cacheUpdate;
  while (update) {
    char** keys = update->keys;
    size_t numKeys = update->cache->numKeys;
    while (numKeys-- > 0) {
      free(keys[numKeys]);
    }
    free(keys);
    // Don't free [update->value]: it's in the cache now!
    uw_Sqlcache_Update *nextUpdate = update->next;
    free(update);
    update = nextUpdate;
  }
  ctx->cacheUpdate = NULL;
  ctx->cacheUpdateTail = NULL;
  uw_Sqlcache_Unlock *unlock = ctx->cacheUnlock;
  while (unlock) {
    pthread_rwlock_unlock(unlock->lock);
    uw_Sqlcache_Unlock *nextUnlock = unlock->next;
    free(unlock);
    unlock = nextUnlock;
  }
  ctx->cacheUnlock = NULL;
}

static void uw_Sqlcache_pushUnlock(uw_context ctx, pthread_rwlock_t *lock) {
  if (!ctx->cacheUnlock) {
    // Just need one registered commit for both updating and unlocking.
    uw_register_transactional(ctx, ctx, uw_Sqlcache_commit, NULL, uw_Sqlcache_free);
  }
  uw_Sqlcache_Unlock *unlock = malloc(sizeof(uw_Sqlcache_Unlock));
  unlock->lock = lock;
  unlock->next = ctx->cacheUnlock;
  ctx->cacheUnlock = unlock;
}

void uw_Sqlcache_rlock(uw_context ctx, uw_Sqlcache_Cache *cache) {
  pthread_rwlock_rdlock(&cache->lockOut);
  uw_Sqlcache_pushUnlock(ctx, &cache->lockOut);
}

void uw_Sqlcache_wlock(uw_context ctx, uw_Sqlcache_Cache *cache) {
  pthread_rwlock_wrlock(&cache->lockOut);
  uw_Sqlcache_pushUnlock(ctx, &cache->lockOut);
}

static char **uw_Sqlcache_copyKeys(char **keys, size_t numKeys) {
  char **copy = malloc(sizeof(char *) * numKeys);
  while (numKeys-- > 0) {
    char *k = keys[numKeys];
    copy[numKeys] = k ? strdup(k) : NULL;
  }
  return copy;
}

void uw_Sqlcache_store(uw_context ctx, uw_Sqlcache_Cache *cache, char **keys, uw_Sqlcache_Value *value) {
  uw_Sqlcache_Update *update = malloc(sizeof(uw_Sqlcache_Update));
  update->cache = cache;
  update->keys = uw_Sqlcache_copyKeys(keys, cache->numKeys);
  update->value = value;
  update->next = NULL;
  // Can't use [uw_Sqlcache_getTimeNow] because it modifies state and we don't have the lock.
  pthread_rwlock_rdlock(&cache->lockIn);
  value->timeValid = cache->timeNow;
  pthread_rwlock_unlock(&cache->lockIn);
  if (ctx->cacheUpdateTail) {
    ctx->cacheUpdateTail->next = update;
  } else {
    ctx->cacheUpdate = update;
  }
  ctx->cacheUpdateTail = update;
}

void uw_Sqlcache_flush(uw_context ctx, uw_Sqlcache_Cache *cache, char **keys) {
  (void)ctx;
  // A flush has to happen immediately so that subsequent stores in the same transaction fail.
  // This is safe to do because we will always call [uw_Sqlcache_wlock] earlier.
  // If the transaction fails, the only harm done is a few extra cache misses.
  pthread_rwlock_wrlock(&cache->lockIn);
  size_t numKeys = cache->numKeys;
  if (numKeys == 0) {
    uw_Sqlcache_Entry *entry = cache->table;
    if (entry) {
      uw_Sqlcache_freeValue(entry->value);
      entry->value = NULL;
    }
  } else {
    char *key = uw_Sqlcache_allocKeyBuffer(keys, numKeys);
    char *buf = key;
    time_t timeNow = uw_Sqlcache_getTimeNow(cache);
    while (numKeys-- > 0) {
      char *k = keys[numKeys];
      if (!k) {
        size_t len = buf - key;
        if (len == 0) {
          // The first key was null.
          cache->timeInvalid = timeNow;
        } else {
          uw_Sqlcache_Entry *entry = uw_Sqlcache_find(cache, key, len, 0);
          if (entry) {
            entry->timeInvalid = timeNow;
          }
        }
        free(key);
        pthread_rwlock_unlock(&cache->lockIn);
        return;
      }
      buf = uw_Sqlcache_keyCopy(buf, k);
    }
    // All the keys were non-null, so we delete the pointed-to entry.
    size_t len = buf - key;
    uw_Sqlcache_Entry *entry = uw_Sqlcache_find(cache, key, len, 0);
    free(key);
    uw_Sqlcache_delete(cache, entry);
  }
  pthread_rwlock_unlock(&cache->lockIn);
}

int strcmp_nullsafe(const char *str1, const char *str2) {
  if (str1)
    return strcmp(str1, str2);
  else
    return 1;
}

static int is_valid_hash(uw_Basis_string hash) {
  for (; *hash; ++hash)
    if (!U8_IS_SINGLE(*hash) || !isxdigit(*hash))
      return 0;

  return 1;
}

uw_unit uw_Basis_cache_file(uw_context ctx, uw_Basis_blob contents) {
  char *dir = ctx->app->file_cache, path[1024], tempfile[1024];
  unsigned char *res, *hash;
  char *hash_encoded;
  int fd, len, i;
  ssize_t written_so_far = 0;

  if (!dir)
    return uw_unit_v;

  hash = uw_malloc(ctx, SHA512_DIGEST_LENGTH);
  res = SHA512((unsigned char *)contents.data, contents.size, hash);
  if (!res)
    uw_error(ctx, FATAL, "Can't hash file contents");

  hash_encoded = uw_malloc(ctx, SHA512_DIGEST_LENGTH * 2 + 1);
  for (i = 0; i < SHA512_DIGEST_LENGTH; ++i)
    sprintf(hash_encoded + 2 * i, "%02x", (int)hash[i]);
  hash_encoded[SHA512_DIGEST_LENGTH * 2] = 0;

  len = snprintf(tempfile, sizeof tempfile, "%s/tmpXXXXXX", dir);
  if (len < 0 || len >= sizeof tempfile)
    uw_error(ctx, FATAL, "Error assembling file path for cache (temporary)");

  fd = mkstemp(tempfile);
  if (fd < 0)
    uw_error(ctx, FATAL, "Error creating temporary file %s for cache", tempfile);

  while (written_so_far < contents.size) {
    ssize_t written_just_now = write(fd, contents.data + written_so_far, contents.size - written_so_far);
    if (written_just_now <= 0) {
      close(fd);
      uw_error(ctx, FATAL, "Error writing all bytes to cached file");
    }
    written_so_far += written_just_now;
  }

  close(fd);

  len = snprintf(path, sizeof path, "%s/%s", dir, hash_encoded);
  if (len < 0 || len >= sizeof path)
    uw_error(ctx, FATAL, "Error assembling file path for cache");

  if (rename(tempfile, path))
    uw_error(ctx, FATAL, "Error renaming temporary file into cache");

  return uw_unit_v;
}

uw_Basis_blob uw_Basis_check_filecache(uw_context ctx, uw_Basis_string hash) {
  char path[1024], *dir = ctx->app->file_cache, *filedata;
  int len;
  long size, read_so_far = 0;
  FILE *fp;
  uw_Basis_blob res;

  // Hashes come formatted for printing by Postgres, which means they start with
  // two extra characters.  Let's remove them.
  if (hash[0] == '\\' && hash[1] == 'x')
    hash += 2;

  if (!dir)
    uw_error(ctx, FATAL, "Checking file cache when no directory is set");

  if (!is_valid_hash(hash))
    uw_error(ctx, FATAL, "Checking file cache with invalid hash %s", hash);

  len = snprintf(path, sizeof path, "%s/%s", dir, hash);
  if (len < 0 || len >= sizeof path)
    uw_error(ctx, FATAL, "Error assembling file path for cache");

  fp = fopen(path, "r");
  if (!fp) {
    ctx->file_cache_missed = 1;
    uw_error(ctx, UNLIMITED_RETRY, "Missed in the file cache for hash %s", hash);
  }
  uw_push_cleanup(ctx, (void (*)(void *))fclose, fp);

  if (fseek(fp, 0L, SEEK_END))
    uw_error(ctx, FATAL, "Error seeking to end of cached file");

  size = ftell(fp);
  if (size < 0)
    uw_error(ctx, FATAL, "Error getting size of cached file");

  rewind(fp);
  filedata = uw_malloc(ctx, size);

  while (read_so_far < size) {
    size_t just_read = fread(filedata + read_so_far, 1, size - read_so_far, fp);
    if (just_read <= 0)
      uw_error(ctx, FATAL, "Error reading all bytes of cached file");
    read_so_far += just_read;
  }

  uw_pop_cleanup(ctx);

  res.size = size;
  res.data = filedata;
  return res;
}

uw_Basis_bool uw_Basis_filecache_missed(uw_context ctx) {
  return !!(ctx->file_cache_missed);
}
