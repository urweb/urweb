#define _XOPEN_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <setjmp.h>
#include <stdarg.h>

#include <pthread.h>

#include "types.h"

uw_unit uw_unit_v = {};


// Socket extras

int uw_really_send(int sock, const void *buf, size_t len) {
  while (len > 0) {
    size_t n = send(sock, buf, len, 0);

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

typedef struct channel_list {
  struct channel *data;
  struct channel_list *next;
} channel_list;

typedef struct client {
  size_t id;
  usage mode;
  union {
    struct client *next;
    struct {
      pthread_mutex_t lock;
      int pass;
      buf msgs;
      int sock;
      time_t last_contact;
      unsigned refcount;
      channel_list *channels;
    } used;
  } data;
} client;

typedef struct client_list {
  client *data;
  struct client_list *next;
} client_list;

typedef struct channel {
  size_t id;
  usage mode;
  union {
    struct channel *next;
    struct {
      pthread_mutex_t lock;
      client_list *clients;
      unsigned refcount;
    } used;
  } data;
} channel;


// Persistent client state

static client **clients, *clients_free;
static size_t n_clients;

static pthread_mutex_t clients_mutex = PTHREAD_MUTEX_INITIALIZER;

static client *uw_new_client() {
  client *c;

  pthread_mutex_lock(&clients_mutex);

  if (clients_free) {
    c = clients_free;
    clients_free = clients_free->data.next;
  }
  else {
    ++n_clients;
    clients = realloc(clients, sizeof(client) * n_clients);
    c = malloc(sizeof(client));
    c->id = n_clients-1;
    clients[n_clients-1] = c;
  }

  c->mode = USED;
  pthread_mutex_init(&c->data.used.lock, NULL);
  c->data.used.pass = rand();
  c->data.used.sock = -1;
  c->data.used.last_contact = time(NULL);
  buf_init(&c->data.used.msgs, 0);
  c->data.used.refcount = 0;
  c->data.used.channels = NULL;

  pthread_mutex_unlock(&clients_mutex);

  return c;
}

static const char begin_msgs[] = "HTTP/1.1 200 OK\r\nContent-type: text/plain\r\n\r\n";

static client *uw_find_client(size_t id) {
  client *c;

  pthread_mutex_lock(&clients_mutex);

  if (id >= n_clients) {
    pthread_mutex_unlock(&clients_mutex);
    return NULL;
  }

  c = clients[id];

  if (c->mode != USED) {
    pthread_mutex_unlock(&clients_mutex);
    return NULL;
  }
  
  pthread_mutex_lock(&c->data.used.lock);
  ++c->data.used.refcount;
  pthread_mutex_unlock(&c->data.used.lock);
  pthread_mutex_unlock(&clients_mutex);
  return c;
}

static void uw_release_client(client *c) {
  pthread_mutex_lock(&c->data.used.lock);
  --c->data.used.refcount;
  pthread_mutex_unlock(&c->data.used.lock);
}

void uw_client_connect(size_t id, int pass, int sock) {
  client *c = uw_find_client(id);

  if (c == NULL) {
    close(sock);
    fprintf(stderr, "Out-of-bounds client request (%d)\n", (int)id);
    return;
  }

  uw_release_client(c);

  pthread_mutex_lock(&c->data.used.lock);

  if (pass != c->data.used.pass) {
    pthread_mutex_unlock(&c->data.used.lock);
    close(sock);
    fprintf(stderr, "Wrong client password (%d)\n", (int)id);
    return;
  }

  if (c->data.used.sock != -1) {
    pthread_mutex_unlock(&c->data.used.lock);
    close(sock);
    fprintf(stderr, "Duplicate client connection (%d)\n", (int)id);
    return;
  }

  c->data.used.last_contact = time(NULL);

  if (buf_used(&c->data.used.msgs) > 0) {
    uw_really_send(sock, begin_msgs, sizeof(begin_msgs) - 1);
    uw_really_send(sock, c->data.used.msgs.start, buf_used(&c->data.used.msgs));
    close(sock);
  }
  else
    c->data.used.sock = sock;

  pthread_mutex_unlock(&c->data.used.lock);
}


static void uw_free_client(client *c) {
  channel_list *chs;

  printf("Freeing client %d\n", c->id);

  if (c->mode == USED) {
    pthread_mutex_lock(&c->data.used.lock);

    for (chs = c->data.used.channels; chs; ) {
      client_list *prev, *cs;
      
      channel *ch = chs->data;
      channel_list *tmp = chs->next;
      free(chs);
      chs = tmp;

      pthread_mutex_lock(&ch->data.used.lock);
      for (prev = NULL, cs = ch->data.used.clients; cs; ) {
        if (cs->data == c) {
          client_list *tmp = cs->next;
          free(cs);
          cs = tmp;
          if (prev)
            prev->next = cs;
          else
            ch->data.used.clients = cs;
        }
        else {
          prev = cs;
          cs = cs->next;
        }
      }
      pthread_mutex_unlock(&ch->data.used.lock);
    }

    if (c->data.used.sock != -1)
      close(c->data.used.sock);

    pthread_mutex_unlock(&c->data.used.lock);
    pthread_mutex_destroy(&c->data.used.lock);
    buf_free(&c->data.used.msgs);
    c->mode = UNUSED;

    c->data.next = clients_free;
    clients_free = c;
  }
}

void uw_prune_clients(time_t timeout) {
  size_t i;
  time_t cutoff;

  cutoff = time(NULL) - timeout;

  pthread_mutex_lock(&clients_mutex);

  for (i = 0; i < n_clients; ++i) {
    if (clients[i]->mode == USED && clients[i]->data.used.last_contact < cutoff
        && clients[i]->data.used.refcount == 0)
      uw_free_client(clients[i]);
  }

  pthread_mutex_unlock(&clients_mutex);
}


// Persistent channel state


static channel **channels, *channels_free;
static size_t n_channels;

static pthread_mutex_t channels_mutex = PTHREAD_MUTEX_INITIALIZER;

static channel *uw_new_channel() {
  channel *ch;

  pthread_mutex_lock(&channels_mutex);

  if (channels_free) {
    ch = channels_free;
    channels_free = channels_free->data.next;
  }
  else {
    ++n_channels;
    channels = realloc(channels, sizeof(channels) * n_channels);
    ch = malloc(sizeof(channel));
    ch->id = n_channels-1;
    channels[n_channels-1] = ch;
  }

  ch->mode = USED;
  pthread_mutex_init(&ch->data.used.lock, NULL);
  ch->data.used.clients = NULL;
  ch->data.used.refcount = 0;

  pthread_mutex_unlock(&channels_mutex);

  return ch;
}

static void uw_free_channel(channel *ch) {
  if (ch->mode == USED) {
    client_list *cs;

    for (cs = ch->data.used.clients; cs; ) {
      client_list *tmp = cs->next;
      free(cs);
      cs = tmp;
    }
    pthread_mutex_destroy(&ch->data.used.lock);
    ch->mode = UNUSED;
    ch->data.next = channels_free;
    channels_free = ch;
  }
}

static channel *uw_find_channel(size_t id) {
  channel *ch = NULL;

  pthread_mutex_lock(&channels_mutex);

  if (id < n_channels && channels[id]->mode == USED) {
    ch = channels[id];

    pthread_mutex_lock(&ch->data.used.lock);
    ++ch->data.used.refcount;
    pthread_mutex_unlock(&ch->data.used.lock);
  }

  pthread_mutex_unlock(&channels_mutex);

  return ch;
}

static void uw_release_channel(channel *ch) {
  pthread_mutex_lock(&ch->data.used.lock);
  ++ch->data.used.refcount;
  pthread_mutex_unlock(&ch->data.used.lock);
}

static void uw_subscribe(channel *ch, client *c) {
  client_list *cs = malloc(sizeof(client_list));

  pthread_mutex_lock(&ch->data.used.lock);

  cs->data = c;
  cs->next = ch->data.used.clients;
  ch->data.used.clients = cs;

  pthread_mutex_unlock(&ch->data.used.lock);
}

static void uw_unsubscribe(channel *ch, client *c) {
  client_list *prev, *cur, *tmp;

  pthread_mutex_lock(&ch->data.used.lock);

  for (prev = NULL, cur = ch->data.used.clients; cur; ) {
    if (cur->data == c) {
      if (prev)
        prev->next = cur->next;
      else
        ch->data.used.clients = cur->next;
      tmp = cur;
      cur = cur->next;
      free(tmp);
    }
    else {
      prev = cur;
      cur = cur->next;
    }
  }

  pthread_mutex_unlock(&ch->data.used.lock);
}

static void uw_channel_send(channel *ch, const char *msg) {
  size_t len = strlen(msg), preLen;
  char pre[INTS_MAX + 2];
  client_list *cs;

  sprintf(pre, "%d\n", (int)ch->id);
  preLen = strlen(pre);

  pthread_mutex_lock(&ch->data.used.lock);

  for (cs = ch->data.used.clients; cs; cs = cs->next) {
    client *c = cs->data;

    pthread_mutex_lock(&c->data.used.lock);

    if (c->data.used.sock != -1) {
      uw_really_send(c->data.used.sock, begin_msgs, sizeof(begin_msgs) - 1);
      uw_really_send(c->data.used.sock, pre, preLen);
      uw_really_send(c->data.used.sock, msg, len);
      uw_really_send(c->data.used.sock, "\n", 1);
      close(c->data.used.sock);
      c->data.used.sock = -1;
    } else {
      buf_append(&c->data.used.msgs, pre, preLen);
      buf_append(&c->data.used.msgs, msg, len);
      buf_append(&c->data.used.msgs, "\n", 1);
    }

    pthread_mutex_unlock(&c->data.used.lock);
  }

  pthread_mutex_unlock(&ch->data.used.lock);
}


// Global entry points

void uw_global_init() {
  srand(time(NULL) ^ getpid());

  clients = malloc(0);
  channels = malloc(0);
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
  usage mode;
  channel *ch;
  enum { OLD, NEW } newness;

  size_t n_subscribed;
  client **subscribed;

  buf msgs;
} channel_delta;

struct uw_context {
  char *headers, *headers_end;

  buf outHeaders, page, heap, script;
  char **inputs;

  int source_count;

  void *db;

  jmp_buf jmp_buf;

  regions *regions;

  cleanup *cleanup, *cleanup_front, *cleanup_back;

  const char *script_header, *url_prefix;

  size_t n_deltas;
  channel_delta *deltas;

  char error_message[ERROR_BUF_LEN];
};

extern int uw_inputs_len;

uw_context uw_init(size_t outHeaders_len, size_t script_len, size_t page_len, size_t heap_len) {
  uw_context ctx = malloc(sizeof(struct uw_context));

  ctx->headers = ctx->headers_end = NULL;

  buf_init(&ctx->outHeaders, outHeaders_len);
  buf_init(&ctx->page, page_len);
  buf_init(&ctx->heap, heap_len);
  buf_init(&ctx->script, script_len);
  ctx->script.start[0] = 0;

  ctx->inputs = calloc(uw_inputs_len, sizeof(char *));

  ctx->db = NULL;

  ctx->regions = NULL;

  ctx->cleanup_front = ctx->cleanup_back = ctx->cleanup = malloc(0);

  ctx->script_header = "";
  ctx->url_prefix = "/";
  
  ctx->error_message[0] = 0;

  ctx->source_count = 0;

  ctx->n_deltas = 0;
  ctx->deltas = malloc(0);

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
  free(ctx->cleanup);

  for (i = 0; i < ctx->n_deltas && ctx->deltas[i].mode == USED; ++i) {
    free(ctx->deltas[i].subscribed);
    buf_free(&ctx->deltas[i].msgs);
  }

  free(ctx);
}

void uw_reset_keep_error_message(uw_context ctx) {
  buf_reset(&ctx->outHeaders);
  buf_reset(&ctx->script);
  ctx->script.start[0] = 0;
  buf_reset(&ctx->page);
  buf_reset(&ctx->heap);
  ctx->regions = NULL;
  ctx->cleanup_front = ctx->cleanup;
  ctx->source_count = 0;
  if (ctx->n_deltas > 0)
    ctx->deltas[0].mode = UNUSED;
}

void uw_reset_keep_request(uw_context ctx) {
  uw_reset_keep_error_message(ctx);
  ctx->error_message[0] = 0;
}

void uw_reset(uw_context ctx) {
  uw_reset_keep_request(ctx);
  memset(ctx->inputs, 0, uw_inputs_len * sizeof(char *));
}

void uw_db_init(uw_context);
void uw_handle(uw_context, char *);

failure_kind uw_begin_init(uw_context ctx) {
  int r = setjmp(ctx->jmp_buf);

  if (r == 0)
    uw_db_init(ctx);

  return r;
}

void uw_set_headers(uw_context ctx, char *headers) {
  char *s = headers, *s2;
  ctx->headers = headers;

  while (s2 = strchr(s, '\r')) {
    s = s2;

    if (s[1] == 0)
      break;

    *s = 0;
    s += 2;
  }

  ctx->headers_end = s;
}

int uw_db_begin(uw_context);

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

failure_kind uw_begin(uw_context ctx, char *path) {
  int r = setjmp(ctx->jmp_buf);

  if (r == 0) {
    if (uw_db_begin(ctx))
      uw_error(ctx, BOUNDED_RETRY, "Error running SQL BEGIN");
    uw_handle(ctx, path);
  }

  return r;
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

int uw_input_num(char*);

void uw_set_input(uw_context ctx, char *name, char *value) {
  int n = uw_input_num(name);

  if (n < 0)
    uw_error(ctx, FATAL, "Bad input name %s", name);

  if (n >= uw_inputs_len)
    uw_error(ctx, FATAL, "For input name %s, index %d is out of range", name, n);

  ctx->inputs[n] = value;

  //printf("[%d] %s = %s\n", n, name, value);
}

char *uw_get_input(uw_context ctx, int n) {
  if (n < 0)
    uw_error(ctx, FATAL, "Negative input index %d", n);
  if (n >= uw_inputs_len)
    uw_error(ctx, FATAL, "Out-of-bounds input index %d", n);
  //printf("[%d] = %s\n", n, ctx->inputs[n]);
  return ctx->inputs[n];
}

char *uw_get_optional_input(uw_context ctx, int n) {
  if (n < 0)
    uw_error(ctx, FATAL, "Negative input index %d", n);
  if (n >= uw_inputs_len)
    uw_error(ctx, FATAL, "Out-of-bounds input index %d", n);
  //printf("[%d] = %s\n", n, ctx->inputs[n]);
  return (ctx->inputs[n] == NULL ? "" : ctx->inputs[n]);
}

void uw_set_script_header(uw_context ctx, const char *s) {
  ctx->script_header = s;
}

void uw_set_url_prefix(uw_context ctx, const char *s) {
  ctx->url_prefix = s;
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

static void uw_check_heap(uw_context ctx, size_t extra) {
  buf_check_ctx(ctx, &ctx->heap, extra, "heap chunk");
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
  if (ctx->script_header[0] == 0)
    return "";
  else {
    int pass;
    client *c = uw_new_client(&pass);

    char *r = uw_malloc(ctx, strlen(ctx->script_header) + 56 + 2 * INTS_MAX + buf_used(&ctx->script)
                        + strlen(ctx->url_prefix));
    sprintf(r, "%s<script>client_id=%d;client_pass=%d;url_prefix=\"%s\";%s</script>",
            ctx->script_header, (int)c->id, c->data.used.pass, ctx->url_prefix, ctx->script.start);
    return r;
  }
}

const char *uw_Basis_get_listener(uw_context ctx, uw_Basis_string onload) {
  if (ctx->script_header[0] == 0)
    return "";
  else if (onload[0] == 0)
    return " onload='listener()'";
  else {
    uw_Basis_string s = uw_malloc(ctx, strlen(onload) + 22);

    sprintf(s, " onload='listener();%s'", onload);
    return s;
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
  ctx->heap.front = s2 + 1;
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
      strcpy(s2, "\\\"");
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

uw_Basis_int uw_Basis_new_client_source(uw_context ctx, uw_Basis_string s) {
  int len;
  size_t s_len = strlen(s);

  uw_check_script(ctx, 12 + INTS_MAX + s_len);
  sprintf(ctx->script.front, "var s%d=sc(%n", ctx->source_count, &len);
  ctx->script.front += len;
  strcpy(ctx->script.front, s);
  ctx->script.front += s_len;
  strcpy(ctx->script.front, ");");
  ctx->script.front += 2;

  return ctx->source_count++;
}

uw_unit uw_Basis_set_client_source(uw_context ctx, uw_Basis_int n, uw_Basis_string s) {
  int len;
  size_t s_len = strlen(s);

  uw_check_script(ctx, 6 + INTS_MAX + s_len);
  sprintf(ctx->script.front, "s%d.v=%n", (int)n, &len);
  ctx->script.front += len;
  strcpy(ctx->script.front, s);
  ctx->script.front += s_len;
  strcpy(ctx->script.front, ";");
  ctx->script.front++;

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

char *uw_Basis_attrifyString(uw_context ctx, uw_Basis_string s) {
  int len = strlen(s);
  char *result, *p;
  uw_check_heap(ctx, len * 6 + 1);

  result = p = ctx->heap.front;

  for (; *s; s++) {
    char c = *s;

    if (c == '"') {
      strcpy(p, "&quot;");
      p += 6;
    } else if (c == '&') {
      strcpy(p, "&amp;");
      p += 5;
    }
    else if (isprint(c))
      *p++ = c;
    else {
      int len2;
      sprintf(p, "&#%d;%n", c, &len2);
      p += len2;
    }
  }

  *p++ = 0;
  ctx->heap.front = p;
  return result;
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
    char c = *s;

    if (c == '"')
      uw_write_unsafe(ctx, "&quot;");
    else if (c == '&')
      uw_write_unsafe(ctx, "&amp;");
    else if (isprint(c))
      uw_writec_unsafe(ctx, c);
    else {
      uw_write_unsafe(ctx, "&#");
      uw_Basis_attrifyInt_w_unsafe(ctx, c);
      uw_writec_unsafe(ctx, ';');
    }
  }

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

  uw_check_heap(ctx, strlen(s) * 3 + 1);

  for (r = p = ctx->heap.front; *s; s++) {
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
  uw_check(ctx, strlen(s) * 3);

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

uw_Basis_channel uw_Basis_unurlifyChannel(uw_context ctx, char **s) {
  return uw_Basis_unurlifyInt(ctx, s);
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

static uw_Basis_string uw_unurlifyString_to(uw_context ctx, char *r, char *s) {
  char *s1, *s2;
  int n;

  for (s1 = r, s2 = s; *s2; ++s1, ++s2) {
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
  ctx->heap.front = uw_unurlifyString_to(ctx, ctx->heap.front, *s);
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

char *uw_Basis_htmlifyChannel(uw_context ctx, uw_Basis_channel ch) {
  return uw_Basis_htmlifyInt(ctx, ch);
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
    char c = *s;

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
      if (isprint(c))
        *s2++ = c;
      else {
        int len2;
        sprintf(s2, "&#%d;%n", c, &len2);
        s2 += len2;
      }
    }
  }

  *s2++ = 0;
  ctx->heap.front = s2;
  return r;
}

uw_unit uw_Basis_htmlifyString_w(uw_context ctx, uw_Basis_string s) {
  uw_check(ctx, strlen(s) * 6);

  for (; *s; s++) {
    char c = *s;

    switch (c) {
    case '<':
      uw_write_unsafe(ctx, "&lt;");
      break;
    case '&':
      uw_write_unsafe(ctx, "&amp;");
      break;
    default:
      if (isprint(c))
        uw_writec_unsafe(ctx, c);
      else {
        uw_write_unsafe(ctx, "&#");
        uw_Basis_attrifyInt_w_unsafe(ctx, c);
        uw_writec_unsafe(ctx, ';');
      }
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

uw_Basis_string uw_Basis_strdup(uw_context ctx, uw_Basis_string s1) {
  int len = strlen(s1) + 1;
  char *s;

  uw_check_heap(ctx, len);

  s = ctx->heap.front;

  strcpy(s, s1);
  ctx->heap.front += len;

  return s;
}

uw_Basis_string uw_Basis_maybe_strdup(uw_context ctx, uw_Basis_string s1) {
  if (s1)
    return uw_Basis_strdup(ctx, s1);
  else
    return NULL;
}


char *uw_Basis_sqlifyInt(uw_context ctx, uw_Basis_int n) {
  int len;
  char *r;

  uw_check_heap(ctx, INTS_MAX + 6);
  r = ctx->heap.front;
  sprintf(r, "%lld::int8%n", n, &len);
  ctx->heap.front += len+1;
  return r;
}

char *uw_Basis_sqlifyIntN(uw_context ctx, uw_Basis_int *n) {
  if (n == NULL)
    return "NULL";
  else
    return uw_Basis_sqlifyInt(ctx, *n);
}

char *uw_Basis_sqlifyFloat(uw_context ctx, uw_Basis_float n) {
  int len;
  char *r;

  uw_check_heap(ctx, FLOATS_MAX + 8);
  r = ctx->heap.front;
  sprintf(r, "%g::float8%n", n, &len);
  ctx->heap.front += len+1;
  return r;
}

char *uw_Basis_sqlifyFloatN(uw_context ctx, uw_Basis_float *n) {
  if (n == NULL)
    return "NULL";
  else
    return uw_Basis_sqlifyFloat(ctx, *n);
}


uw_Basis_string uw_Basis_sqlifyString(uw_context ctx, uw_Basis_string s) {
  char *r, *s2;

  uw_check_heap(ctx, strlen(s) * 2 + 10);

  r = s2 = ctx->heap.front;
  *s2++ = 'E';
  *s2++ = '\'';

  for (; *s; s++) {
    char c = *s;

    switch (c) {
    case '\'':
      strcpy(s2, "\\'");
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

  strcpy(s2, "'::text");
  ctx->heap.front = s2 + 8;
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

uw_Basis_float uw_Basis_stringToFloat_error(uw_context ctx, uw_Basis_string s) {
  char *endptr;
  uw_Basis_float n = strtod(s, &endptr);

  if (*s != '\0' && *endptr == '\0')
    return n;
  else
    uw_error(ctx, FATAL, "Can't parse float: %s", s);
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

uw_Basis_string uw_Basis_requestHeader(uw_context ctx, uw_Basis_string h) {
  int len = strlen(h);
  char *s = ctx->headers, *p;

  while (p = strchr(s, ':')) {
    if (p - s == len && !strncasecmp(s, h, len)) {
      return p + 2;
    } else {
      if ((s = strchr(p, 0)) && s < ctx->headers_end)
        s += 2;
      else
        return NULL;
    }
  }
}

uw_Basis_string uw_Basis_get_cookie(uw_context ctx, uw_Basis_string c) {
  int len = strlen(c);
  char *s = ctx->headers, *p = ctx->outHeaders.start;

  while (p = strstr(p, "\nSet-Cookie: ")) {
    char *p2;
    p += 13;
    p2 = strchr(p, '=');

    if (p2) {
      size_t sz = strcspn(p2+1, ";\r\n");

      if (!strncasecmp(p, c, p2 - p)) {
        char *ret = uw_malloc(ctx, sz + 1);
        memcpy(ret, p2+1, sz);
        ret[sz] = 0;
        return ret;
      }
    }
  }

  while (p = strchr(s, ':')) {
    if (!strncasecmp(s, "Cookie: ", 8)) {
      p += 2;
      while (1) {
        if (!strncmp(p, c, len)
            && p + len < ctx->headers_end && p[len] == '=')
          return p + 1 + len;
        else if (p = strchr(p, ';'))
          p += 2;
        else if ((s = strchr(s, 0)) && s < ctx->headers_end) {
          s += 2;
          break;
        }
        else
          return NULL;
      }
    } else {
      if ((s = strchr(p, 0)) && s < ctx->headers_end)
        s += 2;
      else
        return NULL;
    }
  }
}

uw_unit uw_Basis_set_cookie(uw_context ctx, uw_Basis_string prefix, uw_Basis_string c, uw_Basis_string v) {
  uw_write_header(ctx, "Set-Cookie: ");
  uw_write_header(ctx, c);
  uw_write_header(ctx, "=");
  uw_write_header(ctx, v);
  uw_write_header(ctx, "; path=");
  uw_write_header(ctx, prefix);
  uw_write_header(ctx, "\r\n");

  return uw_unit_v;
}

static channel_delta *allocate_delta(uw_context ctx, channel *ch) {
  size_t i;
  channel_delta *cd;

  for (i = 0; i < ctx->n_deltas && ctx->deltas[i].mode == USED && ctx->deltas[i].ch != ch; ++i);

  if (i < ctx->n_deltas && ctx->deltas[i].mode == USED && ctx->deltas[i].ch == ch)
    return &ctx->deltas[i];

  if (i < ctx->n_deltas)
    cd = &ctx->deltas[i];
  else {
    ++ctx->n_deltas;
    ctx->deltas = realloc(ctx->deltas, sizeof(channel_delta) * ctx->n_deltas);
    cd = &ctx->deltas[ctx->n_deltas-1];
  }
   
  cd->mode = USED;
  cd->newness = OLD;
  cd->ch = ch;
  if (cd->n_subscribed > 0)
    cd->subscribed[0] = NULL;
  buf_reset(&cd->msgs);
  return cd;
}

uw_Basis_channel uw_Basis_new_channel(uw_context ctx, uw_unit u) {
  size_t i;
  channel *ch = uw_new_channel();
  ++ch->data.used.refcount;
  channel_delta *cd = allocate_delta(ctx, ch);

  cd->newness = NEW;

  return ch->id;
}

static int delta_used(channel_delta *cd) {
  return cd->newness == NEW || buf_used(&cd->msgs) > 0 || (cd->n_subscribed > 0 && cd->subscribed[0]);
}

uw_unit uw_Basis_subscribe(uw_context ctx, uw_Basis_channel chn) {
  channel *ch = uw_find_channel(chn);

  if (ch == NULL)
    uw_error(ctx, FATAL, "Bad channel ID %d", (int)chn);
  else {
    size_t id = atoi(uw_Basis_requestHeader(ctx, "UrWeb-Client"));
    int pass = atoi(uw_Basis_requestHeader(ctx, "UrWeb-Pass"));
    client *c = uw_find_client(id);

    if (c == NULL) {
      uw_release_channel(ch);
      uw_error(ctx, FATAL, "Unknown client ID in subscription request");
    } else if (c->data.used.pass != pass) {
      uw_release_channel(ch);
      uw_release_client(c);
      uw_error(ctx, FATAL, "Wrong client password in subscription request");
    } else {
      size_t i;
      channel_delta *cd = allocate_delta(ctx, ch);

      if (delta_used(cd))
        uw_release_channel(ch);

      for (i = 0; i < cd->n_subscribed && cd->subscribed[i]; ++i);

      if (i < cd->n_subscribed)
        cd->subscribed[i] = c;
      else {
        ++cd->n_subscribed;
        cd->subscribed = realloc(cd->subscribed, sizeof(int) * cd->n_subscribed);
        cd->subscribed[cd->n_subscribed-1] = c;
      }
    }
  }

  return uw_unit_v;
}

uw_unit uw_Basis_send(uw_context ctx, uw_Basis_channel chn, uw_Basis_string msg) {
  channel *ch = uw_find_channel(chn);

  if (ch == NULL)
    uw_error(ctx, FATAL, "Bad channel ID %d", (int)chn);
  else {
    channel_delta *cd = allocate_delta(ctx, ch);
    if (delta_used(cd))
      uw_release_channel(ch);
    buf_append(&cd->msgs, msg, strlen(msg));
  }

  return uw_unit_v;
}

int uw_db_commit(uw_context);
int uw_db_rollback(uw_context);

void uw_commit(uw_context ctx) {
  size_t i, j;

  for (i = 0; i < ctx->n_deltas && ctx->deltas[i].mode == USED; ++i) {
    channel *ch = ctx->deltas[i].ch;

    for (j = 0; j < ctx->deltas[i].n_subscribed && ctx->deltas[i].subscribed[j]; ++j) {
      client *c = ctx->deltas[i].subscribed[j];

      uw_subscribe(ch, c);
      uw_release_client(c);
    }

    if (buf_used(&ctx->deltas[i].msgs) > 0) {
      uw_channel_send(ch, ctx->deltas[i].msgs.start);
    }

    uw_release_channel(ch);
  }

  if (uw_db_commit(ctx))
    uw_error(ctx, FATAL, "Error running SQL COMMIT");
}

int uw_rollback(uw_context ctx) {
  size_t i, j;

  for (i = 0; i < ctx->n_deltas && ctx->deltas[i].mode == USED; ++i) {
    channel *ch = ctx->deltas[i].ch;

    for (j = 0; j < ctx->deltas[i].n_subscribed && ctx->deltas[i].subscribed[j]; ++j) {
      client *c = ctx->deltas[i].subscribed[j];

      uw_release_client(c);
    }

    if (ctx->deltas[i].newness == NEW)
      uw_free_channel(ch);
    else
      uw_release_channel(ch);
  }

  return uw_db_rollback(ctx);
}
