#define _XOPEN_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <setjmp.h>
#include <stdarg.h>
#include <assert.h>

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

typedef struct client {
  unsigned id;
  usage mode;
  int pass;
  struct client *next;
  pthread_mutex_t lock;
  buf msgs;
  int sock;
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
}

static void release_client(client *c) {
  pthread_mutex_lock(&c->lock);
  --c->refcount;
  pthread_mutex_unlock(&c->lock);
}

static const char begin_msgs[] = "HTTP/1.1 200 OK\r\nContent-type: text/plain\r\n\r\n";

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

void uw_client_connect(unsigned id, int pass, int sock) {
  client *c = find_client(id);

  if (c == NULL) {
    close(sock);
    fprintf(stderr, "Out-of-bounds client request (%u)\n", id);
    return;
  }

  pthread_mutex_lock(&c->lock);

  if (c->mode != USED) {
    pthread_mutex_unlock(&c->lock);
    close(sock);
    fprintf(stderr, "Client request for unused slot (%u)\n", id);
    return;
  }

  if (pass != c->pass) {
    pthread_mutex_unlock(&c->lock);
    close(sock);
    fprintf(stderr, "Wrong client password (%u, %d)\n", id, pass);
    return;
  }

  if (c->sock != -1) {
    close(c->sock);
    c->sock = -1;
  }

  c->last_contact = time(NULL);

  if (buf_used(&c->msgs) > 0) {
    uw_really_send(sock, begin_msgs, sizeof(begin_msgs) - 1);
    uw_really_send(sock, c->msgs.start, buf_used(&c->msgs));
    buf_reset(&c->msgs);
    close(sock);
  }
  else
    c->sock = sock;

  pthread_mutex_unlock(&c->lock);
}

static void free_client(client *c) {
  printf("Freeing client %u\n", c->id);

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
    uw_really_send(c->sock, begin_msgs, sizeof(begin_msgs) - 1);
    uw_really_send(c->sock, msg->start, buf_used(msg));
    close(c->sock);
    c->sock = -1;
  } else
    buf_append(&c->msgs, msg->start, buf_used(msg));

  pthread_mutex_unlock(&c->lock);
}


// Global entry points

void uw_global_init() {
  srand(time(NULL) ^ getpid());

  clients = malloc(0);
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

  size_t n_deltas, used_deltas;
  delta *deltas;

  int timeout;

  client *client;

  char error_message[ERROR_BUF_LEN];
};

extern int uw_inputs_len, uw_timeout;

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

  ctx->n_deltas = ctx->used_deltas = 0;
  ctx->deltas = malloc(0);

  ctx->timeout = uw_timeout;

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

  for (i = 0; i < ctx->n_deltas; ++i)
    buf_free(&ctx->deltas[i].msgs);

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
  ctx->used_deltas = 0;
  ctx->client = NULL;
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

  return NULL;
}

void uw_login(uw_context ctx) {
  if (ctx->script_header[0]) {
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

uw_Basis_client uw_Basis_self(uw_context ctx, uw_unit u) {
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
    char *r = uw_malloc(ctx, strlen(ctx->script_header) + 18 + buf_used(&ctx->script));
    sprintf(r, "%s<script>%s</script>",
            ctx->script_header,
            ctx->script.start);
    return r;
  }
}

const char *uw_Basis_get_settings(uw_context ctx, uw_Basis_string onload) {
  if (ctx->client == NULL)
    return "";
  else {
    char *r = uw_malloc(ctx, 52 + 3 * INTS_MAX + strlen(ctx->url_prefix) + strlen(onload));
    sprintf(r, " onload='client_id=%u;client_pass=%d;url_prefix=\"%s\";timeout=%d;listener();%s'",
            ctx->client->id,
            ctx->client->pass,
            ctx->url_prefix,
            ctx->timeout,
            onload);
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

char *uw_Basis_sqlifyChannel(uw_context ctx, uw_Basis_channel chn) {
  int len;
  char *r;
  unsigned long long combo = ((unsigned long long)chn.cli << 32) | chn.chn;

  uw_check_heap(ctx, INTS_MAX + 7);
  r = ctx->heap.front;
  sprintf(r, "%lld::int8%n", combo, &len);
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

char *uw_Basis_sqlifyClient(uw_context ctx, uw_Basis_client cli) {
  int len;
  char *r;

  uw_check_heap(ctx, INTS_MAX + 7);
  r = ctx->heap.front;
  sprintf(r, "%u::int4%n", cli, &len);
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
}

int uw_rollback(uw_context ctx) {
  if (ctx->client)
    release_client(ctx->client);

  return uw_db_rollback(ctx);
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
        printf("Expunge blocked by error: %s\n", uw_error_message(ctx));
      }
    }
    else
      prev = c;
    pthread_mutex_unlock(&c->lock);
  }

  pthread_mutex_unlock(&clients_mutex);
}
