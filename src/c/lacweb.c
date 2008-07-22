#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "types.h"

lw_unit lw_unit_v = {};

struct lw_context {
  char *page, *page_front, *page_back;
  char *heap, *heap_front, *heap_back;
  char **inputs;
};

extern int lw_inputs_len;

lw_context lw_init(size_t page_len, size_t heap_len) {
  lw_context ctx = malloc(sizeof(struct lw_context));

  ctx->page_front = ctx->page = malloc(page_len);
  ctx->page_back = ctx->page_front + page_len;

  ctx->heap_front = ctx->heap = malloc(heap_len);
  ctx->heap_back = ctx->heap_front + heap_len;

  ctx->inputs = calloc(lw_inputs_len, sizeof(char *));

  return ctx;
}

void lw_free(lw_context ctx) {
  free(ctx->page);
  free(ctx->heap);
  free(ctx->inputs);
  free(ctx);
}

void lw_reset(lw_context ctx) {
  ctx->page_front = ctx->page;
  ctx->heap_front = ctx->heap;
  memset(ctx->inputs, 0, lw_inputs_len * sizeof(char *));
}

int lw_input_num(char*);

void lw_set_input(lw_context ctx, char *name, char *value) {
  int n = lw_input_num(name);

  if (n < 0) {
    printf("Bad input name");
    exit(1);
  }

  assert(n < lw_inputs_len);
  ctx->inputs[n] = value;

  printf("[%d] %s = %s\n", n, name, value);
}

char *lw_get_input(lw_context ctx, int n) {
  assert(n >= 0);
  assert(n < lw_inputs_len);
  printf("[%d] = %s\n", n, ctx->inputs[n]);
  return ctx->inputs[n];
}

static void lw_check_heap(lw_context ctx, size_t extra) {
  if (ctx->heap_back - ctx->heap_front < extra) {
    size_t desired = ctx->heap_back - ctx->heap_front + extra, next;
    char *new_heap;

    for (next = ctx->heap_back - ctx->heap_front; next < desired; next *= 2);

    new_heap = realloc(ctx->heap, next);

    if (new_heap != ctx->heap) {
      ctx->heap = new_heap;
      puts("Couldn't get contiguous chunk");
      exit(1);
    }

    ctx->heap_back = new_heap + next;
  }
}

void *lw_malloc(lw_context ctx, size_t len) {
  void *result;

  lw_check_heap(ctx, len);

  result = ctx->heap_front;
  ctx->heap_front += len;
  return result;
}

int lw_really_send(int sock, const void *buf, ssize_t len) {
  while (len > 0) {
    ssize_t n = send(sock, buf, len, 0);

    if (n < 0)
      return n;

    buf += n;
    len -= n;
  }

  return 0;
}

int lw_send(lw_context ctx, int sock) {
  return lw_really_send(sock, ctx->page, ctx->page_front - ctx->page);
}

static void lw_check(lw_context ctx, size_t extra) {
  size_t desired = ctx->page_back - ctx->page_front + extra, next;
  char *new_page;

  for (next = ctx->page_back - ctx->page_front; next < desired; next *= 2);

  new_page = realloc(ctx->page, next);
  ctx->page_front = new_page + (ctx->page_front - ctx->page);
  ctx->page_back = new_page + (ctx->page_back - ctx->page);
  ctx->page = new_page;
}

static void lw_writec_unsafe(lw_context ctx, char c) {
  *(ctx->page_front)++ = c;
}

void lw_writec(lw_context ctx, char c) {
  lw_check(ctx, 1);
  lw_writec_unsafe(ctx, c);
}

static void lw_write_unsafe(lw_context ctx, const char* s) {
  int len = strlen(s);
  memcpy(ctx->page_front, s, len);
  ctx->page_front += len;
}

void lw_write(lw_context ctx, const char* s) {
  lw_check(ctx, strlen(s));
  lw_write_unsafe(ctx, s);
}


#define INTS_MAX 50
#define FLOATS_MAX 100

char *lw_Basis_attrifyInt(lw_context ctx, lw_Basis_int n) {
  char *result;
  int len;
  lw_check_heap(ctx, INTS_MAX);
  result = ctx->heap_front;
  sprintf(result, "%d%n", n, &len);
  ctx->heap_front += len+1;
  return result;
}

char *lw_Basis_attrifyFloat(lw_context ctx, lw_Basis_float n) {
  char *result;
  int len;
  lw_check_heap(ctx, FLOATS_MAX);
  result = ctx->heap_front;
  sprintf(result, "%g%n", n, &len);
  ctx->heap_front += len+1;
  return result;
}

char *lw_Basis_attrifyString(lw_context ctx, lw_Basis_string s) {
  int len = strlen(s);
  char *result, *p;
  lw_check_heap(ctx, len * 6 + 1);

  result = p = ctx->heap_front;

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
  ctx->heap_front = p;
  return result;
}

static void lw_Basis_attrifyInt_w_unsafe(lw_context ctx, lw_Basis_int n) {
  int len;

  sprintf(ctx->page_front, "%d%n", n, &len);
  ctx->page_front += len;
}

void lw_Basis_attrifyInt_w(lw_context ctx, lw_Basis_int n) {
  lw_check(ctx, INTS_MAX);
  lw_Basis_attrifyInt_w_unsafe(ctx, n);
}

void lw_Basis_attrifyFloat_w(lw_context ctx, lw_Basis_float n) {
  int len;

  lw_check(ctx, FLOATS_MAX);
  sprintf(ctx->page_front, "%g%n", n, &len);
  ctx->page_front += len;
}

void lw_Basis_attrifyString_w(lw_context ctx, lw_Basis_string s) {
  lw_check(ctx, strlen(s) * 6);

  for (; *s; s++) {
    char c = *s;

    if (c == '"')
      lw_write_unsafe(ctx, "&quot;");
    else if (c == '&')
      lw_write_unsafe(ctx, "&amp;");
    else if (isprint(c))
      lw_writec_unsafe(ctx, c);
    else {
      lw_write_unsafe(ctx, "&#");
      lw_Basis_attrifyInt_w_unsafe(ctx, c);
      lw_writec_unsafe(ctx, ';');
    }
  }
}


char *lw_Basis_urlifyInt(lw_context ctx, lw_Basis_int n) {
  int len;
  char *r;

  lw_check_heap(ctx, INTS_MAX);
  r = ctx->heap_front;
  sprintf(r, "%d%n", n, &len);
  ctx->heap_front += len+1;
  return r;
}

char *lw_Basis_urlifyFloat(lw_context ctx, lw_Basis_float n) {
  int len;
  char *r;

  lw_check_heap(ctx, FLOATS_MAX);
  r = ctx->heap_front;
  sprintf(r, "%g%n", n, &len);
  ctx->heap_front += len+1;
  return r;
}

char *lw_Basis_urlifyString(lw_context ctx, lw_Basis_string s) {
  char *r, *p;

  lw_check_heap(ctx, strlen(s) * 3 + 1);

  for (r = p = ctx->heap_front; *s; s++) {
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
  ctx->heap_front = p;
  return r;
}

static void lw_Basis_urlifyInt_w_unsafe(lw_context ctx, lw_Basis_int n) {
  int len;

  sprintf(ctx->page_front, "%d%n", n, &len);
  ctx->page_front += len;
}

void lw_Basis_urlifyInt_w(lw_context ctx, lw_Basis_int n) {
  lw_check(ctx, INTS_MAX);
  lw_Basis_urlifyInt_w_unsafe(ctx, n);
}

void lw_Basis_urlifyFloat_w(lw_context ctx, lw_Basis_float n) {
  int len;

  lw_check(ctx, FLOATS_MAX);
  sprintf(ctx->page_front, "%g%n", n, &len);
  ctx->page_front += len;
}

void lw_Basis_urlifyString_w(lw_context ctx, lw_Basis_string s) {
  lw_check(ctx, strlen(s) * 3);

  for (; *s; s++) {
    char c = *s;

    if (c == ' ')
      lw_writec_unsafe(ctx, '+');
    else if (isalnum(c))
      lw_writec_unsafe(ctx, c);
    else {
      sprintf(ctx->page_front, "%%%02X", c);
      ctx->page_front += 3;
    }
  }
}


static char *lw_unurlify_advance(char *s) {
  char *new_s = strchr(s, '/');

  if (new_s)
    *new_s++ = 0;
  else
    new_s = strchr(s, 0);

  return new_s;
}

lw_Basis_int lw_unurlifyInt(char **s) {
  char *new_s = lw_unurlify_advance(*s);
  int r;

  r = atoi(*s);
  *s = new_s;
  return r;
}

lw_Basis_float lw_unurlifyFloat(char **s) {
  char *new_s = lw_unurlify_advance(*s);
  int r;

  r = atof(*s);
  *s = new_s;
  return r;
}

static lw_Basis_string lw_unurlifyString_to(char *r, char *s) {
  char *s1, *s2;
  int n;

  for (s1 = r, s2 = s; *s2; ++s1, ++s2) {
    char c = *s2;

    switch (c) {
    case '+':
      *s1 = ' ';
      break;
    case '%':
      assert(s2[1] != 0 && s2[2] != 0);
      sscanf(s2+1, "%02X", &n);
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

lw_Basis_string lw_unurlifyString(lw_context ctx, char **s) {
  char *new_s = lw_unurlify_advance(*s);
  char *r, *s1, *s2;
  int len, n;

  len = strlen(*s);
  lw_check_heap(ctx, len + 1);

  r = ctx->heap_front;
  ctx->heap_front = lw_unurlifyString_to(ctx->heap_front, *s);
  *s = new_s;
  return r;
}


char *lw_Basis_htmlifyString(lw_context ctx, lw_Basis_string s) {
  char *r, *s2;

  lw_check_heap(ctx, strlen(s) * 5 + 1);

  for (r = s2 = ctx->heap_front; *s; s++) {
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
  ctx->heap_front = s2;
  return r;
}

void lw_Basis_htmlifyString_w(lw_context ctx, lw_Basis_string s) {
  lw_check(ctx, strlen(s) * 5);

  for (; *s; s++) {
    char c = *s;

    switch (c) {
    case '<':
      lw_write_unsafe(ctx, "&lt;");
      break;
    case '&':
      lw_write_unsafe(ctx, "&amp;");
      break;
    default:
      if (isprint(c))
        lw_writec_unsafe(ctx, c);
      else {
        lw_write_unsafe(ctx, "&#");
        lw_Basis_attrifyInt_w_unsafe(ctx, c);
        lw_writec_unsafe(ctx, ';');
      }
    }
  }
}
