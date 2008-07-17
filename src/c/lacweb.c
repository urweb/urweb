#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "types.h"

lw_unit lw_unit_v = {};

struct lw_context {
  char *page, *page_front, *page_back;
};

lw_context lw_init(int page_len) {
  lw_context ctx = malloc(sizeof(struct lw_context));
  ctx->page_front = ctx->page = malloc(page_len);
  ctx->page_back = ctx->page_front + page_len;
  return ctx;
}

void lw_free(lw_context ctx) {
  free(ctx->page);
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


char *lw_Basis_attrifyInt(lw_Basis_int n) {
  return "0";
}

char *lw_Basis_attrifyFloat(lw_Basis_float n) {
  return "0.0";
}

char *lw_Basis_attrifyString(lw_Basis_string s) {
  return "";
}

#define INTS_MAX 50
#define FLOATS_MAX 100

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
    else if (isprint(c))
      lw_writec_unsafe(ctx, c);
    else {
      lw_write_unsafe(ctx, "&#");
      lw_Basis_attrifyInt_w_unsafe(ctx, c);
      lw_writec_unsafe(ctx, ';');
    }
  }
}


char *lw_Basis_urlifyInt(lw_Basis_int n) {
  return "0";
}

char *lw_Basis_urlifyFloat(lw_Basis_float n) {
  return "0.0";
}

char *lw_Basis_urlifyString(lw_Basis_string s) {
  return "";
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


lw_Basis_int lw_unurlifyInt(char **s) {
  char *new_s = strchr(*s, '/');
  int r;

  if (new_s)
    *new_s++ = 0;
  else
    new_s = strchr(*s, 0);

  r = atoi(*s);
  *s = new_s;
  return r;
}

lw_Basis_float lw_unurlifyFloat(char **s) {
  char *new_s = strchr(*s, '/');
  int r;

  if (new_s)
    *new_s++ = 0;
  else
    new_s = strchr(*s, 0);

  r = atof(*s);
  *s = new_s;
  return r;
}

lw_Basis_string lw_unurlifyString(char **s) {
  return "";
}


char *lw_Basis_htmlifyString(lw_Basis_string s) {
  return "";
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
