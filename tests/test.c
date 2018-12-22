#include <stdio.h>

#include "urweb/urweb.h"

typedef uw_Basis_string uw_Test_t;

uw_Test_t uw_Test_create(uw_context ctx, uw_Basis_string s) {
  return s;
}

uw_Basis_string uw_Test_out(uw_context ctx, uw_Test_t s) {
  return s;
}

uw_Test_t uw_Test_frob(uw_context ctx, uw_Test_t s1, uw_Basis_string s2) {
  return uw_Basis_strcat(ctx, s1, s2);
}

uw_Basis_unit uw_Test_print(uw_context ctx) {
  printf("Hi there!\n");
  return uw_unit_v;
}

uw_Basis_unit uw_Test_foo(uw_context ctx) {
  printf("FOO!\n");
  return uw_unit_v;
}

static void commit(void *data) {
  printf("Commit: %s\n", (char*)data);
}
static void rollback(void *data) {
  printf("Rollback: %s\n", (char*)data);
}
static void ffree(void *data, int will_retry) {
  printf("Free: %s, %d\n", (char*)data, will_retry);
}

uw_Basis_unit uw_Test_transactional(uw_context ctx) {
  uw_register_transactional(ctx, "Beppo", commit, rollback, ffree);
  return uw_unit_v;
}
