#include <stdio.h>

#include "/usr/local/include/urweb/urweb.h"

static void do_free(void *data, int will_retry) {
  printf("will_retry = %d\n", will_retry);
}

uw_unit uw_Transactional_foo(uw_context ctx) {
  printf("Registering....\n");
  uw_register_transactional(ctx, NULL, NULL, NULL, do_free);
}
