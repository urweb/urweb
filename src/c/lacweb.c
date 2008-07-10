#include <stdio.h>

#include "types.h"

lw_unit lw_unit_v = {};

void lw_write(const char* s) {
  fputs(s, stdout);
}
