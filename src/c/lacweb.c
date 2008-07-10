#include <stdio.h>
#include <ctype.h>

#include "types.h"

lw_unit lw_unit_v = {};

void lw_writec(char c) {
  fputc(c, stdout);
}

void lw_write(const char* s) {
  fputs(s, stdout);
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

char *lw_Basis_attrifyInt_w(lw_Basis_int n) {
  printf("%d", n);
}

char *lw_Basis_attrifyFloat_w(lw_Basis_float n) {
  printf("%g", n);
}

char *lw_Basis_attrifyString_w(lw_Basis_string s) {
  for (; *s; s++) {
    char c = *s;

    if (c == '"')
      lw_write("&quot;");
    else if (isprint(c))
      lw_writec(c);
    else {
      lw_write("&#");
      lw_Basis_attrifyInt_w(c);
      lw_writec(';');
    }
  }
  lw_write(s);
}
