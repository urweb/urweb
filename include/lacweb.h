#include <sys/types.h>

#include "types.h"

int lw_really_send(int sock, void *buf, ssize_t len);

extern lw_unit lw_unit_v;

lw_context lw_init(int page_len);
void lw_free(lw_context);
int lw_send(lw_context, int sock);

void lw_write(lw_context, const char*);

char *lw_Basis_attrifyInt(lw_Basis_int);
char *lw_Basis_attrifyFloat(lw_Basis_float);
char *lw_Basis_attrifyString(lw_Basis_string);

void lw_Basis_attrifyInt_w(lw_context, lw_Basis_int);
void lw_Basis_attrifyFloat_w(lw_context, lw_Basis_float);
void lw_Basis_attrifyString_w(lw_context, lw_Basis_string);
