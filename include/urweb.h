#include <sys/types.h>

#include "types.h"

int lw_really_send(int sock, void *buf, ssize_t len);

extern lw_unit lw_unit_v;

lw_context lw_init(size_t page_len, size_t heap_len);
void lw_set_db(lw_context, void*);
void *lw_get_db(lw_context);
void lw_free(lw_context);
void lw_reset(lw_context);
void lw_reset_keep_request(lw_context);
void lw_reset_keep_error_message(lw_context);

failure_kind lw_begin_init(lw_context);
failure_kind lw_begin(lw_context, char *path);

void lw_error(lw_context, failure_kind, const char *fmt, ...);
char *lw_error_message(lw_context);

void *lw_malloc(lw_context, size_t);
int lw_send(lw_context, int sock);

void lw_set_input(lw_context, char *name, char *value);
char *lw_get_input(lw_context, int name);
char *lw_get_optional_input(lw_context, int name);

void lw_write(lw_context, const char*);


char *lw_Basis_htmlifyString(lw_context, lw_Basis_string);
void lw_Basis_htmlifyString_w(lw_context, lw_Basis_string);

char *lw_Basis_attrifyInt(lw_context, lw_Basis_int);
char *lw_Basis_attrifyFloat(lw_context, lw_Basis_float);
char *lw_Basis_attrifyString(lw_context, lw_Basis_string);

void lw_Basis_attrifyInt_w(lw_context, lw_Basis_int);
void lw_Basis_attrifyFloat_w(lw_context, lw_Basis_float);
void lw_Basis_attrifyString_w(lw_context, lw_Basis_string);


char *lw_Basis_urlifyInt(lw_context, lw_Basis_int);
char *lw_Basis_urlifyFloat(lw_context, lw_Basis_float);
char *lw_Basis_urlifyString(lw_context, lw_Basis_string);
char *lw_Basis_urlifyBool(lw_context, lw_Basis_bool);

void lw_Basis_urlifyInt_w(lw_context, lw_Basis_int);
void lw_Basis_urlifyFloat_w(lw_context, lw_Basis_float);
void lw_Basis_urlifyString_w(lw_context, lw_Basis_string);
void lw_Basis_urlifyBool_w(lw_context, lw_Basis_bool);

lw_Basis_int lw_Basis_unurlifyInt(lw_context, char **);
lw_Basis_float lw_Basis_unurlifyFloat(lw_context, char **);
lw_Basis_string lw_Basis_unurlifyString(lw_context, char **);
lw_Basis_bool lw_Basis_unurlifyBool(lw_context, char **);

lw_Basis_string lw_Basis_strcat(lw_context, lw_Basis_string, lw_Basis_string);
lw_Basis_string lw_Basis_strdup(lw_context, lw_Basis_string);

lw_Basis_int lw_Basis_sqlifyInt(lw_context, lw_Basis_int);
lw_Basis_float lw_Basis_sqlifyFloat(lw_context, lw_Basis_float);
lw_Basis_string lw_Basis_sqlifyString(lw_context, lw_Basis_string);
lw_Basis_bool lw_Basis_sqlifyBool(lw_context, lw_Basis_bool);
