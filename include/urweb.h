#include <sys/types.h>

#include "types.h"

int uw_really_send(int sock, void *buf, ssize_t len);

extern uw_unit uw_unit_v;

void uw_global_init(void);

void uw_client_connect(unsigned id, int pass, int sock);
void uw_prune_clients(uw_context);
failure_kind uw_initialize(uw_context);

uw_context uw_init(void);
void uw_set_db(uw_context, void*);
void *uw_get_db(uw_context);
void uw_free(uw_context);
void uw_reset(uw_context);
void uw_reset_keep_request(uw_context);
void uw_reset_keep_error_message(uw_context);

failure_kind uw_begin_init(uw_context);
void uw_set_headers(uw_context, char *headers);
failure_kind uw_begin(uw_context, char *path);
void uw_login(uw_context);
void uw_commit(uw_context);
int uw_rollback(uw_context);

__attribute__((noreturn)) void uw_error(uw_context, failure_kind, const char *fmt, ...);
char *uw_error_message(uw_context);
void uw_push_cleanup(uw_context, void (*func)(void *), void *arg);
void uw_pop_cleanup(uw_context);

void *uw_malloc(uw_context, size_t);
void uw_begin_region(uw_context);
void uw_end_region(uw_context);
void uw_memstats(uw_context);

int uw_send(uw_context, int sock);

void uw_set_input(uw_context, char *name, char *value);
char *uw_get_input(uw_context, int name);
char *uw_get_optional_input(uw_context, int name);

void uw_write(uw_context, const char*);

uw_Basis_int uw_Basis_new_client_source(uw_context, uw_Basis_string);
uw_unit uw_Basis_set_client_source(uw_context, uw_Basis_int, uw_Basis_string);

void uw_set_script_header(uw_context, const char*);
const char *uw_Basis_get_settings(uw_context, uw_unit);
const char *uw_Basis_get_script(uw_context, uw_unit);

uw_Basis_string uw_Basis_maybe_onload(uw_context, uw_Basis_string);

void uw_set_needs_push(uw_context, int);

char *uw_Basis_htmlifyInt(uw_context, uw_Basis_int);
char *uw_Basis_htmlifyFloat(uw_context, uw_Basis_float);
char *uw_Basis_htmlifyString(uw_context, uw_Basis_string);
char *uw_Basis_htmlifyBool(uw_context, uw_Basis_bool);
char *uw_Basis_htmlifyTime(uw_context, uw_Basis_time);

uw_unit uw_Basis_htmlifyInt_w(uw_context, uw_Basis_int);
uw_unit uw_Basis_htmlifyFloat_w(uw_context, uw_Basis_float);
uw_unit uw_Basis_htmlifyString_w(uw_context, uw_Basis_string);
uw_unit uw_Basis_htmlifyBool_w(uw_context, uw_Basis_bool);
uw_unit uw_Basis_htmlifyTime_w(uw_context, uw_Basis_time);

char *uw_Basis_attrifyInt(uw_context, uw_Basis_int);
char *uw_Basis_attrifyFloat(uw_context, uw_Basis_float);
char *uw_Basis_attrifyString(uw_context, uw_Basis_string);
char *uw_Basis_attrifyTime(uw_context, uw_Basis_time);
char *uw_Basis_attrifyChannel(uw_context, uw_Basis_channel);
char *uw_Basis_attrifyClient(uw_context, uw_Basis_client);
char *uw_Basis_attrifyCss_class(uw_context, uw_Basis_css_class);

uw_unit uw_Basis_attrifyInt_w(uw_context, uw_Basis_int);
uw_unit uw_Basis_attrifyFloat_w(uw_context, uw_Basis_float);
uw_unit uw_Basis_attrifyString_w(uw_context, uw_Basis_string);

char *uw_Basis_urlifyInt(uw_context, uw_Basis_int);
char *uw_Basis_urlifyFloat(uw_context, uw_Basis_float);
char *uw_Basis_urlifyString(uw_context, uw_Basis_string);
char *uw_Basis_urlifyBool(uw_context, uw_Basis_bool);
char *uw_Basis_urlifyTime(uw_context, uw_Basis_time);
char *uw_Basis_urlifyChannel(uw_context, uw_Basis_channel);

uw_unit uw_Basis_urlifyInt_w(uw_context, uw_Basis_int);
uw_unit uw_Basis_urlifyFloat_w(uw_context, uw_Basis_float);
uw_unit uw_Basis_urlifyString_w(uw_context, uw_Basis_string);
uw_unit uw_Basis_urlifyBool_w(uw_context, uw_Basis_bool);
uw_unit uw_Basis_urlifyChannel_w(uw_context, uw_Basis_channel);

uw_Basis_int uw_Basis_unurlifyInt(uw_context, char **);
uw_Basis_float uw_Basis_unurlifyFloat(uw_context, char **);
uw_Basis_string uw_Basis_unurlifyString(uw_context, char **);
uw_Basis_bool uw_Basis_unurlifyBool(uw_context, char **);
uw_Basis_time uw_Basis_unurlifyTime(uw_context, char **);

uw_Basis_string uw_Basis_strcat(uw_context, const char *, const char *);
uw_Basis_string uw_Basis_strdup(uw_context, const char *);
uw_Basis_string uw_Basis_maybe_strdup(uw_context, const char *);

uw_Basis_string uw_Basis_sqlifyInt(uw_context, uw_Basis_int);
uw_Basis_string uw_Basis_sqlifyFloat(uw_context, uw_Basis_float);
uw_Basis_string uw_Basis_sqlifyString(uw_context, uw_Basis_string);
uw_Basis_string uw_Basis_sqlifyBool(uw_context, uw_Basis_bool);
uw_Basis_string uw_Basis_sqlifyTime(uw_context, uw_Basis_time);
uw_Basis_string uw_Basis_sqlifyChannel(uw_context, uw_Basis_channel);
uw_Basis_string uw_Basis_sqlifyClient(uw_context, uw_Basis_client);

uw_Basis_string uw_Basis_sqlifyIntN(uw_context, uw_Basis_int*);
uw_Basis_string uw_Basis_sqlifyFloatN(uw_context, uw_Basis_float*);
uw_Basis_string uw_Basis_sqlifyStringN(uw_context, uw_Basis_string);
uw_Basis_string uw_Basis_sqlifyBoolN(uw_context, uw_Basis_bool*);
uw_Basis_string uw_Basis_sqlifyTimeN(uw_context, uw_Basis_time*);

char *uw_Basis_ensqlBool(uw_Basis_bool);

char *uw_Basis_jsifyString(uw_context, uw_Basis_string);
char *uw_Basis_jsifyChannel(uw_context, uw_Basis_channel);

uw_Basis_string uw_Basis_intToString(uw_context, uw_Basis_int);
uw_Basis_string uw_Basis_floatToString(uw_context, uw_Basis_float);
uw_Basis_string uw_Basis_boolToString(uw_context, uw_Basis_bool);
uw_Basis_string uw_Basis_timeToString(uw_context, uw_Basis_time);

uw_Basis_int *uw_Basis_stringToInt(uw_context, uw_Basis_string);
uw_Basis_float *uw_Basis_stringToFloat(uw_context, uw_Basis_string);
uw_Basis_bool *uw_Basis_stringToBool(uw_context, uw_Basis_string);
uw_Basis_time *uw_Basis_stringToTime(uw_context, uw_Basis_string);

uw_Basis_int uw_Basis_stringToInt_error(uw_context, uw_Basis_string);
uw_Basis_float uw_Basis_stringToFloat_error(uw_context, uw_Basis_string);
uw_Basis_bool uw_Basis_stringToBool_error(uw_context, uw_Basis_string);
uw_Basis_time uw_Basis_stringToTime_error(uw_context, uw_Basis_string);
uw_Basis_channel uw_Basis_stringToChannel_error(uw_context, uw_Basis_string);
uw_Basis_client uw_Basis_stringToClient_error(uw_context, uw_Basis_string);

uw_Basis_string uw_Basis_requestHeader(uw_context, uw_Basis_string);

void uw_write_header(uw_context, uw_Basis_string);

uw_Basis_string uw_Basis_get_cookie(uw_context, uw_Basis_string c);
uw_unit uw_Basis_set_cookie(uw_context, uw_Basis_string prefix, uw_Basis_string c, uw_Basis_string v);

uw_Basis_channel uw_Basis_new_channel(uw_context, uw_unit);
uw_unit uw_Basis_send(uw_context, uw_Basis_channel, uw_Basis_string);

uw_Basis_client uw_Basis_self(uw_context, uw_unit);

uw_Basis_string uw_Basis_bless(uw_context, uw_Basis_string);

uw_Basis_string uw_unnull(uw_Basis_string);
uw_Basis_string uw_Basis_makeSigString(uw_context, uw_Basis_string);
uw_Basis_string uw_Basis_sigString(uw_context, uw_unit);
