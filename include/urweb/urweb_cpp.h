#ifndef URWEB_CPP_H
#define URWEB_CPP_H

#include <sys/types.h>

#include "types_cpp.h"

int uw_really_send(int sock, const void *buf, ssize_t len);
int uw_really_write(int fd, const void *buf, size_t len);

extern uw_unit uw_unit_v;

void uw_global_init(void);
void uw_app_init(uw_app*);

void uw_client_connect(unsigned id, int pass, int sock,
                       int (*send)(int sockfd, const void *buf, ssize_t len),
                       int (*close)(int fd),
                       void *logger_data, uw_logger log_error);
void uw_prune_clients(struct uw_context *);
failure_kind uw_initialize(struct uw_context *);

struct uw_context * uw_init(int id, uw_loggers *lg);
void uw_close(struct uw_context *);
int uw_set_app(struct uw_context *, uw_app*);
uw_app *uw_get_app(struct uw_context *);
void uw_set_db(struct uw_context *, void*);
void *uw_get_db(struct uw_context *);
void uw_free(struct uw_context *);
void uw_reset(struct uw_context *);
void uw_reset_keep_request(struct uw_context *);
void uw_reset_keep_error_message(struct uw_context *);
char *uw_get_url_prefix(struct uw_context *);

failure_kind uw_begin_init(struct uw_context *);
void uw_set_on_success(char *);
void uw_set_headers(struct uw_context *, char *(*get_header)(void *, const char *), void *get_header_data);
void uw_set_env(struct uw_context *, char *(*get_env)(void *, const char *), void *get_env_data);
uw_loggers* uw_get_loggers(struct uw_context *ctx);
uw_loggers* uw_get_loggers(struct uw_context *ctx);
failure_kind uw_begin(struct uw_context *, char *path);
void uw_ensure_transaction(struct uw_context *);
void uw_try_reconnecting_and_restarting(struct uw_context *);
failure_kind uw_begin_onError(struct uw_context *, char *msg);
void uw_login(struct uw_context *);
int uw_commit(struct uw_context *);
// ^-- returns nonzero if the transaction should be restarted
int uw_rollback(struct uw_context *, int will_retry);

__attribute__((noreturn)) void uw_error(struct uw_context *, failure_kind, const char *fmt, ...);
char *uw_error_message(struct uw_context *);
void uw_set_error_message(struct uw_context *, const char *fmt, ...);
uw_Basis_string uw_dup_and_clear_error_message(struct uw_context *);
int uw_has_error(struct uw_context *);
void uw_push_cleanup(struct uw_context *, void (*func)(void *), void *arg);
void uw_pop_cleanup(struct uw_context *);

void *uw_malloc(struct uw_context *, size_t);
void uw_begin_region(struct uw_context *);
void uw_end_region(struct uw_context *);
void uw_memstats(struct uw_context *);

int uw_send(struct uw_context *, int sock);
int uw_print(struct uw_context *, int fd);
int uw_output(struct uw_context * ctx, int (*output)(void *data, const char *buf, size_t len), void *data);
int uw_pagelen(struct uw_context *);

int uw_set_input(struct uw_context *, const char *name, char *value);
int uw_set_file_input(struct uw_context *, char *name, uw_Basis_file);

char *uw_get_input(struct uw_context *, int name);
char *uw_get_optional_input(struct uw_context *, int name);
uw_Basis_file uw_get_file_input(struct uw_context *, int name);
void uw_enter_subform(struct uw_context *, int name);
void uw_leave_subform(struct uw_context *);
int uw_enter_subforms(struct uw_context *, int name);
int uw_next_entry(struct uw_context *);

void uw_write(struct uw_context *, const char*);

// For caching.
void uw_recordingStart(struct uw_context *);
char *uw_recordingRead(struct uw_context *);
char *uw_recordingReadScript(struct uw_context *);

uw_Basis_source uw_Basis_new_client_source(struct uw_context *, uw_Basis_string);
uw_unit uw_Basis_set_client_source(struct uw_context *, uw_Basis_source, uw_Basis_string);

void uw_set_script_header(struct uw_context *, const char*);
char *uw_Basis_get_settings(struct uw_context *, uw_unit);
char *uw_get_real_script(struct uw_context *);

uw_Basis_string uw_Basis_maybe_onload(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_Basis_maybe_onunload(struct uw_context *, uw_Basis_string);

void uw_set_needs_push(struct uw_context *, int);
void uw_set_needs_sig(struct uw_context *, int);
void uw_set_could_write_db(struct uw_context *, int);
void uw_set_at_most_one_query(struct uw_context *, int);

char *uw_Basis_htmlifyInt(struct uw_context *, uw_Basis_int);
char *uw_Basis_htmlifyFloat(struct uw_context *, uw_Basis_float);
char *uw_Basis_htmlifyString(struct uw_context *, uw_Basis_string);
char *uw_Basis_htmlifyBool(struct uw_context *, uw_Basis_bool);
char *uw_Basis_htmlifyTime(struct uw_context *, uw_Basis_time);
char *uw_Basis_htmlifySpecialChar(struct uw_context *, uw_Basis_char);
char *uw_Basis_htmlifySource(struct uw_context *, uw_Basis_source);

uw_unit uw_Basis_htmlifyInt_w(struct uw_context *, uw_Basis_int);
uw_unit uw_Basis_htmlifyFloat_w(struct uw_context *, uw_Basis_float);
uw_unit uw_Basis_htmlifyString_w(struct uw_context *, uw_Basis_string);
uw_unit uw_Basis_htmlifyBool_w(struct uw_context *, uw_Basis_bool);
uw_unit uw_Basis_htmlifyTime_w(struct uw_context *, uw_Basis_time);
uw_unit uw_Basis_htmlifySpecialChar_w(struct uw_context *, uw_Basis_char);
uw_unit uw_Basis_htmlifySource_w(struct uw_context *, uw_Basis_source);

char *uw_Basis_attrifyInt(struct uw_context *, uw_Basis_int);
char *uw_Basis_attrifyFloat(struct uw_context *, uw_Basis_float);
char *uw_Basis_attrifyString(struct uw_context *, uw_Basis_string);
char *uw_Basis_attrifyChar(struct uw_context *, uw_Basis_char);
char *uw_Basis_attrifyTime(struct uw_context *, uw_Basis_time);
char *uw_Basis_attrifyChannel(struct uw_context *, uw_Basis_channel);
char *uw_Basis_attrifyClient(struct uw_context *, uw_Basis_client);
char *uw_Basis_attrifyCss_class(struct uw_context *, uw_Basis_css_class);

uw_unit uw_Basis_attrifyInt_w(struct uw_context *, uw_Basis_int);
uw_unit uw_Basis_attrifyFloat_w(struct uw_context *, uw_Basis_float);
uw_unit uw_Basis_attrifyString_w(struct uw_context *, uw_Basis_string);
uw_unit uw_Basis_attrifyChar_w(struct uw_context *, uw_Basis_char);

char *uw_Basis_urlifyInt(struct uw_context *, uw_Basis_int);
char *uw_Basis_urlifyFloat(struct uw_context *, uw_Basis_float);
char *uw_Basis_urlifyString(struct uw_context *, uw_Basis_string);
char *uw_Basis_urlifyBool(struct uw_context *, uw_Basis_bool);
char *uw_Basis_urlifyTime(struct uw_context *, uw_Basis_time);
char *uw_Basis_urlifyChannel(struct uw_context *, uw_Basis_channel);
char *uw_Basis_urlifySource(struct uw_context *, uw_Basis_source);

uw_unit uw_Basis_urlifyInt_w(struct uw_context *, uw_Basis_int);
uw_unit uw_Basis_urlifyFloat_w(struct uw_context *, uw_Basis_float);
uw_unit uw_Basis_urlifyChar_w(struct uw_context *, uw_Basis_char);
uw_unit uw_Basis_urlifyString_w(struct uw_context *, uw_Basis_string);
uw_unit uw_Basis_urlifyBool_w(struct uw_context *, uw_Basis_bool);
uw_unit uw_Basis_urlifyTime_w(struct uw_context *, uw_Basis_time);
uw_unit uw_Basis_urlifyChannel_w(struct uw_context *, uw_Basis_channel);
uw_unit uw_Basis_urlifySource_w(struct uw_context *, uw_Basis_source);

uw_Basis_unit uw_Basis_unurlifyUnit(struct uw_context * ctx, char **s);
uw_Basis_int uw_Basis_unurlifyInt(struct uw_context *, char **);
uw_Basis_float uw_Basis_unurlifyFloat(struct uw_context *, char **);
uw_Basis_string uw_Basis_unurlifyString(struct uw_context *, char **);
uw_Basis_char uw_Basis_unurlifyChar(struct uw_context *, char **);
uw_Basis_string uw_Basis_unurlifyString_fromClient(struct uw_context *, char **);
uw_Basis_bool uw_Basis_unurlifyBool(struct uw_context *, char **);
uw_Basis_time uw_Basis_unurlifyTime(struct uw_context *, char **);

uw_Basis_int uw_Basis_strlen(struct uw_context *, const char *);
uw_Basis_bool uw_Basis_strlenGe(struct uw_context *, uw_Basis_string, uw_Basis_int);
uw_Basis_char uw_Basis_strsub(struct uw_context *, const char *, uw_Basis_int);
uw_Basis_string uw_Basis_strsuffix(struct uw_context *, const char *, uw_Basis_int);
uw_Basis_string uw_Basis_strcat(struct uw_context *, const char *, const char *);
uw_Basis_string uw_Basis_mstrcat(struct uw_context * ctx, ...);
uw_Basis_int *uw_Basis_strindex(struct uw_context *, const char *, uw_Basis_char);
uw_Basis_int *uw_Basis_strsindex(struct uw_context *, const char *, const char *needle);
uw_Basis_string uw_Basis_strchr(struct uw_context *, const char *, uw_Basis_char);
uw_Basis_int uw_Basis_strcspn(struct uw_context *, const char *, const char *);
uw_Basis_string uw_Basis_substring(struct uw_context *, const char *, uw_Basis_int, uw_Basis_int);
uw_Basis_string uw_Basis_str1(struct uw_context *, uw_Basis_char);
uw_Basis_string uw_Basis_ofUnicode(struct uw_context *, uw_Basis_int);
uw_Basis_int uw_Basis_strlenUtf8(struct uw_context *, const char *);
uw_Basis_char uw_Basis_strsubUtf8(struct uw_context *, const char *, uw_Basis_int);
uw_Basis_string uw_Basis_strsuffixUtf8(struct uw_context *, const char *, uw_Basis_int);

uw_Basis_string uw_strdup(struct uw_context *, const char *);
uw_Basis_string uw_maybe_strdup(struct uw_context *, const char *);
char *uw_memdup(struct uw_context *, const char *, size_t);

uw_Basis_string uw_Basis_sqlifyInt(struct uw_context *, uw_Basis_int);
uw_Basis_string uw_Basis_sqlifyFloat(struct uw_context *, uw_Basis_float);
uw_Basis_string uw_Basis_sqlifyString(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_Basis_sqlifyChar(struct uw_context *, uw_Basis_char);
uw_Basis_string uw_Basis_sqlifyBool(struct uw_context *, uw_Basis_bool);
uw_Basis_string uw_Basis_sqlifyTime(struct uw_context *, uw_Basis_time);
uw_Basis_string uw_Basis_sqlifyBlob(struct uw_context *, uw_Basis_blob);
uw_Basis_string uw_Basis_sqlifyChannel(struct uw_context *, uw_Basis_channel);
uw_Basis_string uw_Basis_sqlifyClient(struct uw_context *, uw_Basis_client);

uw_Basis_string uw_Basis_sqlifyIntN(struct uw_context *, uw_Basis_int*);
uw_Basis_string uw_Basis_sqlifyFloatN(struct uw_context *, uw_Basis_float*);
uw_Basis_string uw_Basis_sqlifyStringN(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_Basis_sqlifyBoolN(struct uw_context *, uw_Basis_bool*);
uw_Basis_string uw_Basis_sqlifyTimeN(struct uw_context *, uw_Basis_time*);

char *uw_Basis_ensqlBool(uw_Basis_bool);
char *uw_Basis_ensqlTime(struct uw_context * ctx, uw_Basis_time);

char *uw_Basis_jsifyString(struct uw_context *, uw_Basis_string);
char *uw_Basis_jsifyChar(struct uw_context *, uw_Basis_char);
char *uw_Basis_jsifyChannel(struct uw_context *, uw_Basis_channel);
char *uw_Basis_jsifyTime(struct uw_context *, uw_Basis_time);

uw_Basis_string uw_Basis_intToString(struct uw_context *, uw_Basis_int);
uw_Basis_string uw_Basis_floatToString(struct uw_context *, uw_Basis_float);
uw_Basis_string uw_Basis_charToString(struct uw_context *, uw_Basis_char);
uw_Basis_string uw_Basis_boolToString(struct uw_context *, uw_Basis_bool);
uw_Basis_string uw_Basis_timeToString(struct uw_context *, uw_Basis_time);

uw_Basis_int *uw_Basis_stringToInt(struct uw_context *, uw_Basis_string);
uw_Basis_float *uw_Basis_stringToFloat(struct uw_context *, uw_Basis_string);
uw_Basis_char *uw_Basis_stringToChar(struct uw_context *, uw_Basis_string);
uw_Basis_bool *uw_Basis_stringToBool(struct uw_context *, uw_Basis_string);
uw_Basis_time *uw_Basis_stringToTime(struct uw_context *, const char *);

uw_Basis_int uw_Basis_stringToInt_error(struct uw_context *, uw_Basis_string);
uw_Basis_float uw_Basis_stringToFloat_error(struct uw_context *, uw_Basis_string);
uw_Basis_char uw_Basis_stringToChar_error(struct uw_context *, uw_Basis_string);
uw_Basis_bool uw_Basis_stringToBool_error(struct uw_context *, uw_Basis_string);
uw_Basis_time uw_Basis_stringToTime_error(struct uw_context *, const char *);
uw_Basis_blob uw_Basis_stringToBlob_error(struct uw_context *, uw_Basis_string, size_t);
uw_Basis_channel uw_Basis_stringToChannel_error(struct uw_context *, uw_Basis_string);
uw_Basis_client uw_Basis_stringToClient_error(struct uw_context *, uw_Basis_string);

uw_Basis_time uw_Basis_unsqlTime(struct uw_context *, uw_Basis_string);

uw_Basis_string uw_Basis_requestHeader(struct uw_context *, uw_Basis_string);

void uw_write_header(struct uw_context *, uw_Basis_string);
void uw_clear_headers(struct uw_context *);
int uw_has_contentLength(struct uw_context *);
void uw_Basis_clear_page(struct uw_context *);

void uw_write_script(struct uw_context *, uw_Basis_string);

uw_Basis_string uw_Basis_get_cookie(struct uw_context *, uw_Basis_string c);
uw_unit uw_Basis_set_cookie(struct uw_context *, uw_Basis_string prefix, uw_Basis_string c, uw_Basis_string v, uw_Basis_time *expires, uw_Basis_bool secure);
uw_unit uw_Basis_clear_cookie(struct uw_context *, uw_Basis_string prefix, uw_Basis_string c);

uw_Basis_channel uw_Basis_new_channel(struct uw_context *, uw_unit);
uw_unit uw_Basis_send(struct uw_context *, uw_Basis_channel, uw_Basis_string);

uw_Basis_client uw_Basis_self(struct uw_context *);

uw_Basis_string uw_Basis_bless(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_Basis_blessMime(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_Basis_blessRequestHeader(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_Basis_blessResponseHeader(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_Basis_blessEnvVar(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_Basis_blessMeta(struct uw_context *, uw_Basis_string);

uw_Basis_string uw_Basis_checkUrl(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_Basis_anchorUrl(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_Basis_checkMime(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_Basis_checkRequestHeader(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_Basis_checkResponseHeader(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_Basis_checkEnvVar(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_Basis_checkMeta(struct uw_context *, uw_Basis_string);

uw_Basis_string uw_Basis_getHeader(struct uw_context *, uw_Basis_string name);
uw_unit uw_Basis_setHeader(struct uw_context *, uw_Basis_string name, uw_Basis_string value);
uw_Basis_string uw_Basis_getenv(struct uw_context *, uw_Basis_string name);

uw_Basis_string uw_unnull(uw_Basis_string);
uw_Basis_string uw_Basis_makeSigString(struct uw_context *, uw_Basis_string);
int uw_streq(uw_Basis_string, uw_Basis_string);
uw_Basis_string uw_Basis_sigString(struct uw_context *, uw_unit);

uw_Basis_string uw_Basis_fileName(struct uw_context *, uw_Basis_file);
uw_Basis_string uw_Basis_fileMimeType(struct uw_context *, uw_Basis_file);
uw_Basis_blob uw_Basis_fileData(struct uw_context *, uw_Basis_file);
uw_Basis_int uw_Basis_blobSize(struct uw_context *, uw_Basis_blob);
uw_Basis_blob uw_Basis_textBlob(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_Basis_textOfBlob(struct uw_context *, uw_Basis_blob);

uw_Basis_string uw_Basis_postType(struct uw_context *, uw_Basis_postBody);
uw_Basis_string uw_Basis_postData(struct uw_context *, uw_Basis_postBody);
void uw_noPostBody(struct uw_context *);
void uw_postBody(struct uw_context *, uw_Basis_postBody);
int uw_hasPostBody(struct uw_context *);
uw_Basis_postBody uw_getPostBody(struct uw_context *);

void uw_mayReturnIndirectly(struct uw_context *);
__attribute__((noreturn)) void uw_return_blob(struct uw_context *, uw_Basis_blob, uw_Basis_string mimeType);
__attribute__((noreturn)) void uw_return_blob_from_page(struct uw_context *, uw_Basis_string mimeType);
__attribute__((noreturn)) void uw_redirect(struct uw_context *, uw_Basis_string url);
void uw_replace_page(struct uw_context *, const char *data, size_t size);

uw_Basis_time uw_Basis_now(struct uw_context *);
uw_Basis_time uw_Basis_addSeconds(struct uw_context *, uw_Basis_time, uw_Basis_int);
uw_Basis_int uw_Basis_diffInSeconds(struct uw_context *, uw_Basis_time, uw_Basis_time);
uw_Basis_int uw_Basis_toSeconds(struct uw_context *, uw_Basis_time);
uw_Basis_int uw_Basis_diffInMilliseconds(struct uw_context *, uw_Basis_time, uw_Basis_time);
uw_Basis_int uw_Basis_toMilliseconds(struct uw_context *, uw_Basis_time);
uw_Basis_time uw_Basis_fromMilliseconds(struct uw_context *, uw_Basis_int);
uw_Basis_time uw_Basis_fromDatetime(struct uw_context *, uw_Basis_int, uw_Basis_int, uw_Basis_int, uw_Basis_int, uw_Basis_int, uw_Basis_int);
uw_Basis_int uw_Basis_datetimeYear(struct uw_context *, uw_Basis_time);
uw_Basis_int uw_Basis_datetimeMonth(struct uw_context *, uw_Basis_time);
uw_Basis_int uw_Basis_datetimeDay(struct uw_context *, uw_Basis_time);
uw_Basis_int uw_Basis_datetimeHour(struct uw_context *, uw_Basis_time);
uw_Basis_int uw_Basis_datetimeMinute(struct uw_context *, uw_Basis_time);
uw_Basis_int uw_Basis_datetimeSecond(struct uw_context *, uw_Basis_time);
uw_Basis_int uw_Basis_datetimeDayOfWeek(struct uw_context *, uw_Basis_time);
extern const uw_Basis_time uw_Basis_minTime;

int uw_register_transactional(struct uw_context *, void *data, uw_callback commit, uw_callback rollback, uw_callback_with_retry free);

void uw_check_heap(struct uw_context *, size_t extra);
char *uw_heap_front(struct uw_context *);
void uw_set_heap_front(struct uw_context *, char*);

uw_Basis_string uw_Basis_unAs(struct uw_context *, uw_Basis_string);

extern char *uw_sqlfmtInt;
extern char *uw_sqlfmtFloat;
extern int uw_Estrings, uw_sql_type_annotations;
extern char *uw_sqlsuffixString;
extern char *uw_sqlsuffixChar;
extern char *uw_sqlsuffixBlob;
extern char *uw_sqlfmtUint4;

void *uw_get_global(struct uw_context *, char *name);
void uw_set_global(struct uw_context *, char *name, void *data, uw_callback free);

uw_Basis_bool uw_Basis_isalnum(struct uw_context *, uw_Basis_char);
uw_Basis_bool uw_Basis_isalpha(struct uw_context *, uw_Basis_char);
uw_Basis_bool uw_Basis_isblank(struct uw_context *, uw_Basis_char);
uw_Basis_bool uw_Basis_iscntrl(struct uw_context *, uw_Basis_char);
uw_Basis_bool uw_Basis_isdigit(struct uw_context *, uw_Basis_char);
uw_Basis_bool uw_Basis_isgraph(struct uw_context *, uw_Basis_char);
uw_Basis_bool uw_Basis_islower(struct uw_context *, uw_Basis_char);
uw_Basis_bool uw_Basis_isprint(struct uw_context *, uw_Basis_char);
uw_Basis_bool uw_Basis_ispunct(struct uw_context *, uw_Basis_char);
uw_Basis_bool uw_Basis_isspace(struct uw_context *, uw_Basis_char);
uw_Basis_bool uw_Basis_isupper(struct uw_context *, uw_Basis_char);
uw_Basis_bool uw_Basis_isxdigit(struct uw_context *, uw_Basis_char);
uw_Basis_char uw_Basis_tolower(struct uw_context *, uw_Basis_char);
uw_Basis_char uw_Basis_toupper(struct uw_context *, uw_Basis_char);

uw_Basis_bool uw_Basis_iscodepoint(struct uw_context *, uw_Basis_int);
uw_Basis_int uw_Basis_ord(struct uw_context *, uw_Basis_char);
uw_Basis_char uw_Basis_chr(struct uw_context *, uw_Basis_int);

uw_Basis_string uw_Basis_currentUrl(struct uw_context *);
void uw_set_currentUrl(struct uw_context *, char *);

extern size_t uw_messages_max, uw_clients_max, uw_headers_max, uw_page_max, uw_heap_max, uw_script_max;
extern size_t uw_inputs_max, uw_cleanup_max, uw_subinputs_max, uw_deltas_max, uw_transactionals_max, uw_globals_max;

extern size_t uw_database_max;

extern int uw_time;

void uw_set_deadline(struct uw_context *, int);
void uw_check_deadline(struct uw_context *);

uw_Basis_unit uw_Basis_debug(struct uw_context *, uw_Basis_string);
uw_Basis_int uw_Basis_naughtyDebug(struct uw_context *, uw_Basis_string);

void uw_set_client_data(struct uw_context *, void *);

uw_Basis_int uw_Basis_rand(struct uw_context *);

extern int uw_time_max, uw_supports_direct_status, uw_min_heap;

failure_kind uw_runCallback(struct uw_context *, void (*callback)(struct uw_context *));

uw_Basis_string uw_Basis_timef(struct uw_context *, const char *fmt, uw_Basis_time);
uw_Basis_time uw_Basis_stringToTimef(struct uw_context *, const char *fmt, uw_Basis_string);
uw_Basis_time uw_Basis_stringToTimef_error(struct uw_context *, const char *fmt, uw_Basis_string);

uw_Basis_bool uw_Basis_eq_time(struct uw_context *, uw_Basis_time, uw_Basis_time);
uw_Basis_bool uw_Basis_lt_time(struct uw_context *, uw_Basis_time, uw_Basis_time);
uw_Basis_bool uw_Basis_le_time(struct uw_context *, uw_Basis_time, uw_Basis_time);

void uw_buffer_init(size_t max, uw_buffer *, size_t initial);
void uw_buffer_free(uw_buffer *);
void uw_buffer_reset(uw_buffer *);
int uw_buffer_check(uw_buffer *, size_t extra);
size_t uw_buffer_used(uw_buffer *);
size_t uw_buffer_avail(uw_buffer *);
int uw_buffer_append(uw_buffer *, const char *, size_t);

void uw_setQueryString(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_queryString(struct uw_context *);

uw_Basis_time *uw_Basis_readUtc(struct uw_context *, uw_Basis_string);

void uw_isPost(struct uw_context *);
uw_Basis_bool uw_Basis_currentUrlHasPost(struct uw_context *);
uw_Basis_bool uw_Basis_currentUrlHasQueryString(struct uw_context *);

uw_Basis_string uw_Basis_fresh(struct uw_context *);

uw_Basis_float uw_Basis_floatFromInt(struct uw_context *, uw_Basis_int);
uw_Basis_int uw_Basis_ceil(struct uw_context *, uw_Basis_float);
uw_Basis_int uw_Basis_trunc(struct uw_context *, uw_Basis_float);
uw_Basis_int uw_Basis_round(struct uw_context *, uw_Basis_float);
uw_Basis_int uw_Basis_floor(struct uw_context *, uw_Basis_float);

uw_Basis_float uw_Basis_pow(struct uw_context *, uw_Basis_float, uw_Basis_float);
uw_Basis_float uw_Basis_sqrt(struct uw_context *, uw_Basis_float);
uw_Basis_float uw_Basis_sin(struct uw_context *, uw_Basis_float);
uw_Basis_float uw_Basis_cos(struct uw_context *, uw_Basis_float);
uw_Basis_float uw_Basis_log(struct uw_context *, uw_Basis_float);
uw_Basis_float uw_Basis_exp(struct uw_context *, uw_Basis_float);
uw_Basis_float uw_Basis_asin(struct uw_context *, uw_Basis_float);
uw_Basis_float uw_Basis_acos(struct uw_context *, uw_Basis_float);
uw_Basis_float uw_Basis_atan(struct uw_context *, uw_Basis_float);
uw_Basis_float uw_Basis_atan2(struct uw_context *, uw_Basis_float, uw_Basis_float);
uw_Basis_float uw_Basis_abs(struct uw_context *, uw_Basis_float);

uw_Basis_string uw_Basis_atom(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_Basis_css_url(struct uw_context *, uw_Basis_string);
uw_Basis_string uw_Basis_property(struct uw_context *, uw_Basis_string);

void uw_begin_initializing(struct uw_context *);
void uw_end_initializing(struct uw_context *);

uw_Basis_string uw_Basis_fieldName(struct uw_context *, uw_Basis_postField);
uw_Basis_string uw_Basis_fieldValue(struct uw_context *, uw_Basis_postField);
uw_Basis_string uw_Basis_remainingFields(struct uw_context *, uw_Basis_postField);
uw_Basis_postField *uw_Basis_firstFormField(struct uw_context *, uw_Basis_string);

uw_Basis_string uw_Basis_blessData(struct uw_context *, uw_Basis_string);

extern const char uw_begin_xhtml[], uw_begin_html5[];

int uw_remoteSock(struct uw_context *);
void uw_set_remoteSock(struct uw_context *, int sock);

void uw_Basis_writec(struct uw_context *, char);

// Sqlcache.

void *uw_Sqlcache_rlock(struct uw_context *, uw_Sqlcache_Cache *);
void *uw_Sqlcache_wlock(struct uw_context *, uw_Sqlcache_Cache *);
uw_Sqlcache_Value *uw_Sqlcache_check(struct uw_context *, uw_Sqlcache_Cache *, char **);
void *uw_Sqlcache_store(struct uw_context *, uw_Sqlcache_Cache *, char **, uw_Sqlcache_Value *);
void *uw_Sqlcache_flush(struct uw_context *, uw_Sqlcache_Cache *, char **);

int strcmp_nullsafe(const char *, const char *);

uw_unit uw_Basis_cache_file(struct uw_context *, uw_Basis_blob contents);
uw_Basis_blob uw_Basis_check_filecache(struct uw_context *, uw_Basis_string hash);
uw_Basis_bool uw_Basis_filecache_missed(struct uw_context *);

// Call at start and end of transaction.
void uw_transaction_arrives();
void uw_transaction_departs();

#endif
