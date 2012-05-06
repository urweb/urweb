#ifndef URWEB_H
#define URWEB_H

#include <sys/types.h>

#include "types.h"

int uw_really_send(int sock, const void *buf, ssize_t len);
int uw_really_write(int fd, const void *buf, size_t len);

extern uw_unit uw_unit_v;

void uw_global_init(void);
void uw_app_init(uw_app*);

void uw_client_connect(unsigned id, int pass, int sock,
                       int (*send)(int sockfd, const void *buf, size_t len),
                       int (*close)(int fd),
                       void *logger_data, uw_logger log_error);
void uw_prune_clients(uw_context);
failure_kind uw_initialize(uw_context);

uw_context uw_init(int id, void *logger_data, uw_logger log_debug);
void uw_close(uw_context);
int uw_set_app(uw_context, uw_app*);
uw_app *uw_get_app(uw_context);
void uw_set_db(uw_context, void*);
void *uw_get_db(uw_context);
void uw_free(uw_context);
void uw_reset(uw_context);
void uw_reset_keep_request(uw_context);
void uw_reset_keep_error_message(uw_context);
char *uw_get_url_prefix(uw_context);

failure_kind uw_begin_init(uw_context);
void uw_set_on_success(char *);
void uw_set_headers(uw_context, char *(*get_header)(void *, const char *), void *get_header_data);
failure_kind uw_begin(uw_context, char *path);
failure_kind uw_begin_onError(uw_context, char *msg);
void uw_login(uw_context);
void uw_commit(uw_context);
int uw_rollback(uw_context, int will_retry);

__attribute__((noreturn)) void uw_error(uw_context, failure_kind, const char *fmt, ...);
char *uw_error_message(uw_context);
void uw_set_error_message(uw_context, const char *fmt, ...);
uw_Basis_string uw_dup_and_clear_error_message(uw_context);
int uw_has_error(uw_context);
void uw_push_cleanup(uw_context, void (*func)(void *), void *arg);
void uw_pop_cleanup(uw_context);

void *uw_malloc(uw_context, size_t);
void uw_begin_region(uw_context);
void uw_end_region(uw_context);
void uw_memstats(uw_context);

int uw_send(uw_context, int sock);
int uw_print(uw_context, int fd);
int uw_output(uw_context ctx, int (*output)(void *data, const char *buf, size_t len), void *data);

int uw_set_input(uw_context, const char *name, char *value);
int uw_set_file_input(uw_context, char *name, uw_Basis_file);

char *uw_get_input(uw_context, int name);
char *uw_get_optional_input(uw_context, int name);
uw_Basis_file uw_get_file_input(uw_context, int name);
void uw_enter_subform(uw_context, int name);
void uw_leave_subform(uw_context);
int uw_enter_subforms(uw_context, int name);
int uw_next_entry(uw_context);

void uw_write(uw_context, const char*);

uw_Basis_source uw_Basis_new_client_source(uw_context, uw_Basis_string);
uw_unit uw_Basis_set_client_source(uw_context, uw_Basis_source, uw_Basis_string);

void uw_set_script_header(uw_context, const char*);
char *uw_Basis_get_settings(uw_context, uw_unit);
char *uw_get_real_script(uw_context);

uw_Basis_string uw_Basis_maybe_onload(uw_context, uw_Basis_string);
uw_Basis_string uw_Basis_maybe_onunload(uw_context, uw_Basis_string);

void uw_set_needs_push(uw_context, int);
void uw_set_needs_sig(uw_context, int);

char *uw_Basis_htmlifyInt(uw_context, uw_Basis_int);
char *uw_Basis_htmlifyFloat(uw_context, uw_Basis_float);
char *uw_Basis_htmlifyString(uw_context, uw_Basis_string);
char *uw_Basis_htmlifyBool(uw_context, uw_Basis_bool);
char *uw_Basis_htmlifyTime(uw_context, uw_Basis_time);
char *uw_Basis_htmlifySpecialChar(uw_context, unsigned char);
char *uw_Basis_htmlifySource(uw_context, uw_Basis_source);

uw_unit uw_Basis_htmlifyInt_w(uw_context, uw_Basis_int);
uw_unit uw_Basis_htmlifyFloat_w(uw_context, uw_Basis_float);
uw_unit uw_Basis_htmlifyString_w(uw_context, uw_Basis_string);
uw_unit uw_Basis_htmlifyBool_w(uw_context, uw_Basis_bool);
uw_unit uw_Basis_htmlifyTime_w(uw_context, uw_Basis_time);
uw_unit uw_Basis_htmlifySpecialChar_w(uw_context, unsigned char);
uw_unit uw_Basis_htmlifySource_w(uw_context, uw_Basis_source);

char *uw_Basis_attrifyInt(uw_context, uw_Basis_int);
char *uw_Basis_attrifyFloat(uw_context, uw_Basis_float);
char *uw_Basis_attrifyString(uw_context, uw_Basis_string);
char *uw_Basis_attrifyChar(uw_context, uw_Basis_char);
char *uw_Basis_attrifyTime(uw_context, uw_Basis_time);
char *uw_Basis_attrifyChannel(uw_context, uw_Basis_channel);
char *uw_Basis_attrifyClient(uw_context, uw_Basis_client);
char *uw_Basis_attrifyCss_class(uw_context, uw_Basis_css_class);

uw_unit uw_Basis_attrifyInt_w(uw_context, uw_Basis_int);
uw_unit uw_Basis_attrifyFloat_w(uw_context, uw_Basis_float);
uw_unit uw_Basis_attrifyString_w(uw_context, uw_Basis_string);
uw_unit uw_Basis_attrifyChar_w(uw_context, uw_Basis_char);

char *uw_Basis_urlifyInt(uw_context, uw_Basis_int);
char *uw_Basis_urlifyFloat(uw_context, uw_Basis_float);
char *uw_Basis_urlifyString(uw_context, uw_Basis_string);
char *uw_Basis_urlifyBool(uw_context, uw_Basis_bool);
char *uw_Basis_urlifyTime(uw_context, uw_Basis_time);
char *uw_Basis_urlifyChannel(uw_context, uw_Basis_channel);
char *uw_Basis_urlifySource(uw_context, uw_Basis_source);

uw_unit uw_Basis_urlifyInt_w(uw_context, uw_Basis_int);
uw_unit uw_Basis_urlifyFloat_w(uw_context, uw_Basis_float);
uw_unit uw_Basis_urlifyString_w(uw_context, uw_Basis_string);
uw_unit uw_Basis_urlifyBool_w(uw_context, uw_Basis_bool);
uw_unit uw_Basis_urlifyTime_w(uw_context, uw_Basis_time);
uw_unit uw_Basis_urlifyChannel_w(uw_context, uw_Basis_channel);
uw_unit uw_Basis_urlifySource_w(uw_context, uw_Basis_source);

uw_Basis_unit uw_Basis_unurlifyUnit(uw_context ctx, char **s);
uw_Basis_int uw_Basis_unurlifyInt(uw_context, char **);
uw_Basis_float uw_Basis_unurlifyFloat(uw_context, char **);
uw_Basis_string uw_Basis_unurlifyString(uw_context, char **);
uw_Basis_string uw_Basis_unurlifyString_fromClient(uw_context, char **);
uw_Basis_bool uw_Basis_unurlifyBool(uw_context, char **);
uw_Basis_time uw_Basis_unurlifyTime(uw_context, char **);

uw_Basis_int uw_Basis_strlen(uw_context, const char *);
uw_Basis_bool uw_Basis_strlenGe(uw_context, uw_Basis_string, uw_Basis_int);
uw_Basis_char uw_Basis_strsub(uw_context, const char *, uw_Basis_int);
uw_Basis_string uw_Basis_strsuffix(uw_context, const char *, uw_Basis_int);
uw_Basis_string uw_Basis_strcat(uw_context, const char *, const char *);
uw_Basis_string uw_Basis_mstrcat(uw_context ctx, ...);
uw_Basis_int *uw_Basis_strindex(uw_context, const char *, uw_Basis_char);
uw_Basis_int *uw_Basis_strsindex(uw_context, const char *, const char *needle);
uw_Basis_string uw_Basis_strchr(uw_context, const char *, uw_Basis_char);
uw_Basis_int uw_Basis_strcspn(uw_context, const char *, const char *);
uw_Basis_string uw_Basis_substring(uw_context, const char *, uw_Basis_int, uw_Basis_int);
uw_Basis_string uw_Basis_str1(uw_context, uw_Basis_char);

uw_Basis_string uw_strdup(uw_context, const char *);
uw_Basis_string uw_maybe_strdup(uw_context, const char *);
char *uw_memdup(uw_context, const char *, size_t);

uw_Basis_string uw_Basis_sqlifyInt(uw_context, uw_Basis_int);
uw_Basis_string uw_Basis_sqlifyFloat(uw_context, uw_Basis_float);
uw_Basis_string uw_Basis_sqlifyString(uw_context, uw_Basis_string);
uw_Basis_string uw_Basis_sqlifyChar(uw_context, uw_Basis_char);
uw_Basis_string uw_Basis_sqlifyBool(uw_context, uw_Basis_bool);
uw_Basis_string uw_Basis_sqlifyTime(uw_context, uw_Basis_time);
uw_Basis_string uw_Basis_sqlifyBlob(uw_context, uw_Basis_blob);
uw_Basis_string uw_Basis_sqlifyChannel(uw_context, uw_Basis_channel);
uw_Basis_string uw_Basis_sqlifyClient(uw_context, uw_Basis_client);

uw_Basis_string uw_Basis_sqlifyIntN(uw_context, uw_Basis_int*);
uw_Basis_string uw_Basis_sqlifyFloatN(uw_context, uw_Basis_float*);
uw_Basis_string uw_Basis_sqlifyStringN(uw_context, uw_Basis_string);
uw_Basis_string uw_Basis_sqlifyBoolN(uw_context, uw_Basis_bool*);
uw_Basis_string uw_Basis_sqlifyTimeN(uw_context, uw_Basis_time*);

char *uw_Basis_ensqlBool(uw_Basis_bool);
char *uw_Basis_ensqlTime(uw_context ctx, uw_Basis_time);

char *uw_Basis_jsifyString(uw_context, uw_Basis_string);
char *uw_Basis_jsifyChar(uw_context, uw_Basis_char);
char *uw_Basis_jsifyChannel(uw_context, uw_Basis_channel);
char *uw_Basis_jsifyTime(uw_context, uw_Basis_time);

uw_Basis_string uw_Basis_intToString(uw_context, uw_Basis_int);
uw_Basis_string uw_Basis_floatToString(uw_context, uw_Basis_float);
uw_Basis_string uw_Basis_charToString(uw_context, uw_Basis_char);
uw_Basis_string uw_Basis_boolToString(uw_context, uw_Basis_bool);
uw_Basis_string uw_Basis_timeToString(uw_context, uw_Basis_time);

uw_Basis_int *uw_Basis_stringToInt(uw_context, uw_Basis_string);
uw_Basis_float *uw_Basis_stringToFloat(uw_context, uw_Basis_string);
uw_Basis_char *uw_Basis_stringToChar(uw_context, uw_Basis_string);
uw_Basis_bool *uw_Basis_stringToBool(uw_context, uw_Basis_string);
uw_Basis_time *uw_Basis_stringToTime(uw_context, const char *);

uw_Basis_int uw_Basis_stringToInt_error(uw_context, uw_Basis_string);
uw_Basis_float uw_Basis_stringToFloat_error(uw_context, uw_Basis_string);
uw_Basis_char uw_Basis_stringToChar_error(uw_context, uw_Basis_string);
uw_Basis_bool uw_Basis_stringToBool_error(uw_context, uw_Basis_string);
uw_Basis_time uw_Basis_stringToTime_error(uw_context, const char *);
uw_Basis_blob uw_Basis_stringToBlob_error(uw_context, uw_Basis_string, size_t);
uw_Basis_channel uw_Basis_stringToChannel_error(uw_context, uw_Basis_string);
uw_Basis_client uw_Basis_stringToClient_error(uw_context, uw_Basis_string);

uw_Basis_time uw_Basis_unsqlTime(uw_context, uw_Basis_string);

uw_Basis_string uw_Basis_requestHeader(uw_context, uw_Basis_string);

void uw_write_header(uw_context, uw_Basis_string);
void uw_clear_headers(uw_context);

uw_Basis_string uw_Basis_get_cookie(uw_context, uw_Basis_string c);
uw_unit uw_Basis_set_cookie(uw_context, uw_Basis_string prefix, uw_Basis_string c, uw_Basis_string v, uw_Basis_time *expires, uw_Basis_bool secure);
uw_unit uw_Basis_clear_cookie(uw_context, uw_Basis_string prefix, uw_Basis_string c);

uw_Basis_channel uw_Basis_new_channel(uw_context, uw_unit);
uw_unit uw_Basis_send(uw_context, uw_Basis_channel, uw_Basis_string);

uw_Basis_client uw_Basis_self(uw_context);

uw_Basis_string uw_Basis_bless(uw_context, uw_Basis_string);
uw_Basis_string uw_Basis_blessMime(uw_context, uw_Basis_string);
uw_Basis_string uw_Basis_blessRequestHeader(uw_context, uw_Basis_string);
uw_Basis_string uw_Basis_blessResponseHeader(uw_context, uw_Basis_string);

uw_Basis_string uw_Basis_checkUrl(uw_context, uw_Basis_string);
uw_Basis_string uw_Basis_checkMime(uw_context, uw_Basis_string);
uw_Basis_string uw_Basis_checkRequestHeader(uw_context, uw_Basis_string);
uw_Basis_string uw_Basis_checkResponseHeader(uw_context, uw_Basis_string);

uw_Basis_string uw_Basis_getHeader(uw_context, uw_Basis_string name);
uw_unit uw_Basis_setHeader(uw_context, uw_Basis_string name, uw_Basis_string value);

uw_Basis_string uw_unnull(uw_Basis_string);
uw_Basis_string uw_Basis_makeSigString(uw_context, uw_Basis_string);
int uw_streq(uw_Basis_string, uw_Basis_string);
uw_Basis_string uw_Basis_sigString(uw_context, uw_unit);

uw_Basis_string uw_Basis_fileName(uw_context, uw_Basis_file);
uw_Basis_string uw_Basis_fileMimeType(uw_context, uw_Basis_file);
uw_Basis_blob uw_Basis_fileData(uw_context, uw_Basis_file);
uw_Basis_int uw_Basis_blobSize(uw_context, uw_Basis_blob);
uw_Basis_blob uw_Basis_textBlob(uw_context, uw_Basis_string);

uw_Basis_string uw_Basis_postType(uw_context, uw_Basis_postBody);
uw_Basis_string uw_Basis_postData(uw_context, uw_Basis_postBody);
void uw_noPostBody(uw_context);
void uw_postBody(uw_context, uw_Basis_postBody);
int uw_hasPostBody(uw_context);
uw_Basis_postBody uw_getPostBody(uw_context);

void uw_mayReturnIndirectly(uw_context);
__attribute__((noreturn)) void uw_return_blob(uw_context, uw_Basis_blob, uw_Basis_string mimeType);
__attribute__((noreturn)) void uw_redirect(uw_context, uw_Basis_string url);

uw_Basis_time uw_Basis_now(uw_context);
uw_Basis_time uw_Basis_addSeconds(uw_context, uw_Basis_time, uw_Basis_int);
uw_Basis_int uw_Basis_diffInSeconds(uw_context, uw_Basis_time, uw_Basis_time);
uw_Basis_int uw_Basis_toSeconds(uw_context, uw_Basis_time);
uw_Basis_int uw_Basis_diffInMilliseconds(uw_context, uw_Basis_time, uw_Basis_time);
uw_Basis_int uw_Basis_toMilliseconds(uw_context, uw_Basis_time);
extern const uw_Basis_time uw_Basis_minTime;

void uw_register_transactional(uw_context, void *data, uw_callback commit, uw_callback rollback, uw_callback_with_retry free);

void uw_check_heap(uw_context, size_t extra);
char *uw_heap_front(uw_context);
void uw_set_heap_front(uw_context, char*);

uw_Basis_string uw_Basis_unAs(uw_context, uw_Basis_string);

extern char *uw_sqlfmtInt;
extern char *uw_sqlfmtFloat;
extern int uw_Estrings;
extern char *uw_sqlsuffixString;
extern char *uw_sqlsuffixChar;
extern char *uw_sqlsuffixBlob;
extern char *uw_sqlfmtUint4;

void *uw_get_global(uw_context, char *name);
void uw_set_global(uw_context, char *name, void *data, uw_callback free);

uw_Basis_bool uw_Basis_isalnum(uw_context, uw_Basis_char);
uw_Basis_bool uw_Basis_isalpha(uw_context, uw_Basis_char);
uw_Basis_bool uw_Basis_isblank(uw_context, uw_Basis_char);
uw_Basis_bool uw_Basis_iscntrl(uw_context, uw_Basis_char);
uw_Basis_bool uw_Basis_isdigit(uw_context, uw_Basis_char);
uw_Basis_bool uw_Basis_isgraph(uw_context, uw_Basis_char);
uw_Basis_bool uw_Basis_islower(uw_context, uw_Basis_char);
uw_Basis_bool uw_Basis_isprint(uw_context, uw_Basis_char);
uw_Basis_bool uw_Basis_ispunct(uw_context, uw_Basis_char);
uw_Basis_bool uw_Basis_isspace(uw_context, uw_Basis_char);
uw_Basis_bool uw_Basis_isupper(uw_context, uw_Basis_char);
uw_Basis_bool uw_Basis_isxdigit(uw_context, uw_Basis_char);
uw_Basis_char uw_Basis_tolower(uw_context, uw_Basis_char);
uw_Basis_char uw_Basis_toupper(uw_context, uw_Basis_char);

uw_Basis_int uw_Basis_ord(uw_context, uw_Basis_char);
uw_Basis_char uw_Basis_chr(uw_context, uw_Basis_int);

uw_Basis_string uw_Basis_currentUrl(uw_context);
void uw_set_currentUrl(uw_context, char *);

extern size_t uw_messages_max, uw_clients_max, uw_headers_max, uw_page_max, uw_heap_max, uw_script_max;
extern size_t uw_inputs_max, uw_cleanup_max, uw_subinputs_max, uw_deltas_max, uw_transactionals_max, uw_globals_max;

extern size_t uw_database_max;

extern int uw_time;

void uw_set_deadline(uw_context, int);
void uw_check_deadline(uw_context);

uw_Basis_unit uw_Basis_debug(uw_context, uw_Basis_string);
uw_Basis_int uw_Basis_naughtyDebug(uw_context, uw_Basis_string);

void uw_set_client_data(uw_context, void *);

uw_Basis_int uw_Basis_rand(uw_context);

extern int uw_time_max, uw_supports_direct_status, uw_min_heap;

failure_kind uw_runCallback(uw_context, void (*callback)(uw_context));

uw_Basis_string uw_Basis_timef(uw_context, const char *fmt, uw_Basis_time);
uw_Basis_time uw_Basis_stringToTimef(uw_context, const char *fmt, uw_Basis_string);
uw_Basis_time uw_Basis_stringToTimef_error(uw_context, const char *fmt, uw_Basis_string);

uw_Basis_string uw_Basis_crypt(uw_context, uw_Basis_string key, uw_Basis_string salt);

uw_Basis_bool uw_Basis_eq_time(uw_context, uw_Basis_time, uw_Basis_time);
uw_Basis_bool uw_Basis_lt_time(uw_context, uw_Basis_time, uw_Basis_time);
uw_Basis_bool uw_Basis_le_time(uw_context, uw_Basis_time, uw_Basis_time);

void uw_buffer_init(size_t max, uw_buffer *, size_t initial);
void uw_buffer_free(uw_buffer *);
void uw_buffer_reset(uw_buffer *);
int uw_buffer_check(uw_buffer *, size_t extra);
size_t uw_buffer_used(uw_buffer *);
size_t uw_buffer_avail(uw_buffer *);
int uw_buffer_append(uw_buffer *, const char *, size_t);

void uw_setQueryString(uw_context, uw_Basis_string);
uw_Basis_string uw_queryString(uw_context);

uw_Basis_time *uw_Basis_readUtc(uw_context, uw_Basis_string);

void uw_isPost(uw_context);
uw_Basis_bool uw_Basis_currentUrlHasPost(uw_context);
uw_Basis_bool uw_Basis_currentUrlHasQueryString(uw_context);

void uw_cutErrorLocation(char *);

uw_Basis_string uw_Basis_fresh(uw_context);

uw_Basis_float uw_Basis_floatFromInt(uw_context, uw_Basis_int);
uw_Basis_int uw_Basis_ceil(uw_context, uw_Basis_float);
uw_Basis_int uw_Basis_trunc(uw_context, uw_Basis_float);
uw_Basis_int uw_Basis_round(uw_context, uw_Basis_float);

uw_Basis_string uw_Basis_atom(uw_context, uw_Basis_string);
uw_Basis_string uw_Basis_css_url(uw_context, uw_Basis_string);
uw_Basis_string uw_Basis_property(uw_context, uw_Basis_string);

#endif
