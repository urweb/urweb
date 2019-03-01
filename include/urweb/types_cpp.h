#ifndef URWEB_TYPES_CPP_H
#define URWEB_TYPES_CPP_H

#include <time.h>
#include <unistd.h>
#include <stdint.h>
#include <unicode/utypes.h>

typedef long long uw_Basis_int;
typedef double uw_Basis_float;
typedef char* uw_Basis_string;
typedef UChar32 uw_Basis_char;
typedef struct {
  time_t seconds;
  unsigned microseconds;
} uw_Basis_time;
typedef struct {
  size_t size;
  char *data;
} uw_Basis_blob;

typedef int uw_unit;
typedef uw_unit uw_Basis_unit;

typedef enum uw_Basis_bool { uw_Basis_False, uw_Basis_True } uw_Basis_bool;

typedef uw_Basis_string uw_Basis_xhtml;
typedef uw_Basis_string uw_Basis_page;
typedef uw_Basis_string uw_Basis_xbody;
typedef uw_Basis_string uw_Basis_css_class;

typedef unsigned uw_Basis_client;
typedef struct {
  unsigned cli, chn;
} uw_Basis_channel;

typedef struct {
  int context;
  unsigned long long source;
} uw_Basis_source;

typedef struct uw_Basis_file {
  uw_Basis_string name, type;
  uw_Basis_blob data;
} uw_Basis_file;

typedef struct uw_Basis_postBody {
  uw_Basis_string type, data;
  size_t len;
} uw_Basis_postBody;

typedef uw_Basis_string uw_Basis_queryString;

typedef struct {
  uw_Basis_string name, value, remaining;
} uw_Basis_postField;

typedef enum { SUCCESS, FATAL, BOUNDED_RETRY, UNLIMITED_RETRY, RETURN_INDIRECTLY } failure_kind;

typedef enum { SERVED, KEEP_OPEN, FAILED } request_result;

#define INTS_MAX 50
#define FLOATS_MAX 100
#define TIMES_MAX 100

typedef void (*uw_callback)(void *);
typedef void (*uw_callback_with_retry)(void *, int will_retry);
typedef void (*uw_logger)(void*, const char *fmt, ...);

struct uw_context;

typedef struct {
  void (*callback)(struct uw_context *);
  unsigned int period;
} uw_periodic;

typedef struct {
  int inputs_len, timeout;
  char *url_prefix;

  void (*client_init)();
  void (*initializer)(struct uw_context *);
  void (*expunger)(struct uw_context *, uw_Basis_client);

  void (*db_init)(struct uw_context *);
  int (*db_begin)(struct uw_context *, int could_write);
  int (*db_commit)(struct uw_context *);
  int (*db_rollback)(struct uw_context *);
  void (*db_close)(struct uw_context *);

  void (*handle)(struct uw_context *, char *);

  int (*input_num)(const char*);
  uw_Basis_string (*cookie_sig)(struct uw_context *);
  int (*check_url)(const char *);
  int (*check_mime)(const char *);
  int (*check_requestHeader)(const char *);
  int (*check_responseHeader)(const char *);
  int (*check_envVar)(const char *);
  int (*check_meta)(const char *);

  void (*on_error)(struct uw_context *, char *);

  uw_periodic *periodics; // 0-terminated array

  uw_Basis_string time_format;

  int is_html5;
  char *file_cache;
} uw_app;

typedef struct {
  /* uw_app *app; */
  void *logger_data;
  uw_logger log_error, log_debug;
} uw_loggers;

#define ERROR_BUF_LEN 10240

typedef struct {
  size_t max;
  char *start, *front, *back;
} uw_buffer;

// Caching

#include <pthread.h>
#include "uthash.h"

typedef struct uw_Sqlcache_Value {
  char *result;
  char *output;
  char *scriptOutput;
  unsigned long timeValid;
} uw_Sqlcache_Value;

typedef struct uw_Sqlcache_Entry {
  char *key;
  uw_Sqlcache_Value *value;
  unsigned long timeInvalid;
  UT_hash_handle hh;
} uw_Sqlcache_Entry;

typedef struct uw_Sqlcache_Cache {
  pthread_rwlock_t lockOut;
  pthread_rwlock_t lockIn;
  uw_Sqlcache_Entry *table;
  unsigned long timeInvalid;
  unsigned long timeNow;
  size_t numKeys;
  UT_hash_handle hh;
} uw_Sqlcache_Cache;

#endif
