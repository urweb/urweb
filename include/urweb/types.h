#ifndef URWEB_TYPES_H
#define URWEB_TYPES_H

#include <time.h>
#include <unistd.h>
#include <stdint.h>

typedef long long uw_Basis_int;
typedef double uw_Basis_float;
typedef char* uw_Basis_string;
typedef char uw_Basis_char;
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

typedef struct uw_context *uw_context;

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
} uw_Basis_postBody;

typedef uw_Basis_string uw_Basis_queryString;

typedef enum { SUCCESS, FATAL, BOUNDED_RETRY, UNLIMITED_RETRY, RETURN_INDIRECTLY } failure_kind;

typedef enum { SERVED, KEEP_OPEN, FAILED } request_result;

typedef struct input *uw_input;

#define INTS_MAX 50
#define FLOATS_MAX 100
#define TIMES_MAX 100

typedef void (*uw_callback)(void *);
typedef void (*uw_callback_with_retry)(void *, int will_retry);
typedef void (*uw_logger)(void*, const char *fmt, ...);

typedef struct {
  void (*callback)(uw_context);
  unsigned int period;
} uw_periodic;

typedef struct {
  int inputs_len, timeout;
  char *url_prefix;

  void (*client_init)();
  void (*initializer)(uw_context);
  void (*expunger)(uw_context, uw_Basis_client);

  void (*db_init)(uw_context);
  int (*db_begin)(uw_context);
  int (*db_commit)(uw_context);
  int (*db_rollback)(uw_context);
  void (*db_close)(uw_context);

  void (*handle)(uw_context, char *);

  int (*input_num)(const char*);
  uw_Basis_string (*cookie_sig)(uw_context);
  int (*check_url)(const char *);
  int (*check_mime)(const char *);
  int (*check_requestHeader)(const char *);
  int (*check_responseHeader)(const char *);

  void (*on_error)(uw_context, char *);

  uw_periodic *periodics; // 0-terminated array

  uw_Basis_string time_format;
} uw_app;

#define ERROR_BUF_LEN 1024

typedef struct {
  size_t max;
  char *start, *front, *back;
} uw_buffer;

#endif
