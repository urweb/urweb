typedef int lw_Basis_int;
typedef float lw_Basis_float;
typedef char* lw_Basis_string;

struct __lws_0 {
};

typedef struct __lws_0 lw_unit;
typedef lw_unit lw_Basis_unit;

enum lw_Basis_bool_enum { lw_Basis_False, lw_Basis_True };

typedef struct lw_Basis_bool {
  enum lw_Basis_bool_enum tag;
} *lw_Basis_bool;

typedef struct lw_context *lw_context;

typedef lw_Basis_string lw_Basis_xhtml;
typedef lw_Basis_string lw_Basis_page;


typedef enum { SUCCESS, FATAL, BOUNDED_RETRY, UNLIMITED_RETRY } failure_kind;
