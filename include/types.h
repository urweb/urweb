typedef long long lw_Basis_int;
typedef double lw_Basis_float;
typedef char* lw_Basis_string;

struct __lws_0 {
};

typedef struct __lws_0 lw_unit;
typedef lw_unit lw_Basis_unit;

typedef enum lw_Basis_bool { lw_Basis_False, lw_Basis_True } lw_Basis_bool;

typedef struct lw_context *lw_context;

typedef lw_Basis_string lw_Basis_xhtml;
typedef lw_Basis_string lw_Basis_page;


typedef enum { SUCCESS, FATAL, BOUNDED_RETRY, UNLIMITED_RETRY } failure_kind;
