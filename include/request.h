#ifndef REQUEST_H
#define REQUEST_H

#include <sys/types.h>

#include "types.h"

typedef struct uw_rc *uw_request_context;

void uw_request_init(void);
void uw_sign(const char *in, char *out);

uw_request_context uw_new_request_context(void);
void uw_free_request_context(uw_request_context);

request_result uw_request(uw_request_context, uw_context, char *request, size_t request_len, int sock);

uw_context uw_request_new_context(void);

void *client_pruner(void *data);

#endif
