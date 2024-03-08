lib_LTLIBRARIES = liburweb.la liburweb_http.la liburweb_cgi.la liburweb_fastcgi.la liburweb_static.la

liburweb_la_SOURCES = memmem.c memmem.h openssl.c urweb.c request.c queue.c
liburweb_http_la_SOURCES = http.c
liburweb_cgi_la_SOURCES = cgi.c
liburweb_fastcgi_la_SOURCES = fastcgi.c fastcgi.h
liburweb_static_la_SOURCES = static.c

AM_CPPFLAGS = -I$(srcdir)/../../include/urweb $(OPENSSL_INCLUDES) $(ICU_INCLUDES)
AM_CFLAGS = -Wall -Wunused-parameter -Werror -Wno-format-security -Wno-deprecated-declarations -Wno-use-after-free -U_FORTIFY_SOURCE $(PTHREAD_CFLAGS)
liburweb_la_LDFLAGS = $(AM_LDFLAGS) $(OPENSSL_LDFLAGS) \
	-export-symbols-regex '^(client_pruner|pthread_create_big|strcmp_nullsafe|uw_.*)' \
	-version-info 1:0:0
liburweb_la_LIBADD = $(PTHREAD_LIBS) -lm $(OPENSSL_LIBS) $(ICU_LIBS) -licui18n -licuuc -licudata -licuio
liburweb_http_la_LIBADD = liburweb.la
liburweb_http_la_LDFLAGS = -export-symbols-regex '^(main|uw_.*)' \
	-version-info 1:0:0
liburweb_cgi_la_LIBADD = liburweb.la
liburweb_cgi_la_LDFLAGS = -export-symbols-regex '^(main|uw_.*)' \
	-version-info 1:0:0
liburweb_fastcgi_la_LIBADD = liburweb.la
liburweb_fastcgi_la_LDFLAGS = -export-symbols-regex '^(main|uw_.*)' \
	-version-info 1:0:0
liburweb_static_la_LIBADD = liburweb.la
liburweb_static_la_LDFLAGS = -export-symbols-regex '^(main|uw_.*)' \
	-version-info 1:0:0
