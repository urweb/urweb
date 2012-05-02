lib_LTLIBRARIES = liburweb.la liburweb_http.la liburweb_cgi.la liburweb_fastcgi.la liburweb_static.la

liburweb_la_SOURCES = memmem.c openssl.c urweb.c request.c queue.c
liburweb_http_la_SOURCES = http.c
liburweb_cgi_la_SOURCES = cgi.c
liburweb_fastcgi_la_SOURCES = fastcgi.c
liburweb_static_la_SOURCES = static.c

AM_CPPFLAGS = -I../../include/urweb @OPENSSL_INCLUDES@
AM_CFLAGS = -Wimplicit -Wall -Werror -Wno-format-security -Wno-deprecated-declarations
