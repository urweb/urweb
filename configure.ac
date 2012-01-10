AC_INIT([urweb], [20120110])
WORKING_VERSION=1
AC_USE_SYSTEM_EXTENSIONS
AM_INIT_AUTOMAKE([-Wall -Werror foreign no-define])
AC_PROG_CC()
AC_PROG_LIBTOOL()
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_HEADERS([config.h])

AX_PTHREAD([echo >/dev/null], [echo "Your C compiler does not support POSIX threads."; exit 1])

AX_CHECK_OPENSSL([echo >/dev/null], [echo "You must install OpenSSL development files."; exit 1])

AC_CHECK_PROG(MLTON, mlton, yes, [])

if test [-z $MLTON]; then
    echo "You must install MLton."
    exit 1
fi

AC_CHECK_PROG(MLLEX, mllex, yes, [])

if test [-z $MLLEX]; then
    echo "You must install MLton (to get mllex)."
    exit 1
fi

AC_CHECK_PROG(MLYACC, mlyacc, yes, [])

if test [-z $MLYACC]; then
    echo "You must install MLton (to get mlyacc)."
    exit 1
fi

if test [$prefix = "NONE"]; then
    prefix=/usr/local
fi

if test [-z $BIN]; then
    BIN=$prefix/bin
fi

if test [-z $LIB]; then
    LIB=$prefix/lib/urweb
fi

if test [-z $INCLUDE]; then
    INCLUDE=$prefix/include/urweb
fi

if test [-z $SITELISP]; then
    SITELISP=$prefix/share/emacs/site-lisp/urweb-mode
fi

AC_ARG_WITH([emacs],
            [AS_HELP_STRING([--without-emacs],
              [disable installation of Emacs mode])],
            [],
            [with_emacs=yes])

AM_CONDITIONAL(USE_EMACS, test "x$with_emacs" = xyes)

if test [-z $PGHEADER]; then
   AC_CHECK_HEADERS([postgresql/libpq-fe.h],
                    [PGHEADER=postgresql/libpq-fe.h])
fi

if test [-z $PGHEADER]; then
   PGHEADER=libpq-fe.h
fi

if test [-z $MSHEADER]; then
   AC_CHECK_HEADERS([mysql/mysql.h],
                    [MSHEADER=mysql/mysql.h])
fi

if test [-z $MSHEADER]; then
   MSHEADER=mysql.h
fi

if test [-z $SQHEADER]; then
   SQHEADER=sqlite3.h
fi

if test [$WORKING_VERSION = "1"]; then
   VERSION="$VERSION + `hg identify || (cat .hg_archival.txt | grep 'node\:') || echo ?`"
fi

AC_SUBST(CC)
AC_SUBST(BIN)
AC_SUBST(LIB)
AC_SUBST(INCLUDE)
AC_SUBST(SITELISP)
AC_SUBST(CCARGS)
AC_SUBST(PGHEADER)
AC_SUBST(MSHEADER)
AC_SUBST(SQHEADER)
AC_SUBST(VERSION)
AC_SUBST(PTHREAD_CFLAGS)
AC_SUBST(PTHREAD_LIBS)

AC_CONFIG_FILES([
        Makefile
        src/c/Makefile
        src/config.sml
])

AC_OUTPUT()

cat <<EOF

Ur/Web configuration:
  bin directory:       BIN            $BIN
  lib directory:       LIB            $LIB
  include directory:   INCLUDE        $INCLUDE
  site-lisp directory: SITELISP       $SITELISP
  C compiler:          CC             $CC
  Extra CC args:       CCARGS         $CCARGS  
  Postgres C header:   PGHEADER       $PGHEADER
  MySQL C header:      MSHEADER       $MSHEADER
  SQLite C header:     SQHEADER       $SQHEADER
  OpenSSL:             OPENSSL_LIBS   $OPENSSL_LIBS

  Version:             $VERSION
EOF
