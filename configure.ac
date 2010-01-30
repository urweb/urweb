AC_INIT([urweb], [1.0])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_PROG_CC()
AC_PROG_LIBTOOL()
AC_CONFIG_MACRO_DIR([m4])

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

AC_SUBST(BIN)
AC_SUBST(LIB)
AC_SUBST(INCLUDE)
AC_SUBST(SITELISP)
AC_SUBST(GCCARGS)

AC_CONFIG_FILES([
        Makefile
        src/c/Makefile
        src/config.sml
])

AC_OUTPUT()

cat <<EOF

Ur/Web configuration:
  bin directory:       BIN       $BIN
  lib directory:       LIB       $LIB
  include directory:   INCLUDE   $INCLUDE
  site-lisp directory: SITELISP  $SITELISP
  Extra GCC args:      GCCARGS   $GCCARGS
EOF
