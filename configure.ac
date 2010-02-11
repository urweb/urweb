AC_INIT([urweb], [1.0])
AC_USE_SYSTEM_EXTENSIONS
AM_INIT_AUTOMAKE([-Wall -Werror foreign no-define])
AC_PROG_CC()
AC_PROG_LIBTOOL()
AC_CONFIG_MACRO_DIR([m4])

AC_CHECK_LIB(mhash, mhash_get_block_size, [], [echo "You must install libmhash."; exit 1])
AC_CHECK_HEADER([mhash.h], [], [echo "You must install libmhash dev files."; exit 1])

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
