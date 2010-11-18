AC_INIT([urweb], [1.0])
AC_USE_SYSTEM_EXTENSIONS
AM_INIT_AUTOMAKE([-Wall -Werror foreign no-define])
AC_PROG_CC()
AC_PROG_LIBTOOL()
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_HEADERS([config.h])

dnl Setting the search directory for mhash.
AC_ARG_WITH(mhash-dir, AS_HELP_STRING([--with-mhash-dir=DIR], [mhash directory; default = /usr]), 
                          with_mhash_dir="$withval", with_mhash_dir="/usr")

dnl Check for libmhash
LIBS_save="$LIBS"
LIBS="-L${with_mhash_dir}/lib/ -lmhash"
AC_CHECK_LIB(mhash, mhash_get_block_size, 
                    [MHASH_LIB_DIR="${with_mhash_dir}/lib"], [echo "You must install libmhash."; exit 1])
LIBS="${LIBS_save}"

dnl Check for mhash.h
AC_CHECK_HEADER(["${with_mhash_dir}/include/mhash.h"],
        [CPPFLAGS="${CPPFLAGS} -I${with_mhash_dir}/include"], [echo "You must install libmhash dev files."; exit 1])

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
AC_SUBST(MHASH_LIB_DIR)

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
  mhash lib directory: MHASH_LIB_DIR  $MHASH_LIB_DIR
  Extra GCC args:      GCCARGS        $GCCARGS  
EOF
