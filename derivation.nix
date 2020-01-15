{ stdenv, lib, fetchFromGitHub, file, openssl, mlton
, mysql, postgresql, sqlite, gcc
, automake, autoconf, libtool, icu, nix-gitignore
}:

stdenv.mkDerivation rec {
  name = "urweb-${version}";
  version = "2018-06-22";
  
  # src = fetchurl {
  #   url = "http://www.impredicative.com/ur/${name}.tgz";
  #   sha256 = "17qh9mcmlhbv6r52yij8l9ik7j7x6x7c09lf6pznnbdh4sf8p5wb";
  # };

  # src = fetchFromGitHub {
  #   owner = "FrigoEU";
  #   repo = "urweb";
  #   rev = "e52ce9f542f64750941cfd84efdb6d993ee20ff0";
  #   sha256 = "19ba5n7g1dxy7q9949aakqplchsyzwrrnxv8v604vx5sg7fdfn3b";
  # };
  src = ./.;

  buildInputs = [ openssl mlton mysql.connector-c postgresql sqlite automake autoconf libtool icu.dev openssl.dev];

  # prePatch = ''
  #   sed -e 's@/usr/bin/file@${file}/bin/file@g' -i configure
  # '';

  configureFlags = "--with-openssl=${openssl.dev}";

  preConfigure = ''
    ./autogen.sh
    export PGHEADER="${postgresql}/include/libpq-fe.h";
    export MSHEADER="${mysql.connector-c}/include/mysql/mysql.h";
    export SQHEADER="${sqlite.dev}/include/sqlite3.h";
    export CC="${gcc}/bin/gcc";
    export CCARGS="-I$out/include \
                   -I${icu.dev}/include \
                   -L${openssl.out}/lib \
                   -L${mysql.connector-c}/lib \
                   -L${postgresql.lib}/lib \
                   -L${sqlite.out}/lib \
                   -L${icu.out}/lib";
  '';

  # Be sure to keep the statically linked libraries
  dontDisableStatic = true;

  meta = {
    description = "Advanced purely-functional web programming language";
    homepage    = "http://www.impredicative.com/ur/";
    license     = stdenv.lib.licenses.bsd3;
    platforms   = stdenv.lib.platforms.linux ++ stdenv.lib.platforms.darwin;
    maintainers = [ stdenv.lib.maintainers.thoughtpolice stdenv.lib.maintainers.sheganinans ];
  };
}
