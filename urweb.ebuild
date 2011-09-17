# Distributed under the terms of the BSD3 license

# This file needs to be renamed to something like "urweb-20110917.ebuild", to reflect the Ur/Web version to use.

inherit eutils

EAPI=3

DESCRIPTION="A domain-specific functional programming language for modern web applications"
HOMEPAGE="http://www.impredicative.com/ur/"
SRC_URI="http://www.impredicative.com/ur/${P}.tgz"

LICENSE="BSD"
SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE=""

DEPEND="dev-lang/mlton
	dev-libs/openssl"
RDEPEND="${DEPEND}"

S="${WORKDIR}/urweb"

src_unpack() {
	unpack ${A}
}

src_configure() {
	econf || die
}

src_compile() {
	emake || die
}

src_install() {
	emake DESTDIR=${D} install || die
	dodoc CHANGELOG || die
}
