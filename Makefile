
PKG=./cabal-dev/packages-7.4.2.conf/

default: build

clean:
	rm -rf dist

build:
	cabal-dev configure --enable-tests
	cabal-dev build

test: build
	cabal-dev test
