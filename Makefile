
SANDBOX=../cabal-dev
PKG=$(SANDBOX)/packages-7.4.2.conf/
CBD=cabal-dev -s $(SANDBOX)

default: build

clean:
	rm -rf dist

build:
	cabal-dev --enable-tests configure -s $(SANDBOX)
	$(CBD) build

test: build
	$(CBD) test

## download selenium-server and start it before run tests
start:
	java -jar ../libs/selenium-server-standalone-2.31.0.jar
