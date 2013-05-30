SANDBOX=../cabal-dev
PKG=$(SANDBOX)/packages-7.4.2.conf/
CBD=cabal-dev -s $(SANDBOX)

default: build

clean:
	rm -rf dist

build:
	cabal-dev --enable-tests configure -s $(SANDBOX)
	$(CBD) build

## In order to run tests, you probably need to have certain webdriver or selenium server.
## Check details at: http://docs.seleniumhq.org/docs/03_webdriver.jsp
test: build
	$(CBD) test


