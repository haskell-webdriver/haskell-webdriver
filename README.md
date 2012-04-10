# About
hs-webdriver is a Selenium WebDriver client for the Haskell programming language. You can use it to automate browser sessions for testing, system administration, etc.

For more information about Selenium itself, see http://seleniumhq.org/

# Installation
hs-webdriver uses the Cabal build system to configure, build, install, and generate documentation on multiple platforms.

For more information on using Cabal and its various installation options, see the Cabal User's Guide at http://www.haskell.org/cabal/users-guide/index.html

## Installation from Hackage
hs-webdriver is hosted on Hackage under the name webdriver. Thus, the simplest way to download and install the most recent version of hs-webdriver is to run:

```sh
cabal install webdriver
```
There are also options to do system-wide installation, version selection, and other build options. See Cabal documentation.

## Installation from this repository

To build and install a git revision for a single user on your system, run these commands from within the repository directory


### using cabal-install

```sh
cabal install
```

### using Cabal

For systems without cabal-install available, you can also run the Setup.hs
script, as such:

```sh
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
runhaskell Setup.hs install
```

For more build options, please refer to the Cabal documentation.
