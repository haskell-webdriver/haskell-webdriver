# About
hs-webdriver is a Selenium WebDriver client for the Haskell programming language. You can use it to automate browser sessions for testing, system administration, etc.

# Installation
hs-webdriver uses the cabal build system to configure, build, install, and generate documentation on multiple platforms.

For more information on using Cabal and its various installation options, see the Cabal User's Guide at http://www.haskell.org/cabal/users-guide/index.html

## Installation from Hackage
hs-webdriver is hosted on Hackage under the name webdriver. Thus, the simplest way to download and install the most recent version of hs-webdriver is to run:

```sh
cabal install webdriver
```
There are also options to do system-wide installation, version selection, and other build options. See cabal documentation.

## Installation from this repository

To build and install a git revision for a single user on your system, run these commands from within the repository directory:

```sh
cabal configure --user
cabal build
cabal install
```
For more build options, please refer to the cabal documentation.
