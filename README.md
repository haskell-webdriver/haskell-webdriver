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
There are also options to do system-wide installation, version selection, and other build options; see cabal-install documentation.

## Installation from this repository

To build and install a git revision for a single user on your system, run these commands from within the repository directory


### Using cabal-install

```sh
cabal install
```

### Using Cabal

For systems without cabal-install available, you can also run the Setup.hs
script, as such:

```sh
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
runhaskell Setup.hs install
```

For more build options, please refer to the Cabal documentation.


#Getting Started
WebDriver is a client-server protocol. Since hs-webdriver only implements a WebDriver client, in order to make use of this library you must have a WebDriver server that you can connect to.

##Using the Selenium stand-alone server
While you can use any WebDriver server out there, probably the simplest server to use with hs-webdriver is the Java stand-alone server. This server is cross-platform and works with every major browser. Head over to http://docs.seleniumhq.org/download/ and download the latest version of Selenium Server. Next, run the Java jar; in a POSIX shell, this should look something like:

    java -jar selenium-server-standalone-X.X.X.jar

The server should now be up and running at localhost on port 4444.

##Hello, World!
With the Selenium server running locally, you're ready to write browser automation scripts in Haskell. Let's start with a simple example.
```hs
    {-# LANGUAGE OverloadedStrings #-}
    import Test.WebDriver
    
    myConfig :: WDConfig
    myConfig = defaultConfig
    
    main :: IO ()
    main = runSession myConfig $ do
      openPage "http://google.com"
      searchInput <- findElem (ByCSS "input[type='text']")
      sendKeys "Hello, World!" searchInput
```
hs-webdriver uses a very simple EDSL implemented within a state monad. Interacting with the remote browser is done via a sequence of commands within this monad. The state monad maintains implicit information about the WebDriver session between commands, so that individual commands only need to specify parameters relevant to the action they perform. If you're new to monads, there are plenty of resources available for learning on the web, but for now you can think of the  `WD` monad as a very simple imperative language operating on an implicitly defined session object.

Let's take a closer look at each piece of this example.

###Demonic invocations: a bit of boilerplate

    {-# LANGUAGE OverloadedStrings #-}

hs-webdriver uses the `Text` type to represent Unicode character sequences, which is significantly more efficient than the standard Haskell implementation for Unicode strings. This directive tells GHC to overload string literals so that they can be used to represent `Text` values.

    import Test.WebDriver

This line is fairly straightforward; we need to import the library so that we can use it! Most of the basic API is available through the `Test.WebDriver` module, so this is the only import you should need for most tests. There are other modules that may be of interest for advanced usage; in particular, `Test.WebDriver.Commands.Wait` provides so-called "implicit waits" as defined by other WebDriver libraries.

###Configuring a WebDriver session

    myConfig :: WDConfig
    myConfig = defaultConfig
   
To configure a new WebDriver session, we use the `WDConfig` type; this is a record type with various configuration fields. To connect to the Selenium server that we spawned earlier, the `defaultConfig` is sufficient. By default, the browser is set to Firefox, but that can be changed; the following configuration will use Google Chrome instead of Firefox for our test:

    myConfig :: WDConfig
    myConfig = defaultConfig { wdCapabilities = defaultCaps { browser = chrome } }
 
###Initializing tests

    main :: IO ()
    main = runSession myConfig $ do

`main` is the standard entry point for a Haskell program, defined as a value of type `IO a`. In order to transform our `WD` action into an `IO` action, we use the `runSession` function, which has the type:

    runSession :: WDConfig -> WD a -> IO a
 
So we pass to `runSession` our configuration record along with a WebDriver "script" to perform, and it transforms the script into a side-effectful `IO` action. The `WDConfig` record is used to automatically initialize our session with the remote server.

NOTE: `runSession` does not automatically close the session it creates. This is intentional, as you may want to manually inspect the browser state after your code executes. If you want to have the session automatically close, you can use the `finallyClose` function to provide this behavior.

    main = runSession myConfig . finallyClose $ do


###Actually writing tests!

      openPage "http://google.com"
      searchInput <- findElem (ByCSS "input[type='text']")
      sendKeys "Hello, World!" searchInput

Interaction with the browser is accomplished via WebDriver "commands", which are just function calls within the `WD` monad. Most of these commands are defined in the `Test.WebDriver.Commands` modules, and are fairly self-explanatory. In this example, `openPage` opens a new URL, and `findElem` searches for a DOM element on the current page which matches the given selector (possible selectors include `ById`, `ByName`, `ByClass`, `ByTag`, `ByLinkText`, `ByCSS`, and `ByXPath`). The DOM Element found by the result of the search is bound to the local variable `searchInput`, and `sendKeys` sends a sequence of emulated keystrokes to the given element.

This example contains all of the basic elements of a simple WebDriver test. For complete documentation on each command, check out the documentation for `Test.WebDriver.Commands` (see [#Documentation](Documentation)).


#Documentation

Documentation for hs-webdriver is available on Hackage at <http://hackage.haskell.org/package/webdriver>. However, here's how to generate local HTML documentation from this source revision:

```sh
runhaskell Setup.hs haddock
```

Haddock will generate documentation and save it in dist/doc/html/webdriver

