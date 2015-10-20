
 # Installation and Environment Setup

This example assumes you already have hs-webdriver installed and already have a Selenium server running on your local
machine. If you haven't done these steps, refer to the README for detailed installation/setup instructions. Since this is a
literate Haskell program, you can compile it with GHC and execute it to see it in action.

What this example does not assume is any prior Haskell knowledge on your part. It is intended for anyone with basic
programming knowledge to understand. If you have any questions or need clarification about anything in this example, please
send an email to the project maintainer.


 # Hello, World!

Before you use the library, you'll want to add a bit of boilerplate to your Haskell code to enable a GHC extension. This
isn't technically mandatory but it makes code a lot easier to write.

> {-# LANGUAGE OverloadedStrings #-}

Normally, Haskell string literals have the type `String`, which is a linked list of Unicode code points. Adding this to the
top of your program file instructs GHC to allow string literals to be overloaded to other string-like types. Since
hs-webdriver uses the very efficient `Text` type for Unicode strings, the OverloadedStrings extension makes it significantly
easier to write your code. Using the [text package](https://hackage.haskell.org/package/text) for efficient Unicode string
handling is considered standard practice in `Real World Haskell™`.


 # Configuration

> import Test.WebDriver

> firefoxConfig :: WDConfig
> firefoxConfig = defaultConfig

 Here we tell Haskell to import the library and define a configuration we want to use for a WebDriver session, naming it
`firefoxConfig`. The `defaultConfig` is a pre-defined configuration exported by the library, which uses firefox by default
and has a very sensible configuration for usage when the WebDriver server is on our local machine. We can, however, choose to
use other browsers instead, or point the client to a remote server.

 Note: The line with a `::` is a type signature for our newly defined variable, but it can be omitted because Haskell will
automatically infer the types of values. It's only purpose here is to indicate to the reader that configurations are values
of the type `WDConfig`


 Now let's make a new configuration that uses Chrome instead of Firefox.

> chromeConfig = useBrowser chrome defaultConfig

The `useBrowser` function overrides the browser setting of `defaultConfig` to use `chrome` instead. Let's say we want to
modify our current `chromeConfig` to work with remote sessions.

> remoteConfig = useBrowser opera defaultConfig { wdHost = "secret-offshore-testing-facility.org"
>                                               , wdPort = 666 }

It's also possible to modify a previously defined configuration to create a new one, like this:

> remoteConfig2 = chromeConfig { wdHost = "really-bad-network-connection.biz"
>                              , wdPort = 1234
>                              , wdHTTPRetryCount = 50 }

Here we use the `chromeConfig` that we defined previously and modify it with new options. The `wdHTTPRetryCount` option sets
the number of times we try to send a WebDriver command to an unresponsive remote end.

*Note*: To use Google Chrome, you need to install Google's proprietary
[ChromeDriver](https://sites.google.com/a/chromium.org/chromedriver/) in a directory where it can be recognized by Selenium
Server (see: https://code.google.com/p/selenium/wiki/ChromeDriver). When using propriertary webdrivers such as chromedriver,
Selenium Server simply acts as a proxy server, automatically spawning a proprietary server process as needed.


 # Let's Get Started

Interacting with the browser is done via writing a sequence of commands that are executed in order. For those familiar with
so-called turtle languages, which are used to teach programming by typing commands that draw shapes on screen, you should
feel pretty comfortable with this sort of interface.

> main = runSession firefoxConfig $ do                      -- starts a WebDriver session with the given firefox config, then
>                                                           -- runs the supplied commands

>   openPage "http://google.com"                            -- tells the browser to open the URL http://google.com

>   searchInput <- findElem ( ByCSS "input[type='text']" )  -- asks the browser to find an element on the page with the given
>                                                           -- CSS selector then stores the resulting element in the varialbe 
>                                                           -- named`searchInput`

>   submit searchInput                                      -- submit the input form (technically not required with Google
>                                                           -- but included for example purposes)

>   sendKeys "Hello, World!" searchInput                    -- type into the element, as though a user had issued the
>                                                           -- keystrokes `Hello, World!`
>
>   closeSession                                            -- finally, close the WebDriver session and its associated
>                                                           -- browser process 


The code should be clear even without the comments. But the important things to note about the syntax are:

* The `do` notation is used to begin an indented block of commands. This is analogous to curly braces in C-based language.

* The `<-` notation binds  the result of executing the command on the right side to the variable on the left side.
  In order to do anything meaningful with the results of a command, we must first bind the result to a name in this way.

* Parentheses are required for function parameters that are more than one word long. Instead of ` f(x, g(y), z)` as in most
  languages you instead write ` f x (g y) z `

* The $ operator is equivalent to normal function application. Because it has very low precedence, we can use it to avoid
  parentheses when group function parameters. Without the $ in the example above, we would have to wrap the entire do block
  in a set of parentheses. We could also choose to rewrite the findElem command as

```hs 
	findElem $ ByCSS "input[type='text']"
```

 # About `main`

In a Haskell program, the special variable `main` refers to the entry point of execution. In order for our program to be
valid, `main` must have the following type

```hs
	main :: IO a
```
So, to run a test we need to define a valid `main`, but our do-block has the type `WD a`, which is the type of WebDriver
command sequences. In order to convert our `WD` to an `IO` we have to use the `runSession` function, which has this type

```hs
  runSession :: WDConfig -> WD a -> IO a
```

This says that runSession takes a webdriver config (as described previously) and a list of webdriver commands, and converts
them into a sequence of IO actions that the GHC runtime can understand. In doing so, it also initialzes a new WebDriver
session with information from the supplied `WDConfig`.


 #WebDriver commands

This example contains all of the basic elements of a simple WebDriver test, using a few simple and commonly used commands.
For complete documentation on all the other commands you can use, check out
[Test.WebDriver.Commands](https://hackage.haskell.org/package/webdriver-0.6.0.4/docs/Test-WebDriver-Commands.html) on
Hackage.


 # Monads and other advanced library concepts

hs-webdriver implements a very simple EDSL (embedded domain-specific language) for users to write web automation scripts.
This EDSL is implemented using a state monad. The state monad maintains implicit information about the WebDriver session
between commands, so that individual commands only require you to specify the parameters relevant to the action they perform.
Ironically enough, in some ways this makes the Haskell bindings for WebDriver more imperative than the bindings used by
imperative languages.

A quick aside about monads: Monads are a core concept of Haskell programming. But it's important to realize *you do not need
to understand how monads work to use this library*, in much the same way that you do not need to understand a sorting
algorithm to sort data with it. In fact the choice of using a monadic interface was intended to make the interface easy to
understand, because it conforms naturally to the sequential imperative nature of WebDriver, which is essentially a list of
instructions for a browser to perform. I've even heard some anecdotes from developers at companies that chose to adopt
Haskell for web development, saying that their QA testers, with limited programming knowledge, found the Haskell bindings
easier to understand than the NodeJS WebDriver bindings.

 # Literate Haskell

This example can be compiled as-is with GHC thanks to GHC's Literate Haskell support. Just make sure the file extension
is `.lhs` before compiling. Since this example is also part of the cabal test suites, you can run it like this.

```sh
	cabal test readme-example-beginner
```
