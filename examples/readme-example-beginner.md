
# Installation and Environment Setup

This example assumes you already have hs-webdriver installed and already have a Selenium server running on your local
machine. If you haven't done these steps, refer to the [README](/README.md) for detailed installation/setup instructions.

What this example does not assume is any prior Haskell knowledge on your part. It is intended for anyone with basic
programming knowledge to understand. However, it cannot fully replace a good tutorial or guide on Haskell. For that,
I recommend reading [Learn You a Haskell for Great Good](http://learnyouahaskell.com/).

If you have any questions or need clarification about anything in this example, please send an email to the project
maintainer.

# Boilerplate

Before you use the library, you'll need to add a bit of boilerplate to your Haskell code:

~~~ {.haskell}
{-# LANGUAGE OverloadedStrings #-}

import Test.WebDriver
~~~

Normally, Haskell string literals (such as `"Hello!"`) have the type `String`, which is a linked list of Unicode code points.
Adding this to the top of your program file instructs GHC to allow string literals to be overloaded to other string-like
types. Since hs-webdriver uses the very efficient `Text` type for Unicode strings, the OverloadedStrings extension makes
it significantly easier to write your code without having to convert all of your `String` values to `Text` ones. Using the 
[text package](https://hackage.haskell.org/package/text) for efficient Unicode string handling is considered standard
practice in "Real World Haskell™".

And, of course, you need an `import` declaration so Haskell knows how to find the library!

# Configuration

~~~ {.haskell}
firefoxConfig :: WDConfig
firefoxConfig = defaultConfig
~~~

Here we tell Haskell to define a very basic configuration for a WebDriver session, naming it `firefoxConfig`.
The `defaultConfig` here refers to a pre-defined data structure exported by the library, which tells webdriver
to use firefox as our browser and has a very sensible configuration for usage when the WebDriver server is on our local
machine.

Note: The first line, containing the `::`, is a type signature for our newly defined variable, but it can be omitted because
Haskell will automatically infer the types of values. Its only purpose here is to indicate to the reader that `firefoxConfig` 
has the type`WDConfig`


Now let's make a new configuration that uses Chrome instead of Firefox.

~~~ {.haskell}
chromeConfig = useBrowser chrome defaultConfig
~~~

The `useBrowser` function above overrides the browser of `defaultConfig` to use `chrome` instead.

Now for a more complex configuration. Let's say we want a session that connects to a remote server and runs the Opera
browser.

~~~ {.haskell}
remoteConfig = useBrowser opera defaultConfig { wdHost = "secret-offshore-testing-facility.org"
                                              , wdPort = 666 }
~~~

The curly braces are part of Haskell's record update syntax. They take the record immediately to the left of
the opening brace (in this case `defaultConfig`) and update some of its fields (in this case `wdHost` and `wdPort`)
with new values.

Now a second remote server configuration:

~~~ {.haskell}
remoteConfig2 = chromeConfig { wdHost = "really-bad-network-connection.biz"
                             , wdPort = 1234
                             , wdHTTPRetryCount = 50 }
~~~

Here we use the `chromeConfig` that we defined previously and modify it with new options. The `wdHTTPRetryCount` option sets
the number of times we try to send a WebDriver command to an unresponsive remote end.

*Note*: To use Google Chrome, you need to install Google's proprietary
[ChromeDriver](https://sites.google.com/a/chromium.org/chromedriver/) in a directory where it can be recognized by Selenium
Server (see: https://code.google.com/p/selenium/wiki/ChromeDriver). When using propriertary webdrivers such as chromedriver,
Selenium Server simply acts as a proxy server, automatically spawning a proprietary server process as needed. Because of
this extra installation step, we simply use the `firefoxConfig` for the remainder of the example.


# Hello, World!

Interacting with the browser is done via writing a sequence of commands that are executed in order. For those familiar with
so-called turtle languages, which are used to teach programming by typing commands that draw shapes on screen, you should
feel pretty comfortable with this sort of interface.

~~~ {.haskell}
main = runSession firefoxConfig $ do                      -- starts a WebDriver session with the given firefox config, then
                                                          -- runs the supplied commands

  openPage "http://google.com"                            -- tells the browser to open the URL http://google.com

  searchInput <- findElem ( ByCSS "input[type='text']" )  -- asks the browser to find an element on the page with the given
                                                          -- CSS selector then stores the resulting element in the varialbe 
                                                          -- named`searchInput`

  submit searchInput                                      -- submit the input form (technically not required with Google
                                                          -- but included for example purposes)

  sendKeys "Hello, World!" searchInput                    -- type into the element, as though a user had issued the
                                                          -- keystrokes `Hello, World!`

  closeSession                                            -- finally, close the WebDriver session and its associated
                                                          -- browser process 
~~~

The code should be clear even without the comments. But the important things to note about the syntax are:

* The `do` notation is used to begin an indented block of commands. This is analogous to curly braces in C-based language.

* The `<-` notation binds the result of executing the command on the right to the variable on the left.
  In order to do anything meaningful with the results of a command, we must first bind the result to a name in this way.

* Parentheses are required for function parameters that are more than one word long. Instead of ` f(x, g(y), z)` as in most
  languages you instead write ` f x (g y) z `

* The `$` operator is equivalent to normal function application. Because it has very low precedence, we can use it to avoid
  parentheses when grouping function parameters. Without the $ in the example above, 
  we would have to wrap the entire do block in a set of parentheses. We could also choose to rewrite the findElem command as

	```hs
		findElem $ ByCSS "input[type='text']"
	```

  *More about `$`*: Haskell beginners are often put off by `$`, because they assume it's a special syntax form. This leads 
  them to believe that Haskell must have a lot of unnecessary and convoluted syntax, but this is not true.
  Because Haskell allows you to define custom infix operators, `$` can be easily defined in code:

  	```hs
  		f $ x = f x
  		infxr 1 $   -- makes $ right-associative with lowest possible precedence
	```
  The full details of Haskell syntax are outside the scope of this introduction, but for more information this 
  [FPComplete blog post](https://www.fpcomplete.com/blog/2012/09/ten-things-you-should-know-about-haskell-syntax) 
  provides a basic overview of Haskell syntax for beginners.


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

Using [markdown-unlit](https://github.com/sol/markdown-unlit), you can compile this example as Literate Haskell with GHC.
Just make sure to use the file extension `.lhs` before compiling.

```sh
cabal update && cabal install markdown-unlit
ghc -pgmL markdown-unlit examples/readme-example-beginner.lhs
```

All example code is also part of the webdriver test suites, so a `cabal test` command will also run example code.
