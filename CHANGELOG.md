#Change Log
##0.8.5
* Added support for experimental Chrome options that are not part of the API
* Added a Phantomjs constructor for the Browser type
* Changed the type of StackFrame line numbers from Word to Int to avoid parse errors in newer Aeson versions (sometimes the server returns negative line numbers)
* Fixed support for http-client > 0.5

##0.8.4
* Added a new `Test.WebDriver.Common.Keys` module with named constants for use with `sendKeys`
* Updated old URLs in documentation
* Introduced support for http-client 5.0

##0.8.3
* Removed most upper bounds on dependencies in our cabal file to avoid stackage version madness.

##0.8.2
* Added a `saveScreenshot` command, for conenience, which writes the results of the `screenshot` command directly to a given file path.
* Added new `WebDriver` instance for `ExceptT`.

##0.8.1
* Previously internal convenience functions `noReturn` and `ignoreReturn` are now exported in Test.WebDriver.JSON
* `elemInfo` is now deprecated due to it being phased out in the Marionette (Firefox) driver. It will likely be removed once Selenium 4 is released.
* Fixed an issue causing PAC settings to not work.  

##0.8.0.4
* Quick fix to parse "unsupported command" errors when using Marionette driver (Selenium + Marionette has nonstandard behavior when reporting that error type)

##0.8.0.3
* Fixed build errors for GHC < 7.10. webdriver now builds with GHC stable releases 7.4.2, 7.6.3, and 7.8.4
* Fixed support for bytestring 0.9.*

##0.8.0.2
* Fixed issue introduced in 0.8 that caused build failure when using aeson < 0.10

##0.8.0.1
* findElems and and findElemsFrom were accidentally changed to return a single Element in the last release. This has been fixed. 

##0.8

###Command changes
* All commands that previously accepted a list parameter now accepts any instance of `Foldable` instead.



###Overloadable configuration
It is now possible to define custom configuration types that can be used to initialize webdriver sessions.

`runSession` now has the following type:
```hs
  runSession :: WebDriverConfig conf => conf -> WD a -> IO a
```
And the typeclass to create new config types looks like this:

```hs
  -- |Class of types that can configure a WebDriver session.
  class WebDriverConfig c where
    -- |Produces a 'Capabilities' from the given configuration.
    mkCaps :: MonadBase IO m => c -> m Capabilities

    -- |Produces a 'WDSession' from the given configuration.
    mkSession :: MonadBase IO m => c -> m WDSession
```

Of course you can still use `WDConfig`, as it is now an instance of `WebDriverConfig`.

###Reworked custom HTTP headers interface
* Support for custom request headers was added rather hastily, resulting in several functions having explicit `RequestHeaders` parameters. The interface has been reworked now so that custom request headers are stored inside `WDSession` and explicit `RequestHeaders` parameters have been removed.
* There's also now a distinction in configuration between `wdAuthHeaders` which are used only during initial session creation, and `wdRequestHeaders`, which are used with all other HTTP requests
* Two new utility functions were added to make working with custom HTTP headers easier: `withRequestHeaders` and `withAuthHeaders`

###Clean-up and dependency changes
* Removed a whole mess of unusued import and deprecation warnings when building with GHC 7.10
* We now enforce an attoparsec lower bound of 0.10 (there was no lower bound before)
* The unnecessary dependency on mtl is now removed.
* Added some monad transformer instances for WebDriver and WDSessionState that were mysteriously missing: strict WriterT, ReaderT, ListT
* data-default dependency was changed to data-default-class

##0.7

Because this is a fairly major update, changes have been described in detail and organized into categories. Most of the potentially breaking changes are to the "intermediate" API that might affect library code or advanced applications; changes that are not entirely "user-facing" but also not quite "internal".

Basic web test code only has to contend with a few additional symbol exports, overloading of type signatures on some existing functions, and the reworked session history API.

###Top-level API (exposed by`Test.WebDriver` module)
  * `withSession` is now overloaded over the new constraint alias `WDSessionStateControl` ([see below](#typeclass-api)). This means that it can be used with monad transformer stacks over `WD` without any lifting required.
  * Added several new `WDConfig` convenience functions: `modifyCaps`, `useBrowser`, `useVersion`, `usePlatform`, `useProxy`; these functions are overloaded over the new `HasCapabilities` constraint alias ([see below](#typeclass-api))
  * Reworked and improved session history API
    * Added a `SessionHistory` record type to replace the old `(Request, Response ByteString)` type. The new type has the same data as the previous tuple, but additionally records the number of attempted HTTP retries, and instead of `Response ByteString` uses `Either SomeException (Response ByteString)` so that HTTP request errors can be logged.
    * Removed `wdKeepSessHist` field from `WDConfig` and replaced it with `wdHistoryConfig`, which uses a new `SessionHistoryConfig` type.
      
      ```hs
        -- |A function used to append new requests/responses to session history.
        type SessionHistoryConfig = SessionHistory -> [SessionHistory] -> [SessionHistory]
      ```
    * The new field can be configured using several new constants: `noHistory`, `onlyMostRecentHistory`, and `unlimitedHistory`. Note: `unlimitedHistory` is now the default configuration for history. For the old behavior, use `onlyMostRecentHistory`.
    * New top-level functions for accessing session history
      
      ```hs
        -- |Gets the command history for the current session.
        getSessionHistory :: WDSessionState wd => wd [SessionHistory]
        
        -- |Prints a history of API requests to stdout after computing the given action
        --  or after an exception is thrown
        dumpSessionHistory :: WDSessionStateControl wd => wd a -> wd a
      ```

###Implicit waits API (`Test.WebDriver.Commands.Wait`)
* Added functions for checking some expected conditions in explicit waits: `expectNotStale`, `expectAlertOpen`, `catchFailedCommand`

###Typeclass API
* `WDSessionState` is now a superclass of `Monad` and `Applicative` instead of `MonadBaseControl IO`. This makes the class significantly more flexible in how it can be used, as it no longer requires `IO` as a base monad.
  * For convenience the following constraint aliases were added (requires `ConstraintKinds` extension to use). Several existing API functions have been updated to use these new constraints where appropriate.
    
    ```hs
      type WDSessionStateIO s = (WDSessionState s, MonadBase IO s)
      type WDSessionStateControl s = (WDSessionState s, MonadBaseControl IO s)
    ```
    
  * The `WDSessionStateControl` constraint is equivalent to the previous `WDSessionState` constraint.
  * The `WebDriver` class is unaffected (it is now a superclass of `WDSessionStateControl`), so code using the basic `Test.WebDriver` API will not be affected.

* New typeclasses added to `Test.WebDriver.Capabilities`: `GetCapabilities` and `SetCapabilities`; for convenience a constraint alias `HasCapabilities` has been added to work with both of these classes (requires `ConstraintKinds` extension to use)
      
      ```hs
        -- |A typeclass for readable 'Capabilities'
        class GetCapabilities t where
          getCaps :: t -> Capabilities
        
        -- |A typeclass for writable 'Capabilities'
        class SetCapabilities t where
          setCaps :: Capabilities -> t -> t
        
        -- |Read/write 'Capabilities'
        type HasCapabilities t = (GetCapabilities t, SetCapabilities t)
      ```
      
###Minor API changes (not exposed to `Test.WebDriver` module)
* `Test.WebDriver.Session` changes
  * new function `mostRecentHistory` added
  * `lastHTTPRequest` renamed to `mostRecentHTTPRequest` (for consistency)
  * `mkSession` moved from `Test.WebDriver.Config` to `Test.WebDriver.Session`
* `Test.WebDriver.Internal` changes
  * `sendHTTPRequest` now returns `(Either SomeException (Response ByteString))` and catches any exceptions that occur during the request. When using default configuration, the exceptions are also saved in 'wdSessHist'.
  * `Test.WebDriver.Internal` no longer defines and exports exception types. All exception defined here previously have been moved to an unexposed `Test.WebDriver.Exceptions.Internal` module. These types are still exported in the usual places.

###Dependencies
* Now supports http-types up to 0.9

##0.6.3.1
* Fixed an issue with aeson 0.10 support

##0.6.3
* Support aeson 0.10
* Added support for multiple HTTP attempts per command request, using the new WDConfig field wdHTTPRetryCount

##0.6.2.1
* Supports vector 0.11, aeson 0.9, attoparsec 0.13

##0.6.2
* Supports GHC 7.10
* Supports reworked Chrome capabilities used by newer versions of WebDriver
* Servers that return empty JSON strings for commands with no return value will no longer cause parse errors 

##0.6.1
* Added the ability to pass HTTP request headers at session creation
* Fixed an issue involving an obsolete JSON representation of Chrome capabilities
* Relax upper bound on exceptions dependency

##0.6.0.4
* Support for monad-control 1.0

##0.6.0.3
* Relaxed upper bounds on text and http-client versions 

##0.6.0.2
* Added support for aeson > 0.8 and network > 2.6
* Added support for the "X-Response-Body-Start" HTTP header used for error responses in newer http-client versions

##0.6.0.1
* Fixed Haddock parse errors. No code changes introduced in this version.

##0.6
* Rather than WDSession serving dual roles as configuration and state, its functionality has been split into 2 respective types: WDConfig and WDSession.
* runSession now takes a WDConfig instead of WDSession and Capabilities parameters.
* runSession no longer closes its session on successful completion; use finallyClose or closeOnException for this behavior
* The old Test.WebDriver.Classes module has been split into Test.WebDriver.Session and Test.WebDriver.Class
* SessionState typeclass renamed to WDSessionState
* We now use the http-client package instead of HTTP. This is reflected in the addition of Manager fields in both WDConfig and WDSession

##0.5.5
* Added optional HTTP history tracking for debugging purposes.

##0.5.4
* MonadCatchIO is deprecated in favour of exceptions.
* Relaxed dependencies on mtl, network and scientific.

##0.5.3.3
* Relaxed text dependency up to 1.2

##0.5.3.2
###bug fixes
* fixed remaining compilation problems with aeson. now supports aeson >= 0.6.2.0 && < 0.8
* now depends on directory-tree instead of filesystem-trees. this fixes several problems with firefox/chrome profile support.

##0.5.3.1
###bug fixes
* fixed compilation error with aeson 0.7 and greater

##0.5.3

###new features
* SessionNotCreated constructor added to FailedCommandType
* new command deleteCookieByName added

###bug fixes
* asyncJS now properly distinguishes between a null return and a script timeout
* fixed a change in waitWhile causing the opposite of expected behavior

##0.5.2
###API changes
* added many new Internet Explorer capabilities
* added additionalCaps to Capabilities, which allows support for non-standard capabilities
* Browser type now supports non-standard browsers via the new Browser constructor
* added support for the new unexpectedAlertBehaviour capability

###new features
* new command getApplicationCacheStatus supported
* error reporting for unknown commands now slightly improved

###bug fixes
* internal request URIs are now properly percent-encoded
* improved handling of browser-specific capabilities
* fixed incompatability with new session creation protocol changes introduced in selenium 2.35
* updated to work with Aeson 0.6.2 and onward

##hs-webdriver 0.5.1
###API changes
* Test.WebDriver.Internal.FailedCommandInfo now stores screenshots as lazy bytestrings
* Test.WebDriver.Common.Profile now stores PreparedProfile as a lazy bytestring
* Test.WebDriver.Chrome.Extension now stores ChromeExtension as a lazy bytestring
* The LogPref type has been renamed to LogLevel to reflect its use within the new log interface

###new features
* a new log interface as specified by the webdriver standard. This includes the functions getLogs and getLogTypes, and the types LogType and LogEntry. 
* waitWhile and waitUntil now show more detailed information about why an explicit wait timed out.

##hs-webdriver 0.5.0.1
###bug fixes
* hs-webdriver now correctly handles a wider variety of server-specific responses when a webdriver command expects no return value.
* An issue with the redirect status codes used during session creation has been fixed.
* As a result of the above fixes, hs-webdriver should now work with chromedriver. Note that, prior to this version, you can still use chromedriver if you use the selenium standalone server jar as a proxy.


##hs-webdriver 0.5
###API changes
* Test.WebDriver.Commands.Wait.unexpected now accepts a String argument, which is used as an error message
* screenshot and uploadZipEntry from Test.WebDriver.Commands now use lazy bytestrings
* wdBasePath field added to WDSession. This allows you to specify a custom base path for all WebDriver requests. The default, as specified in the WebDriver standard, is "/wd/hub"

###new features
* added Test.WebDriver.Commands.screenshotBase64

##hs-webdriver 0.4

###API changes
* finallyClose and closeOnException are now overloaded on the WebDriver class.
* NoSessionId and doSessCommand were moved from Test.WebDriver.Classes to Test.WebDriver.Commands.Internal

###bug fixes
* fixed a typo in the export list of Firefox.Profile; deleteFile is now correctly exported instead of removeFile from System.Directory 
* fixed an error in the JSON representation of MouseButton

###new features
* A new module, Test.WebDriver.Commands.Internal, which exports some low-level functions used to implement the high-level interface. This makes it possible for library users to extend hs-webdriver with nonstandard or unimplemented features.

## hs-webdriver 0.3.3

###API changes
* The representation of profile files has been changed to use a HashMap instead of an association list. This ensures that destination paths are always unique.

###bug fixes
* The default preferences used by Selenium are now merged into the preferences of Firefox profiles loaded from disk.
* addExtension will now correctly add extension directories to a profile.

###known issues
* Because of the way loadProfile currently adds directories to the profileFiles HashMap, it's possible for extensions added via addExtension to be overriden by the extensions originally listed in the on-disk extensions directory.

###new features 
* It's now possible to add entire directories to a profile in pure code using addFile and addExtension.
* new functions in Common.Profile: unionProfiles, onProfileFiles, onProfilePrefs
* new function in Commands.Wait: onTimeout
* the WD monad now has a MonadCatchIO instance, as an alternative to lifted-base for exception handling


## hs-webdriver 0.3.2.1

###bug fixes
* Removed a bug in waitWhile' that resulted in an infinite loop
* Fixed the incorrect representation of JSON profiles
* Fixed relative path issues when zipping profile directories from disk

## hs-webdriver 0.3.2

###bug fixes
* Changed the constraint on filesystrem-trees to avoid a broken version
* Added the missing exports for addFile and deleteFile in Common.Profile and Firefox.Profile

###new features
* new Common.Profile functions: hasExtension, hasFile

## hs-webdriver 0.3.1 

###API changes
* The representation of Profiles has changed, allowing it to store arbitrary files as well as extensions. The functional API for working with preferences and extensions ismostly unchanged, except for the behavior of calling addExtension consecutively with the same filepath argument.
* The old <&&> and <||> operators in Test.WebDriver.Commands.Wait have been removed and replaced with the ones exported from Control.Conditional from the cond package.

###bug fixes
* Fixed memory leak resulting from an infinite recursion in the FromJSON instance of PreparedProfile.
* loadProfile now properly loads an entire Firefox profile from disk, rather than just the extensions and preferences.

###known issues
* An issue involving lazy bytestring IO in the zip-archive package means that unusually large profiles might exceed the OSes open file limit.

###new features
* several new functions for working with Firefox/Opera profiles have been added. This includes functions for loading large profiles from disk, functions for working with zipped profiles, and functions for adding arbitrary files to a profile in pure code. 
* new helper functions were added to Test.WebDriver.Commands.Wait, exported from the cond package.

## hs-webdriver 0.3.0.1

###bug fixes
* due to a nonconformance in the spec from the Grid server, wire responses were being received that contained no sessionId key, which subsequently resulted in a parse error from our JSON parser. This has been fixed, so that an omitted sessionId defaults to Nothing.
* major bux fixes in the Firefox profile code. Note that loadProfile is unlikely
to work as expected, but prepareTempProfile should.

## hs-webdriver 0.3 

### API changes
* 2 typeclasses were introduced. All WebDriver commands are now overloaded on WebDriver class, so that monad transformers with a WD base can be used conveniently.
* The MonadState instance of WD has been removed and replaced by SessionState.
* The Firefox profile code has been generalized to work with either Opera or Firefox profiles. A phantom type parameter is used to create a distinction between the two. See documentation on Common.Profile and Firefox.Profile to learn about the specific changes that were made.
* FFLogPref is now removed and replaced by the LogPref type, because both Firefox and Opera config use the same logging preference values.
* Several new modules have been created, including: Capabilities, Monad, Classes, Exceptions. Many of the definitions have been moved around, but the export lists of the pre-existing modules are the same.

### bug fixes
* Various issues with the serialization of capabilities meant that Chrome, IE, and Opera weren't able to startup correctly with default capabilities. This is now fixed.

### new features
* General documentation improvements.
* Opera configuration is now implemented.

## hs-webdriver 0.2

### API changes
* FailedCommandInfo changed so that it stores a WDSession rather than just a Maybe SessionId, thus providing server host and port information as well as the session ID.
* As a result, mkFailedCommandInfo is now String -> WD FailedCommandInfo, since it requires access to the WDSession state.
* HTML5StorageType changed to the more accurate WebStorageType

### new features
* general documentation improvements
* the uploadFile, uploadRawFile, and uploadZipEntry functions, which support uploading file contents to the remote server

## hs-webdriver 0.1

### API changes
* getWindowSize, setWindowSize, getWindowPos, and setWindowPos have all been deprived of their WindowHandle argument. This is due to the fact that using unfocused windows with those commands causes undefined behavior. 

### new features
* the mkCookie function for convenient cookie construction
* the focusFrame function for focusing to individual frames
* the setPageLoadTimeout function
* the maximize function for maximizing windows
* support for HTML 5 web storage
