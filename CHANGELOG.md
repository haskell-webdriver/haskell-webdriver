#Change Log

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
