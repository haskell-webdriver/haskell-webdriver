#Change Log

## upcoming release

###bug fixes
*due to a nonconformance in the spec from the Grid server, wire responses were being received that contained no sessionId key, which subsequently resulted in a parse error from our JSON parser. This has been fixed, so that an omitted sessionId defaults to Nothing.

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
