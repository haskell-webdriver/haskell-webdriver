#Change Log

## hs-webdriver 0.2.1

### API changes
* 2 typeclasses were introduced. All WebDriver commands are now overloaded on WebDriver class, so that monad transformers with a WD base can be used conveniently.
* The MonadState instance of WD has been removed and replaced by SessionState.

### new features
* Opera configuration is now implemented.
* Several new modules have been created, including: Capabilities, Monad, Classes, Exceptions. Many of the definitions have been moved around, but the export lists of the pre-existing modules are the same.

### Bug fix
* An issue with the serialization of browser names meant that Chrome and IE weren't working correctly. This is now fixed.
* FFLogPref (which was never properly exported due to carelessness) is now removed and replaced by the LogPref type, because both Firefox and Opera config use the same logging preference values. Since only FFLogPref's type and not its constructors were ever properly exported, this isn't considered worthy of a major version bump.

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
