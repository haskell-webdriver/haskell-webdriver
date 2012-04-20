#Change Log

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
