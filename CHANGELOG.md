#Change Log

## version 0.1

### API changes
* getWindowSize, setWindowSize, getWindowPos, and setWindowPos have all been deprived of their WindowHandle argument. This is due to the fact that using unfocused windows with those commands causes undefined behavior. 

### new features
* the focus function which changes focus of windows and frames 
* the mkCookie function for convenient cookie construction
* the setPageLoadTimeout function
* the maximize function
* support for HTML 5 web storage
