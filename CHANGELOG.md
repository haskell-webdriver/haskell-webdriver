#Change Log

## version 0.0.2

### API changes
* getWindowSize, setWindowSize, getWindowPos, and setWindowPos have all been deprived of their WindowHandle argument. This is due to the fact that using unfocused windows with those commands causes undefined behavior. 

### removals
* focusWindow was removed as the new focus function is a generalization of it 

### new features
* the focus function which changes focus of windows and frames; 
* the mkCookie function for convenient cookie construction
* the setPageLoadTimeout function
* support for HTML 5 web storage