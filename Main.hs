{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
import Control.Monad.IO.Class
import Control.Monad.Error
import Test.WebDriver


--convenience function to print output
p = (liftIO . print =<<)

main = (print =<<) . runSession defaultSession defaultCaps $ do
  waitUntil 5 (return False)
  {-
  p serverStatus
  p sessions
  p getCaps
  setImplicitWait 5000
  setScriptTimeout 5000
  openPage "http://google.com"
  p currentURL
  screenshot
  openPage "http://yahoo.com"
  void (findElem ById "Not an Id") 
    `catchError` \err -> liftIO (print err)
  back
  forward
  back
  refresh
  --liftIO . print =<< availableIMEEngines
  --liftIO . print =<< activeIMEEngine
  --liftIO . print =<< checkIMEActive
  
  (c:_) <- cookies
  liftIO . print $ c
  deleteCookie c
  p cookies
  deleteVisibleCookies
  p cookies
  
  getSource
  p getTitle
  
  p $ executeJS [] "return document.title"
  
  e1 <- findElem ByTagName "input"
  e2 <- findElem ByTagName "input"
  p $ e1 <==> e2
  p $ e1 </=> e2
  p $ isSelected e1
  p $ isDisplayed e1
  p $ e1 `attr` "id"
  p $ e1 `attr` "incorrect"
  p $ e1 `cssAttr` "incorrect"
  
  p $ findElem ByTagName "div"
  e3 <- findElem ByXPath "//[not(ancestor::div[contains(@style,'display:none')])][1]"
  click e3
  p $ elemInfo =<< activeElem
  
  p getCurrentWindow
  p $ getWindowSize currentWindow
  p $ getWindowPos currentWindow
  setWindowSize currentWindow (20,20)
  -}