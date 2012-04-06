{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception.Lifted
import Test.WebDriver
import Prelude hiding (catch)

--convenience function to print output
p = (liftIO . print =<<)

main = (print =<<) . runSession defaultSession defaultCaps $ do
  p serverStatus
  p sessions
  p getCaps
  setImplicitWait 5000
  setScriptTimeout 5000
  openPage "http://google.com"
  p getCurrentURL
  screenshot
  openPage "http://yahoo.com"
  (void . findElem . ById $ "Not an Id")
    `catch` \err -> liftIO . print $ (err :: SomeException)
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
  
  e1 <- findElem $ ByTag "input"
  e2 <- findElem $ ByTag "input"
  p $ e1 <==> e2
  p $ e1 </=> e2
  p $ isSelected e1
  p $ isDisplayed e1
  p $ e1 `attr` "id"
  p $ e1 `attr` "incorrect"
  p $ e1 `cssAttr` "incorrect"
  
  p $ findElems $ ByTag "div"
  e3 <- findElem $ ById "hplogo"
  click e3
  p $ elemInfo =<< activeElem
  
  p getCurrentWindow
  p $ getWindowSize currentWindow
  p $ getWindowPos currentWindow
  setWindowSize currentWindow (20,20)