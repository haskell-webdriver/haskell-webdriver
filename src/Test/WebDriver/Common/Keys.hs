{-# LANGUAGE OverloadedStrings #-}
-- |This module contains named constants corresponding to the special characters recognized by 'sendKeys'.
-- For more details on these special characters, consult the Selenium documentation at 
-- <https://github.com/SeleniumHQ/selenium/wiki/JsonWireProtocol#sessionsessionidelementidvalue>
module Test.WebDriver.Common.Keys where

import Data.Text (Text)

add :: Text
add = "\xe025"

alt :: Text
alt = "\xe00a"

arrowDown :: Text
arrowDown = "\xe015"

arrowLeft :: Text
arrowLeft = "\xe012"

arrowRight :: Text
arrowRight = "\xe014"

arrowUp :: Text
arrowUp = "\xe013"

backspace :: Text
backspace = "\xe003"

backSpace :: Text
backSpace = "\xe003"

cancel :: Text
cancel = "\xe001"

clear :: Text
clear = "\xe005"

command :: Text
command = "\xe03d"

control :: Text
control = "\xe009"

decimal :: Text
decimal = "\xe028"

delete :: Text
delete = "\xe017"

divide :: Text
divide = "\xe029"

down :: Text
down = "\xe015"

end :: Text
end = "\xe010"

enter :: Text
enter = "\xe007"

equals :: Text
equals = "\xe019"

escape :: Text
escape = "\xe00c"

f1 :: Text
f1 = "\xe031"

f2 :: Text
f2 = "\xe032"

f3 :: Text
f3 = "\xe033"

f4 :: Text
f4 = "\xe034"

f5 :: Text
f5 = "\xe035"

f6 :: Text
f6 = "\xe036"

f7 :: Text
f7 = "\xe037"

f8 :: Text
f8 = "\xe038"

f9 :: Text
f9 = "\xe039"

f10 :: Text
f10 = "\xe03a"

f11 :: Text
f11 = "\xe03b"

f12 :: Text
f12 = "\xe03c"

help :: Text
help = "\xe002"

home :: Text
home = "\xe011"

insert :: Text
insert = "\xe016"

left :: Text
left = "\xe012"

leftAlt :: Text
leftAlt = "\xe00a"

leftControl :: Text
leftControl = "\xe009"

leftShift :: Text
leftShift = "\xe008"

meta :: Text
meta = "\xe03d"

multiply :: Text
multiply = "\xe024"

null :: Text
null = "\xe000"

numpad0 :: Text
numpad0 = "\xe01a"

numpad1 :: Text
numpad1 = "\xe01b"

numpad2 :: Text
numpad2 = "\xe01c"

numpad3 :: Text
numpad3 = "\xe01d"

numpad4 :: Text
numpad4 = "\xe01e"

numpad5 :: Text
numpad5 = "\xe01f"

numpad6 :: Text
numpad6 = "\xe020"

numpad7 :: Text
numpad7 = "\xe021"

numpad8 :: Text
numpad8 = "\xe022"

numpad9 :: Text
numpad9 = "\xe023"

pageDown :: Text
pageDown = "\xe00f"

pageUp :: Text
pageUp = "\xe00e"

pause :: Text
pause = "\xe00b"

return :: Text
return = "\xe006"

right :: Text
right = "\xe014"

semicolon :: Text
semicolon = "\xe018"

separator :: Text
separator = "\xe026"

shift :: Text
shift = "\xe008"

space :: Text
space = "\xe00d"

subtract :: Text
subtract = "\xe027"

tab :: Text
tab = "\xe004"

up :: Text
up = "\xe013"

