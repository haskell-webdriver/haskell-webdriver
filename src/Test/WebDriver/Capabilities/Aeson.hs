
module Test.WebDriver.Capabilities.Aeson where

import Data.Aeson as A
import Data.Char
import Data.Function
import qualified Data.List as L


baseOptions :: A.Options
baseOptions = A.defaultOptions

toCamel1, toCamel2 :: A.Options
toCamel1 = baseOptions { A.fieldLabelModifier = snakeToCamelCase . toSnakeAndDropN 1 . dropLeadingUnderscore }
toCamel2 = baseOptions { A.fieldLabelModifier = snakeToCamelCase . toSnakeAndDropN 2 . dropLeadingUnderscore }

toCamelC1, toCamelC2 :: A.Options
toCamelC1 = baseOptions { A.constructorTagModifier = snakeToCamelCase . toSnakeAndDropN 1 }
toCamelC2 = baseOptions { A.constructorTagModifier = snakeToCamelCase . toSnakeAndDropN 2 }

-- | For 'UserPromptHandler', which maps things like
-- UserPromptHandlerAcceptAndNotify -> "accept and notify"
toSpacedC3 :: A.Options
toSpacedC3 = baseOptions { A.constructorTagModifier = snakeToSpaced . toSnakeAndDropN 3 }

capabilitiesOptions :: A.Options
capabilitiesOptions = A.defaultOptions { A.fieldLabelModifier = specialCases . snakeToCamelCase . toSnakeAndDropN 1 . dropLeadingUnderscore }
  where
    specialCases "googChromeOptions" = "goog:chromeOptions"
    specialCases "mozFirefoxOptions" = "moz:firefoxOptions"
    specialCases x = x


-- * Util

toSnakeAndDropN :: Int -> String -> String
toSnakeAndDropN n s = L.intercalate "_" snakeParts
  where
    snakeParts :: [String]
    snakeParts = s
                 & dropLeadingUnderscore
                 & splitR isUpper
                 & L.drop n
                 & fmap (fmap toLower)

splitR :: (Char -> Bool) -> String -> [String]
splitR _ [] = []
splitR p s =
  let
    go :: Char -> String -> [String]
    go m s' = case L.break p s' of
      (b', [])     -> [ m:b' ]
      (b', x:xs) -> ( m:b' ) : go x xs
  in case L.break p s of
    (b,  [])    -> [ b ]
    ([], h:t) -> go h t
    (b,  h:t) -> b : go h t

snakeToCamelCase :: String -> String
snakeToCamelCase s = case parts of
  (x:xs) -> x <> concatMap capitalize xs
  [] -> ""
  where
    parts = case splitR (== '_') s of
      (x:xs) -> x : (fmap (L.drop 1) xs)
      [] -> []

snakeToSpaced :: String -> String
snakeToSpaced s = L.intercalate " " parts
  where
    parts = case splitR (== '_') s of
      (x:xs) -> x : (fmap (L.drop 1) xs)
      [] -> []

dropLeadingUnderscore :: [Char] -> [Char]
dropLeadingUnderscore ('_':xs) = xs
dropLeadingUnderscore xs = xs

capitalize :: String -> String
capitalize (x:xs) = toUpper x : (fmap toLower xs)
capitalize x = x
