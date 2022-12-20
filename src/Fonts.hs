module Fonts
  (langMatchFamily,
   fontFamilies,
   langFontFamilies)
where

import Data.List.Extra (nubOrd, stripInfix)
import qualified Data.Text as T
--import qualified Data.Text.IO as T
import GI.Pango.Structs.Language
import SimpleCmd

-- FIXME handle segfaulting
fcList :: Bool -> [String] -> IO [String]
fcList faces query = do
  res <- cmdLines "fc-list" query
  return $
    nubOrd $ map (familyOrFace faces) res

langFontFamilies :: Bool -> Language -> IO [String]
langFontFamilies faces language = do
  lang <- languageToString language
--  T.putStrLn lang
  fcList faces [":lang=" ++ T.unpack lang, "family"]

fontFamilies :: Bool -> IO [String]
fontFamilies faces =
  fcList faces [":", "family"]

fcMatch :: Bool -> [String] -> IO String
fcMatch face query = do
  res <- cmd "fc-match" query
  return $ familyOrFace face res

familyOrFace :: Bool -> String -> String
familyOrFace face f =
  case stripInfix "," f of
    Nothing -> f
    Just (fam,fac) ->
      if face
      then if head (words fam) == "Droid"
           then fam
           else fac
      else fam

langMatchFamily :: Bool -> String -> Language -> IO String
langMatchFamily face f language = do
  lang <- languageToString language
--  T.putStrLn lang
  fcMatch face [f ++ ":lang=" ++ T.unpack lang, "family"]
