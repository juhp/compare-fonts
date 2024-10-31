module Fonts
  (langMatchFamily,
   fontFamilies,
   langFontFamilies)
where

import Data.List.Extra (nubSort, replace, stripInfix)
import qualified Data.Text as T
--import qualified Data.Text.IO as T
import GI.Pango.Structs.Language
import SimpleCmd

fcElements :: [String]
fcElements = ["family","style"]

-- FIXME handle segfaulting
fcList :: Bool -> String -> IO [String]
fcList useface pat = do
  res <- nubSort <$> cmdLines "fc-list" (pat : fcElements)
--  mapM_ putStrLn res
  return $
    map (familyOrFace useface) res

langFontFamilies :: Bool -> Maybe String -> Language -> IO [String]
langFontFamilies useface mstyle language = do
  lang <- languageToString language
--  T.putStrLn lang
  fcList useface $ ":lang=" ++ T.unpack lang ++ maybeStyle mstyle

fontFamilies :: Bool -> Maybe String -> IO [String]
fontFamilies useface mstyle =
  fcList useface $ ":" ++ maybeStyle mstyle

fcMatch :: Bool -> String -> IO String
fcMatch useface pat = do
  res <- cmd "fc-match" $ pat : fcElements
  return $ familyOrFace useface res

familyOrFace :: Bool -> String -> String
familyOrFace useface font =
  let (f,style,facestyle) =
        case stripInfix ":style=" font of
          Nothing -> (font,"","")
          Just (f, st) ->
            case stripInfix "," st of
              Nothing -> (f,st,"")
              Just (s1,s2) -> (f,s1,s2)
  in
    case stripInfix "," f of
      Nothing -> f +-+ style
      Just (fam,fac) ->
        if useface
        then if head (words fam) == "Droid"
             then fam +-+ style
             else fac +-+ facestyle
        else fam +-+ style

langMatchFamily :: Bool -> String -> Maybe String -> Language -> IO String
langMatchFamily useface f mstyle language = do
  lang <- languageToString language
--  T.putStrLn lang
  res <- fcMatch useface $ f ++ ":lang=" ++ T.unpack lang ++ maybeStyle mstyle
  return $ replaceStyle res

maybeStyle :: Maybe String -> String
maybeStyle Nothing = ""
maybeStyle (Just style) = ':' : style

replaceStyle :: String -> String
replaceStyle = replace ":style=" " "
