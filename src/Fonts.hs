module Fonts
  (langMatchFamily,
   fontFamilies,
   langFontFamilies)
where

import Data.List.Extra (nubSort, replace, stripInfix)
import qualified Data.Text as T
import GI.Pango.Structs.Language
import Safe
import SimpleCmd

fcElements :: Bool -> [String]
fcElements useStyle =
  if useStyle then ["family","style"] else ["family"]

-- FIXME handle segfaulting
fcList :: Bool -> Bool -> String -> IO [String]
fcList usestyle useface pat = do
  res <- nubSort <$> cmdLines "fc-list" (pat : fcElements usestyle)
--  mapM_ putStrLn res
  return $
    map (familyOrFace usestyle useface) res

langFontFamilies :: Bool -> Bool -> Maybe String -> Language -> IO [String]
langFontFamilies usestyle useface mstyle language = do
  lang <- languageToString language
--  T.putStrLn lang
  fcList usestyle useface $ ":lang=" ++ T.unpack lang ++ maybeStyle mstyle

fontFamilies :: Bool -> Bool -> Maybe String -> IO [String]
fontFamilies usestyle useface mstyle =
  fcList usestyle useface $ ":" ++ maybeStyle mstyle

fcMatch :: Bool -> Bool -> String -> IO String
fcMatch usestyle useface pat = do
  res <- cmd "fc-match" $ pat : fcElements usestyle
  return $ familyOrFace usestyle useface res

familyOrFace :: Bool -> Bool -> String -> String
familyOrFace usestyle useface font =
  let (f,style,facestyle) =
        if usestyle
        then
          case stripInfix ":style=" font of
            Nothing -> (font,"","")
            Just (f1, st) ->
              case stripInfix "," st of
                Nothing -> (f1,st,"")
                Just (s1,s2) -> (f1,s1,s2)
        else
          case stripInfix "," font of
            Nothing -> (font,"","")
            Just (f1,_f2) -> (f1,"","")
  in
    filter (/= '\\') $
    case stripInfix "," f of
      Nothing -> f +-+ style
      Just (fam,fac) ->
        if useface
        then if headMay (words fam) == Just "Droid"
             then fam +-+ style
             else fac +-+ facestyle
        else fam +-+ style

langMatchFamily :: Bool -> Bool -> String -> Maybe String -> Language -> IO String
langMatchFamily usestyle useface f mstyle language = do
  lang <- languageToString language
  res <- fcMatch usestyle useface $ f ++ ":lang=" ++ T.unpack lang ++ maybeStyle mstyle
  return $ replaceStyle res

maybeStyle :: Maybe String -> String
maybeStyle Nothing = ""
maybeStyle (Just style) = ':' : style

replaceStyle :: String -> String
replaceStyle = replace ":style=" " "
