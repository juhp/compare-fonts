module InitialFont (selectFont, FontSelect (..))
where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.List.Extra (delete, trim)
import qualified Data.Text as T
import GI.Pango.Structs.Language
import SimpleCmd ((+-+), error')
import SimplePrompt (promptNonEmpty)

import Fonts

data FontSelect = FontFamily String | FontSubString String

selectFont :: String -> Maybe Language -> Maybe String -> Bool -> Bool
           -> Maybe FontSelect -> Maybe String -> IO String
selectFont describe mlang mdefault usestyle face Nothing mstyle =
  case mlang of
    Nothing -> return $ "Sans" +-+ fromMaybe "" mstyle
    Just lang -> do
      case mdefault of
        Nothing -> do
          putStr $ describe +-+ "font: "
          res <- langMatchFamily usestyle face "Sans" mstyle lang
          putStrLn res
          return res
        Just def -> do
          fams <- langFontFamilies usestyle face mstyle lang
          putStr $ describe +-+ "font: "
          case delete def fams of
            [] -> do
              putStrLn def
              return def
            [other] -> do
              putStrLn other
              return other
            fs -> do
              putChar '\n'
              chooseFont fs
selectFont _ mlang _ usestyle face (Just (FontFamily fam)) mstyle =
  if null fam
  then error' "family name must not be empty"
  else
    case mlang of
      Nothing -> return fam
      Just lang -> langMatchFamily usestyle face fam mstyle lang
selectFont describe mlang _ usestyle face (Just (FontSubString subs)) mstyle =
  if null (trim subs)
  then error' "substring must not be empty"
  else do
    fams <-
      case mlang of
        Nothing -> fontFamilies usestyle face mstyle
        Just lang -> langFontFamilies usestyle face mstyle lang
    putStr $ describe +-+ "font: "
    case filter (allWords (words subs) . words) fams of
      [] -> do
        lang <- maybe (return "") (fmap T.unpack . languageToString) mlang
        error' $ "no" +-+ lang +-+ "match for" +-+ show subs ++ "\nAvailable families:\n" ++ unlines fams
      [f] -> do
        putStrLn f
        return f
      fs -> do
        putChar '\n'
        chooseFont fs
  where
    allWords :: [String] -> [String] -> Bool
    allWords [] _ = True
    allWords (s:ss) fs = s `elem` fs && allWords ss fs

chooseFont :: [String] -> IO String
chooseFont fs = do
  mapM_ (\(n,f) -> putStrLn (show n ++ "." +-+ f)) zs
  prompt
  where
    zs = zip [1..length fs] fs

    prompt = do
      i <- promptNonEmpty "Select font"
      if all isDigit i
        then maybe prompt return (lookup (read i) zs)
        else prompt
