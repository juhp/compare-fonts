{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import Control.Monad ( void )
import Data.Char (isDigit)
import Data.List.Extra (delete, sort, trim)
import Data.Maybe (fromMaybe)
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Vector (Vector)
import GI.Gtk (Align(..), Box(..), FontButton(..), Label(..), Window(..),
               Orientation(OrientationVertical), fontChooserGetFont)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import GI.Pango.Structs.Language
import SimpleCmd ((+-+), error')
import SimpleCmdArgs
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

import Fonts
import Paths_compare_fonts (version)

data State = State {font1 :: Text,
                    font2 :: Text}

data Event = Font1Changed Text
           | Font2Changed Text
           | Closed

view' :: Text -> Maybe Int -> Maybe Int -> Int -> Maybe Bool -> Bool -> State
      -> AppView Window Event
view' sample mwidth mheight margin mwrap showsize (State {..}) =
  bin
  Window winSizeProps
  $ container
  Box [#orientation := OrientationVertical]
  [
    container
    Box [#margin := fromIntegral margin ]
    [BoxChild
     { properties = defaultBoxChildProperties,
       child = widget FontButton $ fontProps Font1Changed font1
     }
    ],
    BoxChild
    { properties = defaultBoxChildProperties
    , child = widget Label $ textProps font1
    },
    BoxChild
    { properties = defaultBoxChildProperties
    , child = widget Label $ textProps font2
    },
    container
    Box [#margin := fromIntegral margin ]
    [BoxChild
     { properties = defaultBoxChildProperties
     , child = widget FontButton $ fontProps Font2Changed font2
     }
    ]
  ]
  where
    wrap =
      case mwrap of
        Just w -> w
        Nothing -> T.length sample > 100

    textProps :: Text -> Vector (Attribute Label Event)
    textProps font =
      [ #label := ("<span font=\""<>font<>"\">" <> sample <> "</span>"),
        #useMarkup := True, #hexpand := True, #vexpand := True, #wrap := wrap,
        #margin := fromIntegral margin, #halign := AlignStart]

    fontProps :: (Text -> Event) -> Text -> Vector (Attribute FontButton Event)
    fontProps ev font =
      [ onM #fontSet (fmap (ev . fromMaybe "No default") . fontChooserGetFont),
        #fontName := font,
        #showSize := showsize]

    winSizeProps :: Vector (Attribute Window Event)
    winSizeProps =
      [ #title := "Compare fonts"
      , on #deleteEvent (const (True, Closed))
        -- affects wrapping
      , #widthRequest := (case mwidth of
                            Just width -> fromIntegral width
                            Nothing ->
                              let len = T.length sample
                              in if len < 100
                              then fromIntegral (10 * len)
                              else 100)
      , #heightRequest := maybe 100 fromIntegral mheight
      ]

update' :: State -> Event -> Transition State Event
update' (State _ f2) (Font1Changed fnt) = Transition (State fnt f2) (return Nothing)
update' (State f1 _) (Font2Changed fnt) = Transition (State f1 fnt) (return Nothing)
update' _ Closed = Exit

data SampleText = SampleLang String | SampleText String

data FontSelect = FontFamily String | FontSubString String

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  simpleCmdArgs (Just version) "compare-fonts"
    "GUI tool to compare two fonts" $
    prog
    <$> optional
    (SampleText <$> strOptionWith 't' "text" "TEXT" "text to display" <|>
     SampleLang <$> strOptionWith 'l' "lang" "LANG" "sample text by language" )
    <*> optional (optionWith auto 'W' "width" "WIDTH" "Window width")
    <*> optional (optionWith auto 'H' "height" "HEIGHT" "Window height")
    <*> optionalWith auto 'm' "margin" "MARGIN" "Margin size [default 10]" 10
    <*> optional (fontSelector '1' "1st")
    <*> optional (fontSelector '2' "2nd")
    <*> switchWith 'f' "use-face" "Use face results rather than families"
    <*> optionalWith auto 's' "font-size" "SIZE" "Font size [default 16]" 16
    <*> optional (flagWith' True 'w' "wrap" "Enable text wrapping" <|>
                  flagWith' False 'n' "no-wrap" "Disable text wrapping")
    <*> (not <$> switchLongWith "hide-font-size" "Hide font size in FontButtons")
  where
    fontSelector :: Char -> String -> Parser FontSelect
    fontSelector s num =
      FontFamily <$> strOptionWith s ("font" ++ [s]) "FAMILY" (num +-+ "font family") <|>
      FontSubString <$> strOptionLongWith ("match" ++ [s]) "WORDS" ("Match" +-+ num +-+ "font words")

    prog :: Maybe SampleText -> Maybe Int -> Maybe Int -> Int
         -> Maybe FontSelect -> Maybe FontSelect -> Bool -> Int -> Maybe Bool
         -> Bool -> IO ()
    prog msample mwidth mheight margin mfontsel1 mfontsel2 faces size mwrap showsize = do
      mlang <-
        case msample of
          Just (SampleLang la) ->
            -- FIXME check fc orth exists first
            languageFromString (Just (T.pack la)) -- always succeeds
          _ -> return Nothing
      f1 <- selectFont mlang Nothing faces mfontsel1
      f2 <- selectFont mlang (Just f1) faces mfontsel2
      print (f1,f2)
      sample <-
        case msample of
          Just (SampleText txt) -> return $ T.pack txt
          _ -> maybe languageGetDefault return mlang
               >>= languageGetSampleString
      let samplelen = T.length sample
      putStrLn $ show samplelen +-+ "chars"
      void $ run App { view = view' sample mwidth mheight margin mwrap showsize
                     , update = update'
                     , inputs = []
                     , initialState =
                         State
                         (T.pack (trim f1 +-+ show size))
                         (T.pack (trim f2 +-+ show size))
                     }

-- FIX param for Sans/Serif/Mono style
selectFont :: Maybe Language -> Maybe String -> Bool -> Maybe FontSelect -> IO String
selectFont mlang mdefault face Nothing =
    case mlang of
      Nothing -> return "Sans"
      Just lang -> do
        case mdefault of
          Nothing -> langMatchFamily face "Sans" lang
          Just def -> do
            fams <- langFontFamilies face lang
            case delete def fams of
              [] -> return def
              [other] -> return other
              fs -> chooseFont fs
selectFont mlang _ face (Just (FontFamily fam)) =
  if null fam
  then error' "family name must not be empty"
  else
    case mlang of
      Nothing -> return fam
      Just lang -> langMatchFamily face fam lang
selectFont mlang _ face (Just (FontSubString subs)) =
  if null (trim subs)
  then error' "substring must not be empty"
  else do
    fams <-
      case mlang of
        Nothing -> fontFamilies face
        Just lang -> langFontFamilies face lang
    case filter (allWords (words subs) . words) fams of
      [] -> do
        lang <- maybe (return "") (fmap T.unpack . languageToString) mlang
        error' $ "no" +-+ lang +-+ "match for" +-+ show subs ++ "\nAvailable families:\n" ++ unlines fams
      [f] -> return f
      fs -> chooseFont fs
  where
    allWords :: [String] -> [String] -> Bool
    allWords [] _ = True
    allWords (s:ss) fs = s `elem` fs && allWords ss fs

chooseFont :: [String] -> IO String
chooseFont fs = do
  mapM_ (\(n,f) -> putStrLn (show n ++ "." +-+ f)) zs
  prompt
  where
    zs = zip [1..length fs] $ sort fs

    prompt = do
      putStr $ "Select font: "
      i <- getLine
      if all isDigit i
        then
        let n = read i
        in
          case lookup n zs of
            Nothing -> prompt
            Just f -> return f
        else prompt
