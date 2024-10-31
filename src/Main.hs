{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import Control.Monad ( void )
import Data.List.Extra (trim)
import Data.Maybe (fromMaybe)
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Vector (Vector)
import GI.Gtk (Align(..), Box(..), FontButton(..), Label(..), Window(..),
               Orientation(OrientationVertical), fontChooserGetFont,
               FontChooserLevel(..)
              )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import GI.Pango.Structs.Language
import SimpleCmd ((+-+))
import SimpleCmdArgs
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

import InitialFont
import Paths_compare_fonts (version)

data State = State {font1 :: Text,
                    font2 :: Text}

data Event = Font1Changed Text
           | Font2Changed Text
           | Closed

view' :: Text -> Maybe Int -> Maybe Int -> Int -> Maybe Bool -> Bool -> Bool
      -> State -> AppView Window Event
view' sample mwidth mheight margin mwrap showsize usestyle (State {..}) =
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
      [ onM #fontSet (fmap (ev . fromMaybe "No default") . fontChooserGetFont)
      , #fontName := font
      , #showSize := showsize
      , #level := [if usestyle then FontChooserLevelStyle else FontChooserLevelFamily]
      , #showStyle := usestyle
      ]

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

defaultSize :: Int
defaultSize = 24

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
    <*> optional (fontStyle '1' "1st")
    <*> optional (fontSelector '2' "2nd")
    <*> optional (fontStyle '2' "2nd")
    <*> switchWith 'S' "use-style" "List font styles"
    <*> switchWith 'f' "use-face" "Use face results rather than families"
    <*> optionalWith auto 's' "font-size" "SIZE" ("Font size [default" +-+ show defaultSize ++ "]") defaultSize
    <*> optional (flagWith' True 'w' "wrap" "Enable text wrapping" <|>
                  flagWith' False 'n' "no-wrap" "Disable text wrapping")
    <*> (not <$> switchLongWith "hide-font-size" "Hide font size in FontButtons")
  where
    fontSelector :: Char -> String -> Parser FontSelect
    fontSelector s num =
      FontFamily <$> strOptionWith s ("font" ++ [s]) "FAMILY" (num +-+ "font family") <|>
      FontSubString <$> strOptionLongWith ("match" ++ [s]) "WORDS" ("Match" +-+ num +-+ "font words")

    fontStyle :: Char -> String -> Parser String
    fontStyle s num =
      strOptionLongWith ("style" ++ [s]) "STYLE" (num +-+ "font style")

    prog :: Maybe SampleText -> Maybe Int -> Maybe Int -> Int
         -> Maybe FontSelect -> Maybe String
         -> Maybe FontSelect -> Maybe String
         -> Bool -> Bool -> Int -> Maybe Bool
         -> Bool -> IO ()
    prog msample mwidth mheight margin mfontsel1 mstyle1 mfontsel2 mstyle2 usestyle faces size mwrap showsize = do
      mlang <-
        case msample of
          Just (SampleLang la) ->
            -- FIXME check fc orth exists first
            languageFromString (Just (T.pack la)) -- always succeeds
          _ -> return Nothing
      putStr $ "First font: "
      f1 <- selectFont mlang Nothing usestyle faces mfontsel1 mstyle1
      putStrLn $ f1 ++ "\n"
      putStrLn $ "Second font: "
      f2 <- selectFont mlang (Just f1) usestyle faces mfontsel2 mstyle2
      putStrLn f2
      sample <-
        case msample of
          Just (SampleText txt) -> return $ T.pack txt
          _ -> languageGetSampleString mlang
      let samplelen = T.length sample
      putStrLn $ show samplelen +-+ "chars"
      void $ run App { view = view' sample mwidth mheight margin mwrap showsize usestyle
                     , update = update'
                     , inputs = []
                     , initialState =
                         State
                         (T.pack (trim f1 +-+ show size))
                         (T.pack (trim f2 +-+ show size))
                     }
