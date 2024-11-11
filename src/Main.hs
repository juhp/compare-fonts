{-# LANGUAGE CPP  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import Control.Monad ( void )
import Data.Char (isDigit)
import Data.List.Extra (trim)
import Data.Maybe (fromMaybe)
#if MIN_VERSION_extra(1,7,13)
import Data.Monoid.Extra (mwhen)
#endif
import Data.Text ( Text )
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Vector (Vector)
import GHC.Int(Int32)
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

view' :: Text -> Int -> Maybe Int -> Maybe Int -> Int -> Maybe Bool -> Bool
      -> Bool -> Bool -> State -> AppView Window Event
view' sample size mwidth mheight margin mwrap showsize usestyle nofallback (State {..}) =
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
        Nothing -> maxLength sample > 100

    textProps :: Text -> Vector (Attribute Label Event)
    textProps font =
      [ #label := ("<span font=\""<>font<>"\"" <> mwhen nofallback " fallback=\"false\"" <> ">" <> sample <> "</span>"),
        #useMarkup := True, #hexpand := True, #vexpand := True, #wrap := wrap,
        #margin := fromIntegral margin, #halign := AlignStart]

    fontProps :: (Text -> Event) -> Text -> Vector (Attribute FontButton Event)
    fontProps ev font =
      [ onM #fontSet (fmap (ev . fromMaybe "No default") . fontChooserGetFont)
      , #fontName := font
      , #level := ((if usestyle then FontChooserLevelStyle else FontChooserLevelFamily) : [FontChooserLevelSize | showsize])
      , #showStyle := usestyle
      , #previewText := sample
      ]

    winSizeProps :: Vector (Attribute Window Event)
    winSizeProps =
      [ #title := "Compare fonts"
      , on #deleteEvent (const (True, Closed))
        -- affects wrapping
      , #widthRequest := (case mwidth of
                            Just width -> fromIntegral width
                            Nothing -> neededWidth size sample)
      , #heightRequest := maybe 100 fromIntegral mheight
      ]

neededWidth :: Int -> Text -> Int32
neededWidth size txt =
  fromIntegral $
  size *
  let len = maxLength txt
  in if len < 100
     then len
     else 100

maxLength :: Text -> Int
maxLength = maximum . (0 :) . map T.length . T.lines

update' :: State -> Event -> Transition State Event
update' (State _ f2) (Font1Changed fnt) = Transition (State fnt (adjustSize fnt f2)) (return Nothing)
update' (State f1 _) (Font2Changed fnt) = Transition (State (adjustSize fnt f1) fnt) (return Nothing)
update' _ Closed = Exit

adjustSize :: Text -> Text -> Text
adjustSize other fnt =
  let osize = sizeOf other
      fsize = sizeOf fnt
  in if osize == fsize
     then fnt
     else T.replace fsize osize fnt
  where
    sizeOf :: Text -> Text
    sizeOf = T.takeWhileEnd (\c -> isDigit c || c == '.')

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
    <*> switchLongWith "no-fallback" "Disable pango font fallback"
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
         -> Bool -> Bool -> IO ()
    prog msample mwidth mheight margin mfontsel1 mstyle1 mfontsel2 mstyle2 usestyle useface size mwrap showsize nofallback = do
      mlang <-
        case msample of
          Just (SampleLang la) ->
            -- FIXME check fc orth exists first
            languageFromString (Just (T.pack la)) -- always succeeds
          _ -> return Nothing
      putStr "First font: "
      f1 <- selectFont mlang Nothing usestyle useface mfontsel1 mstyle1
      putStrLn $ f1 ++ "\n"
      putStrLn "Second font: "
      f2 <- selectFont mlang (Just f1) usestyle useface mfontsel2 mstyle2
      putStrLn f2
      sample <-
        case msample of
          Just (SampleText txt) -> return $ T.pack txt
          _ -> languageGetSampleString mlang
      let samplelen = T.length sample
      putStrLn $ show samplelen +-+ "chars"
      mapM_ T.putStrLn $ T.lines sample
      -- print $ neededWidth size sample
      void $ run App { view = view' sample size mwidth mheight margin mwrap showsize usestyle nofallback
                     , update = update'
                     , inputs = []
                     , initialState =
                         State
                         (T.pack (trim f1 +-+ show size))
                         (T.pack (trim f2 +-+ show size))
                     }

#if !MIN_VERSION_extra(1,7,13)
mwhen :: Monoid a => Bool -> a -> a
mwhen b x = if b then x else mempty
#endif
