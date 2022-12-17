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
import GI.Gtk
  ( Align(..), Box(..), FontButton(..), Label(..), Window(..),
    Orientation(OrientationVertical), fontChooserGetFont,

  )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import GI.Pango.Structs.Language
import SimpleCmdArgs

data State = State {font1 :: Text,
                    font2 :: Text}

data Event = Font1Changed Text
           | Font2Changed Text
           | Closed

view' :: Text -> Maybe Int -> Maybe Int -> Int -> Bool -> Bool -> State
      -> AppView Window Event
view' sample mwidth mheight margin wrap showsize (State {..}) =
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
    textProps :: Text -> Vector (Attribute Label Event)
    textProps font =
      [ #label := ("<span font=\""<>font<>"\">" <> sample <> "</span>"),
        #useMarkup := True, #hexpand := True, #vexpand := True, #wrap := wrap,
        #margin := fromIntegral margin, #halign := AlignStart]

    fontProps :: (Text -> Event) -> Text -> Vector (Attribute FontButton Event)
    fontProps ev font =
      [ onM #fontSet (fmap (ev . fromMaybe "No default") . fontChooserGetFont),
        #fontName := font, #showSize := showsize]

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

main :: IO ()
main =
  simpleCmdArgs Nothing "compare-fonts"
  "GUI tool to compare two fonts" $
  prog
  <$> optional
  (SampleText <$> strOptionWith 't' "text" "TEXT" "text to display" <|>
   SampleLang <$> strOptionWith 'l' "lang" "LANG" "sample text by language" )
  <*> optional (optionWith auto 'W' "width" "WIDTH" "Window width")
  <*> optional (optionWith auto 'H' "height" "HEIGHT" "Window height")
  <*> optionalWith auto 'm' "margin" "MARGIN" "Margin size [default 10]" 10
  <*> strOptionalWith '1' "font1" "FONT" "First font [default Sans]" "Sans"
  <*> strOptionalWith '2' "font2" "FONT" "Second font [default Serif]" "Serif"
  <*> optionalWith auto 'f' "font-size" "SIZE" "Font size [default 16]" 16
  <*> switchWith 'w' "wrap" "Enable text wrapping"
  <*> (not <$> switchLongWith "hide-font-size" "Hide font size in FontButtons")
  where
    prog :: Maybe SampleText -> Maybe Int -> Maybe Int -> Int
         -> String -> String -> Int -> Bool -> Bool -> IO ()
    prog msample mwidth mheight margin font1 font2 size wrap showsize = do
      sample <-
        case msample of
          Just (SampleText txt) -> return $ T.pack txt
          _ ->
            case msample of
              Just (SampleLang la) ->
                -- FIXME check fc orth exists first
                languageFromString (Just (T.pack la)) -- always succeeds
                >>= maybe languageGetDefault return
              _ -> languageGetDefault
            >>= languageGetSampleString
      putStrLn $ show (T.length sample) ++ " chars"
      void $ run App { view = view' sample mwidth mheight margin wrap showsize
                     , update = update'
                     , inputs = []
                     , initialState =
                         State
                         (T.pack (trim font1 ++ " " ++ show size))
                         (T.pack (trim font2 ++ " " ++ show size))
                     }
