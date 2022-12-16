{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import Control.Monad ( void )
import Data.Maybe (fromMaybe)
import Data.Text ( Text )
import Data.Vector (Vector)
import GI.Gtk
  ( Box(..), FontButton(..), Label(..), Window(..),
    Orientation(OrientationVertical), fontChooserGetFont
  )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import GI.Pango.Structs.Language

data State = State {font1 :: Text,
                    font2 :: Text}

data Event = Font1Changed Text
           | Font2Changed Text
           | Closed

view' :: Text -> State -> AppView Window Event
view' sample (State {..}) =
  bin
  Window
  [ #title := "Compare fonts"
  , on #deleteEvent (const (True, Closed))
  -- , #widthRequest := 400
  -- , #heightRequest := 300
  ]
  $ container
  Box [#orientation := OrientationVertical]
  [
    BoxChild
    { properties = defaultBoxChildProperties
    , child = widget FontButton $ fontProps Font1Changed font1
    },
    BoxChild
    { properties = defaultBoxChildProperties
    , child = widget Label $ textProps font1
    },
    BoxChild
    { properties = defaultBoxChildProperties
    , child = widget Label $ textProps font2
    },
    BoxChild
    { properties = defaultBoxChildProperties
    , child = widget FontButton $ fontProps Font2Changed font2
    }
  ]
  where
    textProps :: Text -> Vector (Attribute Label Event)
    textProps font =
      [ #label := ("<span font=\""<>font<>"\">" <> sample <> "</span>"),
        #useMarkup := True, #hexpand := True, #vexpand := True, #wrap := True]

    fontProps :: (Text -> Event) -> Text -> Vector (Attribute FontButton Event)
    fontProps ev font =
      [ onM #fontSet (fmap (ev . fromMaybe "No default") . fontChooserGetFont),
        #fontName := font]

update' :: State -> Event -> Transition State Event
update' (State _ f2) (Font1Changed fnt) = Transition (State fnt f2) (return Nothing)
update' (State f1 _) (Font2Changed fnt) = Transition (State f1 fnt) (return Nothing)
update' _ Closed = Exit

main :: IO ()
main = do
  lang <- languageGetDefault
  sample <- languageGetSampleString lang
  void $ run App { view         = view' sample
                 , update       = update'
                 , inputs       = []
                 , initialState = State "Sans 16" "Serif 16"
                 }
