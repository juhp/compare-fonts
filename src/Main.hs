{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import Control.Monad ( void )
--import Data.Maybe (fromMaybe)
import Data.Text ( Text )
import GI.Gtk
  ( {-Button(..),-} Box(..), FontButton(..), {-Grid(..),-} Label(..),
    Window(..), Orientation(OrientationVertical), fontChooserGetFont
  )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
--import GI.Gtk.Declarative.Container.Grid

data State = State { mfont :: Maybe Text }

data Event = FontChanged (Maybe Text) | Closed

view' :: State -> AppView Window Event
view' State {..} =
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
    { properties = defaultBoxChildProperties -- { width      = 1
                                             --  , height     = 1
                                             -- }
    , child = widget FontButton
              [ onM #fontSet (fmap FontChanged . fontChooserGetFont),
                #showStyle := False
              ]
    },
    BoxChild
    { properties = defaultBoxChildProperties --{ width      = 2
                                             -- , height     = 1
                                             -- , leftAttach = 0
                                             -- , topAttach  = 1
                                             -- }
    , child = widget Label
      [#label := ("<span " <> maybe "" (\f -> "font=\""<>f<>"\"") mfont <> ">Hello world</span>"),
       #useMarkup := True, #hexpand := True, #vexpand := True]
    }
  ]

update' :: State -> Event -> Transition State Event
update' (State _) (FontChanged mfnt) = Transition (State mfnt) (return Nothing)
update' _ Closed = Exit

main :: IO ()
main = void $ run App { view         = view'
                      , update       = update'
                      , inputs       = []
                      , initialState = State Nothing
                      }
