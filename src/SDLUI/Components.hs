module SDLUI.Components where

import           Foreign.C.Types (CInt)
import           GHC.Word        (Word8)

import           Data.Text       (Text)
import           Linear.V2
import           Linear.V4

import           Apecs
import qualified SDL

type Rect = SDL.Rectangle CInt
pattern Rect :: V2 a -> V2 a -> SDL.Rectangle a
pattern Rect v0 v1 = SDL.Rectangle (SDL.P v0) v1

type Color = V4 Word8

newtype Box = Box { boxBounds :: Rect } deriving (Eq, Show)
instance Component Box where
  type Storage Box = Map Box

newtype Label = Label { labelText :: Text } deriving (Eq, Show)
instance Component Label where
  type Storage Label = Map Label

newtype Clickable = Clickable { handleClick :: forall w. SDL.MouseButtonEventData -> System w () }
instance Component Clickable where
  type Storage Clickable = Map Clickable

data Colored = Colored { colorFG :: !Color, colorBG :: !Color } deriving (Eq, Show)
instance Component Colored where
  type Storage Colored = Map Colored
