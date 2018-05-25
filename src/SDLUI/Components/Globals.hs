module SDLUI.Components.Globals where

import           Apecs
import           Apecs.Constant
import qualified SDL

newtype RenderTarget = RenderTarget SDL.Renderer
instance Component RenderTarget where
  type Storage RenderTarget = Constant RenderTarget
