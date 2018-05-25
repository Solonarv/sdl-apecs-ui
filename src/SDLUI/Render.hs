module SDLUI.Render where

import           Apecs
import qualified SDL

import           SDLUI.Components
import           SDLUI.Components.Globals

renderUI :: (Has w Box, Has w Colored, Has w Label, Has w RenderTarget) => System w ()
renderUI =
  renderBoxes

renderBoxes :: (Has w Box, Has w Colored, Has w RenderTarget) => System w ()
renderBoxes = cimapM_ $ \(ety, Box bounds) -> do
  RenderTarget r <- getGlobal
  color <- getSafe <$> get ety
  SDL.rendererDrawColor r SDL.$= color
  SDL.fillRect r $ Just bounds
