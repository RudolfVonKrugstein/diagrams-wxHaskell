module Diagrams.Backend.WX (
  renderToWindow
) where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Diagrams.Backend.WX.Internals
import Diagrams.Prelude

renderToWindow :: Window a -> Bool -> Diagram WX R2 -> IO ()
renderToWindow win bypassAdjust dia = do
  Size w h <- windowGetSize win
  gc <- graphicsContextCreateFromWindow win
  renderDia WX (WXOptions (downcastGraphicsContext gc) (fromIntegral w, fromIntegral h) bypassAdjust)  dia
  graphicsContextDelete gc
