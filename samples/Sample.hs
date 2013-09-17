module Main where

import Graphics.UI.WXCore
import Graphics.UI.WX hiding (circle)
import Diagrams.Prelude hiding (start, text)
import Diagrams.Backend.WX
import Diagrams.Backend.WX.Internals
import Data.Maybe


main :: IO ()
main = start gui

gui :: IO ()
gui = do
  -- creare variable with endless repeating diagrams
  dias  <- varCreate (concat . repeat $ sampleDiagrams)
  f  <- frame [text := "Diagrams demo"]
  sw <- scrolledWindow f []
  b  <- button f [ text := "Next", on command := onpress dias sw ]
  cb <- checkBox f [ text := "Bypass adjust dia", on command := repaint sw ]
  windowOnPaintRaw sw (onpaint sw dias cb)
  set f [layout := minsize (sz 400 400 ) $ column  2 [fill $ widget sw,hfill $ row 2 [widget b, widget cb]]]
  return ()
 where
  onpaint sw dias cb _ _ _ = do
    bypassAdjustDia <- get cb checked
    dia <- head <$> varGet dias
    putStrLn "OnPaint"
    renderToWindow sw bypassAdjustDia dia
  onpress dias sw = do
    varUpdate dias (\d -> tail d)
    repaint sw

sampleDiagrams = [circle 100] :: [Diagram WX R2]
