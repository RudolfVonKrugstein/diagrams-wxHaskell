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
  windowOnPaintRaw sw (onpaint sw dias)
  set f [layout := minsize (sz 400 400 ) $ column  2 [fill $ widget sw,hfill $ widget b]]
  return ()
 where
  onpaint sw dias _ _ _ = do
    dia <- head <$> varGet dias
    putStrLn "OnPaint"
    renderToWindow sw True dia
  onpress dias sw = do
    varUpdate dias (\d -> tail d)
    repaint sw

sampleDiagrams = [circle 100] :: [Diagram WX R2]
