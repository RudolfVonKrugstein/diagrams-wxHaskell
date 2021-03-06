module Main where

import Graphics.UI.WXCore hiding ((#),blue,purple)
import Graphics.UI.WX hiding (circle,(#),blue,purple)
import Diagrams.Prelude hiding (start, text)
import qualified Diagrams.Prelude as DP
import Diagrams.Backend.WX
import Data.Maybe

import qualified SquareLimit as SL
import qualified Chart as CH

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
    putStrLn $ "Diagram dimensions: " ++ show (width dia, height dia)
    renderToWindow sw bypassAdjustDia dia
  onpress dias sw = do
    varUpdate dias (\d -> tail d)
    repaint sw

sampleDiagrams = [circle 100
                 ,circle 100 # lc purple # fc blue 
                 ,DP.text "Hello World" # DP.font "ROMAN" # fc DP.black # DP.fontSize 20
                 ,DP.text "Hello Rotated World" # DP.font "ROMAN" # fc DP.black # DP.fontSize 10 # DP.rotateBy (1/3)
                 ,square 10 <> square 5 # DP.rotateBy (1/3)
                 ,SL.example
                 ,CH.example] :: [Diagram WX R2]
