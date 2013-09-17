{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}

module Diagrams.Backend.WX.Internals where

import Control.Monad.Reader
import Graphics.UI.WX (Point2(..))
import Graphics.UI.WXCore (GraphicsPath
                          ,GraphicsContext
                          ,Pen
                          ,Brush
                          ,penCreateDefault
                          ,penSetColour
                          ,penDelete
                          ,graphicsContextSetPen
                          ,brushCreateDefault
                          ,brushSetColour
                          ,brushDelete
                          ,graphicsContextSetBrush
                          ,graphicsContextPushState
                          ,graphicsContextPopState
                          ,graphicsContextConcatTransform
                          ,graphicsContextCreatePath
                          ,graphicsContextStrokePath
                          ,graphicsPathGetCurrentPoint
                          ,graphicsPathAddLineToPoint
                          ,graphicsPathAddCurveToPoint
                          ,graphicsPathMoveToPoint
                          ,graphicsPathCloseSubpath
                          ,graphicsPathDelete
                          ,graphicsContextCreateMatrix
                          ,graphicsMatrixSet
                          ,graphicsMatrixDelete
                          )
import qualified Graphics.UI.WXCore.WxcTypes as WXT
import Diagrams.Prelude
import Diagrams.TwoD.Path              (Clip (..), getFillRule)
import Diagrams.Attributes
import Diagrams.TwoD.Adjust            (adjustDia2D,
                                        setDefault2DAttributes)

import Data.Typeable
import Data.Maybe

data WX = WX
  deriving (Eq,Ord,Read,Show,Typeable)

data ReaderState = ReaderState
                   { graphicsPath    :: GraphicsPath ()
                   , graphicsContext :: GraphicsContext ()
                   , style           :: Style R2 -- the style currently applied.
                   }

instance Monoid (Render WX R2) where
  mempty  = C $ return ()
  (C rd1) `mappend` (C rd2) = C (rd1 >> rd2)

type RenderM a = ReaderT ReaderState IO a

-- | Render an object that the WX backend knows how to render.
renderC :: (Renderable a WX, V a ~ R2) => a -> RenderM ()
renderC a = case (render WX a) of C r -> r

-- | convert a color to a wx color
colorToWXColor :: Color c => Style v -> c -> WXT.Color
colorToWXColor s c = WXT.rgba (round (r * 255.0)) (round (g * 255.0)) (round (b * 255.0)) (round (a' * 255.0))
  where
    (r,g,b,a) =colorToSRGBA c
    a'        = case getOpacity <$> getAttr s of --the alpha value has to be multiplied by the overall opacity
                  Nothing -> a
                  Just  d -> d * a

-- | Create the default pen and brush, to be used when no style is applied
wxCreateDefaultPenAndBrush :: IO (Pen (), Brush ())
wxCreateDefaultPenAndBrush = do
  pen <- penCreateDefault
  penSetColour pen (WXT.rgba 0 0 0 255)
  brush <- brushCreateDefault
  brushSetColour brush (WXT.rgba 0 0 0 255)
  return (pen, brush)

-- | Apply all the styles
wxApplyStyle :: Style v -> RenderM ()
wxApplyStyle s = do
  context <- graphicsContext <$> ask
  -- create default pen and brush
  liftIO $ do
    (pen, brush)   <- wxCreateDefaultPenAndBrush
    -- apply styles to it
    sequence_ . catMaybes $ [
        handle (lineColor pen)
      , handle (fillColor brush)
      ]
    -- apply and delete pen and brush
    graphicsContextSetPen context pen
    graphicsContextSetBrush context brush
    brushDelete brush
    penDelete pen
  return ()
    where
     handle :: AttributeClass a => (a -> IO ()) -> Maybe (IO ())
     handle f = f `fmap` getAttr s
     clip context  = undefined
     lineColor pen   = penSetColour pen     . (colorToWXColor s) <$> getLineColor
     fillColor brush = brushSetColour brush . (colorToWXColor s) <$> getFillColor
      

instance Backend WX R2 where
  data Render   WX R2 = C (RenderM ())
  type Result   WX R2 = IO ()
  data Options WX R2  = WXOptions {optContext :: GraphicsContext ()
                                  ,optSize    :: (Double, Double)
                                  ,optBypassAdjust :: Bool}

  withStyle _ s t (C r) = C $ do
    ReaderState path context oldStyle <- ask
    -- remember/push the old state
    liftIO $ graphicsContextPushState context
    -- apply the style
    matrix <- liftIO $ graphicsContextCreateMatrix context a b c d dx dy
    liftIO $ graphicsContextConcatTransform context matrix
    liftIO $ graphicsMatrixDelete matrix
    let newStyle = oldStyle <> s
    wxApplyStyle newStyle
    -- do the action
    withReaderT (\s -> s {style = newStyle}) r
    -- recreate the old state
    liftIO $ graphicsContextPopState context
    wxApplyStyle oldStyle
     where
      (unr2 -> (dx, dy)) = transl t
      (unr2 -> (a , b )) = apply t unitX
      (unr2 -> (c , d )) = apply t unitY
    

  doRender _ opts (C r) = do
    path <- graphicsContextCreatePath (optContext opts)
    runReaderT r (ReaderState path (optContext opts) mempty)
    graphicsContextStrokePath (optContext opts) path
    graphicsPathDelete path

  adjustDia c opts d = if optBypassAdjust opts
                         then (opts, transDia # setDefault2DAttributes)
                         else adjustDia2D (\o -> let (w,h) = optSize o in mkSizeSpec (Just w) (Just h))
                                          (\_ o -> o)
                                          c opts d
                          where
                           (optWidth, optHeight) = optSize opts
                           transDia = moveTo (p2 (optWidth/2.0, optHeight/2.0)) d

-- Draw a relative line
graphicsPathAddRelLine :: GraphicsPath a -> (Point2 Double) -> IO ()
graphicsPathAddRelLine path (Point dx dy) = do
  (Point x y) <- graphicsPathGetCurrentPoint path
  graphicsPathAddLineToPoint path (Point (x + dx) (y + dy))

graphicsPathAddRelCurve :: GraphicsPath a -> (Point2 Double) -> (Point2 Double) -> (Point2 Double) -> IO ()
graphicsPathAddRelCurve path (Point cp1x cp1y) (Point cp2x cp2y) (Point dx dy) = do
  (Point x y) <- graphicsPathGetCurrentPoint path
  graphicsPathAddCurveToPoint path (Point (x + cp1x) (y + cp1y))
                                   (Point (x + cp2x) (y + cp2y))
                                   (Point (x + dx  ) (y + dy  ))


instance Renderable (Segment Closed R2) WX where
  render _ (Linear (OffsetClosed (unr2 -> (x,y)))) = C $ do
    path <- graphicsPath <$> ask
    liftIO $ graphicsPathAddRelLine path (Point x y)
  render _ (Cubic (unr2 -> (x1, y1))
                  (unr2 -> (x2, y2))
                  (OffsetClosed (unr2 -> (x3, y3)))) = C $ do
    path <- graphicsPath <$> ask
    liftIO $ graphicsPathAddRelCurve path (Point x1 y1) (Point x2 y2) (Point x3 y3)

instance Renderable (Trail R2) WX where
  render _ t = flip withLine t $ renderT . lineSegments
   where
    renderT segs = C $ do
      mapM_ renderC segs
      when (isLoop t) $ do
        path <- graphicsPath <$> ask
        liftIO $ graphicsPathCloseSubpath path

instance Renderable (Path R2) WX where
  render _ (Path trs) = C $ do
    context <- graphicsContext <$> ask
    nPath <- liftIO $ graphicsContextCreatePath context
    withReaderT (\r -> r {graphicsPath = nPath}) $ mapM_ renderTrail trs
    gc <- graphicsContext <$> ask
    liftIO $ graphicsContextStrokePath gc nPath
    liftIO $ graphicsPathDelete nPath
     where
      renderTrail (viewLoc -> (unp2 -> (px, py), tr)) = do
        path <- graphicsPath <$> ask
        liftIO $ graphicsPathMoveToPoint path (Point px py)
        renderC tr
