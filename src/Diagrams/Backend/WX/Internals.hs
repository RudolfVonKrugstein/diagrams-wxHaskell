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
                          ,penSetWidth
                          ,penSetJoin
                          ,penSetCap
                          ,graphicsContextSetPen
                          ,brushCreateDefault
                          ,brushSetColour
                          ,brushDelete
                          ,graphicsContextSetBrush
                          ,graphicsContextPushState
                          ,graphicsContextPopState
                          ,graphicsContextConcatTransform
                          ,graphicsContextCreatePath
                          ,graphicsContextDrawPath
                          ,graphicsContextSetFont
                          ,wxODDEVEN_RULE
                          ,wxWINDING_RULE
                          ,graphicsPathGetCurrentPoint
                          ,graphicsPathAddLineToPoint
                          ,graphicsPathAddCurveToPoint
                          ,graphicsPathMoveToPoint
                          ,graphicsPathCloseSubpath
                          ,graphicsPathDelete
                          ,graphicsContextCreateMatrix
                          ,graphicsMatrixSet
                          ,graphicsMatrixDelete
                          ,wxCAP_BUTT
                          ,wxCAP_ROUND
                          ,wxCAP_PROJECTING
                          ,wxJOIN_MITER
                          ,wxJOIN_ROUND
                          ,wxJOIN_BEVEL
                          ,fontCreateDefault
                          ,fontSetStyle
                          ,fontSetWeight
                          ,fontSetPointSize
                          ,fontSetFamily
                          ,fontDelete
                          ,wxNORMAL
                          ,wxITALIC
                          ,wxSLANT
                          ,wxBOLD
                          ,wxDECORATIVE
                          ,wxROMAN 	
                          ,wxSCRIPT
                          ,wxSWISS 	
                          ,wxMODERN 	
                          ,wxTELETYPE 	
                          )
import qualified Graphics.UI.WXCore.WxcTypes as WXT
import Diagrams.Prelude
import Diagrams.TwoD.Path              (Clip (..), getFillRule)
import Diagrams.Attributes
import Diagrams.TwoD.Adjust            (adjustDia2D, adjustDiaSize2D,
                                        setDefault2DAttributes)
import Diagrams.TwoD.Text
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

-- | Apply all the styles
wxApplyStyle :: Style v -> RenderM ()
wxApplyStyle s = do
  context <- graphicsContext <$> ask
  -- create default pen, brush and font
  liftIO $ do
    pen <- penCreateDefault
    brush <- brushCreateDefault
    font  <- fontCreateDefault
    -- apply styles to it
    sequence_ . catMaybes $ [
        handle (lineColor pen)
      , handle (fillColor brush)
      , handle (lineWidth pen)
      , handle (lineCap pen)
      , handle (lineJoin pen)
      , handle (fontSize font)
      , handle (fontSlant font)
      , handle (fontWeight font)
      , handle (fontFamily font)
      ]
    -- apply and delete pen and brush and font
    graphicsContextSetPen context pen
    graphicsContextSetBrush context brush
    graphicsContextSetFont context font (WXT.rgba 0 0 0 255)
    brushDelete brush
    penDelete pen
    fontDelete font
  return ()
    where
     handle :: AttributeClass a => (a -> IO ()) -> Maybe (IO ())
     handle f = f `fmap` getAttr s
     clip context  = undefined
     lineColor pen   = penSetColour pen     . (colorToWXColor s)     <$> getLineColor
     fillColor brush = brushSetColour brush . (colorToWXColor s)     <$> getFillColor
     lineWidth pen   = penSetWidth pen . lineWidthToWXLineWidth      <$> getLineWidth
     lineCap   pen   = penSetCap  pen . lineCapToWXLineCap           <$> getLineCap
     lineJoin  pen   = penSetJoin pen . lineJoinToWXLineJoin         <$> getLineJoin
     fontSize  font  = fontSetPointSize font . round                 <$> getFontSize
     fontSlant font  = fontSetStyle font . fontSlantToWXFontSlant    <$> getFontSlant
     fontWeight font = fontSetWeight font . fontWeightToWXFontWeight <$> getFontWeight
     fontFamily font = fontSetFamily font . fontFamilyToWXFontFamily <$> getFont

-- | wxHaskell pens only take integer width. So we have to round the pen width
--   to the nearest integer. But we do not want to round number < 0.5 to 0, but to 1
lineWidthToWXLineWidth :: Double -> Int
lineWidthToWXLineWidth w | w < 1.0   = 1
                         | otherwise = round w

lineCapToWXLineCap :: LineCap -> Int
lineCapToWXLineCap LineCapButt   = wxCAP_BUTT
lineCapToWXLineCap LineCapRound  = wxCAP_ROUND
lineCapToWXLineCap LineCapSquare = wxCAP_PROJECTING

lineJoinToWXLineJoin :: LineJoin -> Int
lineJoinToWXLineJoin LineJoinMiter = wxJOIN_MITER
lineJoinToWXLineJoin LineJoinRound = wxJOIN_ROUND
lineJoinToWXLineJoin LineJoinBevel = wxJOIN_BEVEL

fillRuleToWXFillRule :: FillRule -> Int
fillRuleToWXFillRule Winding = wxWINDING_RULE
fillRuleToWXFillRule EvenOdd = wxODDEVEN_RULE

fontSlantToWXFontSlant :: FontSlant -> Int
fontSlantToWXFontSlant FontSlantNormal  = wxNORMAL
fontSlantToWXFontSlant FontSlantItalic  = wxITALIC
fontSlantToWXFontSlant FontSlantOblique = wxSLANT

fontWeightToWXFontWeight :: FontWeight -> Int
fontWeightToWXFontWeight FontWeightNormal = wxNORMAL
fontWeightToWXFontWeight FontWeightBold   = wxBOLD

fontFamilyToWXFontFamily :: String -> Int
fontFamilyToWXFontFamily "DECORATIVE" = wxDECORATIVE
fontFamilyToWXFontFamily "ROMAN"      = wxROMAN 	
fontFamilyToWXFontFamily "SCRIPT"     = wxSCRIPT
fontFamilyToWXFontFamily "SWISS"      = wxSWISS 	
fontFamilyToWXFontFamily "MODERN"     = wxMODERN 	
fontFamilyToWXFontFamily "TELETYPE"   = wxTELETYPE
fontFamilyToWXFontFamily _            = wxNORMAL

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
    -- create a path that will be rendered with this style
    nPath <- liftIO $ graphicsContextCreatePath context
    -- do the action
    withReaderT (\s -> s {style = newStyle, graphicsPath = nPath}) r
    -- render the path and delete it
    liftIO $ graphicsContextDrawPath context nPath
                    --get fillrule, take ODDEVEN if none given
                    (maybe wxODDEVEN_RULE fillRuleToWXFillRule (getFillRule <$> getAttr s))
    liftIO $ graphicsPathDelete nPath
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
    graphicsContextDrawPath (optContext opts) path wxODDEVEN_RULE
    graphicsPathDelete path

  adjustDia c opts d = if optBypassAdjust opts
                         then (opts, transDia # setDefault2DAttributes)
                         else  adjustDiaSize2D (\o -> let (w,h) = optSize o in mkSizeSpec (Just $ w) (Just $ h))
                                          setWXSizeWithSizeSpec
                                          c opts (d # setDefault2DAttributes)
                          where
                           (optWidth, optHeight) = optSize opts
                           transDia = moveTo (p2 (optWidth/2.0, optHeight/2.0)) d
                           setWXSizeWithSizeSpec s o = case s of
                             Absolute -> o
                             Width w  -> o {optSize = (w,w)}
                             Height h -> o {optSize = (h,h)}
                             Dims w h -> o {optSize = (w,h)}

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
    -- I am not sure if I should create a different path here or just use the existing one ...
    mapM_ renderTrail trs
     where
      renderTrail (viewLoc -> (unp2 -> (px, py), tr)) = do
        path <- graphicsPath <$> ask
        liftIO $ graphicsPathMoveToPoint path (Point px py)
        renderC tr
