module Main where

import           Control.Lens         hiding ((#), parts, plate)
import           Control.Monad.Reader

import           Data.Function (on)
import           Data.List     (minimumBy)
import           Data.Maybe    (fromJust)

import           Diagrams.Backend.SVG
import           Diagrams.Path
import           Diagrams.Prelude hiding (difference, fromVertices,
                                   intersection, parts, plate, render, sep,
                                   trace, union, _sep)

import           Diagrams.TwoD.Offset
import           Diagrams.TwoD.Path.Boolean

type KBD = Reader KBDCfg

data KBDCfg = KBDCfg
  { _nRows          :: Integer
  , _nCols          :: Integer
  , _nThumb         :: Integer
  , _columnSpacing  :: Double
  , _rowSpacing     :: Double
  , _switchHoleSize :: Double
  , _angle          :: Angle Double
  , _staggering     :: [Double]
  , _screwSize      :: Double
  , _washerSize     :: Double
  , _sep            :: Double
  }

makeLenses ''KBDCfg

rotP :: Angle Double -> (Double, Double) -> V2 Double
rotP a p = apply (rotation a) (r2 p)

roundPath :: Double -> Path V2 Double -> Path V2 Double
roundPath = offsetPath' (with & offsetJoin .~ LineJoinRound)

mirror :: Path V2 Double -> Path V2 Double
mirror p = union Winding $ p <> p # reversePath # reflectX

fromVertices :: (Metric v, Floating n, Ord n) => [Point v n] -> Path v n
fromVertices ps =
  let fv = toPath . closeLine . lineFromVertices
      ps' = fv ps
   in ps' # moveOriginTo (pathCentroid ps')

toVertices :: Path V2 Double -> [Point V2 Double]
toVertices = mconcat . pathVertices

tpos :: Integer -> KBD (Double, Double)
tpos n = do
  k <- ask
  return (fromInteger n * _rowSpacing k + (_sep k / 2), _columnSpacing k / 2)

kpos :: Integer -> Integer -> KBD (Double, Double)
kpos m n = do
  k <- ask
  st <- staggering'
  (x, _) <- tpos (m + _nThumb k)
  return (x, (st !! fromInteger m) + (fromInteger n * _columnSpacing k))

staggering' :: KBD [Double]
staggering' = do
  k <- ask
  return $ _staggering k ++ repeat (last $ _staggering k)

switchHoleNotched :: KBD (Path V2 Double)
switchHoleNotched = do
  k <- ask
  let notchWidth = 3.5001
      notchOffset = 4.2545
      notchDepth = 0.8128
  return $
    union Winding $
    square (_switchHoleSize k) <>
    rect (_switchHoleSize k + 2 * notchDepth) notchWidth #
    translate (V2 0 notchOffset) <>
    rect (_switchHoleSize k + 2 * notchDepth) notchWidth #
    translate (V2 0 (-notchOffset))

switchHolesPos :: [Integer] -> [Integer] -> [Integer] -> KBD [(Double, Double)]
switchHolesPos xr yr tr = do
  keys <- sequence [kpos x y | x <- xr, y <- yr]
  tkeys <- sequence [tpos i | i <- tr]
  return $ keys ++ tkeys

allSwitchHolesPos :: KBD [(Double, Double)]
allSwitchHolesPos = do
  k <- ask
  switchHolesPos [0 .. _nCols k - 1] [0 .. _nRows k - 1] [0 .. _nThumb k - 1]

switchHoles :: Path V2 Double -> KBD (Path V2 Double)
switchHoles hole = do
  k <- ask
  ashp <- allSwitchHolesPos
  let keys =
        rotate (_angle k) $
        union Winding $
        mconcat $ (\(x, y) -> hole # translate (V2 x y)) <$> ashp
  return $ mirror keys

roundHole :: KBD (Path V2 Double)
roundHole = do
  k <- ask
  return $ circle (_screwSize k / 2)

screwPos :: KBD [(Double, Double)]
screwPos = do
  k <- ask
  (r, t) <- kpos (_nCols k - 1) (_nRows k - 1)
  (l, b) <- kpos 0 0
  let hx = (_rowSpacing k + _switchHoleSize k / 2) / 2
      hy = (_columnSpacing k + _switchHoleSize k / 2) / 2
      l' = (r + hx, t + hy - 2)
  return
    [ (l - hx, b - hy + 2)
    , (r + hx, b - hy + 3)
    , l'
    , unr2 $ rotP (negated $ _angle k) (_sep k / 4, rotP (_angle k) l' ^. _y)
    ]

placeRotated :: [(Double, Double)] -> Path V2 Double -> KBD (Path V2 Double)
placeRotated ps s = do
  k <- ask
  return $ mconcat $ (\p -> s # translate (r2 p) # rotate (_angle k)) <$> ps

screwHoles :: KBD (Path V2 Double)
screwHoles = fmap mirror (join $ placeRotated <$> screwPos <*> roundHole)

outline :: KBD (Path V2 Double)
outline = do
  k <- ask
  ashp <- allSwitchHolesPos
  let srt f g =
        p2 $
        minimumBy
          (g `on` (\p -> maximum $ f <$> [rotP (_angle k) p, r2 p]))
          ashp
      minx = srt (^. _x) compare
      maxx = srt (^. _x) $ flip compare
      miny = srt (^. _y) compare
      maxy = srt (^. _y) $ flip compare
      dx = _rowSpacing k - 3
      dy = _columnSpacing k / 3
      w =
        (maxx ^. _x) - (minx ^. _x) + _sep k / 2 + dx / 2 +
        _switchHoleSize k / 2
      h = (maxy ^. _y) - (miny ^. _y) + 2 * dy + _switchHoleSize k
      r =
        rect w h # alignBL # translate (r2 (0, -_switchHoleSize k / 2 - dy / 2))
      rot' = papply (rotation $ _angle k)
      pt1 = rot' miny - p2 (0, _switchHoleSize k / 2 + dy)
      pt2 = rot' maxy + p2 (0, _switchHoleSize k / 2 + dy - 1)
      r' = r # rotate (_angle k)
      tr p =
        let a = fromVertices p # alignBL
         in a # translate (r2 . unp2 $ (head ps - head (toVertices a)))
      ps = toVertices r'
      vs1 =
        [ head ps
        , p2 (head ps ^. _x, pt2 ^. _y)
        , p2 (0, pt2 ^. _y)
        , p2 (0, pt1 ^. _y)
        , pt1
        ]
      mask1 = tr vs1
      np = fromJust $ maxTraceP (head ps) (unitY # rotate (_angle k)) mask1
      vs2 = [head ps, np, p2 (0, pt2 ^. _y), p2 (0, pt1 ^. _y), pt1]
  return $ tr vs2 # mirror # roundPath (-_washerSize k / 4)

bottomPlate :: KBD (Path V2 Double)
bottomPlate = do
  o <- outline
  difference Winding o <$> screwHoles

switchPlate :: KBD (Path V2 Double)
switchPlate = do
  bp <- bottomPlate
  sh <- switchHoleNotched >>= switchHoles
  return $ difference Winding bp sh

topPlate :: KBD (Path V2 Double)
topPlate = do
  k <- ask
  ashp <- allSwitchHolesPos
  bp <- bottomPlate
  let innerR =
        rect
          (_switchHoleSize k + _rowSpacing k / 4)
          (_switchHoleSize k + _columnSpacing k / 4)
  inner <- placeRotated ashp innerR
  let mask = roundPath (-1) $ union Winding inner
  return $ difference Winding bp $ mirror mask

spacerPlate :: KBD (Path V2 Double)
spacerPlate = do
  k <- ask
  o <- outline
  sp <- screwPos
  sh <- screwHoles
  let w = 12
      punch =
        o # scaleToX (width o - w) # scaleToY (height o - w) #
        translate (V2 0 w / 2)
  disks <- union Winding <$> placeRotated sp (circle (_washerSize k / 3))
  let fr = difference Winding o punch
      plate = intersection Winding o $ mirror disks <> fr
  return $ difference Winding plate sh

render :: KBDCfg -> IO ()
render k =
  let parts =
        (`runReader` k) <$> [bottomPlate, spacerPlate, switchPlate, topPlate]
      name = "atreus" ++ show (2 * (_nRows k * _nCols k + _nThumb k)) ++ ".svg"
      dpi = 96
      sf = dpi / 25.4
      lineW = sf * 0.1
      stops = mkStops [(gray, 0, 1), (white, 0.5, 1), (gray, 1, 1)]
      gradient =
        mkLinearGradient
          stops
          ((-width (head parts) / 4) ^& 0)
          ((width (head parts) / 4) ^& (-60))
          GradPad
      aStyles =
        fillTexture gradient :
        fmap (\c -> fcA (c `withOpacity` 0.5)) (cycle [yellow, green, blue])
      assembly = reverse $ zipWith (\s p -> strokePath p # s) aStyles parts
      diagram = frame 1.05 (vsep 5 (assembly ++ [mconcat assembly])) # lwG lineW
      sizeSp = dims2D (sf * width diagram) (sf * height diagram)
   in renderSVG ("images/" ++ name) sizeSp diagram

main :: IO ()
main = do
  let ang = 10 @@ deg
      atreus42 =
        KBDCfg
          { _nRows = 4
          , _nCols = 5
          , _nThumb = 1
          , _columnSpacing = 19
          , _rowSpacing = 19
          , _switchHoleSize = 13.97
          , _angle = ang
          , _staggering = [0, 5, 11, 6, 3, 2]
          , _screwSize = 3
          , _washerSize = 15
          , _sep = 40
          }
      atreus50 = atreus42 & nCols .~ 6
      atreus62 = atreus50 & nRows .~ 5
      atreus204 = atreus42 & nCols .~ 10 & nRows .~ 10 & nThumb .~ 2
      ks = [atreus42, atreus50, atreus62, atreus204]
  mapM_ render ks
