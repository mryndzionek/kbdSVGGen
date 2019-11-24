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
  { _nRows          :: Int
  , _nCols          :: Int
  , _nThumb         :: Int
  , _columnSpacing  :: Double
  , _rowSpacing     :: Double
  , _switchHoleSize :: Double
  , _angle          :: Angle Double
  , _staggering     :: [Double]
  , _screwSize      :: Double
  , _washerSize     :: Double
  , _sep            :: Double
  , _hooks          :: Bool
  , _split          :: Bool
  }

instance Show KBDCfg where
  show k =
    "atreus" ++
    show (2 * (_nRows k * _nCols k + _nThumb k)) ++
    (if _split k
       then "s"
       else "") ++
    (if _hooks k
       then "h"
       else "")

makeLenses ''KBDCfg

rotP :: Angle Double -> (Double, Double) -> V2 Double
rotP a p = apply (rotation a) (r2 p)

roundPath :: Double -> Path V2 Double -> Path V2 Double
roundPath = offsetPath' (with & offsetJoin .~ LineJoinRound)

mirror :: Path V2 Double -> KBD (Path V2 Double)
mirror p = do
  isSplit <- asks _split
  return . union Winding $
    if isSplit
      then reversePath p
      else p <> p # reversePath # reflectX

fromVertices :: (Metric v, Floating n, Ord n) => [Point v n] -> Path v n
fromVertices ps =
  let fv = toPath . closeLine . lineFromVertices
      ps' = fv ps
   in ps' # moveOriginTo (pathCentroid ps')

toVertices :: Path V2 Double -> [Point V2 Double]
toVertices = mconcat . pathVertices

tpos :: Int -> KBD (Double, Double)
tpos n = do
  k <- ask
  let lut =
        [ ( fromIntegral $
            (if odd (_nThumb k)
               then i + 1
               else i) `quot`
            2
          , fromIntegral $ i `rem` 2)
        | i <- [0 .. _nThumb k - 1]
        ]
  return
    ( fst (lut !! n) * _rowSpacing k + (_sep k / 2)
    , (if _nThumb k > 1 && odd (_nThumb k) && (n == 0)
         then 0
         else _columnSpacing k / 2) -
      snd (lut !! n) * _columnSpacing k)

kpos :: Int -> Int -> KBD (Double, Double)
kpos m n = do
  k <- ask
  st <- staggering'
  let nt = fromIntegral $ (_nThumb k + 1) `quot` 2
  let x = nt * _rowSpacing k + (_sep k / 2)
  return
    ( x + fromIntegral m * _rowSpacing k
    , (st !! fromIntegral m) + (fromIntegral n * _columnSpacing k))

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

boundaryPos :: KBD (P2 Double, P2 Double, P2 Double, P2 Double)
boundaryPos = do
  k <- ask
  ashp <- allSwitchHolesPos
  let srt f g =
        p2 $
        minimumBy
          (g `on` (\p -> maximum $ f <$> [rotP (_angle k) p, r2 p]))
          ashp
  return
    ( srt (^. _x) compare
    , srt (^. _x) $ flip compare
    , srt (^. _y) compare
    , srt (^. _y) $ flip compare)

switchHolesPos :: [Int] -> [Int] -> [Int] -> KBD [(Double, Double)]
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
  mirror keys

roundHole :: KBD (Path V2 Double)
roundHole = asks $ circle . (/ 2) . _screwSize

screwPos :: KBD [(Double, Double)]
screwPos = do
  k <- ask
  (_, _, miny, _) <- boundaryPos
  (r, t) <- kpos (_nCols k - 1) (_nRows k - 1)
  (_, b') <- kpos 0 0
  let (b, l) = (miny ^. _y, miny ^. _x)
      hx = (_rowSpacing k + _switchHoleSize k / 2) / 2
      hy = (_columnSpacing k + _switchHoleSize k / 2) / 2
      l' = (r + hx + 1.4, t + hy - 1)
  return
    [ (l, b - hy - 2)
    , (r + hx + 1.2, b' - hy + 3)
    , l'
    , unr2 $ rotP (negated $ _angle k) (_sep k / 4, rotP (_angle k) l' ^. _y)
    ]

placeRotated :: [(Double, Double)] -> Path V2 Double -> KBD (Path V2 Double)
placeRotated ps s = do
  a <- asks _angle
  return $ mconcat $ (\p -> s # translate (r2 p) # rotate a) <$> ps

screwHoles :: KBD (Path V2 Double)
screwHoles = join (placeRotated <$> screwPos <*> roundHole) >>= mirror

outline :: KBD (Path V2 Double)
outline = do
  k <- ask
  (minx, maxx, miny, maxy) <- boundaryPos
  let dx = _rowSpacing k - 2
      dy = _columnSpacing k / 3
      w =
        (maxx ^. _x) - (minx ^. _x) + _sep k / 2 + dx / 2 +
        _switchHoleSize k / 2
      h = (maxy ^. _y) - (miny ^. _y) + 2 * dy + _switchHoleSize k
      r =
        rect w h # alignBL # translate (r2 (0, -_switchHoleSize k / 2 - dy / 2))
      rot' = papply (rotation $ _angle k)
      pt1 = rot' miny - p2 (0, _switchHoleSize k / 2 + dy + 3)
      pt2 = rot' maxy + p2 (0, _switchHoleSize k / 2 + dy + 2)
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
  roundPath (-_washerSize k / 4) <$> (tr vs2 # mirror)

bottomPlate :: KBD (Path V2 Double)
bottomPlate = do
  o <- outline
  difference Winding o <$> screwHoles

addHooks :: Path V2 Double -> KBD (Path V2 Double)
addHooks p = do
  sp <- screwPos
  ang <- asks _angle
  isSplit <- asks _split
  let a =
        ang ^+^ negated (signedAngleBetween unitX (r2 (sp !! 1) - r2 (head sp)))
      an = if isSplit then (-45 @@ deg) else (-20 @@ deg)
      hp =
        fromJust $
        maxTraceP (fromJust (mCenterPoint p)) (unitX # rotate an) p
      hook =
        difference Winding (rect 9 9) (rect 8 3 # translate (-unitY * 2.5)) #
        roundPath (-0.5) #
        rotate a #
        moveTo hp
  mappend p <$> mirror hook

switchPlate :: KBD (Path V2 Double)
switchPlate = do
  hasHooks <- asks _hooks
  difference Winding <$>
    (if hasHooks
       then bottomPlate >>= addHooks
       else bottomPlate) <*>
    (switchHoleNotched >>= switchHoles)

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
  let mask = roundPath (-1) . union Winding $ inner
  difference Winding bp <$> mirror mask

spacerPlate :: KBD (Path V2 Double)
spacerPlate = do
  k <- ask
  o <- outline
  sp <- screwPos
  sh <- screwHoles
  let w = 12
      punch = o # scaleToX (width o - w) # scaleToY (height o - w)
      (c1, c2) = (cntr punch ^. _y, cntr o ^. _y)
        where
          cntr :: Path V2 Double -> Point V2 Double
          cntr p = fromJust (mCenterPoint p)
  disks <- union Winding <$> placeRotated sp (circle (_washerSize k / 3))
  let fr =
        difference
          Winding
          o
          (punch #
           translate
             (V2
                (if _split k
                   then w / 2
                   else 0)
                (c2 - c1)))
  plate <- intersection Winding o <$> (mappend fr <$> mirror disks)
  return $ difference Winding plate sh

render :: KBDCfg -> IO ()
render k =
  let parts =
        (`runReader` k) <$> [bottomPlate, spacerPlate, switchPlate, topPlate]
      fname = "images/" ++ show k ++ ".svg"
      dpi = 96
      sf = dpi / 25.4
      lineW = sf * 0.2
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
      diagram = frame 1.05 (vsep 5 (assembly ++ [mconcat assembly])) # lwO lineW
      sizeSp = dims2D (sf * width diagram) (sf * height diagram)
   in renderSVG fname sizeSp diagram

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
          , _washerSize = 13
          , _sep = 40
          , _hooks = False
          , _split = False
          }
      atreus44 = atreus42 & nThumb .~ 2
      atreus50 = atreus42 & nCols .~ 6
      atreus52h = atreus50 & nThumb .~ 2 & hooks .~ True
      atreus52s = atreus52h & split .~ True & hooks .~ False
      atreus62 = atreus50 & nRows .~ 5
      atreus62s = atreus62 & split .~ True
      atreus206 = atreus42 & nCols .~ 10 & nRows .~ 10 & nThumb .~ 3
      atreus208 = atreus206 & nThumb .~ 4
      atreus210 = atreus208 & nThumb .~ 5
      ks =
        [ atreus42
        , atreus44
        , atreus50
        , atreus52h
        , atreus52s
        , atreus62
        , atreus62s
        , atreus206
        , atreus208
        , atreus210
        ]
  mapM_ render ks
