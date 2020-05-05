{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Main where

import           Control.Lens               hiding ((#), parts, plate)
import qualified Control.Monad.Parallel     as P
import           Control.Monad.Reader

import           System.Process

import           Data.List        (minimumBy, maximumBy)
import           Data.Maybe       (fromJust, isJust, isNothing)
import           Data.Ord         (comparing)
import           Data.Time.Clock
import           Data.Time.Format
import           Data.UUID        (UUID, toString)
import           Data.UUID.V1     (nextUUID)

import           Diagrams.Backend.SVG
import           Diagrams.Path
import           Diagrams.Prelude           hiding (difference, fromVertices,
                                             intersection, parts, plate,
                                             project, render, sep, trace, union,
                                             _sep)

import           Diagrams.TwoD.Offset
import           Diagrams.TwoD.Path.Boolean

import qualified Graphics.Svg               as S
import           Graphics.SVGFonts
import           Graphics.SVGFonts.ReadFont (PreparedFont)

import           System.Directory  (findExecutable)

type KBD = Reader KBDCfg

data KBDCfg = KBDCfg
  { _nRows          :: Int
  , _nCols          :: Int
  , _nThumb         :: Int
  , _columnSpacing  :: Double
  , _rowSpacing     :: Double
  , _switchHoleSize :: Double
  , _switchHole     :: Double -> Path V2 Double
  , _spacerWidth    :: Double
  , _angle          :: Angle Double
  , _staggering     :: [Double]
  , _screwSize      :: Double
  , _washerSize     :: Double
  , _screwHole      :: Double -> Path V2 Double
  , _sep            :: Double
  , _hooks          :: Bool
  , _split          :: Bool
  , _topNotch       :: Maybe Double
  , _logo           :: Maybe (Double, Double, Path V2 Double)
  , _textFont       :: PreparedFont Double
  , _date           :: String
  , _uuid           :: UUID
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
makeLensesFor [("_elements", "sElements")] ''S.Document
makePrisms ''S.Tree
makeLensesFor [("_groupChildren", "groupChildren")] ''S.Group
makeLensesFor [("_pathDefinition", "pathDefinition")] ''S.Path
makePrisms ''S.PathCommand

rotP :: Angle Double -> (Double, Double) -> V2 Double
rotP a p = apply (rotation a) (r2 p)

roundPath :: Double -> Path V2 Double -> Path V2 Double
roundPath = offsetPath' (with & offsetJoin .~ LineJoinRound)

mirror :: (Transformable b, Monoid b, N b ~ Double, V b ~ V2) => b -> KBD b
mirror p = do
  isSplit <- asks _split
  return $
    if isSplit
      then p
      else p <> p # reflectX

mirrorP :: Path V2 Double -> KBD (Path V2 Double)
mirrorP p = do
  isSplit <- asks _split
  if isSplit
    then return $ p # reversePath
    else union Winding <$> mirror p

fromVertices :: (Metric v, Floating n, Ord n) => [Point v n] -> Path v n
fromVertices ps =
  let fv = toPath . closeLine . lineFromVertices
      ps' = fv ps
   in ps' # moveOriginTo (pathCentroid ps')

toVertices :: Path V2 Double -> [Point V2 Double]
toVertices = mconcat . pathVertices

adjP :: [Point V2 Double] -> Path V2 Double
adjP ps =
  let a = fromVertices ps # alignBL
   in a # translate (r2 . unp2 $ (head ps - head (toVertices a)))

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
      snd (lut !! n) *
      _columnSpacing k)

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

switchHoleSquare :: Double -> Path V2 Double
switchHoleSquare = square

switchHoleNotched :: Double -> Path V2 Double
switchHoleNotched s =
  let notchWidth = 3.5001
      notchOffset = 4.2545
      notchDepth = 0.8128
   in union Winding $ switchHoleSquare s <> rect (s + 2 * notchDepth) notchWidth #
      translate (V2 0 notchOffset) <>
      rect (s + 2 * notchDepth) notchWidth #
      translate (V2 0 (-notchOffset))

boundaryPos ::
     ((Double, Double) -> V2 Double)
  -> KBD (P2 Double, P2 Double, P2 Double, P2 Double)
boundaryPos f = do
  ashp <- allSwitchHolesPos
  let rt = fmap f ashp
      get = p2 . unr2
  return
    ( get $ minimumBy (comparing (^. _x) <> comparing (^. _y)) rt
    , get $ maximumBy (comparing (^. _x) <> comparing (^. _y)) rt
    , get $ minimumBy (comparing (^. _y) <> comparing (^. _x)) rt
    , get $ maximumBy (comparing (^. _y) <> comparing (^. _x)) rt)

allSwitchHolesPos :: KBD [(Double, Double)]
allSwitchHolesPos = do
  (nc, nr, nt) <- traverseOf each asks (_nCols, _nRows, _nThumb)
  shp [0 .. nc - 1] [0 .. nr - 1] [0 .. nt - 1]
  where
    shp xr yr tr = do
      keys <- sequence [kpos x y | x <- xr, y <- yr]
      tkeys <- sequence [tpos i | i <- tr]
      return $ keys ++ tkeys

switchHoles :: (Transformable b, Monoid b, N b ~ Double, V b ~ V2) => b -> KBD b
switchHoles hole = do
  k <- ask
  ashp <- allSwitchHolesPos
  return $ rotate (_angle k) $ mconcat $ (\(x, y) -> hole # translate (V2 x y)) <$>
    ashp

roundHole :: Double -> Path V2 Double
roundHole = circle . (/ 2)

hexagonalHole :: Double -> Path V2 Double
hexagonalHole d =
  let r = d / (2.0 * sinA (60 @@ deg))
   in hexagon r

screwPos :: KBD [(Double, Double)]
screwPos = do
  sp <- asks _sep
  sw <- asks _spacerWidth
  tn <- asks _topNotch
  isSplit <- asks _split
  ps <- outlinePos
  let d = max 1 ((sw / 2) - 2)
      (br, tr, tl, bl) = (head ps, ps !! 1, ps !! 2, ps !! 4)
  return
    [ bl + (0, d)
    , br + (-d, d)
    , tr + (-d, -d)
    , tl +
      (if isJust tn && not isSplit
         then (15, -(fromJust tn + 6))
         else (sp / 2, -d))
    ]

placeRotated ::
     (Transformable b, Monoid b, N b ~ Double, V b ~ V2)
  => Angle Double
  -> [(Double, Double)]
  -> b
  -> b
placeRotated a ps s = mconcat $ (\p -> s # translate (r2 p) # rotate a) <$> ps

screwHoles :: KBD (Path V2 Double)
screwHoles = do
  let hole = asks _screwHole <*> asks _screwSize
  (placeRotated (0 @@ deg) <$> screwPos <*> hole) >>= mirrorP

outlinePos :: KBD [(Double, Double)]
outlinePos = do
  k <- ask
  (_, _, minyr, maxyr) <- boundaryPos (rotP (_angle k))
  (minx, maxx, miny, maxy) <- boundaryPos r2
  let dx = _rowSpacing k - 2
      dy = _columnSpacing k / 3 - 1
      w =
        (maxx ^. _x) - (minx ^. _x) + dx / 2 +
        _switchHoleSize k / 2
      h = (maxy ^. _y) - (miny ^. _y) + 2 * dy + _switchHoleSize k
      r =
        rect w h # alignBL # translate (r2 (_sep k / 2, -_switchHoleSize k / 2 - dy / 2))
      pt1 = minyr - p2 (0, _switchHoleSize k / 2 + dy + 4)
      pt2 = maxyr + p2 (0, _switchHoleSize k / 2 + dy + 3)
      r' = r # rotate (_angle k)
      ps = toVertices r'
      vs1 =
        [ head ps
        , p2 (head ps ^. _x, pt2 ^. _y)
        , p2 (0, pt2 ^. _y)
        , p2 (0, pt1 ^. _y)
        , pt1
        ]
      mask1 = adjP vs1
      np = fromJust $ maxTraceP (head ps) (unitY # rotate (_angle k)) mask1
  return $ fmap unp2 [head ps, np, p2 (0, pt2 ^. _y), p2 (0, pt1 ^. _y), pt1]

outline :: KBD (Path V2 Double)
outline = do
  ws <- asks _washerSize
  isSplit <- asks _split
  tn <- asks _topNotch
  vs2 <- outlinePos
  let s = adjP (p2 <$> vs2)
      t =
        if isSplit || isNothing tn
          then s
          else let r = 7 * fromJust tn
                   dpt = fromJust tn :: Double
                   tp =
                     fromJust (maxTraceP (mkP2 0 0) unitY s) #
                     translate (-unitY * pure dpt)
                   notch =
                     circle r # reversePath #
                     translate (unitY * pure (r - dpt / 2)) #
                     moveTo tp
                in difference Winding s notch
  roundPath (-ws / 3) <$> (t # mirrorP)

bottomPlate :: KBD (Path V2 Double)
bottomPlate = do
  o <- outline
  difference Winding o <$> screwHoles

addHooks :: Path V2 Double -> KBD (Path V2 Double)
addHooks p = do
  sp <- screwPos
  isSplit <- asks _split
  let (w, h, d) = (16, 3, 1.5)
      a = negated (signedAngleBetween unitX (r2 (sp !! 1) - r2 (head sp)))
      an =
        if isSplit
          then (-45 @@ deg)
          else (-20 @@ deg)
      hp =
        fromJust $ maxTraceP (fromJust (mCenterPoint p)) (unitX # rotate an) p
      hook =
        difference
          Winding
          (rect (w + d) 9)
          (rect w h # translate (-unitY * 2.5)) #
        roundPath (-0.5) #
        rotate a #
        moveTo hp
  mappend p <$> mirrorP hook

switchPlate :: KBD (Path V2 Double)
switchPlate = do
  let hole = asks _switchHole <*> asks _switchHoleSize
  hasHooks <- asks _hooks
  difference Winding <$>
    (if hasHooks
       then bottomPlate >>= addHooks
       else bottomPlate) <*>
    (hole >>= switchHoles >>= mirrorP)

serialNumber :: KBD (Path V2 Double)
serialNumber = do
  f <- asks _textFont
  n <- asks show
  d <- asks _date
  u <- toString <$> asks _uuid
  let t = ["MODEL: " ++ n, "SN   : " ++ u, "DATE : " ++ d]
      s = replicate (maximum (map length t)) '-'
      txt2svg t' =
        textSVG' (TextOpts f INSIDE_H KERN False 1 1) t' # reversePath # alignL
  return $ alignT $ center $ vsep 0.4 (map txt2svg (s : t ++ [s]))

topPlate :: KBD (Path V2 Double)
topPlate = do
  (sh, rs, cs) <-
    traverseOf each asks (_switchHoleSize, _rowSpacing, _columnSpacing)
  a <- asks _angle
  lg <- asks _logo
  ashp <- allSwitchHolesPos
  bp <- bottomPlate
  isSplit <- asks _split
  sn <- serialNumber
  let lg' =
        over (_Just . _3) ?? lg $ (\p -> vsep 5 [p, sn # scaleUToX (width p)])
      innerR = roundPath 1 $ rect (sh + rs / 4 + 1) (sh + cs / 4 + 1)
      mask = placeRotated a ashp innerR
      addLogo l p
        | isNothing l || isSplit = p
        | otherwise =
          let (w, d, l') = fromJust l
           in (l' # scaleUToX w # translate (pure d * unitY)) <>
              (p # reversePath)
  addLogo lg' <$> (difference Winding bp <$> mirrorP mask)

cableGuide :: Double -> Path V2 Double
cableGuide a =
  let c = a / 2
      d = -a / 2
   in mconcat (rect 3 (a + 2) : map (\x -> rect 5 1 # translateY x) [d,d + 2 .. c])

spacerPlate :: Double -> Path V2 Double -> KBD (Path V2 Double)
spacerPlate c cg = do
  k <- ask
  o <- outline
  sp <- screwPos
  sh <- screwHoles
  isSplit <- asks _split
  let w = 2.5 * _spacerWidth k
      punch = o # scaleToX (width o - w) # scaleToY (height o - w) # reversePath
      (c1, c2) = (cntr punch ^. _y, cntr o ^. _y)
        where
          cntr p = fromJust (mCenterPoint p)
      disks = placeRotated (0 @@ deg) sp (circle (_washerSize k / 3))
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
      tp = fromJust (maxTraceP (mkP2 0 0) unitY o)
  plate <- intersection Winding o <$> (mappend fr <$> mirrorP disks)
  let r = rect 20 8 # reversePath # roundPath (-2)
      plate2 =
        Winding `union`
        (plate <> (r # moveTo tp # translateY (-height r + (w / 4))))
      plate3 =
        if isSplit
          then plate
          else difference Winding plate2 (cg # moveTo tp # translateY c)
  return $ difference Winding plate3 sh

mkGradient :: Fractional n => Double -> Colour Double -> n -> Texture n
mkGradient o c w =
  let stops = mkStops [(c, 0, o), (white, 0.5, o), (c, 1, o)]
   in mkLinearGradient stops ((-w) ^& 0) (w ^& 0) GradPad

keycaps :: KBD (Diagram B)
keycaps = do
  hs <- asks _switchHoleSize
  let style c o = fcA (c `withOpacity` o)
      cap s = square s # roundPath 2
      cap' =
        (cap (hs - 5) # strokePath # fillTexture (mkGradient 0.1 black 5)) <>
        (cap (hs + 0.7) # strokePath # style black 0.7)
  cs <- switchHoles cap'
  mirror cs

screws :: KBD (Diagram B)
screws = do
  let hole =
        pure $ hexagonalHole 5.5 # strokePath # fcA (black `withOpacity` 0.5)
  (placeRotated (0 @@ deg) <$> screwPos <*> hole) >>= mirror

render :: KBDCfg -> IO ()
render k = do
  let parts =
        (`runReader` k) <$>
        [ bottomPlate
        , spacerPlate 0 (cableGuide 15)
        , spacerPlate (-15) (cableGuide 15)
        , switchPlate
        , topPlate
        ]
      dpi = 96
      sf = dpi / 25.4
      lineW = sf * 0.1
      (kc, scrs) = over each (`runReader` k) (keycaps, screws)
      gradient = mkGradient 1 gray (w / 4)
        where
          w = width $ head parts
      aStyles =
        fillTexture gradient :
        fmap (\c -> fcA (c `withOpacity` 0.5)) (cycle [yellow, yellow, black, blue])
      diagram = reverse $ zipWith (\s p -> strokePath p # s) aStyles parts
      project = frame 1.05 (vsep 5 diagram) # lwO lineW
      assembly = frame 1.05 $ mconcat (scrs : kc : diagram) # lwO lineW
      sizeSp d = dims2D (sf * width d) (sf * height d)
      generate n d = do
        let sp = sizeSp d
        putStrLn $ "Generating '" ++ n ++ "', " ++ show sp
        renderSVG n sp d
  generate ("images/" ++ show k ++ ".svg") project
  generate ("images/" ++ show k ++ "_a.svg") assembly
  blp <- findExecutable "blender"
  case blp of
    Just fp ->
      callProcess
        fp
        [ "--background"
        , "--factory-startup"
        , "--python"
        , "svgto3dpng.py"
        , "--"
        , "images/" ++ show k ++ "_a.svg"
        ]
    Nothing -> return ()

svgToPath :: FilePath -> IO (Path V2 Double)
svgToPath f = do
  l <- fromJust <$> S.loadSvgFile f
  let pc =
        l ^.. sElements . traverse . _GroupTree . groupChildren . traverse .
        _PathTree .
        pathDefinition
      update (o, as)
        | o == S.OriginRelative = scanl1 (+) bs
        | otherwise = bs
        where
          bs = fmap ((_2 %~ (* (-1))) . unr2) as
      ps =
        fmap
          ((mconcat . fmap update) .
           (^.. traverse . (_MoveTo `failing` _LineTo)))
          pc
  return . center . hsep 3 $ fmap (center . fromVertices . fmap p2) ps

gallery :: (Foldable t, Show a) => t a -> IO ()
gallery ks =
  let gen k =
        let entry = show k
         in unlines
              [ "## " ++ entry ++ "\n"
              , "Rendered in Blender\n"
              , "![" ++ entry ++ "3d](images/" ++ entry ++ "_a.png)\n"
              , "[" ++ entry ++ " STL file](images/" ++ entry ++ "_a.stl)\n"
              , "SVG files for CNC cutting\n"
              , "![" ++ entry ++ "a](images/" ++ entry ++ "_a.svg)\n"
              , "![" ++ entry ++ "](images/" ++ entry ++ ".svg)\n"
              ]
   in writeFile "GALLERY.md" $ "# Gallery\n\n" ++ concatMap gen ks

main :: IO ()
main = do
  l <- svgToPath "logo.svg"
  f <- bit
  now <- getCurrentTime
  let ds = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S.%3q" now
  u <- fromJust <$> nextUUID
  let ang = 18.5 @@ deg
      atreus42 =
        KBDCfg
          { _nRows = 4
          , _nCols = 5
          , _nThumb = 1
          , _columnSpacing = 19
          , _rowSpacing = 19
          , _switchHoleSize = 13.97
          , _switchHole = switchHoleNotched
          , _spacerWidth = 6
          , _angle = ang
          , _staggering = [0, 5, 11, 6, 3, 2]
          , _screwSize = 3
          , _washerSize = 13
          , _screwHole = roundHole
          , _sep = 60
          , _hooks = False
          , _split = False
          , _topNotch = Nothing
          , _logo = Just (35, 45, l)
          , _textFont = f
          , _date = ds
          , _uuid = u
          }
      smallBase =
        atreus42 & nThumb .~ 0 & logo .~ Nothing & angle .~ (0.001 @@ deg) &
        staggering .~ repeat 0 &
        sep .~ 30
      atreus32 = smallBase & nRows .~ 4 & nCols .~ 4
      atreus44 = atreus42 & nThumb .~ 2
      atreus50 = atreus42 & nCols .~ 6 & (topNotch ?~ 15)
      atreus52h = atreus50 & nThumb .~ 2 & hooks .~ True
      atreus52s =
        atreus52h & split .~ True & hooks .~ False & angle .~ (0 @@ deg) &
        sep .~ 45
      atreus62 = atreus50 & nRows .~ 5 & sep .~ 55
      atreus62s = atreus62 & split .~ True
      atreus206 =
        atreus42 & nCols .~ 10 & nRows .~ 10 & nThumb .~ 3 & sep .~ 80 &
        (logo ?~ (60, 80, l)) &
        (topNotch ?~ 15)
      atreus208 = atreus206 & nThumb .~ 4
      atreus210 = atreus208 & nThumb .~ 5 & (logo ?~ (80, 90, l))
      ks =
        [ atreus32
        , atreus42
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
  gallery ks
  P.mapM_ render ks
