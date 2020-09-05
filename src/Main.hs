{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Main where

import           Control.Lens               hiding ((#), parts, plate)
import           Control.Monad.Reader

import           System.Process

import           Data.List                  (minimumBy)
import           Data.List.Index            (insertAt)
import           Data.Maybe                 (fromJust, fromMaybe, isNothing)
import           Data.Ord                   (comparing)
import           Data.Time.Clock
import           Data.Time.Format
import           Data.UUID                  (UUID, toString)
import           Data.UUID.V1               (nextUUID)

import           Diagrams.Backend.SVG
import           Diagrams.Path
import           Diagrams.Prelude           hiding (connect, difference,
                                             fromVertices, intersection, parts,
                                             plate, project, render, sep, trace,
                                             union, _sep)

import           Diagrams.TwoD.Offset
import           Diagrams.TwoD.Path.Boolean

import qualified Graphics.Svg               as S
import           Graphics.SVGFonts
import           Graphics.SVGFonts.ReadFont (PreparedFont)

import           System.Directory           (findExecutable)

type KBD = Reader KBDCfg

data KBDCfg = KBDCfg
  { _nRows          :: Int
  , _nCols          :: Int
  , _thumb          :: Either [(Int, Int)] Int
  , _lowerOffset    :: Maybe Double
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
    show (2 * (_nRows k * _nCols k + nt)) ++
    (if _split k
       then "s"
       else "") ++
    ts
    where
      (nt, ts) =
        case _thumb k of
          Right a -> (a, "")
          Left a  -> (length a, "ct")

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

tpos :: KBD [(Double, Double)]
tpos = do
  k <- ask
  (sx, sy) <- kpos 0 0
  let ts =
        case _thumb k of
          Right a -> [(i `quot` 2, i `rem` 2) | i <- [0 .. a - 1]]
          Left a  -> a
      i2pos (i, j) =
        ( sx - (fromIntegral i + 1) * _rowSpacing k
        , sy - fromIntegral j * _columnSpacing k +
          if i >= 0
            then _columnSpacing k / 3
            else 0)
  return $
    fmap i2pos ts &
    if odd (length ts) && length ts > 1 && fst (last ts) >= 0
      then (ix (length ts - 1) . _2) %~ (\x -> x - _columnSpacing k / 3)
      else id

kpos :: Int -> Int -> KBD (Double, Double)
kpos m n = do
  k <- ask
  st <- staggering'
  nt <- length <$> tpos
  let ntq = fromIntegral $ (nt + 1) `quot` 2
  let x = ntq * _rowSpacing k + (_sep k / 2)
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

allSwitchHolesPos :: KBD [(Double, Double)]
allSwitchHolesPos = do
  (nc, nr) <- traverseOf each asks (_nCols, _nRows)
  shp [0 .. nc - 1] [0 .. nr - 1]
  where
    shp xr yr = do
      keys <- sequence [kpos x y | x <- xr, y <- yr]
      tkeys <- tpos
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
  hs <- asks _switchHoleSize
  a <- asks _angle
  isSplit <- asks _split
  (nc, nr) <- traverseOf each asks (_nCols, _nRows)
  tp <- (:) <$> kpos 0 0 <*> tpos
  let mtp = p2 $ minimumBy (comparing snd <> comparing fst) tp
  ps <-
    mapM (fmap p2 . uncurry kpos) [(nc - 1, 0), (0, nr - 1), (nc - 1, nr - 1)]
  let o = ((hs * sqrt 2) / 2) + 3
      offs =
        fmap
          p2
          [ if a < 10 @@ deg
              then (0, -hs)
              else (-o, -o)
          , (o, -o)
          , (0, hs)
          , (o, o)
          ]
      ps' = unp2 . rotate a <$> zipWith (+) (mtp : ps) offs
  return $
    if isSplit
      then ps'
      else ps' & ix 2 . _1 .~ (15 / 2)

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

connect :: Path V2 Double -> KBD (Path V2 Double)
connect p = do
  isSplit <- asks _split
  a <- asks _angle
  k <- rotate a . p2 <$> kpos 0 0
  lo <- fromMaybe 0 <$> asks _lowerOffset
  let u = fromJust $ maxTraceP k (rotate a unitY) p
      d = fromJust (maxTraceP k (rotate a (-unitY)) p) & _y %~ (+lo)
      c = adjP [p2 (0, u ^. _y), u, d, p2 (0, d ^. _y)]
  return $
    if isSplit
      then p
      else mconcat [p, c]

outline :: KBD (Path V2 Double)
outline = do
  o <- roundPath (-7) <$> switchesCutout
  connect o >>= mirrorP

bottomPlate :: KBD (Path V2 Double)
bottomPlate = union Winding <$> outline

switchPlate :: KBD (Path V2 Double)
switchPlate = do
  let hole = asks _switchHole <*> asks _switchHoleSize
  difference Winding <$> bottomPlate <*> (hole >>= switchHoles >>= mirrorP)

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

switchesCutout :: KBD (Path V2 Double)
switchesCutout = do
  (sh, rs, cs) <-
    traverseOf each asks (_switchHoleSize, _rowSpacing, _columnSpacing)
  a <- asks _angle
  ashp <- allSwitchHolesPos
  let innerR = roundPath 1 $ rect (sh + rs / 4 + 1) (sh + cs / 4 + 1)
      mask = placeRotated a ashp innerR
  return $ union Winding mask

topPlate :: KBD (Path V2 Double)
topPlate = do
  lg <- asks _logo
  bp <- outline
  isSplit <- asks _split
  mask <- switchesCutout
  let lg' = over (_Just . _3) ?? lg $ (\p -> vsep 5 [p])
      addLogo l p
        | isNothing l || isSplit = p
        | otherwise =
          let (w, d, l') = fromJust l
           in p <> (l' # scaleUToX w # translate (pure d * unitY) # reversePath)
  addLogo lg' <$> (difference Winding bp <$> mirrorP mask)

cableGuide :: Double -> Path V2 Double
cableGuide a =
  let c = a / 2
      d = -a / 2
   in mconcat (rect 3 (a + 2) : map (\x -> rect 5 1 # translateY x) [d,d + 2 .. c])

spacerPunch :: KBD (Path V2 Double)
spacerPunch = do
  sp <- screwPos
  ws <- asks _washerSize
  p1' <- roundPath (-1) <$> switchesCutout
  p2' <- connect p1' >>= mirrorP
  disks <- mirrorP $ placeRotated (0 @@ deg) sp (circle (ws / 3))
  return $ difference Winding p2' disks

spacerPlate :: KBD (Path V2 Double)
spacerPlate = do
  o <- outline
  difference Winding o <$> spacerPunch

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

render :: KBDCfg -> IO ()
render k = do
  let drillHoles p = (<>) <$> p <*> (reversePath <$> screwHoles)
      parts =
        (`runReader` k) <$> insertAt 1 spacerPunch (fmap drillHoles
        [ bottomPlate
        , spacerPlate
        , switchPlate
        , topPlate
        ])
      dpi = 96
      sf = dpi / 25.4
      lineW = sf * 0.1
      kc = (`runReader` k) keycaps
      aStyles = fmap (\c -> fcA (c `withOpacity` 0.5)) (cycle [gray, gray, yellow, black, blue])
      diagram = reverse $ zipWith (\s p -> strokePath p # s) aStyles parts
      project = frame 1.05 (vsep 5 diagram) # lwO lineW
      assembly = frame 1.05 $ mconcat (kc : diagram) # lwO lineW
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
      zeroAng = 0.001 @@ deg
      atreus42 =
        KBDCfg
          { _nRows = 4
          , _nCols = 5
          , _thumb = Right 1
          , _lowerOffset = Nothing
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
          , _sep = 50
          , _split = False
          , _topNotch = Nothing
          , _logo = Just (35, 35, l)
          , _textFont = f
          , _date = ds
          , _uuid = u
          }
      smallBase =
        atreus42 & thumb .~ Right 0 & logo .~ Nothing & angle .~ zeroAng &
        staggering .~ repeat 0 &
        sep .~ 38
      atreus8 = smallBase & nRows .~ 1 & nCols .~ 4
      atreus10 = atreus8 & thumb .~ Right 1 & angle .~ ang
      atreus32 = smallBase & nRows .~ 4 & nCols .~ 4
      atreus44 = atreus42 & thumb .~ Right 2
      atreus50 = atreus42 & nCols .~ 6 & (topNotch ?~ 15)
      atreus52 = atreus50 & thumb .~ Right 2
      atreus52ct =
        atreus50 & thumb .~ Left [(0, 0), (-1, 1)] & lowerOffset ?~ 12
      atreus52s = atreus52 & split .~ True & angle .~ zeroAng & sep .~ 45
      atreus54 = atreus50 & thumb .~ Right 3 & sep .~ 40
      atreus62 = atreus50 & nRows .~ 5 & sep .~ 55
      atreus62s = atreus62 & split .~ True
      atreus72 = atreus50 & nRows .~ 5 & nCols .~ 7 & sep .~ 60
      atreus206 =
        atreus42 & nCols .~ 10 & nRows .~ 10 & thumb .~ Right 3 & sep .~ 80 &
        (logo ?~ (60, 80, l)) &
        (topNotch ?~ 15)
      atreus208 = atreus206 & thumb .~ Right 4
      atreus210 = atreus208 & thumb .~ Right 5 & (logo ?~ (80, 90, l))
      ks =
        [ atreus8
        , atreus10
        , atreus32
        , atreus42
        , atreus44
        , atreus50
        , atreus52
        , atreus52s
        , atreus52ct
        , atreus54
        , atreus62
        , atreus62s
        , atreus72
        , atreus206
        , atreus208
        , atreus210
        ]
  gallery ks
  mapM_ render ks
