module BinPartOrg where

import Graphics.Implicit
import Lib


main :: IO ()
main = do
  writeSTL 0.5 "/tmp/binpart.stl" $ do
    -- difference
        union
          [ chambers p_spacing p_height p_size $ bpMxN 4 3
          , atCorners p_size $ stackingPin p_height
          ]
      -- , atCorners p_size $ pinHead p_head_rad



data BinPart
  = Full
  | SplitW [BinPart]
  | SplitH [BinPart]
  deriving (Eq, Ord, Show)


p_height :: R
p_height = 0

p_pin_rad :: R
p_pin_rad = 3

p_base :: R
p_base = p_thickness

p_size :: R2
p_size = mk2 20 20

p_thickness :: R
p_thickness = 2.5

p_spacing :: R
p_spacing = 1.6

p_head_height :: R
p_head_height = 3

p_head_rad :: R
p_head_rad = 2

p_hole_rad :: R
p_hole_rad = p_head_rad * 1.2



------------------------------------------------------------------------------

chamber :: R -> R2 -> R2 -> SymbolicObj3
chamber h xy wd = extrude h $ translate xy $ rectR 0 (mk2 0 0) wd

chambers :: R -> R -> R2 -> BinPart -> SymbolicObj3
chambers sp h (w, d) bp =
  unionR 2
    [ translate (mk3 0 0 p_base) $
        differenceR 0.5
          [ box w d (h - p_base)
          , union
              $ fmap (uncurry $ chamber (h - p_base))
              $ partition
                  sp
                  (mk2 p_thickness p_thickness)
                  (mk2 (w - 2 * p_thickness) (d - 2 * p_thickness))
              $ bp
          ]
    , rect3R 5 (0, 0, 0) (w, d, p_base)
    ]

atCorners :: R2 -> SymbolicObj3 -> SymbolicObj3
atCorners (w, d) obj = union
  [ translate (mk3 0 0 0) obj
  , translate (mk3 w 0 0) obj
  , translate (mk3 w d 0) obj
  , translate (mk3 0 d 0) obj
  ]


partition :: R -> R2 -> R2 -> BinPart -> [(R2, R2)]
partition _ xy wh Full = pure (xy, wh)
partition sp (x, y) (w, h) (SplitW bps) = do
  let n = length bps
      w' = (w - fromIntegral (n - 1) * sp) / fromIntegral n
      x' n = x + (w' + sp) * fromIntegral n
  (ix, bp) <- zip [0..] bps
  partition sp (mk2 (x' ix) y) (mk2 w' h) bp
partition sp (x, y) (w, h) (SplitH bps) = do
  let n = length bps
      h' = (h - fromIntegral (n - 1) * sp) / fromIntegral n
      y' n = y + (h' + sp) * fromIntegral n
  (ix, bp) <- zip [0..] bps
  partition sp (mk2 x (y' ix)) (mk2 w h') bp


bpMxN :: Int -> Int -> BinPart
bpMxN m n = SplitW $ replicate m col
  where
    col = SplitH $ replicate n Full


stackingPin :: R -> SymbolicObj3
stackingPin h =
  difference
    [ union
        [ cylinder p_pin_rad h
        , translate (mk3 0 0 h) $ pinHead p_head_rad
        ]
    , pinHead p_hole_rad
    ]


pinHead :: Double -> SymbolicObj3
pinHead rad = union
  [ cylinder rad p_head_height
  , translate (mk3 0 0 p_head_height) $ sphere rad
  ]

