{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Components.Explosion_component where

import Constants.Game_constants
import Components.Grid_tile_component
import Enums.Tile_types
import Models.Playerdata

data Explosion = Explosion
  { cells :: [Cell]
  , startedAt :: Double
  }
  deriving (Eq, Show)

explosionCells :: Grid -> Cell -> [Cell]
explosionCells grid start =
  start : concat [ fs | (fs, _) <- map (ray grid start) dirs4 ]

softBlocksDestroyed :: Grid -> Cell -> [Cell]
softBlocksDestroyed grid start =
  concat [ ds | (_, ds) <- map (ray grid start) dirs4 ]

dirs4 :: [(Int, Int)]
dirs4 = [(1, 0), (-1, 0), (0, 1), (0, -1)]

-- One cardinal ray, up to bombRange tiles: (flame cells, destroyed soft blocks). 
-- Hard blocks stop the ray outright, a soft block is destroyed and also stops it.
ray :: Grid -> Cell -> (Int, Int) -> ([Cell], [Cell])
ray grid start delta = go (add start delta) bombRange ([], [])
  where
    go _ 0 acc = acc
    go cell n (fs, ds) =
      case tileAt grid cell of
        HardBlock -> (fs, ds)
        SoftBlock -> (fs, cell : ds)
        Empty -> go (add cell delta) (n - 1) (cell : fs, ds)
    add (c, r) (dc, dr) = (c + dc, r + dr)

-- explosions fade after explosionDurationMs.
tickExplosions :: Double -> [Explosion] -> [Explosion]
tickExplosions t es = [ e | e <- es, t - e.startedAt < explosionDurationMs ]

-- player dies if any live flame overlaps his hitbox.
playerBurned :: Double -> Bomberman -> [Explosion] -> Bool
playerBurned now player es =
  or
    [ rectsOverlap (playerRect player) (cellRect c)
    | e <- es
    , let age = now - e.startedAt
    , age >= 0 && age < explosionDurationMs
    , c <- e.cells
    ]
