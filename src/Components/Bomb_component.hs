{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE FlexibleContexts #-}

module Components.Bomb_component where

import Constants.Game_constants
import Components.Explosion_component (Explosion (..), explosionCells, softBlocksDestroyed)
import Components.Grid_tile_component
import Enums.Tile_types
import Models.Playerdata

import Data.List (partition)
import qualified Data.Set as Set

data Bomb = Bomb
  { cell :: Cell
  , plantedAt :: Double
  }
  deriving (Eq, Show)

-- set bomb overlapping bomberman's hitbox to passable
-- turns solid the moment his hitbox fully clears the bomb's cell
bombSolid :: Bomberman -> Bomb -> Bool
bombSolid player bomb = not (rectsOverlap (playerRect player) (cellRect bomb.cell))

-- bomb goes to the tile nearest the player's center only on a walkable tile and never past maxActiveBombs
plantBomb :: Bool -> Grid -> Bomberman -> Double -> [Bomb] -> [Bomb]
plantBomb justPressed grid player now bombs
  | justPressed && canPlant = bombs ++ [Bomb { cell = target, plantedAt = now }]
  | otherwise = bombs
  where
    target = nearestCell (playerCenter player)
    canPlant = tileAt grid target == Empty && length bombs < maxActiveBombs

tickBombs :: Double -> Grid -> [Bomb] -> (Grid, [Bomb], [Explosion])
tickBombs t grid bombs =
  let (flames, kept, g') = detonateAll t grid bombs
  in (g', kept, [Explosion { cells = fs, startedAt = t } | fs <- flames])

detonateAll :: Double -> Grid -> [Bomb] -> ([[Cell]], [Bomb], Grid)
detonateAll t g0 bs0 = go Set.empty g0 bs0 []
  where
    go flame g bs acc =
      let (nowBoom, rest) = partition (shouldBoom flame) bs
      in if null nowBoom
           then (reverse acc, rest, g)
           else
             let cells = concatMap (\b -> explosionCells g b.cell) nowBoom
                 wrecked = concatMap (\b -> softBlocksDestroyed g b.cell) nowBoom
                 flame' = flame `Set.union` Set.fromList cells
                 g' = destroyCells wrecked g
             in go flame' g' rest (cells : acc)
    shouldBoom flame b = t - b.plantedAt >= bombTimerMs || Set.member b.cell flame
