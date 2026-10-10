module Components.Grid_tile_component where

import Constants.Game_constants
import Enums.Tile_types

import Data.Array (Array, assocs, listArray, (!), (//))
import qualified Data.Set as Set

type Cell = (Int, Int)

type Grid = Array Cell TileType

type Rect = (Double, Double, Double, Double)

inBounds :: Cell -> Bool
inBounds (c, r) = c >= 0 && c < gridCols && r >= 0 && r < gridRows

hardCells :: [Cell]
hardCells =
  [ (c, r)
  | r <- [0 .. gridRows - 1]
  , c <- [0 .. gridCols - 1]
  , r == 0 || r == gridRows - 1 || c == 0 || c == gridCols - 1
    || (even c && even r)
  ]

baseGrid :: Grid
baseGrid =
  listArray ((0, 0), (gridCols - 1, gridRows - 1))
    [ if Set.member (c, r) hardSet then HardBlock else Empty
    | c <- [0 .. gridCols - 1]
    , r <- [0 .. gridRows - 1]
    ]
  where
    hardSet = Set.fromList hardCells

walkableCells :: [Cell]
walkableCells = [ (c, r) | ((c, r), tile) <- assocs baseGrid, tile == Empty ]

softBlockCount :: Int
softBlockCount = 12

softCells :: [Cell]
softCells =
  take softBlockCount
    [ (c, r)
    | (i, (c, r)) <- zip [0 :: Int ..] walkableCells
    , i `mod` 3 == 1
    , not (isNearSpawn (c, r))
    ]

isNearSpawn :: Cell -> Bool
isNearSpawn (c, r) = abs (c - sc) <= 1 && abs (r - sr) <= 1
  where
    (sc, sr) = spawnCell

initialGrid :: Grid
initialGrid = baseGrid // [ (c, SoftBlock) | c <- softCells ]

tileAt :: Grid -> Cell -> TileType
tileAt grid cell
  | inBounds cell = grid ! cell
  | otherwise = HardBlock

destroyCells :: [Cell] -> Grid -> Grid
destroyCells cells grid = grid // [ (c, Empty) | c <- cells ]

blockingTile :: Grid -> Cell -> Bool
blockingTile grid cell = tileAt grid cell /= Empty

cellToPixels :: Cell -> (Double, Double)
cellToPixels (c, r) = (fromIntegral (c * tileSize), fromIntegral (r * tileSize))

cellCenter :: Cell -> (Double, Double)
cellCenter (c, r) =
  ( fromIntegral c * tile + tile / 2
  , fromIntegral r * tile + tile / 2
  )
  where
    tile = fromIntegral tileSize

cellRect :: Cell -> Rect
cellRect (c, r) = (fromIntegral (c * tileSize), fromIntegral (r * tileSize), tile, tile)
  where
    tile = fromIntegral tileSize

nearestCell :: (Double, Double) -> Cell
nearestCell (x, y) =
  ( floor (x / tile + 0.5)
  , floor (y / tile + 0.5)
  )
  where
    tile = fromIntegral tileSize

rectsOverlap :: Rect -> Rect -> Bool
rectsOverlap (ax, ay, aw, ah) (bx, by, bw, bh) =
  ax < bx + bw && bx < ax + aw && ay < by + bh && by < ay + ah

rectCells :: Rect -> [Cell]
rectCells (x, y, w, h) =
  [ (c, r)
  | c <- [floor (x / tile) .. floor ((x + w - 1) / tile)]
  , r <- [floor (y / tile) .. floor ((y + h - 1) / tile)]
  ]
  where
    tile = fromIntegral tileSize
