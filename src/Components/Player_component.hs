{-# LANGUAGE OverloadedRecordDot #-}

module Components.Player_component where

import Prelude hiding (Left, Right)

import Constants.Game_constants
import Components.Bomb_component (Bomb (..), bombSolid)
import Components.Grid_tile_component
import Enums.Tile_types
import Models.Playerdata

import Data.Array ((//))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

initialPlayer :: Bomberman
initialPlayer = Bomberman { pos = cellToPixels spawnCell, dir = Down, alive = True }

movePlayer :: Grid -> Bomberman -> (Double, Double) -> Bomberman
movePlayer grid player (dx, dy)
  | not player.alive = player
  | otherwise = player { pos = (nextX, nextY) }
  where
    (x, y) = player.pos
    blockedX = any (blockingTile grid) (rectCells (x + dx, y, playerSize, playerSize))
    blockedY = any (blockingTile grid) (rectCells (x, y + dy, playerSize, playerSize))
    nextX = if blockedX then x else x + dx
    nextY = if blockedY then y else y + dy

-- A bomb overlapping the player is passable (he can walk off a freshly
-- planted bomb); once his hitbox fully clears the bomb's cell it is solid
-- and blocks him like a wall. Solidity has to be judged from the player's
-- current position: judged at the movement destination instead, the very
-- step that walks him off the bomb gets rejected, trapping him in its cell.
moveAxis :: Grid -> [Bomb] -> Bomberman -> (Double, Double) -> (Double, Double)
moveAxis grid bombs p (dx, dy) = moved.pos
  where
    moved = movePlayer gridWithSolidBombs p (dx, dy)
    gridWithSolidBombs =
      grid // [ (b.cell, HardBlock) | b <- bombs, bombSolid p b ]

-- Orthogonal-only: when both axes are held, horizontal wins, so two arrow
-- keys never produce diagonal movement.
inputVector :: IntSet -> Cell
inputVector ks = if dx /= 0 then (dx, 0) else (0, dy)
  where
    dx = (if IS.member keyRight ks then 1 else 0) + (if IS.member keyLeft ks then -1 else 0)
    dy = (if IS.member keyDown ks then 1 else 0) + (if IS.member keyUp ks then -1 else 0)

dirOf :: Int -> Int -> Direction -> Direction
dirOf 0 0 d = d
dirOf dx dy _ = fromVector dx dy

fromVector :: Int -> Int -> Direction
fromVector dx dy
  | dx < 0 = Left
  | dx > 0 = Right
  | dy < 0 = Up
  | otherwise = Down

-- One player tick: read held keys, move at playerSpeed px/ms, face the
-- movement direction. Bomberman never moves on Game Over (the root step
-- never calls this once state /= Running).
stepPlayer :: Double -> IntSet -> Grid -> [Bomb] -> Bomberman -> Bomberman
stepPlayer dt keys grid bombs p0 = p0 { pos = (fx, fy), dir = dirOf dx dy p0.dir }
  where
    (dx, dy) = inputVector keys
    dist = playerSpeed * dt
    (fx, fy) = moveAxis grid bombs p0 (fromIntegral dx * dist, fromIntegral dy * dist)
