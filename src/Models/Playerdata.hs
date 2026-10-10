{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Models.Playerdata where

import Components.Grid_tile_component (Rect)
import Constants.Game_constants (playerSize)

data Direction = Up | Down | Left | Right
  deriving (Eq, Show)

data Bomberman = Bomberman
  { pos :: (Double, Double)
  , dir :: Direction
  , alive :: Bool
  }
  deriving (Eq, Show)

-- The player's hitbox: a playerSize square anchored at pos.
playerRect :: Bomberman -> Rect
playerRect player = (fst player.pos, snd player.pos, playerSize, playerSize)

playerCenter :: Bomberman -> (Double, Double)
playerCenter player =
  ( fst player.pos + playerSize / 2
  , snd player.pos + playerSize / 2
  )
