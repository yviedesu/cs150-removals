{-# LANGUAGE OverloadedRecordDot #-}

-- Composition root. Domain logic lives in Components/, Constants/, Enums/
-- and Models/; this module only wires the frame loop together and re-exports
-- the assembled game API for Main, Controller, View and the test-suite.
module Model
  ( module Constants.Game_constants
  , module Components.Bomb_component
  , module Components.Explosion_component
  , module Components.Grid_tile_component
  , module Components.Player_component
  , module Enums.Tile_types
  , module Models.Gamestate
  , module Models.Playerdata
  , Msg (..)
  , step
  , simulate
  ) where

import Prelude hiding (Left, Right)

import Constants.Game_constants
import Components.Bomb_component
import Components.Explosion_component
import Components.Grid_tile_component
import Components.Player_component
import Enums.Tile_types
import Models.Gamestate
import Models.Playerdata

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

data Msg
  = MsgGetTime
  | MsgSetTime Double
  | MsgKeyCodes IntSet
  deriving (Eq, Show)

step :: Msg -> Model -> (Model, [Msg])
step MsgGetTime m = (m, [])
step (MsgSetTime t) m
  | m.state == GameOver = (m, [])
  | otherwise = (m', [MsgGetTime | m'.state == Running])
  where
    m' = simulate t m
step (MsgKeyCodes kc) m
  | m.state == GameOver = (m { keys = kc }, [])
  | otherwise =
      ( m { keys = kc, bombs = plantBomb justPressed m.grid m.player m.nowMs m.bombs }
      , []
      )
  where
    justPressed = IS.member keySpace kc && not (IS.member keySpace m.keys)

simulate :: Double -> Model -> Model
simulate t m0 = checkGameOver afterFlames
  where
    dt = max 0 (min 100 (t - m0.nowMs))
    m1 = m0 { nowMs = t }
    moved = stepPlayer dt m1.keys m1.grid m1.bombs m1.player
    m2 = m1 { player = moved }
    m3 = tickClock dt m2
    (g', kept, flames) = tickBombs t m3.grid m3.bombs
    m4 = m3 { grid = g', bombs = kept, explosions = m3.explosions ++ flames }
    afterFlames = m4 { explosions = tickExplosions t m4.explosions }
