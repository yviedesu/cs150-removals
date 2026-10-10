{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Models.Gamestate where

import Constants.Game_constants
import Components.Bomb_component (Bomb)
import Components.Explosion_component (Explosion, playerBurned)
import Components.Grid_tile_component (Grid, initialGrid)
import Components.Player_component (initialPlayer)
import Models.Playerdata (Bomberman (..))

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

data GameState = Running | GameOver
  deriving (Eq, Show)

data Model = Model
  { grid :: Grid
  , player :: Bomberman
  , bombs :: [Bomb]
  , explosions :: [Explosion]
  , keys :: IntSet
  , state :: GameState
  , nowMs :: Double
  , secondsLeft :: Double
  }
  deriving (Eq, Show)

initialModel :: Model
initialModel = Model
  { grid = initialGrid
  , player = initialPlayer
  , bombs = []
  , explosions = []
  , keys = IS.empty
  , state = Running
  , nowMs = 0
  , secondsLeft = gameTimerDurationS
  }

tickClock :: Double -> Model -> Model
tickClock dt m = m { secondsLeft = max 0 (m.secondsLeft - dt / 1000) }

formatClock :: Double -> String
formatClock sec =
  let total = max 0 (ceiling sec :: Int)
  in pad (total `div` 60) ++ ":" ++ pad (total `mod` 60)
  where
    pad n = if n < 10 then '0' : show n else show n

checkGameOver :: Model -> Model
checkGameOver m
  | m.state /= Running = m
  | m.secondsLeft <= 0 = m { state = GameOver }
  | playerBurned m.nowMs m.player m.explosions =
      m { state = GameOver, player = m.player { alive = False } }
  | otherwise = m
