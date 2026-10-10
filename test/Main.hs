{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import qualified Data.Array as A
import qualified Data.IntSet as IS
import Data.IORef
import Data.List (nub)
import qualified Data.Set as Set
import System.Exit (ExitCode (..), exitWith)

import Model (Msg (MsgKeyCodes, MsgSetTime), detonateAll)
import qualified Model as M

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  modelTests failures
  controllerTests failures
  n <- readIORef failures
  if n == 0
    then putStrLn "All tests passed."
    else putStrLn (show n ++ " test(s) failed.")
  exitWith (if n == 0 then ExitSuccess else ExitFailure 1)

check :: IORef Int -> String -> Bool -> IO ()
check ref label ok = do
  putStrLn (if ok then "PASS " ++ label else "FAIL " ++ label)
  if ok then pure () else modifyIORef' ref (+ 1)

applyKey :: IS.IntSet -> M.Model -> M.Model
applyKey kc m = fst (M.step (MsgKeyCodes kc) m)

applyTick :: Double -> M.Model -> M.Model
applyTick t m = fst (M.step (MsgSetTime t) m)

hardInterior :: M.Grid -> Int
hardInterior g =
  length
    [ ()
    | (cell, t) <- A.assocs (A.ixmap ((2, 2), (12, 10)) id g)
    , even (fst cell)
    , even (snd cell)
    , t == M.HardBlock
    ]

eCount :: M.TileType -> M.Grid -> Int
eCount t g = length [ () | (_, x) <- A.assocs g, x == t ]

modelTests :: IORef Int -> IO ()
modelTests ref = do
  let g = M.initialGrid
  check ref "interior hard blocks = 30" (hardInterior g == 30)
  check ref "hard cells in-grid = 82" (eCount M.HardBlock g == 82)
  check ref "soft blocks = 12" (eCount M.SoftBlock g == 12)
  check ref "floor cells = 101" (eCount M.Empty g == 13 * 15 - 82 - 12)
  check ref "soft blocks avoid spawn area" (all (not . M.isNearSpawn) M.softCells)
  check ref "soft blocks are unique" (length (nub M.softCells) == length M.softCells)
  check ref "spawn cell is empty" (M.tileAt g M.spawnCell == M.Empty)
  check ref "out-of-bounds tiles are hard" (M.tileAt g (-1, 0) == M.HardBlock)
  check ref "spawn neighbor (2,1) walkable" (M.tileAt g (2, 1) == M.Empty)
  check ref "hard block stops up-move" (M.movePlayer g M.initialPlayer (0, -100) == M.initialPlayer)
  let gs = A.accum (\_ new -> new) g [((2, 1), M.SoftBlock)]
  check ref "soft block stops 40px right-move"
    (M.movePlayer gs (M.initialPlayer { M.pos = (40, 40) }) (40, 0) == M.initialPlayer { M.pos = (40, 40) })
  check ref "free floor move reaches x=80"
    (M.movePlayer g (M.initialPlayer { M.pos = (40, 40) }) (40, 0) == M.initialPlayer { M.pos = (80, 40) })
  let bombAtSpawn = M.Bomb { M.cell = M.spawnCell, M.plantedAt = 0 }
  check ref "bomb not solid while standing on it (walk-off)" (not (M.bombSolid M.initialPlayer bombAtSpawn))
  check ref "bomb solid once player leaves cell"
    (M.bombSolid (M.initialPlayer { M.pos = (90, 40) }) bombAtSpawn)
  let boom = M.explosionCells g M.spawnCell
  check ref "explosion includes bomb cell" (Set.member M.spawnCell (Set.fromList boom))
  check ref "explosion includes open neighbour"
    (Set.member (2, 1) (Set.fromList boom))
  check ref "explosion stops at hard perimeter above spawn" (not (Set.member (1, 0) (Set.fromList boom)))
  check ref "flame range is 1 tile" (not ((3, 1) `elem` M.explosionCells M.baseGrid M.spawnCell))
  check ref "soft block neighbour destroyed but not flamed"
    ( (2, 1) `elem` M.softBlocksDestroyed gs M.spawnCell
        && not ((2, 1) `elem` M.explosionCells gs M.spawnCell)
    )
  check ref "nearestCell (55,55) = (1,1)" (M.nearestCell (55, 55) == (1, 1))
  check ref "nearestCell (95,55) = (2,1)" (M.nearestCell (95, 55) == (2, 1))
  check ref "formatClock 0 = 00:00" (M.formatClock 0 == "00:00")
  check ref "formatClock 9 = 00:09" (M.formatClock 9 == "00:09")
  check ref "formatClock 61 = 01:01" (M.formatClock 61 == "01:01")
  check ref "initial clock shows 01:00" (M.initialModel.secondsLeft == M.gameTimerDurationS)
  check ref "cellCenter (1,1) = (60,60)" (M.cellCenter M.spawnCell == (60, 60))

controllerTests :: IORef Int -> IO ()
controllerTests ref = do
  let m0 = M.initialModel
      plant = applyKey (IS.singleton M.keySpace)

  let m1 = plant m0
  check ref "space plants one bomb" (length m1.bombs == 1)
  check ref "bomb planted at snapped spawn cell"
    (case m1.bombs of [b] -> b.cell == M.spawnCell; _ -> False)

  let m2 = applyKey (IS.singleton M.keySpace) m1
  check ref "holding space does not double plant" (length m2.bombs == 1)

  let m3 = applyKey IS.empty m2
      m4 = applyKey (IS.singleton M.keySpace) m3
  check ref "second press still capped at maxActiveBombs" (length m4.bombs == 1)

  let m5 = applyKey (IS.singleton M.keyRight) m4
      m6 = applyTick 50 m5
  check ref "right movement advances x by dt*speed"
    (fst m6.player.pos == 40 + 50 * M.playerSpeed)

  let planted = plant m0
      rightKeyed = applyKey (IS.singleton M.keyRight) planted
      walk1 = applyTick 100 rightKeyed
      walk2 = applyTick 200 walk1
      walk3 = applyTick 300 walk2
  check ref "walk-off: player fully exits the bomb's cell"
    (fst walk3.player.pos >= 80)
  let leftKeyed = applyKey (IS.singleton M.keyLeft) walk3
      back1 = applyTick 400 leftKeyed
      back2 = applyTick 500 back1
  check ref "cleared bomb is solid: cannot walk back onto it"
    (abs (fst back2.player.pos - 88) < 1e-6)

  let plantedT = m0 { M.nowMs = 0, M.bombs = [M.Bomb { M.cell = M.spawnCell, M.plantedAt = 0 }] }
      beforeBoom = applyTick 2999 plantedT
      boom = applyTick 3000 beforeBoom
  check ref "no explosion before fuse" (null beforeBoom.explosions && length beforeBoom.bombs == 1)
  check ref "explosion at fuse expiry" (not (null boom.explosions) && null boom.bombs)
  check ref "flame covers bomb cell and open neighbour"
    ((M.spawnCell `elem` concatMap (.cells) boom.explosions) && (2, 1) `elem` concatMap (.cells) boom.explosions)

  let bs = [ M.Bomb { M.cell = (1, 1), M.plantedAt = 0 }
           , M.Bomb { M.cell = (2, 1), M.plantedAt = 999999 }
           ]
      (flames, kept, _) = detonateAll 3000 M.initialGrid bs
  check ref "chain reaction detonates both bombs" (length flames == 2 && null kept)

  let gSoft = M.initialGrid A.// [((2, 1), M.SoftBlock)]
      boomSoft = applyTick 3000 (m0 { M.nowMs = 0, M.bombs = [M.Bomb { M.cell = M.spawnCell, M.plantedAt = 0 }], M.grid = gSoft })
      flameCellsSoft = concatMap (.cells) boomSoft.explosions
  check ref "soft block destroyed by blast" (M.tileAt boomSoft.grid (2, 1) == M.Empty)
  check ref "ray stops at soft (no (3,1) flame)" (not ((3, 1) `elem` flameCellsSoft))

  let timerRun = foldl' (\acc i -> applyTick (fromIntegral i * 16) acc) (m0 { M.nowMs = 0 }) [1 :: Int .. 3800]
  check ref "timer expiry triggers GameOver" (timerRun.state == M.GameOver && timerRun.secondsLeft == 0)

  let standBoom = applyTick 3000 (m0 { M.nowMs = 0, M.bombs = [M.Bomb { M.cell = M.spawnCell, M.plantedAt = 0 }] })
  check ref "player caught in own blast dies and game over" (standBoom.state == M.GameOver && not standBoom.player.alive)

  let dead = applyTick 3100 standBoom
  check ref "GameOver freezes gameplay state"
    (dead.player == standBoom.player && dead.bombs == standBoom.bombs && dead.secondsLeft == standBoom.secondsLeft)