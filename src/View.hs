{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module View where

import Prelude hiding (Left, Right)

import Control.Monad (forM_, when)
import qualified Data.Array as A
import qualified Language.Javascript.JSaddle as JSaddle
import qualified Miso as M
import qualified Miso.Canvas as C
import Miso.Canvas (TextAlignType (TextAlignCenter, TextAlignLeft, TextAlignRight))
import qualified Miso.CSS as CSS
import Miso.CSS (Color (..))
import qualified Miso.Html as H
import qualified Miso.Html.Property as P

import qualified Model as GameModel

data CanvasState = CanvasState
  { sprFloor :: M.Image
  , sprHardBlock :: M.Image
  , sprSoftBlock :: M.Image
  , sprPlayer :: M.Image
  , sprBomb :: M.Image
  , sprExplosion :: M.Image
  }

instance JSaddle.FromJSVal CanvasState where
  fromJSVal v = do
    (a, b, c, d, e, f) <- JSaddle.fromJSValUnchecked v
    pure (Just (CanvasState a b c d e f))

instance JSaddle.ToJSVal CanvasState where
  toJSVal (CanvasState a b c d e f) = JSaddle.toJSVal (a, b, c, d, e, f)

-- Bump this whenever a PNG in resources/ changes. The dev server sends
-- Last-Modified only (no ETag), and jsaddle reconnects without reloading
-- the page, so the browser would otherwise keep serving cached sprites.
assetVersion :: Int
assetVersion = 2

asset :: String -> String
asset src = src ++ "?v=" ++ show assetVersion

initCanvas :: M.DOMRef -> C.Canvas CanvasState
initCanvas _ = JSaddle.liftJSM $ do
  floorImg <- M.newImage (M.ms (asset "resources/floor.png"))
  hardImg <- M.newImage (M.ms (asset "resources/hard_block.png"))
  softImg <- M.newImage (M.ms (asset "resources/soft_block.png"))
  playerImg <- M.newImage (M.ms (asset "resources/player.png"))
  bombImg <- M.newImage (M.ms (asset "resources/bomb.png"))
  boomImg <- M.newImage (M.ms (asset "resources/explosion.png"))
  pure (CanvasState floorImg hardImg softImg playerImg bombImg boomImg)

view :: GameModel.Model -> M.View GameModel.Model GameModel.Msg
view model =
  H.div_
    []
    [ C.canvas
        [ P.width_ (M.ms GameModel.canvasWidth)
        , P.height_ (M.ms GameModel.canvasHeight)
        ]
        initCanvas
        (viewCanvas model)
    , hiddenImage "resources/floor.png"
    , hiddenImage "resources/hard_block.png"
    , hiddenImage "resources/soft_block.png"
    , hiddenImage "resources/player.png"
    , hiddenImage "resources/bomb.png"
    , hiddenImage "resources/explosion.png"
    ]

hiddenImage :: String -> M.View model action
hiddenImage src =
  H.img_
    [ CSS.style_ [CSS.display "none"]
    , P.src_ (M.ms (asset src))
    ]

viewCanvas :: GameModel.Model -> CanvasState -> C.Canvas ()
viewCanvas model st = do
  C.fillStyle (C.ColorArg (RGB 0 0 0))
  C.fillRect (0, 0, w, h)
  forM_ (A.assocs model.grid) drawCell
  forM_ model.explosions drawExplosion
  forM_ model.bombs drawBomb
  when model.player.alive drawPlayer
  drawHud
  when (model.state == GameModel.GameOver) drawGameOver
  where
    w = fromIntegral GameModel.canvasWidth
    h = fromIntegral GameModel.canvasHeight
    tile = fromIntegral GameModel.tileSize

    drawCell (cell, tileType) =
      C.drawImage'
        ( case tileType of
            GameModel.Empty -> st.sprFloor
            GameModel.HardBlock -> st.sprHardBlock
            GameModel.SoftBlock -> st.sprSoftBlock
        , x
        , y
        , tile
        , tile
        )
      where
        (x, y) = GameModel.cellToPixels cell

    drawBomb b =
      C.drawImage' (st.sprBomb, x, y, tile, tile)
      where
        (x, y) = GameModel.cellToPixels b.cell

    drawExplosion e =
      forM_ e.cells drawFlame
      where
        drawFlame cell =
          C.drawImage'
            (st.sprExplosion, x, y, tile, tile)
          where
            (x, y) = GameModel.cellToPixels cell

    drawPlayer =
      C.drawImage' (st.sprPlayer, px, py, s, s)
      where
        (px, py) = model.player.pos
        s = GameModel.playerSize

    drawHud = do
      C.font "20px monospace"
      C.textAlign TextAlignLeft
      C.fillText (M.ms ("Bombs: " ++ show (length model.bombs)), 10, 28)
      C.textAlign TextAlignRight
      C.fillText (M.ms (GameModel.formatClock model.secondsLeft), w - 10, 28)

    drawGameOver = do
      C.drawImage' (st.sprExplosion, w / 2 - 25, h / 2 - 60, 50, 50)
      C.font "28px monospace"
      C.textAlign TextAlignCenter
      C.fillText ("GAME OVER", w / 2, h / 2)
      C.fillText (M.ms ("Bombs planted: " ++ show (length model.bombs)), w / 2, h / 2 + 40)