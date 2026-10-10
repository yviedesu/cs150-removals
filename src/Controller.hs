{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NamedFieldPuns #-}

module Controller where

import Miso
import qualified Miso as M

import Model hiding (Left, Right)
import qualified Model as GameModel

update :: Msg -> Transition Model Msg
update MsgGetTime = do
  -- performance.now(): monotonic ms since page load. Date#getMilliseconds
  -- is only the 0-999 ms field of the current second, so it can never be
  -- subtracted across a 3000 ms fuse.
  M.io (MsgSetTime <$> M.now)
update msg = do
  m <- M.get
  let (m', further) = GameModel.step msg m
  M.modify (const m')
  mapM_ M.issue further