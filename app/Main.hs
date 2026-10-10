{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import System.IO (hSetBuffering, stdout, BufferMode (LineBuffering))

import Control.Monad.Catch (SomeException, catch, throwM)
import Control.Monad.IO.Class (liftIO)

import Miso ()
import qualified Miso as M

import qualified Controller
import qualified Model
import qualified View

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  M.run $ M.startApp app `catch` \e -> do
    liftIO $ putStrLn ("APPLICATION CRASHED (root cause): " ++ show (e :: SomeException))
    throwM e

app :: M.App Model.Model Model.Msg
app =
  ( M.component Model.initialModel Controller.update View.view
  ) { M.initialAction = Just Model.MsgGetTime
    , M.subs = [M.keyboardSub Model.MsgKeyCodes]
    }