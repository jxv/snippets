module Main where

import Control.Monad
import Control.Concurrent
import Controller
import View

main :: IO ()
main = do
  c <- newChan :: IO ControllerChan
  _ <- forkIO $ controller c
  _ <- forkIO $ view c
  return ()

