module TVarEcho where

import Data.Function
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

main :: IO ()
main =
  do echo_this <- newTVarIO Nothing
     thid <- forkIO . forever $
       do str <- atomically $
                   do ms <- readTVar echo_this
                      case ms of Nothing -> retry
                                 Just s  -> do writeTVar echo_this Nothing
                                               return s
          putStrLn str
     fix $ \loop ->
       do line <- getLine
          if line /= "quit"
             then do atomically (writeTVar echo_this (Just line))
                     loop
             else return ()
     killThread thid
