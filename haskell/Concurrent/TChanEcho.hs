module TChanEcho where

import Data.Function
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

main :: IO ()
main =
  do tchan <- newTChanIO
     thid <- forkIO . forever $
       do str <- atomically (readTChan tchan)
          putStrLn str
     fix $ \loop ->
       do line <- getLine
          if line /= "quit"
             then atomically (writeTChan tchan line) >> loop
             else return ()
     killThread thid 


