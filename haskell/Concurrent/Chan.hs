module Chan where

import Control.Monad
import Control.Concurrent
import System.IO

oneSec = 1000000
printChan chan = readChan chan >>= putStrLn

main = do
  chan <- newChan :: IO (Chan String)
  chan' <- dupChan chan
  thIdPrint <- forkIO $ forever $ printChan chan'
  str <- getLine
  writeChan chan str
  threadDelay oneSec
  killThread thIdPrint

