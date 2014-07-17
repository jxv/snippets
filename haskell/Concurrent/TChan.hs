import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan

oneSec = 1000000

writerThread :: TChan Int -> IO ()
writerThread chan = do
  atomically $ writeTChan chan 1
  threadDelay oneSec
  atomically $ writeTChan chan 2
  threadDelay oneSec
  atomically $ writeTChan chan 3
  threadDelay oneSec

readerThread :: TChan Int -> IO ()
readerThread chan = do
  newInt <- ((putStrLn "hello") >> (atomically $ readTChan chan))
  putStrLn $ "read new value: " ++ show newInt
  readerThread chan

main = do
  chan <- atomically $ newTChan
  readerId <- forkIO $ readerThread chan
  writerId <- forkIO $ writerThread chan
  threadDelay $ 5 * oneSecond
  killThread readerId
  killThread writerId

