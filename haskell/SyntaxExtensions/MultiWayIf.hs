{-# LANGUAGE MultiWayIf #-}

import Control.Monad (forever)

main :: IO ()
main = forever $
  do input <- getLine
     putStrLn $ if | input == ""       -> "<No Input>"
                   | length input < 10 -> input ++ input
                   | otherwise         -> input


