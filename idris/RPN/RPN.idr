module Main

import System
import Data.Floats

data Value = Number Float
           | UniFn (Float -> Float)
           | BinFn (Float -> Float -> Float)

logBase : Float -> Float -> Float
logBase base arg = log arg / log base

toValue : String -> Value
toValue str = case str of
    "e" => Number euler; "pi" => Number pi; "tau" => Number (pi * 2); "phi" => Number 1.6180339887
    "sqrt" => UniFn sqrt; "exp" => UniFn exp; "ld" => UniFn log; "ln" => UniFn (logBase euler)
    "+" => BinFn (+); "-" => BinFn (-); "x" => BinFn (*); "/" => BinFn (/); "log" => BinFn logBase
    s => Number (cast s)

eval : List Value -> List Float -> Either String Float
eval Nil Nil = Left "No input."
eval Nil (n::Nil) = Right n
eval Nil (_::_::_) = Left "Not enough functions."
--
eval ((Number n)::vs) ns = eval vs (n::ns)
--
eval ((UniFn _)::_) Nil = Left "No stacked numbers."
eval ((UniFn fn)::vs) (n::ns) = eval vs ((fn n)::ns)
--
eval ((BinFn _)::_) Nil = Left "No stacked numbers."
eval ((BinFn _)::_) (_::Nil) = Left "Not enough stacked numbers."
eval ((BinFn fn)::vs) (n::m::ns) = eval vs ((fn m n)::ns)

main : IO ()
main = do (_::input) <- System.getArgs
          case eval (map toValue input) Nil of Right n => print n
                                               Left er => putStrLn er
