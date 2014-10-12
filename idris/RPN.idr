module Main

import System

data Op = Add | Sub | Mul | Div

data Value = Operator Op | Number Float

instance Show Op where
  show op = case op of Add => "+"; Sub => "-"; Mul => "x"; Div => "/"

instance Show Value where
  show v = case v of Operator op => show op; Number n => show n

toValue : String -> Value
toValue str = case str of "+" => Operator Add; "-" => Operator Sub; "x" => Operator Mul; "/" => Operator Div
                          s   => Number (cast s)

toFun : Op -> Float -> Float -> Float
toFun op = case op of Add => (+); Sub => (-); Mul => (*); Div => (/)

eval : List Value -> List Float -> Either String Float
eval Nil Nil = Left "No input."
eval Nil (n::Nil) = Right n
eval Nil (_::_::_) = Left "Not enough operations."
eval ((Operator op)::_) Nil = Left "No stacked numbers."
eval ((Operator op)::_) (_::Nil) = Left "Not enough stacked numbers."
eval ((Operator op)::vs) (n::m::ns) = eval vs ((toFun op m n)::ns)
eval ((Number n)::vs) ns = eval vs (n::ns)

main : IO ()
main = do (_::input) <- System.getArgs
          case eval (map toValue input) Nil of Right n => print n
                                               Left er => putStrLn er
