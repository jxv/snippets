{-# LANGUAGE LambdaCase #-}

data Expr
  = Con Int
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Exp Expr Expr
  deriving (Show)


d :: String -> Expr -> Expr
d x = \case 
  Con _   -> Con 0
  Var y   -> if (y == x) then (Con 1) else (Var y)
  Add u v -> Add (d x u) (d x v)
  Sub u v -> Sub (d x u) (d x v)
  Mul u v -> Add (Mul u (d x v)) (Mul v (d x u))
  Div u v -> Div (Sub (Mul v (d x u)) (Mul u (d x v))) (Exp v (Con 2))
  Exp u v -> Mul (Mul v (Exp u (Sub v (Con 1)))) (d x u)


