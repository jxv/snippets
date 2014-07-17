{-# LANGUAGE LambdaCase #-}

add2 :: Maybe Int -> Maybe Int
add2 = \case
  Nothing -> Just 2
  Just n  -> Just (n + 2)

