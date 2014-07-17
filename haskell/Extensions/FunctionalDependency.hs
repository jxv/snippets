{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class Foo a b | b -> a where
  bar :: a -> b

class Quux b a | b -> a where
  qax :: b -> a

func :: (Foo a b, Quux b a) => b -> b
func b = bar (qax b)


