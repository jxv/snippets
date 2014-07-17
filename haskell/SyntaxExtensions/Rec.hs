{-# LANGUAGE RecursiveDo #-}

justOnes :: Num a => Maybe [a]
justOnes = do { rec { xs <- Just (1:xs) }
              ; return (map negate xs) }
