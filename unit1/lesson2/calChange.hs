
calChange :: (Ord p, Num p) => p -> p -> p
calChange owed given = if change > 0
                           then change
                        else 0
                 where change = given - owed


doublePlusTwo :: Num a => a -> a
doublePlusTwo x = doubleX  + 2
   where doubleX = 2 * x

-- >>> doublePlusTwo 2
-- 6
