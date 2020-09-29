dividedBy :: (Ord t1, Num t1, Num t2) => t1 -> t1 -> (t1, t2)
dividedBy num denom = go (abs num) (abs denom) 0 ((signum num) * (signum denom))
    where go n d count sm 
              | n < d     = (sm , count)
              | otherwise = go (n - d) d (count + 1) sm


-- >>> dividedBy (100) (10)
-- <command line>: unknown package: main


-- >>> dividedBy (-100) (10)
-- (-1,10)
--




dividedBy1 :: (Ord t1, Num t1, Num t2) => t1 -> t1 -> t2
dividedBy1 num denom = go (num) (denom) 0 
    where go n d count 
              | n < d     = ( count)
              | otherwise = go (n - d) d (count + 1)

-- >>> dividedBy1 (-100) (10)
-- <command line>: unknown package: main

-- >>> dividedBy1 (-100) (-10)
-- <interactive>:621:2-11: error:
--     • Variable not in scope: dividedBy1 :: Integer -> Integer -> t
--     • Perhaps you meant ‘dividedBy’ (line 2)
--
-- >>> dividedBy1 (100) (10)
-- 10
--
-- >>> dividedBy1 (100) (-10)
-- <command line>: unknown package: main

-- >>> dividedBy1 (-100) (10)

-- >>> dividedBy1 (-100) (-10)
