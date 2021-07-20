-- Q30.1    To prove that Monad is strictly more powerful than Functor, write a universal version of <$>, as in the preceding lesson’s exercise, called allFmapM, that defines <$> for all members of the Monad type class. Because it works for all instances of Monad, the only functions you can use are the methods required by the Monad type class (and lambda func-tions). To get you started, here’s your type signature:

-- Recall 29.1
allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap function applicInstance = pure function <*> applicInstance

-- recall also (getLine >>= (\name ->return (nameStatement name))) in unit5/lesson30/2monadIO.hs
allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM function val = val >>= (\x -> return (function x))


-- Q30.2    To prove that Monad is strictly more powerful than Applicative, write a universal version of <*>, called allApp, that defines <*> for all members of the Monad type class. Because it works for all instances of Monad, the only functions you can use are the meth-ods required by the Monad type class (and lambda functions). To get you started, here’s your type signature:

allApp :: Monad m => m (a -> b) -> m a -> m b
-- <*> :: Applicative f :: f(a -> b) -> f a -> f b
-- >>= :: Monad  m :: m a -> (a -> m b) -> m b

-- This question is much trickier than the last one. Two hints:
-- 1. Try to think exclusively in terms of the type signatures.
-- 2. Use <$> if you want and replace it with your answer to Q29.1
-- recall creditsFromId id = lookupUserName id >>= lookupCredits

allApp function val = function >>= (\f -> val  >>= (\x -> return (f x)))


-- Q30.3    Implement a bind function which is the same as (>>=) for Maybe:

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just val) function = function val

