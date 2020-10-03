import Data.Semigroup
--Quickcheck 17.1 solut
myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)
-- >>> myAny even [1,2,3]
-- True
--
-- Quickcheck 17.2
instance Semigroup Integer where
    (<>) x y = x + y

-- instance Semigroup Int where
--     (/) x y = x / y -- No, because division doesn’t always return an Int type, which violates the rule.


data Color = Red 
         |   Yellow
         |   Blue 
         |   Green 
         |   Purple 
         |   Orange 
         |   Brown deriving (Show,Eq)


instance Semigroup Color where   
    (<>) Red Blue = Purple   
    (<>) Blue Red = Purple   
    (<>) Yellow Blue = Green   
    (<>) Blue Yellow = Green   
    (<>) Yellow Red = Orange   
    (<>) Red Yellow = Orange   
    (<>) a b = if a == b              
               then a
               else Brown

-- >>> Red <> Yellow
-- Orange
--
instance Semigroup Color where   
    (<>) Red Blue = Purple   
    (<>) Blue Red = Purple   
    (<>) Yellow Blue = Green   
    (<>) Blue Yellow = Green   
    (<>) Yellow Red = Orange   
    (<>) Red Yellow = Orange   
    (<>) a b | a == b = a
             | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
             | all (`elem` [Blue,Yellow,Green]) [a,b] = Green
             | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange
             | otherwise = Brown

-- Quickcheck 17.3
--Yes as addition of Integers are associative.  1 + 3 = 3 = 1

--Quickcheck 17.4 
mappend :: a -> a -> a

instance Semigroup Integer where
    (<>) x y = x * y

-- 1, because x × 1 = x.
