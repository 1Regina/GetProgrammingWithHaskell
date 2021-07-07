import qualified Data.Map as Map

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi*(size/2)^2

type Pizza = (Double,Double)

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

-- compare cost per inch of 2 pizzas
comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                         then p1
                         else p2
             where costP1 = costPerInch p1
                   costP2 = costPerInch p2

-- a statement to indicates which pizza is cheaper with its price per sq inch

describePizza :: Pizza -> String
describePizza (size, cost) = "The " ++ show size ++ " pizza is cheaper at " ++
                             show costPerSqInch ++ " per square inch." where
                             costPerSqInch = costPerInch (size,cost)

-- success!
cheaperPizza :: Pizza -> Pizza -> String
cheaperPizza p1 p2 = if costP1 < costP2
                         then "The " ++ show (fst (p1))  ++ " pizza is cheaper at " ++
                             show costP1 ++ " per square inch."
                         else "The " ++ show (fst (p2))  ++ " pizza is cheaper at " ++
                             show costP2 ++ " per square inch."
             where costP1 = costPerInch p1
                   costP2 = costPerInch p2


main :: IO ()
main = do                                         -- to work with IO String by making it String
    putStrLn "What is the size of pizza 1"
    size1 <- getLine
    putStrLn "What is the cost of pizza 1"
    cost1 <- getLine
    putStrLn "What is the size of pizza 2"
    size2 <-  getLine
    putStrLn "What is the cost of pizza 2"
    cost2 <- getLine
    let pizza1 = (read size1, read cost1)           -- change IO 'Double' to IO '[Char]' for size &  cost
    let pizza2 = (read size2, read cost2)           -- change IO 'Double' to IO '[Char]' for size &  cost
    let betterPizza = comparePizzas pizza1 pizza2
    putStrLn (describePizza betterPizza)
    putStrLn "Alternatively, using my cheaperPizza function"
    putStrLn (cheaperPizza pizza1 pizza2)


-- 21.3.1 with Maybe mONAD
-- Having pizza by ID, cost into a Map (aka Dictionary)
costData :: Map.Map Int Double
costData = Map.fromList [(1,18.0),(2,16.0)]

-- recall unit3/lesson18/l18exercises.hs
-- organPairs :: [(Int,Organ)]
-- organPairs = zip ids organs
-- -- >>> organPairs
-- -- [(2,Heart),(7,Heart),(13,Brain),(14,Spleen),(21,Spleen),(24,Kidney)]

-- organCatalog :: Map.Map Int Organ
-- organCatalog = Map.fromList organPairs
-- -- >>> organCatalog
-- -- fromList [(2,Heart),(7,Heart),(13,Brain),(14,Spleen),(21,Spleen),(24,Kidney)]

-- Having pizza by ID, size into a Map (aka Dictionary)
sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1,20.0),(2,15.0)]


maybeMain :: Maybe String
maybeMain = do
    size1 <- Map.lookup 1 sizeData
    cost1 <- Map.lookup 1 costData
    size2 <- Map.lookup 2 sizeData
    cost2 <- Map.lookup 2 costData
    let pizza1 = (size1,cost1)
    let pizza2 = (size2,cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return  (describePizza betterPizza)

-- >>> maybeMain
-- Just "The 20.0 pizza is cheaper at 5.729577951308232e-2 per square inch."
--`return` function, which takes a value of a type and puts it back in the context of the do-notation.`