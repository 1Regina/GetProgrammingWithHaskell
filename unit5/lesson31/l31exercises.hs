-- Q31.1    At the end of lesson 21, you saw the following program used to calculate the cost of pizza. Desugar this code to use >>=, >>, return and lambda functions rather than do-notation.
import qualified Data.Map as Map

main :: IO ()
main = do
    putStrLn "What is the size of pizza 1"
    size1 <- getLine
    putStrLn "What is the cost of pizza 1"
    cost1 <- getLine
    putStrLn "What is the size of pizza 2"
    size2 <-  getLine
    putStrLn "What is the cost of pizza 2"
    cost2 <- getLine
    let pizza1 = (read size1, read cost1)
    let pizza2 = (read size2, read cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    putStrLn (describePizza betterPizza)


type Pizza = (Double,Double) -- (size, cost)

areaGivenDiameter :: Double -> Double
areaGivenDiameter diameter = pi*(diameter/2)^2


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

-- Monad alternative for do notation
cheaperPizza :: IO ()
cheaperPizza =  putStrLn "What is the size of pizza 1" >>
                getLine >>=
                (\size1 ->
                    putStrLn "What is the cost of pizza 1" >>
                    getLine >>=
                    (\cost1 ->
                        putStrLn "What is the size of pizza 2" >>
                        getLine >>=
                        (\size2 ->
                            putStrLn "What is the cost of pizza 2" >>
                            getLine >>=
                            (\cost2 ->
                                (\pizza1 ->
                                    (\pizza2 ->
                                        (\betterPizza ->
                                            putStrLn (describePizza betterPizza)
                                        )   (comparePizzas pizza1 pizza2)
                                    )(read size2 , read cost2)
                                )(read size1, read cost1)
                            ))))
-- Steps
-- 1. ghci l31exercises.hs
-- 2. cheaperPizza

-- Q31.2    At the end of lesson 21 in unit 4, we first introduced the idea that do-notation isn’t specific to IO. You ended up with this function for a Maybe type:
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

-- Having pizza by ID, cost into a Map (aka Dictionary)
costData :: Map.Map Int Double
costData = Map.fromList [(1,18.0),(2,16.0)]

-- Having pizza by ID, size into a Map (aka Dictionary)
sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1,20.0),(2,15.0)]

-- Rewrite this function so it works with the List type (don’t worry if the results seem strange).

listMain2pizza :: [String]
listMain2pizza = do
    size1 <- [2]
    cost1 <- [10]
    size2 <- [8]
    cost2 <- [12]
    let pizza1 = (size1,cost1)
    let pizza2 = (size2,cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return  (describePizza betterPizza)

-- >>> listMain2pizza
-- ["The 8.0 pizza is cheaper at 0.238732414637843 per square inch."]

listMain :: [String]
listMain = do
    size1 <- [10,12,17]
    cost1 <- [12.0,15.0,20.0]
    size2 <- [10,11,18]
    cost2 <- [13.0,14.0,21.0]
    let pizza1 = (size1,cost1)
    let pizza2 = (size2,cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return  (describePizza betterPizza)
-- >>> listMain
-- ["The 10.0 pizza is cheaper at 0.15278874536821951 per square inch.","The 10.0 pizza is cheaper at 0.15278874536821951 per square inch.","The 10.0 pizza is cheaper at 0.15278874536821951 per square inch.","The 11.0 pizza is cheaper at 0.13679433125253815 per square inch.","The 11.0 pizza is cheaper at 0.147316972118118 per square inch.","The 10.0 pizza is cheaper at 0.15278874536821951 per square inch.","The 18.0 pizza is cheaper at 5.108677185665777e-2 per square inch.","The 18.0 pizza is cheaper at 5.5016523537939135e-2 per square inch.","The 18.0 pizza is cheaper at 8.25247853069087e-2 per square inch.","The 10.0 pizza is cheaper at 0.16552114081557115 per square inch.","The 10.0 pizza is cheaper at 0.17825353626292278 per square inch.","The 10.0 pizza is cheaper at 0.1909859317102744 per square inch.","The 11.0 pizza is cheaper at 0.13679433125253815 per square inch.","The 11.0 pizza is cheaper at 0.147316972118118 per square inch.","The 10.0 pizza is cheaper at 0.1909859317102744 per square inch.","The 18.0 pizza is cheaper at 5.108677185665777e-2 per square inch.","The 18.0 pizza is cheaper at 5.5016523537939135e-2 per square inch.","The 18.0 pizza is cheaper at 8.25247853069087e-2 per square inch.","The 10.0 pizza is cheaper at 0.16552114081557115 per square inch.","The 10.0 pizza is cheaper at 0.17825353626292278 per square inch.","The 10.0 pizza is cheaper at 0.25464790894703254 per square inch.","The 11.0 pizza is cheaper at 0.13679433125253815 per square inch.","The 11.0 pizza is cheaper at 0.147316972118118 per square inch.","The 11.0 pizza is cheaper at 0.22097545817717698 per square inch.","The 18.0 pizza is cheaper at 5.108677185665777e-2 per square inch.","The 18.0 pizza is cheaper at 5.5016523537939135e-2 per square inch.","The 18.0 pizza is cheaper at 8.25247853069087e-2 per square inch.","The 12.0 pizza is cheaper at 0.1061032953945969 per square inch.","The 12.0 pizza is cheaper at 0.1061032953945969 per square inch.","The 12.0 pizza is cheaper at 0.1061032953945969 per square inch.","The 12.0 pizza is cheaper at 0.1061032953945969 per square inch.","The 12.0 pizza is cheaper at 0.1061032953945969 per square inch.","The 12.0 pizza is cheaper at 0.1061032953945969 per square inch.","The 18.0 pizza is cheaper at 5.108677185665777e-2 per square inch.","The 18.0 pizza is cheaper at 5.5016523537939135e-2 per square inch.","The 18.0 pizza is cheaper at 8.25247853069087e-2 per square inch.","The 12.0 pizza is cheaper at 0.1326291192432461 per square inch.","The 12.0 pizza is cheaper at 0.1326291192432461 per square inch.","The 12.0 pizza is cheaper at 0.1326291192432461 per square inch.","The 12.0 pizza is cheaper at 0.1326291192432461 per square inch.","The 12.0 pizza is cheaper at 0.1326291192432461 per square inch.","The 12.0 pizza is cheaper at 0.1326291192432461 per square inch.","The 18.0 pizza is cheaper at 5.108677185665777e-2 per square inch.","The 18.0 pizza is cheaper at 5.5016523537939135e-2 per square inch.","The 18.0 pizza is cheaper at 8.25247853069087e-2 per square inch.","The 10.0 pizza is cheaper at 0.16552114081557115 per square inch.","The 12.0 pizza is cheaper at 0.17683882565766149 per square inch.","The 12.0 pizza is cheaper at 0.17683882565766149 per square inch.","The 11.0 pizza is cheaper at 0.13679433125253815 per square inch.","The 11.0 pizza is cheaper at 0.147316972118118 per square inch.","The 12.0 pizza is cheaper at 0.17683882565766149 per square inch.","The 18.0 pizza is cheaper at 5.108677185665777e-2 per square inch.","The 18.0 pizza is cheaper at 5.5016523537939135e-2 per square inch.","The 18.0 pizza is cheaper at 8.25247853069087e-2 per square inch.","The 17.0 pizza is cheaper at 5.286807798208288e-2 per square inch.","The 17.0 pizza is cheaper at 5.286807798208288e-2 per square inch.","The 17.0 pizza is cheaper at 5.286807798208288e-2 per square inch.","The 17.0 pizza is cheaper at 5.286807798208288e-2 per square inch.","The 17.0 pizza is cheaper at 5.286807798208288e-2 per square inch.","The 17.0 pizza is cheaper at 5.286807798208288e-2 per square inch.","The 18.0 pizza is cheaper at 5.108677185665777e-2 per square inch.","The 17.0 pizza is cheaper at 5.286807798208288e-2 per square inch.","The 17.0 pizza is cheaper at 5.286807798208288e-2 per square inch.","The 17.0 pizza is cheaper at 6.60850974776036e-2 per square inch.","The 17.0 pizza is cheaper at 6.60850974776036e-2 per square inch.","The 17.0 pizza is cheaper at 6.60850974776036e-2 per square inch.","The 17.0 pizza is cheaper at 6.60850974776036e-2 per square inch.","The 17.0 pizza is cheaper at 6.60850974776036e-2 per square inch.","The 17.0 pizza is cheaper at 6.60850974776036e-2 per square inch.","The 18.0 pizza is cheaper at 5.108677185665777e-2 per square inch.","The 18.0 pizza is cheaper at 5.5016523537939135e-2 per square inch.","The 17.0 pizza is cheaper at 6.60850974776036e-2 per square inch.","The 17.0 pizza is cheaper at 8.811346330347147e-2 per square inch.","The 17.0 pizza is cheaper at 8.811346330347147e-2 per square inch.","The 17.0 pizza is cheaper at 8.811346330347147e-2 per square inch.","The 17.0 pizza is cheaper at 8.811346330347147e-2 per square inch.","The 17.0 pizza is cheaper at 8.811346330347147e-2 per square inch.","The 17.0 pizza is cheaper at 8.811346330347147e-2 per square inch.","The 18.0 pizza is cheaper at 5.108677185665777e-2 per square inch.","The 18.0 pizza is cheaper at 5.5016523537939135e-2 per square inch.","The 18.0 pizza is cheaper at 8.25247853069087e-2 per square inch."]

-- Q31.3    Refactor    the    maybeMain function from the preceding exercise so that it works with any Monad. You’ll need to change the type signature as well as remove the type-specific parts from the body of the function.

monadMain :: Monad m => m Double -> m Double
            -> m Double -> m Double -> m String
monadMain s1 c1 s2 c2 = do
    size1 <- s1
    cost1 <- c1
    size2 <- s2
    cost2 <- c2
    let pizza1 = (size1,cost1)
    let pizza2 = (size2,cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return  (describePizza betterPizza)

-- >>> monadMain 2 4 8 1


-- >>> monadMain (Map.lookup 1 sizeData) (Map.lookup 1 costData) ( Map.lookup 2 sizeData) ( Map.lookup 2 costData)
-- Just "The 20.0 pizza is cheaper at 5.729577951308232e-2 per square inch."

-- >>> monadMain  [2] [10] [8][12]
-- ["The 8.0 pizza is cheaper at 0.238732414637843 per square inch."]

