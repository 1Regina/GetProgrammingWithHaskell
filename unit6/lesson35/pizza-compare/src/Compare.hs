-- {-# LANGUAGE OverloadedStrings #-}
module Compare where
    
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