
calChange :: (Ord p, Num p) => p -> p -> p
calChange owed given = if change > 0 
                           then change
                        else 0  
                 where change = given - owed