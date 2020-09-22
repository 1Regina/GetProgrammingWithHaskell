import Data.List
compareLastNames name1 name2 = if lastName1 > lastName2
                               then GT
                               else if lastName1 < lastName2
                                    then LT
                                    else EQ  
    where lastName1 = snd name1
          lastName2 = snd name2


names = [("Ian", "Curtis"),
         ("Bernard","Sumner"),
         ("Peter", "Hook"),
         ("Stephen","Morris")]

-- >>> sort names
-- [("Bernard","Sumner"),("Ian","Curtis"),("Peter","Hook"),("Stephen","Morris")]
--

-- >>> import Data.List
-- >>> sortBy compareLastNames names 
-- [("Ian","Curtis"),("Peter","Hook"),("Stephen","Morris"),("Bernard","Sumner")]
--
-- >>> sortBy compareLastNames [("Ian","Curtis"),("Peter","Hook")] 
-- [("Ian","Curtis"),("Peter","Hook")]
--

compareSameLastNames :: (Ord a1, Ord a2) => (a2, a1) -> (a2, a1) -> Ordering
compareSameLastNames name1 name2 = if lastName1 > lastName2
                               then GT
                               else if lastName1 < lastName2
                                    then LT
                                    else if fst(name1) >  fst(name2)
                                    then GT
                                          else if fst(name1) <  fst(name2)
                                          then LT 
                                          else EQ     
    where lastName1 = snd name1
          lastName2 = snd name2

-- >>> sortBy compareSameLastNames [("Ian","Curtis"),("Peter","Hook")] 
-- [("Ian","Curtis"),("Peter","Hook")]
-- >>> sortBy compareSameLastNames [("Ian","Curtis"),("Han","Curtis")] 
-- [("Han","Curtis"),("Ian","Curtis")]
--
