
-- Q32.1    Use a list comprehension that generates a list of correct calendar dates, given that you know the number of days in each month. For example, it should start with 1 ..31 for January and be followed by 1 .. 28  for February.
monthEnds :: [Int]
monthEnds = [31,28] --,31,30,31,30,31,31,30,31,30,31]

months :: [String]
months = ["January", "February"]

dates :: [Int] -> [Int]
dates monthEnds = [date | end <- monthEnds
                        , date <- [1 ..end ] ]
-- >>> dates monthEnds
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28]

calender0 :: [(String, Int)]
calender0 = [ (month, date)
             | month <- ["January", "February"]
             , date <- [1 .. 31] ]
-- >>> calender0
-- [("January",1),("January",2),("January",3),("January",4),("January",5),("January",6),("January",7),("January",8),("January",9),("January",10),("January",11),("January",12),("January",13),("January",14),("January",15),("January",16),("January",17),("January",18),("January",19),("January",20),("January",21),("January",22),("January",23),("January",24),("January",25),("January",26),("January",27),("January",28),("January",29),("January",30),("January",31),("February",1),("February",2),("February",3),("February",4),("February",5),("February",6),("February",7),("February",8),("February",9),("February",10),("February",11),("February",12),("February",13),("February",14),("February",15),("February",16),("February",17),("February",18),("February",19),("February",20),("February",21),("February",22),("February",23),("February",24),("February",25),("February",26),("February",27),("February",28),("February",29),("February",30),("February",31)]

-- refining to number of dates (Method 1)
calender1 :: [(String, Int)]
calender1 = [ (month, date)
            | (month, dateEnds) <- monthLengths -- zip ["January", "February"] [31,28]
            , date <- [1 .. dateEnds]]
            -- , let date = (\[1 .. dateEnds] -> dateEnds (31,28))

monthLengths = [("January", 31), ("February", 28)]

-- >>> calender1
-- [("January",1),("January",2),("January",3),("January",4),("January",5),("January",6),("January",7),("January",8),("January",9),("January",10),("January",11),("January",12),("January",13),("January",14),("January",15),("January",16),("January",17),("January",18),("January",19),("January",20),("January",21),("January",22),("January",23),("January",24),("January",25),("January",26),("January",27),("January",28),("January",29),("January",30),("January",31),("February",1),("February",2),("February",3),("February",4),("February",5),("February",6),("February",7),("February",8),("February",9),("February",10),("February",11),("February",12),("February",13),("February",14),("February",15),("February",16),("February",17),("February",18),("February",19),("February",20),("February",21),("February",22),("February",23),("February",24),("February",25),("February",26),("February",27),("February",28)]

-- refining to number of dates (Method 2)
calender2 :: [(String, Int)]
calender2 = [ (month, date)
            | month <- ["January", "February"]
            , let dateEnds = getMonthLength month
            , date <- [1 .. dateEnds]]

getMonthLength "February" = 28
getMonthLength _ = 31

-- >>> calender2
-- [("January",1),("January",2),("January",3),("January",4),("January",5),("January",6),("January",7),("January",8),("January",9),("January",10),("January",11),("January",12),("January",13),("January",14),("January",15),("January",16),("January",17),("January",18),("January",19),("January",20),("January",21),("January",22),("January",23),("January",24),("January",25),("January",26),("January",27),("January",28),("January",29),("January",30),("January",31),("February",1),("February",2),("February",3),("February",4),("February",5),("February",6),("February",7),("February",8),("February",9),("February",10),("February",11),("February",12),("February",13),("February",14),("February",15),("February",16),("February",17),("February",18),("February",19),("February",20),("February",21),("February",22),("February",23),("February",24),("February",25),("February",26),("February",27),("February",28)]

-- refining to number of dates (Method 3)
calender :: [(String, Int)]
calender = [ (month, date)
            | (month, dateEnds) <-[("January", 31), ("February", 28)]
            , date <- [1 .. dateEnds]]

-- >>> calender
-- [("January",1),("January",2),("January",3),("January",4),("January",5),("January",6),("January",7),("January",8),("January",9),("January",10),("January",11),("January",12),("January",13),("January",14),("January",15),("January",16),("January",17),("January",18),("January",19),("January",20),("January",21),("January",22),("January",23),("January",24),("January",25),("January",26),("January",27),("January",28),("January",29),("January",30),("January",31),("February",1),("February",2),("February",3),("February",4),("February",5),("February",6),("February",7),("February",8),("February",9),("February",10),("February",11),("February",12),("February",13),("February",14),("February",15),("February",16),("February",17),("February",18),("February",19),("February",20),("February",21),("February",22),("February",23),("February",24),("February",25),("February",26),("February",27),("February",28)]

-- refining to number of dates (Method 4)
calenderZip :: [(String, Int)]
calenderZip = [ (month, date)
               | (month, dateEnds) <- zip ["January", "February"] [31,28]
               , date <- [1 .. dateEnds]]
-- >>> calenderZip
-- [("January",1),("January",2),("January",3),("January",4),("January",5),("January",6),("January",7),("January",8),("January",9),("January",10),("January",11),("January",12),("January",13),("January",14),("January",15),("January",16),("January",17),("January",18),("January",19),("January",20),("January",21),("January",22),("January",23),("January",24),("January",25),("January",26),("January",27),("January",28),("January",29),("January",30),("January",31),("February",1),("February",2),("February",3),("February",4),("February",5),("February",6),("February",7),("February",8),("February",9),("February",10),("February",11),("February",12),("February",13),("February",14),("February",15),("February",16),("February",17),("February",18),("February",19),("February",20),("February",21),("February",22),("February",23),("February",24),("February",25),("February",26),("February",27),("February",28)]

-- refining to number of dates (Method 5)
calenderExtList :: [(String, Int)]
calenderExtList = [ (month, date)
                | (month, dateEnds) <- zip months monthEnds
                , date <- [1 .. dateEnds]]
-- >>> calenderExtList
-- [("January",1),("January",2),("January",3),("January",4),("January",5),("January",6),("January",7),("January",8),("January",9),("January",10),("January",11),("January",12),("January",13),("January",14),("January",15),("January",16),("January",17),("January",18),("January",19),("January",20),("January",21),("January",22),("January",23),("January",24),("January",25),("January",26),("January",27),("January",28),("January",29),("January",30),("January",31),("February",1),("February",2),("February",3),("February",4),("February",5),("February",6),("February",7),("February",8),("February",9),("February",10),("February",11),("February",12),("February",13),("February",14),("February",15),("February",16),("February",17),("February",18),("February",19),("February",20),("February",21),("February",22),("February",23),("February",24),("February",25),("February",26),("February",27),("February",28)]


-- Q32.2    Translate the preceding question into do-notation, and then into Monad methods and lambdas.

datesDo :: [Int] -> [Int]
datesDo ends = do
    end <- ends
    date <- [1 .. end]
    return date

datesMonad :: [Int] -> [Int]
datesMonad ends  =  ends >>=
            (\end ->
                [1 .. end] >>=
                    (\date -> return date))

-- >>> datesDo [31]
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]

-- >>> datesMonad [31]
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]
