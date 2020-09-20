messyMain :: IO()
messyMain = do   
    print "Who is the email for?"   
    recipient <- getLine   
    print "What is the Title?"   
    title <- getLine   
    print "Who is the Author?"   
    author <- getLine   
    print ("Dear " ++ recipient ++ ",\n" ++ 
        "Thanks for buying " ++ title  ++ "\nthanks,\n" ++  
         author )

toPart :: [Char] -> [Char]
toPart recipient = "Dear " ++ recipient ++ ",\n"

bodyPart :: [Char] -> [Char]
bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"

fromPart :: [Char] -> [Char]
fromPart author = "Thanks,\n"++ author


createEmail :: [Char] -> [Char] -> [Char] -> [Char]
createEmail recipient bookTitle author = toPart recipient      ++ 
                                         bodyPart bookTitle    ++
                                         fromPart author

main :: IO()
main = do   
    print "Who is the email for?"   
    recipient   <- getLine   
    print "What is the Title?"   
    title       <- getLine   
    print "Who is the Author?"   
    author      <- getLine   
    print (createEmail recipient title author)

-- >>> 2^123
-- 10633823966279326983230456482242756608
--
message :: IO()
message = do   
    print "Who is the message for?"   
    person   <- getLine   
    print "What time of the day is it?"   
    time        <- getLine   
    print "What do you wish for today?"   
    wish        <- getLine   
    print (createMessage person time wish)

toPartMsg :: [Char] -> [Char]
toPartMsg person = "Dear " ++ person ++ ",\n"

bodyPartMsg :: [Char] -> [Char]
bodyPartMsg time = "Good " ++ time ++ " to you" ++ ".\n"

endPartMsg :: [Char] -> [Char]
endPartMsg wish = "Hope you get your "++ wish ++ " today. \n"


createMessage :: [Char] -> [Char] -> [Char] -> [Char]
createMessage person time wish = toPartMsg person      ++ 
                                 bodyPartMsg time     ++
                                 endPartMsg wish

