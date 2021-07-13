import qualified Data.Map as Map -- Step 1: for implementation of maps from keys to values (dictionaries)

-- 27.3.1 One interface for four problems
-- 2. RobotPart defined using record syntax
data RobotPart = RobotPart
    { name :: String
    , description :: String
    , cost :: Double
    , count :: Int
    } deriving Show
-- Example robot parts: leftArm, rightArm, and robotHead
leftArm :: RobotPart
leftArm  = RobotPart
    { name = "left arm"
    , description = "left arm for face punching!"
    , cost = 1000.00
    , count = 3
    }
rightArm :: RobotPart
rightArm  = RobotPart
    { name = "right arm"
    , description = "right arm for kind hand gestures"
    , cost = 1025.00
    , count = 5
    }
robotHead :: RobotPart
robotHead  = RobotPart
    { name = "robot head"
    , description = "this head looks mad"
    , cost = 5092.25
    , count = 2
    }

-- 3.  render the information contained in a RobotPart as HTML.
-- code for rendering an individual RobotPart as an HTML snippet
type Html = String
renderHtml :: RobotPart -> Html
renderHtml part = mconcat ["<h2>",partName, "</h2>"
                          ,"<p><h3>desc</h3>",partDesc
                          ,"</p><p><h3>cost</h3>"
                          ,partCost
                          ,"</p><p><h3>count</h3>"
                          ,partCount,"</p>"]
    where partName = name part
          partDesc = description part
          partCost = show (cost part)
          partCount = show (count part)

-- 4. partsDB, which is your internal database of RobotParts. Build a Map is with the fromList function

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
    where keys = [1,2,3]
          vals = [leftArm,rightArm,robotHead]
          keyVals = zip keys vals
-- >>> partsDB
-- fromList [(1,RobotPart {name = "left arm", description = "left arm for face punching!", cost = 1000.0, count = 3}),(2,RobotPart {name = "right arm", description = "right arm for kind hand gestures", cost = 1025.0, count = 5}),(3,RobotPart {name = "robot head", description = "this head looks mad", cost = 5092.25, count = 2})]

-- 5. Converting a Maybe RobotPart to Maybe Html bcos out of the many data models, one may have an error.
-- insertSnippet :: Maybe Html -> IO ()

--6. look up a part and pass it as Maybe Html to `insertSnippet`. Map.lookup to pick 1 value in a Data.Map ie dictionary.
partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB
-- >>> partVal
-- Just (RobotPart {name = "left arm", description = "left arm for face punching!", cost = 1000.0, count = 3})

-- 7. use <$> to transform RobotPart to HTML - Because Maybe is a Functor, can use <$> to transform RobotPart into HTML while remaining in a Maybe. can now pass partHtml to insertSnippet easily because of Functor
partHtml :: Maybe Html
partHtml = renderHtml <$> partVal
-- >>> partHtml
-- Just "<h2>left arm</h2><p><h3>desc</h3>left arm for face punching!</p><p><h3>cost</h3>1000.0</p><p><h3>count</h3>3</p>"

-- 27.3.3Converting a list of RobotParts to a list of HTML
-- 8. create an index page of all the parts by get a list of parts from your partsDB. Map.toList to put the extracted values into a list.
allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)
-- >>> allParts
-- [RobotPart {name = "left arm", description = "left arm for face punching!", cost = 1000.0, count = 3},RobotPart {name = "right arm", description = "right arm for kind hand gestures", cost = 1025.0, count = 5},RobotPart {name = "robot head", description = "this head looks mad", cost = 5092.25, count = 2}]

-- Quick check 27.3    Rewrite the definition of all parts to use <$> instead of map.
allParts1 :: [RobotPart]
allParts1 = snd <$> (Map.toList partsDB)
-- >>> allParts1
-- [RobotPart {name = "left arm", description = "left arm for face punching!", cost = 1000.0, count = 3},RobotPart {name = "right arm", description = "right arm for kind hand gestures", cost = 1025.0, count = 5},RobotPart {name = "robot head", description = "this head looks mad", cost = 5092.25, count = 2}]
-- [RobotPart {name = "left arm", description = "left arm for face punching!", cost = 1000.0, count = 3},RobotPart {name = "right arm", description = "right arm for kind hand gestures", cost = 1025.0, count = 5},RobotPart {name = "robot head", description = "this head looks mad", cost = 5092.25, count = 2}]

-- 9. Transforming a list of RobotParts to HTML with <$> instead of map. Rem List is also an instance of Functor
allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts
-- >>> allPartsHtml
-- ["<h2>left arm</h2><p><h3>desc</h3>left arm for face punching!</p><p><h3>cost</h3>1000.0</p><p><h3>count</h3>3</p>","<h2>right arm</h2><p><h3>desc</h3>right arm for kind hand gestures</p><p><h3>cost</h3>1025.0</p><p><h3>count</h3>5</p>","<h2>robot head</h2><p><h3>desc</h3>this head looks mad</p><p><h3>cost</h3>5092.25</p><p><h3>count</h3>2</p>"]
allPartsHtml1 :: [Html]
allPartsHtml1 = map renderHtml allParts
-- >>> allPartsHtml1
-- ["<h2>left arm</h2><p><h3>desc</h3>left arm for face punching!</p><p><h3>cost</h3>1000.0</p><p><h3>count</h3>3</p>","<h2>right arm</h2><p><h3>desc</h3>right arm for kind hand gestures</p><p><h3>cost</h3>1025.0</p><p><h3>count</h3>5</p>","<h2>robot head</h2><p><h3>desc</h3>this head looks mad</p><p><h3>cost</h3>5092.25</p><p><h3>count</h3>2</p>"]

--10. For converting RobotParts to HTML, just have an htmlPartsDB so you don’t have to continually convert? With Map as an instance of Functor
-- So to turn your partsDB into a Map of HTML rather than RobotParts
htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB
-- >>> htmlPartsDB
-- fromList [(1,"<h2>left arm</h2><p><h3>desc</h3>left arm for face punching!</p><p><h3>cost</h3>1000.0</p><p><h3>count</h3>3</p>"),(2,"<h2>right arm</h2><p><h3>desc</h3>right arm for kind hand gestures</p><p><h3>cost</h3>1025.0</p><p><h3>count</h3>5</p>"),(3,"<h2>robot head</h2><p><h3>desc</h3>this head looks mad</p><p><h3>cost</h3>5092.25</p><p><h3>count</h3>2</p>")]
-- >>> Map.lookup 1 htmlPartsDB
-- Just "<h2>left arm</h2><p><h3>desc</h3>left arm for face punching!</p><p><h3>cost</h3>1000.0</p><p><h3>count</h3>3</p>"

-- 27.3.5Transforming an IO RobotPart into IO Html

-- 1. using return to create an IO type of a RobotPart
leftArmIO :: IO RobotPart
leftArmIO = return leftArm

-- 2. turn this into HTML so that you can write the HTML snippet to a file
htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO
-- 3. Recap all HTML trnsformation with renderHTML. see, Functor’s <$> provides a common interface to apply any function to a value in a contex
-- partHtml :: Maybe Html
-- partHtml = renderHtml <$> partVal      --[*fmap with a **Maybe***]
-- allPartsHtml :: [Html]
-- allPartsHtml = renderHtml <$> allParts -- [*fmap with a **list***]
-- htmlPartsDB :: Map.Map Int Html
-- htmlPartsDB = renderHtml <$> partsDB   -- [*fmap with a **Map ie Dictionary***]
-- htmlSnippet :: IO Html
-- htmlSnippet = renderHtml <$> leftArmIO -- [*fmap with a **IO***]

