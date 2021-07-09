{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO  -- to perform text I/O

dharma :: T.Text
dharma = "धम"
bgText :: T.Text
bgText = " श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्मो भयावहः "

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
    where pieces = T.splitOn query fullText
          highlighted = mconcat ["{",query,"}"]

main1 = do
    TIO.putStrLn (highlight dharma bgText)


day :: T.Text
day = "day"

allDay :: T.Text
allDay = "Today is Friday. Thank God it's Friday. It's my favourite day of the week."
main = do
    TIO.putStrLn (highlight day allDay)


-- Steps:
--         1.  ghc --make 3highlightText.hs
--         2.  ./3highlightText
--         3.  [DONT NEED] <ctdl-d> to end and get results of computation