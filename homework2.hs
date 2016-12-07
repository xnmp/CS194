-- {-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
-- import Data.Listb


parseMessage :: String -> LogMessage
parseMessage ss
    | mcode == "E" = LogMessage (Error (read q1)) (read q2) q3
    | mcode == "I" = LogMessage Info (read q1) q4
    | mcode == "W" = LogMessage Warning (read q1) q4
    | otherwise = Unknown ss
    where 
        mcode = head slist
        q1 = slist !! 1
        q2 = slist !! 2
        q3 = unwords $ drop 3 slist
        q4 = unwords $ drop 2 slist
        slist = words ss

parse :: String -> [LogMessage]
parse = fmap parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf = Node Leaf lm Leaf
insert lm1@(LogMessage _ nts _) mt@(Node left lm2@(LogMessage _ ts _) right)
    | nts > ts = Node left lm2 (insert lm1 right) 
    | nts == ts = mt
    | nts < ts = Node (insert lm1 left) lm2 right

build :: [LogMessage] -> MessageTree
build lms = foldr insert Leaf lms 

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right

getTime :: LogMessage -> TimeStamp
getTime (Unknown _) = 0
getTime (LogMessage _ ts _) = ts

getSev :: LogMessage -> Int
getSev (Unknown _) = 0
getSev (LogMessage (Error n) _ _) = n
getSev _ = 0

getLog :: LogMessage -> String
getLog (Unknown _) = ""
getLog (LogMessage _ _ lms) = lms

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = fmap getLog $ filter (\x->getSev x>=50) $ inOrder $ build lms


-- main :: IO [LogMessage]
main = do
    kk <- testWhatWentWrong parse whatWentWrong "error.log"
    print $ kk