{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Prelude
import Log

---------- Exericse 1-5 ----------

-- Parse a individual line from the log file
parseMessage :: String -> LogMessage
parseMessage st = case sts of
                 ("E":a:b:c) -> LogMessage (Error $ read a) (read b) (unwords c)
                 ("I":a:b)   -> LogMessage Info (read a) (unwords b)
                 ("W":a:b)   -> LogMessage Warning (read a) (unwords b)
                 _           -> Unknown st
                 where sts = words st

-- Parse an entire log file
parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

-- Insert a new LogMessage into a existing MessageTree
insert :: LogMessage -> MessageTree -> MessageTree
insert m@(LogMessage _ time _) (Node lt now@(LogMessage _ nowTime _) rt)
    | time <= nowTime  = Node (insert m lt) now rt
    | otherwise = Node lt now (insert m rt)
insert m@(LogMessage _ _ _) Leaf = Node Leaf m Leaf
insert _ t                       = t

-- Build a MessageTree from a list of LogMessage
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf 

-- Take all the LogMessage from a MessageTree sorted by timestamp
inOrder :: MessageTree -> [LogMessage]
inOrder (Node ls me rs) = inOrder ls ++ [me] ++ inOrder rs
inOrder _ = []

-- Extract the content from a LogMessage
extract :: LogMessage -> String
extract (Unknown s) = s
extract (LogMessage _ _ s) = s

-- True only when it is a Error LogMessage with level > 50
isWrong :: LogMessage -> Bool
isWrong (LogMessage (Error l) _ _)
    | l > 50 = True
isWrong _ = False

-- Take a list of LogMessage and return a list of the messages corresponding 
-- to any errors with a level > 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extract . inOrder . build . filter isWrong