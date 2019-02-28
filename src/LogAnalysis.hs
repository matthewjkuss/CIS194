{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

import Text.Read

readOrLeave :: Read a => String -> Either String a
readOrLeave str = case readMaybe str of
  (Just x) -> Right x
  Nothing -> Left str

backToString :: Show a => Either String a -> String
backToString (Left str) = str
backToString (Right int) = show int

-- parseMessage "E 2 562 help help"
-- == LogMessage (Error 2) 562 "help help"
parseMessage :: String -> LogMessage
parseMessage line = case readOrLeave <$> words line of
  (Left "I" : Right timestamp : xs) -> 
    LogMessage Info timestamp (unwords $ backToString <$> xs)
  (Left "W" : Right timestamp : xs) -> 
    LogMessage Warning timestamp (unwords $ backToString <$> xs)
  (Left "E" : Right severity : Right timestamp : xs) -> 
    LogMessage (Error severity) timestamp (unwords $ backToString <$> xs)
  string -> Unknown (unwords $ backToString <$> string)

parse :: String -> [LogMessage]
parse =  map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree 
insert newMessage@(LogMessage _ newTime _) messageTree = case messageTree of
  Leaf -> Node Leaf newMessage Leaf 
  (Node left message@(LogMessage _ time _) right) -> 
    if newTime < time
      then Node (insert newMessage left) message right
      else Node left message (insert newMessage right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMessage right) = 
  (inOrder left) ++ [logMessage] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map m . filter f . inOrder . build
  where 
    f (LogMessage (Error s) _ _) = s >= 50
    f _ = False
    m (LogMessage _ _ s) = s


-- Jean-Yves Girard ??