{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseTimeStamp :: String -> TimeStamp
parseTimeStamp s = read s

parseInt :: String -> Int
parseInt s = read s

parseMessage :: String -> LogMessage
parseMessage line =
  let w = words line in
    case w!!0 of
      "I" -> LogMessage Info (parseTimeStamp (w!!1)) (unwords $ drop 2 w)
      "W" -> LogMessage Warning (parseTimeStamp (w!!1)) (unwords $ drop 2 w)
      "E" -> LogMessage (Error (parseInt (w!!1))) (parseTimeStamp (w!!2)) (unwords $ drop 3 w)
      _ -> Unknown line

parse :: String -> [LogMessage]
parse "" =[]
parse text =
  let ls = lines text in
    map parseMessage ls

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ timeStamp _) = timeStamp

getMessageType :: LogMessage -> Maybe MessageType
getMessageType (LogMessage t _ _) =  Just t
getMessageType (Unknown _) = Nothing

getMessage :: LogMessage -> String
getMessage (Unknown s) = s
getMessage (LogMessage _ _ s) = s

getLeftMessageTree :: MessageTree -> MessageTree
-- getLeftMessageTree Leaf
getLeftMessageTree (Node left _ _) = left

getRightMessageTree :: MessageTree -> MessageTree
-- getRightMessageTree Leaf
getRightMessageTree (Node _ _ right) = right

getMsgMessageTree :: MessageTree -> LogMessage
-- getMsgMessageTree Leaf
getMsgMessageTree (Node _ l _) =l

insert :: LogMessage -> MessageTree -> MessageTree
insert l Leaf =
  case getMessageType l of
    Nothing -> Leaf
    _ -> Node Leaf l Leaf
insert l tree =
  case getMessageType l of
    Nothing -> tree
    _ ->
      let timeLog = getTimeStamp l
          timeMessageTree = getTimeStamp (getMsgMessageTree tree)
          ltree = getLeftMessageTree tree
          rtree = getRightMessageTree tree
      in
        case (timeLog > timeMessageTree) of
          True -> Node ltree (getMsgMessageTree tree) (insert l rtree)
          _ -> Node (insert l ltree) (getMsgMessageTree tree) rtree


build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder tree =
  let ltree = getLeftMessageTree tree
      msg = getMsgMessageTree tree
      rtree = getRightMessageTree tree
  in
    (inOrder ltree)++[msg]++(inOrder rtree)


getRelevantInfo :: Int -> [LogMessage] -> [LogMessage]
getRelevantInfo _ [] = []
getRelevantInfo errno (x:xs)  =
    case getMessageType x of
      Just (Error e) ->
        case e > errno of
          True -> x:(getRelevantInfo errno xs)
          _ -> (getRelevantInfo errno xs)
      _ -> (getRelevantInfo errno xs)

-- getRelevantInfo' :: [LogMessage] -> [LogMessage]
-- getRelevantInfo' [] = []
-- getRelevantInfo' (x:xs)  =
--     case getMessageType x of
--       Just Info -> x:(getRelevantInfo' xs)
--       _ -> (getRelevantInfo' xs)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  map getMessage.inOrder.build.(getRelevantInfo 49)

