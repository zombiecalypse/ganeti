module Ganeti.Utils.Queue( PriorityQueue
                         , mkQueue
                         , decrement
                         , due
                         , updateQueue) where

import Data.List (sort)

-- | Sorted list of names with attached priorities.
type PriorityQueue = [(Integer, String)]

mkQueue :: [String] -> PriorityQueue
mkQueue l = map ((,) 0) l

decrement :: PriorityQueue -> Integer -> PriorityQueue
decrement queue i = map (\(t, x) -> (t - i, x)) queue

due :: PriorityQueue -> [String]
due queue = map snd $ filter ((<= 0) . fst) queue

updateQueue :: PriorityQueue -> [(Integer, String)] -> PriorityQueue
updateQueue queue newEntries = sort . (newEntries ++) $ filter (not . flip elem (map snd newEntries) . snd) queue
