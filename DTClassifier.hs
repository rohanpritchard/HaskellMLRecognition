module DTClassifier where

import DecisionTree
import DatasetLib
import Data.List

-- bestPartition :: DataSet a b -> a
-- This function must never split on the final (?) attribute, as this is reserved for the result.

buildTree :: (Eq v, Eq c) => DataSet c v -> (DataSet c v -> c) -> DecisionTree c v v
buildTree ds@(cs,vs) bestSplit
  | sameResult ds = Leaf (resultAtt ds)
  | length (vs!!0) == 1  = Leaf (mostPopular $ concat vs)
  | otherwise     = Node (splitCat) (map (\(v,ds') ->  (v,buildTree ds' bestSplit)) (partitionData ds splitCat))
    where
      splitCat = bestSplit ds

mostPopular :: Eq a => [a] -> a
mostPopular xs 
  = snd $ foldl1 (\(a1,b1) (a2,b2) -> if (max a1 a2 == a1) then (a1,b1) else (a2,b2)) (map (\x -> (length [b | b <- xs, b == x],x)) xs)
