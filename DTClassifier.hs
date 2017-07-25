module DTClassifier where

import DecisionTree
import DatasetLib
import Data.List

buildTree :: (Eq v, Eq c) => DataSet c v -> (DataSet c v -> c) -> DecisionTree c v v
buildTree ds@(cs,vs) bestSplit
  | sameResult ds = Leaf (resultAtt ds)
  | length (vs!!0) == 1  = Leaf (highestFreq $ concat vs)
  | otherwise     = Node (splitCat) (map (\(v,ds') ->  (v,buildTree ds' bestSplit)) (partitionData ds splitCat))
    where
      splitCat = bestSplit ds

highestFreq :: Eq a => [a] -> a
highestFreq xs 
  = snd $ foldl1 (\(a1,b1) (a2,b2) -> if (max a1 a2 == a1) then (a1,b1) else (a2,b2)) (map (\x -> (length [b | b <- xs, b == x],x)) xs)

entropy :: (Eq v) => DataSet c v -> Double
entropy ds@(hs,vs) = -1 * (sum $ map xlogx probabilities)
  where
    resultIndex = length hs - 1
    countPerResult = freqCount (map (!!resultIndex) vs)
    total       = length vs
    probabilities = map (\(_,x) -> (fromIntegral x) / (fromIntegral total)) countPerResult

xlogx x = x * log x

freqCount :: Eq a => [a] -> [(a,Int)]
freqCount xs = map (\x -> (x,length [a | a <- xs, x == a])) (nub xs)

informationGain :: (Eq c, Eq v) => DataSet c v -> c -> Double
informationGain ds c = (entropy ds) - sum (map (entropy.snd) (partitionData ds c))

splitByEntropy :: (Eq c, Eq v) => DataSet c v -> c
splitByEntropy ds@(cs,vs) = snd $ foldl1 (\(a1,b1) (a2,b2) -> if (max a1 a2 == a1) then (a1,b1) else (a2,b2)) catEntropy
  where
    catEntropy = zip (map (informationGain ds) fields) fields
    fields = init cs 
