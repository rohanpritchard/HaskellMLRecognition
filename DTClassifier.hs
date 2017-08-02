module DTClassifier where

import DecisionTree
import DatasetLib
import Data.List
import Data.Maybe

buildTree :: (Eq v, Eq c) => DataSet c v -> (DataSet c v -> c) -> DecisionTree c v v
buildTree ds@(cs,vs) bestSplit
  | sameResult ds = Leaf (resultAtt ds)
  | length (vs!!0) == 1  = Leaf (highestFreq $ concat vs)
  | otherwise     = Node (splitCat) (map (\(v,ds') ->  (v,buildTree ds' bestSplit)) (partitionData ds splitCat))
    where
      splitCat = bestSplit ds

highestFreq :: Eq a => [a] -> a
highestFreq xs 
  = snd $ foldl1 (firstOrder) (map (\x -> (length [b | b <- xs, b == x],x)) xs)

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
splitByEntropy ds@(cs,vs) = snd $ foldl1 firstOrder catEntropy
  where
    catEntropy = zip (map (informationGain ds) fields) fields
    fields = init cs 

firstOrder :: (Ord a) => (a,b) -> (a,b) -> (a,b)
firstOrder a@(x1,y1) b@(x2,y2) = if (max x1 x2 == x1) then a else b

predict :: (Eq c, Eq v) => DecisionTree c v v -> (Header c,Value v) -> v
predict (Leaf x) _           = x
predict (Node q cs) p@(hs,v)
  | isNothing child  = childrenVote (map snd cs) p
  | otherwise        = predict (fromJust child) p
    where
      child    = lookup attVal cs
      catIndex = getIndex hs q
      attVal   = v !! catIndex

-- If no child node has this attribute value, it seperately lets each child vote on the other attributes.
childrenVote :: (Eq c, Eq v) => [DecisionTree c v v] -> (Header c, Value v) -> v
childrenVote ts hv = snd $ foldl1 firstOrder results
  where
    results = map swap (freqCount $ map (flip predict hv) ts)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)







