module Dataset where

import Data.List
import Data.Maybe

type Header a = [a]
type Value  a = [a]
type Values a = [Value a]
type DataSet a b = (Header a, Values b)

displayEntry :: (Show a, Show b) => DataSet a b -> Int -> IO ()
displayEntry (cs,vs) i = mapM_ putStrLn (zipWith (\h v -> show h ++ ": " ++ show v) cs (vs!!i))

catEntries :: (Eq a, Eq b) => DataSet a b -> a -> [b]
catEntries (cs, as) c = nub (map (!! (fromJust (elemIndex c cs))) as)

removeCat :: (Eq a) => a -> DataSet a b -> DataSet a b
removeCat c (cs, as) = (removeIndex i cs, map (removeIndex i) as)
  where
    i = fromJust (elemIndex c cs)

removeIndex :: Int -> [a] -> [a]
removeIndex i xs = first ++ second
  where
    (first,_:second) = splitAt i xs

partitionData :: (Eq a, Eq b) => DataSet a b -> a -> [(b,DataSet a b)]
partitionData ds@(cs,vs) c = map (\val -> (val,(processEntries val))) (catEntries ds c)
  where
    processEntries val = removeCat c (cs,[e | e <- vs, val == (e !! (fromJust $ elemIndex c cs))])
