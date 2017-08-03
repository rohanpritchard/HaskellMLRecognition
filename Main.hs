import DTClassifier
import Data.List.Split

getData fp header = do allInOne <- readFile fp
                       let ds = stripAllSpaces $ map (splitOn ",") (splitOn "\n" allInOne)
                       let dt = buildTree (header,ds) splitByEntropy
                       return dt

stripAllSpaces x = map (map strip) x

strip x = dropWhile (==' ') x



compress xs = (map (\x -> show (div (read x :: Int) 10)) (init xs)) ++ [(last xs)]
