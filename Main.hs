import DTClassifier
import Data.List.Split

getData fp = do allInOne <- readFile fp
                let dt = map (splitOn ",") (splitOn "\n" allInOne)
                print $ dt
                return ()
