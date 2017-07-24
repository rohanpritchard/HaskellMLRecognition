module DecisionTree where

import Data.Maybe
import Control.Monad
import Control.Applicative
import Data.Traversable

data DecisionTree attCat attVal ans =
    Null
  | Leaf {answer :: ans}
  | Node {category :: attCat, children :: [(attVal, DecisionTree attCat attVal ans)]}  

instance (Show c, Show v, Show a) => Show (DecisionTree c v a) where
  show Null              = "Null"
  show (Leaf a)          = "Leaf " ++ show a
  show (Node q cs)       = "Node " ++ show q ++ " " ++ show (map fst cs)

instance Foldable (DecisionTree c v) where 
  foldMap f Null         = mempty
  foldMap f (Leaf x)     = f x
  foldMap f (Node q cs)  = foldl mappend mempty (map (foldMap f . snd) cs)

instance Functor (DecisionTree c v) where
  fmap f (Node q cs)     = Node q (map (\(v,t) ->(v,fmap f t)) cs)
  fmap f (Leaf x)        = Leaf (f x)
  fmap f Null            = Null

numLeaf :: DecisionTree c v a -> Int
numLeaf t = foldl (\x _ -> x + 1) 0 t

addChild :: DecisionTree c v a -> (v,DecisionTree c v a) -> DecisionTree c v a
addChild (Node cat cs) child = Node cat (child:cs)

retrieveChild :: (Eq v) => DecisionTree c v a -> v -> DecisionTree c v a
retrieveChild (Node cat cs) v = fromJust $ lookup v cs









