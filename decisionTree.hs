import Data.Maybe
import Control.Monad
import Control.Applicative
import Data.Traversable

data DecisionTree attCat attVal ans =
    Null
  | Leaf {answer :: ans}
  | Node {question :: attCat, children :: [(attVal, DecisionTree attCat attVal ans)]}

instance (Show c, Show v, Show a) => Show (DecisionTree c v a) where
  show Null              = "Null"
  show (Leaf a)          = "Leaf " ++ show a
  show (Node q cs)       = "Node " ++ show q ++ show (map (\(v,t) -> (v,question t)) cs)
  -- TODO: This isn't going to work when printing leaf children, needs to fail (monad?)

instance Foldable (DecisionTree c v) where 
  foldMap f Null         = mempty
  foldMap f (Leaf x)     = f x
  foldMap f (Node q cs)  = foldl (mappend) mempty (map (foldMap f . snd) cs)

instance Functor (DecisionTree c v) where
  fmap f (Node q cs)     = Node q (map (\(v,t) ->(v,fmap f t)) cs)
  fmap f (Leaf x)        = Leaf (f x)
  fmap f Null            = Null

numLeaf :: DecisionTree c v a -> Int
numLeaf t = foldl (\x _ -> x + 1) 0 t
