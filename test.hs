data List a = Elem a (List a) | End deriving Show

instance Foldable (List) where
  foldMap f (Elem e n) 
       = mappend (f e) (foldMap f n)
  foldMap f (End)      = mempty
