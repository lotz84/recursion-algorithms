# Levels

```hs
module DataStructures.Levels where

import Control.Applicative
```

`Levels` is not recursive data type, but it is important to implement [breadth-first search](Algorithms/Tree/BreadthFirstSearch.md). Implementations in here reference to [Kidne and Wu 2021][^1].

```hs
newtype Levels a = Levels [[a]]
                 deriving (Show, Eq)

choices :: Alternative f => (a -> f b) -> [a] -> f b
choices f []     = empty
choices f (x:xs) = f x <|> choices f xs

wrap :: Levels a -> Levels a
wrap (Levels xs) = Levels ([] : xs)

zipL :: [[a]] -> [[a]] -> [[a]]
zipL      []      yss  = yss
zipL     xss       []  = xss
zipL (xs:xss) (ys:yss) = (xs ++ ys) : zipL xss yss

instance Functor Levels where
  fmap f (Levels xss) = Levels (map (map f) xss)

instance Foldable Levels where
  foldMap f (Levels xss) = mconcat $ map (mconcat . map f) xss

instance Applicative Levels where
  pure x = Levels [[x]]
  (Levels []) <*> _ = Levels []
  (Levels (fs:fss)) <*> (Levels xss) = Levels (map (fs <*>) xss) <|>  wrap (Levels fss <*> Levels xss)

instance Alternative Levels where
  empty = Levels []
  (Levels xss) <|> (Levels yss) = Levels (zipL xss yss)

instance Monad Levels where
  (Levels      [])  >>= k = empty
  (Levels (xs:xss)) >>= k = choices k xs <|> wrap (Levels xss >>= k)
```

## References
[1] Donnacha Oisín Kidney and Nicolas Wu. 2021. Algebras for weighted search. Proc. ACM Program. Lang. 5, ICFP, Article 72 (August 2021), 30 pages. DOI:https://doi.org/10.1145/3473577