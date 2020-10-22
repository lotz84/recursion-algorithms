# depth

```hs
module Algorithms.Tree.BasicOperations.Depth where

import GHC.Natural

import Data.Functor.Foldable

import DataStructures.Tree
```

`depth` is a function for calculating the hight of the tree.

```hs
-- | >>> depthCata (node 1 (leaf 2) (node 3 (node 4 (leaf 5) (leaf 6)) (leaf 7)))
-- 4
depthCata :: Tree a -> Natural
depthCata = cata \case
  Leaf _     -> 1
  Node _ l r -> 1 + l `max` r
```
