# Depth First Search

```hs
module Algorithms.Tree.DepthFirstSearch where

import Data.Functor.Foldable

import DataStructures.Tree
```

Deep First Search is an algorithm to find the elements in the tree that satisfy certain conditions. It is characterized by exploring deeper nodes first. The algorithm can be divided into two processes. The first step is to create a list of elements in the order in which they are to be explored. The next step is to find the elements from the list using the usual find function. The benefit of the lazy evaluation allows these to be combined efficiently.

We will only implement the first step, converting the tree to a list.

```hs
-- | >>> dfsCata (node 1 (leaf 2) (node 3 (node 4 (leaf 5) (leaf 6)) (leaf 7)))
-- [1,2,3,4,5,6,7]
dfsCata :: Tree a -> [a]
dfsCata = cata \case
  Leaf a -> [a]
  Node a l r -> a : (l ++ r)
```
