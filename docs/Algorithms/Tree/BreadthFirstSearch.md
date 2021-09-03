# Breadth First Search

```hs
module Algorithms.Tree.BreadthFirstSearch where

import Control.Applicative

import Data.Functor.Foldable

import DataStructures.Levels
import DataStructures.Tree
```

Breadth First Search is an algorithm to find the elements in the tree that satisfy certain conditions. It is characterized by exploring nodes in same depth. The algorithm can be divided into two processes. The first step is to enumerate elements in the order in which they are to be explored. The next step is to find the elements from enumerated items using the usual find function. The benefit of the lazy evaluation allows these to be combined efficiently. We use `Levels` to enumerate items in which the depth of items is presereved as a level. This implementation references [Kidne and Wu 2021][^1].

We will only implement the first step, converting the tree to a `Levels`.

```hs
-- | >>> bfsCata (node 1 (leaf 2) (node 3 (node 4 (leaf 6) (leaf 7)) (leaf 5)))
-- Levels [[1],[2,3],[4,5],[6,7]]
bfsCata :: Tree a -> Levels a
bfsCata = cata \case
  Leaf a     -> pure a
  Node a l r -> pure a <|> wrap (l <|> r)
```

[1] Donnacha Oisín Kidney and Nicolas Wu. 2021. Algebras for weighted search. Proc. ACM Program. Lang. 5, ICFP, Article 72 (August 2021), 30 pages. DOI:https://doi.org/10.1145/3473577