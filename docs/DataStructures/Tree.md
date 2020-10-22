# Tree

```hs
module DataStructures.Tree where

import Data.Functor.Foldable
```

There are a lot of trees in the world, but here we are dealing with a binary tree. Tree structures are also one of the data types that can be represented as the fixed point.


```hs
data TreeF a r = Leaf a | Node a r r
  deriving Functor

type Tree a = Fix (TreeF a)

leaf :: a -> Tree a
leaf = Fix . Leaf

node :: a -> Tree a -> Tree a -> Tree a
node a l r = Fix $ Node a l r
```
