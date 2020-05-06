# product

```hs
module Algorithms.Nat.BasicOperations.Product where

import GHC.Natural

import Data.Functor.Foldable
```

Multiplication can be represented as catamorphism.

```hs
-- | >>> prodCata 2 3
-- 6
prodCata :: Natural -> Natural -> Natural
prodCata n = cata \case
          Nothing -> 0
          Just m  -> n + m
```

