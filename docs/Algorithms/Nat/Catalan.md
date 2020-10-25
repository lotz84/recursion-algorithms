# Catalan Numbers

```hs
module Algorithms.Nat.Catalan where

import GHC.Natural

import Data.Foldable

import Control.Comonad.Cofree
import Data.Functor.Foldable
```

In order to calculate the Catalan number, we need to refer to all the results of the previous calculations. This type calculation is called as course-of-values recursion. We can implement the Catalan number by using Histomorphism[^1].

```hs
-- | >>> catalanHisto 5
-- 42
catalanHisto :: Natural -> Natural
catalanHisto = histo \case
  Nothing    -> 1
  Just table ->
    let xs = toList table
     in sum $ zipWith (*) xs (reverse xs)
```

`toList` is too specific and only for this situation. There is also an implementation of Catalan numbers using Dynamorphism in [1] so that the more general case can be assumed.

## References
[1] Hinze, Ralf, and Nicolas Wu. "Histo-and Dynamorphisms Revisited."
