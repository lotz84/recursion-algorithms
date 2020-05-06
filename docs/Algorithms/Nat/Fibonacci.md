# Fibonacci Numbers

```hs
module Algorithms.Nat.Fibonacci where

import GHC.Natural

import Control.Comonad.Cofree
import Data.Functor.Foldable
```

In order to calculate the Fibonacci number, we need to know the previous two Fibonacci numbers. The Fibonacci number can be calculated using Histomorphism, which can refer to any previous result[^1].

```hs
-- | >>> fibCata 5
-- 8
fibCata :: Natural -> Natural
fibCata = histo \case
            Nothing                   -> 1
            Just (_ :< Nothing)       -> 1
            Just (n :< Just (m :< _)) -> n + m
```

## References
[1] Uustalu, Tarmo, and Varmo Vene. "Primitive (co) recursion and course-of-value (co) iteration, categorically." Informatica 10.1 (1999): 5-26.
