# Fibonacci Numbers

```hs
module Algorithms.Nat.Fibonacci where

import GHC.Natural

import Control.Comonad.Cofree
import Data.Functor.Foldable
```

In order to calculate the Fibonacci number, we need to know the previous two Fibonacci numbers. The Fibonacci number can be calculated using Histomorphism, which can refer to any previous result[^1].

```hs
-- | >>> fibHisto 5
-- 8
fibHisto :: Natural -> Natural
fibHisto = histo \case
  Nothing                   -> 1
  Just (_ :< Nothing)       -> 1
  Just (n :< Just (m :< _)) -> n + m
```

Using Mendler-style histo, it can be implemented without being aware of the intermediate structure[^2].

```hs
-- | >>> fiboMHisto 5
-- 8
fiboMHisto :: Natural -> Natural
fiboMHisto = mhisto phi . refix
  where
  phi f unIn Nothing = 1
  phi f unIn (Just n) = case unIn n of
    Nothing   -> 1
    (Just n') -> f n + f n'
```

## References
[1] Uustalu, Tarmo, and Varmo Vene. "Primitive (co) recursion and course-of-value (co) iteration, categorically." Informatica 10.1 (1999): 5-26.  
[2] Uustalu, Tarmo, and Varmo Vene. "Coding recursion a la Mendler." Department of Computer Science, Utrecht University. 2000.  
