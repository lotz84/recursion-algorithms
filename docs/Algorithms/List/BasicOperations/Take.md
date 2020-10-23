# take

```hs
module Algorithms.List.BasicOperations.Take where

import GHC.Natural

import Data.Functor.Foldable

import RecursionSchemes.Extra
```

This implementation can be found in Uustalu (2000)[^1].

```hs
-- | >>> takeMMulti 3 [1..]
-- [1,2,3]
takeMMulti :: Natural -> [a] -> [a]
takeMMulti = mmulti phi
  where
  phi _ Nothing Nil         = []
  phi _ Nothing (Cons _ _)  = []
  phi _ (Just _) Nil        = []
  phi f (Just a) (Cons b c) = b : f a c
```

## References
[1] Uustalu, Tarmo, and Varmo Vene. "Coding recursion a la Mendler." Department of Computer Science, Utrecht University. 2000.  
