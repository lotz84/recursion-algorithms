# lessThan

```hs
module Algorithms.Nat.BasicOperations.LessThan where

import GHC.Natural

import RecursionSchemes.Extra
```

The `lessThan` can be implemented by comparing two natural numbers recursively by multimorphism[^1]; the same is true for `greaterThan` and `equal`.

```hs
-- | >>> 1 `lessThan` 2
-- True
lessThan :: Natural -> Natural -> Bool
lessThan = mmulti phi
  where
  phi _  Nothing  Nothing  = False
  phi _  Nothing  (Just _) = True
  phi _  (Just _) Nothing  = False
  phi lt (Just a) (Just b) = lt a b
```

## References
[1] Uustalu, Tarmo, and Varmo Vene. "Coding recursion a la Mendler." Department of Computer Science, Utrecht University. 2000.  

