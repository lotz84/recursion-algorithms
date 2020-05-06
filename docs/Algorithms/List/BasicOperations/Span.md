# span

```hs
module Algorithms.List.BasicOperations.Span where

import Control.Arrow (first)
import Data.Functor.Foldable

-- | >>> span even [2, 4, 7, 8]
-- ([2,4],[7,8])
spanPara :: (a -> Bool) -> [a] -> ([a], [a])
spanPara p = para phi
  where
    phi = \ case
	  Nil            -> ([], [])
      Cons a (as, b) -> if p a then first (a :) b else ([], a : as)
```
