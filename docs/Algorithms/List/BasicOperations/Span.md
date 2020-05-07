# span

```hs
module Algorithms.List.BasicOperations.Span where

import Control.Arrow (first)
import Data.Functor.Foldable
```

``span`` is a Prelude function and its spec is a following:

```hs
-- @
--     span :: (a -> Bool) -> [a] -> ([a], [a])
--     span p xs = (takeWhile p xs, dropWhile p xs)
-- @
```

We can implement ``span`` as an instance of paramorphism.

```hs
-- | >>> spanPara even [2, 4, 7, 8]
-- ([2,4],[7,8])
spanPara :: (a -> Bool) -> [a] -> ([a], [a])
spanPara p = para phi
  where
    phi = \ case
	  Nil            -> ([], [])
      Cons a (as, b) -> if p a then first (a :) b else ([], a : as)
```
