# tails

```hs
module Algorithms.List.BasicOperations.Tails where

import Algorithms.List.BasicOperations.DropWhile
```

`inits` is a function that returns all substrings of the list in succession from the beginning[^1].

```hs
-- | >>> tails "abc"
-- ["abc","bc","c",""]
tails :: [a] -> [[a]]
tails = dropWhileParaM (const [False, True])
```

## References
[1] [Monadic versions · Issue #5 · vmchale/recursion_schemes](https://github.com/vmchale/recursion_schemes/issues/5)