# subsequences

```hs
module Algorithms.List.BasicOperations.Subsequences where

import Algorithms.List.BasicOperations.Filter
```

`subsequences` is a function that returns a partial list of all parts of a given list[^1].

```hs
-- | >>> subsequences "abc"
-- ["","a","b","ab","c","ac","bc","abc"]
subsequences :: [a] -> [[a]]
subsequences = filterCataM (const [False, True])
```

## References
[1] [Monadic versions · Issue #5 · vmchale/recursion_schemes](https://github.com/vmchale/recursion_schemes/issues/5)