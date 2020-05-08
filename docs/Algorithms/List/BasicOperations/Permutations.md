# permutations

```hs
module Algorithms.List.BasicOperations.Permutations where

import Algorithms.List.Sorting.InsertionSort
```

`permutations` is a function that enumerates all combinations of permutations of the elements of a given list[^1].

```hs
-- | >>> permutations [1, 2, 3]
-- [[3,2,1],[3,1,2],[1,3,2],[2,3,1],[2,1,3],[1,2,3]]
permutations :: [a] -> [[a]]
permutations = sortByCataM (\_ _ -> [False, True])
```

## References
[1] [Monadic versions · Issue #5 · vmchale/recursion_schemes](https://github.com/vmchale/recursion_schemes/issues/5)