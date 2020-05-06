# List

```hs
module DataStructures.List where

import Data.Functor.Foldable hiding (ListF(..))
```

The list is the most basic data type to consider in recursion schemes.

```hs
data ListF a r = Nil | Cons a r

type List a = Fix (ListF a)
```
