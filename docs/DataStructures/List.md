# List

```hs
module DataStructures.List where

import Data.Functor.Foldable

import RecursionSchemes.Extra
```

The list is the most basic data type to consider in recursion schemes.

```hs
-- data ListF a r = Nil | Cons a r
--
-- type List a = Fix (ListF a)

listFold :: FoldAlgebra ListF
listFold Nil        = id
listFold (Cons a b) = b . a
```
