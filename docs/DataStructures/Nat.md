# Nat

```hs
module DataStructures.Nat where

import Data.Functor.Foldable
```

The natural number can be seen as the fixed point of a certain functor. This functor is isomorphic to the data type Maybe, and is implemented as such in the recursion schemes library.

```hs
data NatF r = Zero | Succ r

type Nat = Fix NatF
```
