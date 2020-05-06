# factorial

```hs
module Algorithms.Nat.BasicOperations.Factorial where

import GHC.Natural

import Data.Functor.Foldable
```

The implementation of factorialization by hylo is described in Meijer (1991)[^1]. And it is also known as the implementation by "Cartesianally-inclined Haskell programmer"[^3].

```hs
-- | >>> factorialHylo 5
-- 120
factorialHylo :: Natural -> Natural
factorialHylo = hylo g f
                where
                f 0 = Nil
                f n = Cons n (n-1)
                g Nil        = 1
                g (Cons a b) = a * b
```

The implementation of factorialization by para is presented in Meertens(1990). And it's also known as the implementation by "Post-doc Haskell programmer"[^3].

```hs
-- | >>> factorialPara 5
-- 120
factorialPara :: Natural -> Natural
factorialPara = para \case
                  Nothing     -> 1
                  Just (n, a) -> (1 + n) * a
```

## References
[1] Meijer, Erik, Maarten Fokkinga, and Ross Paterson. "Functional programming with bananas, lenses, envelopes and barbed wire." Conference on Functional Programming Languages and Computer Architecture. Springer, Berlin, Heidelberg, 1991.  
[2] Meertens, Lambert. "Paramorphisms." Formal aspects of computing 4.5 (1992): 413-424.  
[3] Fritz Ruehr, Willamette University. "The Evolution of a Haskell Programmer"
