# Extra Recursion Schemes

```hs
{-# LANGUAGE FlexibleContexts #-}
module RecursionSchemes.Extra where

import Control.Monad ((>=>))

import Control.Comonad
import Control.Comonad.Cofree
import Data.Functor.Foldable
```

Here is a collection of implementations of recursion schemes that are not implemented in recursion-schemes.

## Dynamorphism
Dynamorphism is the recursion schemes proposed by Kabanov and Vene to realize dynamic programming[^1]. Simply put, it is represented as a refold of Anamorphism and Histomorphism.However, here we implement Dynamorphism as an extension of Hylomorphism in order not to lose generality.

```hs
dyna :: Functor f => (f (Cofree f x) -> x) -> (y -> f y) -> (y -> x)
dyna phi psi = extract . hylo ap psi
  where ap f = phi f :< f
```

## Monadic Recursion Schemes
Monadic catamorphism[^2] can be implemented as a special case of ordinary catamorphism[^3].

```hs
cataM :: (Traversable (Base t), Monad m, Recursive t)
      => (Base t c -> m c) -> t -> m c
cataM = cata . (sequence >=>)
```

You can also implement monadic paramorphism in a similar way.

```hs
paraM :: (Recursive t, Monad m, Traversable (Base t))
      => (Base t (t, c) -> m c) -> t -> m c
paraM = para . (sequence . fmap sequence >=>)
```


## References
[1] Kabanov, Jevgeni, and Varmo Vene. "Recursion schemes for dynamic programming." International Conference on Mathematics of Program Construction. Springer, Berlin, Heidelberg, 2006.  
[2] Fokkinga, Maarten Maria. Monadic maps and folds for arbitrary datatypes. University of Twente, Department of Computer Science, 1994.  
[3] [Suggestion: Add monadic variants of various ...morphism functions. · Issue #3 · ekmett/recursion-schemes](https://github.com/ekmett/recursion-schemes/issues/3)