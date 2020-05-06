# Extra Recursion Schemes

```hs
module RecursionSchemes.Extra where

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

## References
[1] Kabanov, Jevgeni, and Varmo Vene. "Recursion schemes for dynamic programming." International Conference on Mathematics of Program Construction. Springer, Berlin, Heidelberg, 2006.

