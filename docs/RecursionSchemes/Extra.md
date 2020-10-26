# Extra Recursion Schemes

```hs
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module RecursionSchemes.Extra where

import Control.Monad ((>=>))
import Data.Bifunctor

import Control.Comonad
import Control.Comonad.Cofree
import Control.Foldl
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

## Multimorphism

Multimorphism is a recursion schemes that deals with the two recursive type at the same time[^2].

```hs
mmulti :: (Recursive f, Recursive g) => (forall a b. (a -> b -> c) -> Base f a -> Base g b -> c) -> f -> g -> c
mmulti phi f g = phi (mmulti phi) (project f) (project g)
```

`mmulti` can also be implemented using Day convolution as type `(Day (Base f) (Base g) c -> c) -> f -> g -> c`[^3].

## Monoidal Catamorphism

This idea was introduced by Bartosz Milewski as an extension of Foldl to RecursionSchemes[^4]. Here we use endomorphsm instead of monoids to match the implementation in the Foldl library.

```hs
type FoldAlgebra f = forall x. f (x -> x) (x -> x) -> (x -> x)

cat :: (Bifunctor f, Functor (f a)) => FoldAlgebra f -> Fold a b -> Fix (f a) -> b
cat falg (Fold step begin done) = done . ($ begin) . cata (falg . bimap (flip step) id)
```

## Monadic Recursion Schemes
Monadic catamorphism[^5] can be implemented as a special case of ordinary catamorphism[^6].

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
[2] Uustalu, Tarmo, and Varmo Vene. "Coding recursion a la Mendler." Department of Computer Science, Utrecht University. 2000.  
[3] [sellout/yaya - Yaya.Fold#cata2](https://github.com/sellout/yaya/blob/d75598e08b4ea85946857f7c0643811b858a9b2b/core/src/Yaya/Fold.hs#L178-L181)  
[4] [Monoidal Catamorphisms \| Bartosz Milewski's Programming Cafe](https://bartoszmilewski.com/2020/06/15/monoidal-catamorphisms/)  
[5] Fokkinga, Maarten Maria. Monadic maps and folds for arbitrary datatypes. University of Twente, Department of Computer Science, 1994.  
[6] [Suggestion: Add monadic variants of various ...morphism functions. · Issue #3 · ekmett/recursion-schemes](https://github.com/ekmett/recursion-schemes/issues/3)
