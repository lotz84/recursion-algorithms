# scanr / scanl

```hs
module Algorithms.List.BasicOperations.Scanr where

import Data.List.NonEmpty (NonEmpty(..), (<|))

import Data.Functor.Base (NonEmptyF(NonEmptyF))
import Data.Functor.Foldable
```

## scanr

`scanr` is a function that returns a list of foldr calculations, including the process.

```hs
-- | >>> scanrCata (+) 0 [1, 2, 3]
-- [6,5,3,0]
scanrCata :: (a -> b -> b) -> b -> [a] -> [b]
scanrCata f b = cata $ \case
               Nil       -> [b]
               Cons a bs -> (f a $ head bs) : bs
```

However, this implementation by cata is not very natural as it requires the use of head. Considering that the result of the scanr calculation always contains at least an initial value, it is natural to interpret the result as NonEmpty. This allows you to get a new implementation.

```hs
-- | >>> scanrCata' (+) 0 [1, 2, 3]
-- 6 :| [5,3,0]
scanrCata' :: (a -> b -> b) -> b -> [a] -> NonEmpty b
scanrCata' f b = cata $ \case
                Nil                -> b :| []
                Cons a bs@(b :| _) -> f a b <| bs
```

## scanl

Since both list and NonEmpty are types that can be represented as fixed points, it is conceivable to decompose them into a combination of cata and ana. This means that any implementation where the order of the computations can be reversed will be a scanl.


```hs
dist :: (a -> b -> b) -> b
     -> ListF a (NonEmptyF b r) -> NonEmptyF b (ListF a r)
dist _ b Nil                             = NonEmptyF b Nothing
dist f _ (Cons a (NonEmptyF b Nothing))  = NonEmptyF (f a b) (Just Nil)
dist f _ (Cons a (NonEmptyF b (Just r))) = NonEmptyF (f a b) (Just $ Cons a r)

-- | >>> scanlCataAna (+) 0 [1, 2, 3]
-- 6 :| [3,1,0]
scanlCataAna :: (a -> b -> b) -> b -> [a] -> NonEmpty b
scanlCataAna f b = cata $ ana (dist f b . fmap project)

-- | >>> scanlAnaCata (+) 0 [1, 2, 3]
-- 6 :| [3,1,0]
scanlAnaCata :: (a -> b -> b) -> b -> [a] -> NonEmpty b
scanlAnaCata f b = ana $ cata (fmap embed . dist f b)
```
