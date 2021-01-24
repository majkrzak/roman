-- | Polymorphic view for monomorphic containers.
--   Standard Haskell typeclasses like Functor or Foldable
--   require types of kind `* -> *`. Therefore is not possible
--   to create their instances for monomorphic containers.
--   This module introduces simple concept of Polymorphic View.
module Data.Viewable
  ( View(..)
  , Viewable
  , view
  , VFoldable
  , vfold
  , UnViewable
  , unview
  ) where

import Prelude (Functor, Foldable, Monoid, fmap, foldMap, (.), (<$>))

-- | View datatype.
--   Describes view of container `c` of `ce`
--   viewed as `e` using `(ce -> e)` kernel.
data View c ce e where
  View ::Viewable c ce => c -> (ce -> e) -> View c ce e

-- | Functor instance of View.
--   Each View of Viewable container is
--   Functor by design.
instance Viewable c ce => Functor (View c ce) where
  fmap f (View t f') = View t (f . f')

-- | Foldable instance of View.
--   Additionally if container is also VFoldable,
--   View is also Foldable.
instance (Viewable c ce, VFoldable c ce) => Foldable (View c ce) where
  foldMap f v = vfold (f <$> v)

-- | Class of Viewable containers.
class Viewable c ce where
  -- | Creates polymorphic View out of monomorphic container.
  view :: c -> View c ce ce

-- | Class of VFoldable containers.
class VFoldable c ce where
  -- | Folds polymorphic View of monomorphic container.
  vfold :: Monoid e => View c ce e -> e

-- | Class of UnViewable containers.
class UnViewable c ce where
  -- | Creates monomorphic container out of polymorphic View.
  unview :: View c ce ce -> c
