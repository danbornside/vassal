{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Vassal.MaybeSelector where


import Vassal.Class
import Vassal.Orphans ()

import Prelude hiding (lookup, null, (.))

import Control.Category ((.))
import Control.Lens.Indexed (FoldableWithIndex, FunctorWithIndex, TraversableWithIndex, itraverse)
import Data.Aeson (FromJSON, FromJSON1, ToJSON, ToJSON1)
import Data.Align
import Data.AppendMap ()
import Data.Constraint
import Data.Functor.Apply
import Data.Functor.Classes
import Data.Maybe (isJust, isNothing)
import Data.Semigroup (First (..), Option (..), Semigroup)
import Reflex.FunctorMaybe

import Vassal.WrappedShow1


-- {-# OPTIONS_GHC -ddump-splices #-}
-- The simplest is "Single" which is a global value that can be queried or not,
-- and be updated or not.

newtype MaybeSelector a = MaybeSelector { unMaybeSelector :: Option a }
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable, Monoid, Semigroup, ToJSON, ToJSON1, FromJSON, FromJSON1, FunctorMaybe, Align, Applicative, Apply, Monad)



viewJust :: a -> MaybeSelector a
viewJust = MaybeSelector . Option . Just

instance ViewSelector MaybeSelector where
  newtype ViewResult MaybeSelector v = MaybeView { unSingle :: Option (First v) } -- TODO: we don't actually use the semigroup instance on viewResult's, this should just be Maybe
    deriving (Eq, Show, Ord, Semigroup, Monoid, Functor, Foldable, Traversable, FromJSON, ToJSON)

  type ViewIndex MaybeSelector = ()
  viewSelectorIsSemigroup  = Sub Dict

  lookup _ = getOption . unMaybeSelector
  {-# INLINE lookup #-}

instance SubViewSelector MaybeSelector where
  isSubView (MaybeSelector (Option x)) (MaybeSelector (Option y)) = isJust y || isNothing x

instance ViewSelectorJSON MaybeSelector where
  viewSelectorIsFromJSON = Sub Dict
  viewIsFromJSON = Sub Dict
  viewSelectorIsToJSON = Sub Dict
  viewIsToJSON = Sub Dict

instance FunctorMaybe (ViewResult MaybeSelector) where
  fmapMaybe f = MaybeView . fmap First . fmapMaybe (f . getFirst) . unSingle

instance FunctorWithIndex () (ViewResult MaybeSelector)
instance FoldableWithIndex () (ViewResult MaybeSelector)
instance TraversableWithIndex () (ViewResult MaybeSelector) where
  itraverse f = traverse $ f ()

instance Apply (ViewResult MaybeSelector) where
  MaybeView (Option fs) <.> MaybeView (Option xs) = MaybeView $ Option $ fmap First (fmap getFirst fs <*> fmap getFirst xs)

instance Align (ViewResult MaybeSelector) where
  nil = MaybeView mempty
  alignWith f (MaybeView (Option xs)) (MaybeView (Option ys)) = MaybeView $ Option $ fmap First
    $ alignWith f (fmap getFirst xs) (fmap getFirst ys)


deriveShow1Methods [d|instance Show1 (ViewResult MaybeSelector)|]
-- deriveShow1Methods [d|instance Show1             MaybeSelector   |]


