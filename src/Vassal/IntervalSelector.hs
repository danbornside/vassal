{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Vassal.IntervalSelector where
import Vassal.Class

import Prelude hiding (lookup, null, (.))

import Control.Category ((.))
import Control.Lens (_1)
import Control.Lens.Indexed (FoldableWithIndex, FunctorWithIndex, TraversableWithIndex, itraverse)
import Data.Aeson
import Data.Align
import Data.AppendMap ()
import Data.Constraint
import Data.Functor.Classes
import Data.Functor.Apply
import Data.IntervalMap.Interval (Interval(..))
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup (First (..), Option (..), Semigroup)
import Data.These (These(..))
import Reflex.FunctorMaybe

import Vassal.WrappedShow1
import Vassal.Orphans ()
import Vassal.AppendIntervalMap (AppendIntervalMap, WithInfinity (..), unionIntervals)
import qualified Vassal.AppendIntervalMap as IMap

newtype IntervalSelector e (i :: *) a = IntervalSelector
  { unIntervalSelector :: AppendIntervalMap (Interval e) a }
  deriving (Eq, Ord, Eq1, Ord1, Show, Functor, Foldable, Traversable, Monoid, Semigroup, FromJSON, FromJSON1, ToJSON, ToJSON1, FunctorMaybe, Align, Apply)

type IntervalSelector' e = IntervalSelector (WithInfinity e)

instance (Ord i, Ord e) => ViewSelector (IntervalSelector e i) where
  -- | though counterintuitivie, this is the most normalized way to represent
  -- this data.  other helpers will give you more intervalmap-ish results
  newtype ViewResult (IntervalSelector e i) v = IntervalView
    { unIntervalView :: MMap.MonoidalMap i (First (v, Interval e))
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, ToJSON, FromJSON, Semigroup, Monoid)
  type ViewIndex (IntervalSelector e i) = Interval e

  viewSelectorIsSemigroup  = Sub Dict

  lookup k (IntervalSelector xs) = getOption $ foldMap (Option . Just) $ IMap.intersecting xs k

  -- i need to think a lot more to come up with a way to support deletable things in IntervalView.  in the mean time, this is a more conservative append
  appendViews = appendViewsConst

  -- this doesn't really do just the right thing.  it's certainly not enough to tell you about deletes, since the objects that are deleted are not selected by the view selector in the first place, and any given 
  cropView :: forall a a' b. Semigroup a => IntervalSelector e i a -> View (IntervalSelector e i) b a' -> View (IntervalSelector e i) b a
  cropView vs (View _ v) = View newVs newV
    where
      newVs = IntervalSelector $ flip foldMap (unIntervalView v) $ \(First (_b, i)) -> maybe mempty (IMap.singleton i) $ lookup i vs
      newV = iMapMaybe (\i x -> x <$ lookup i newVs) v


instance (Ord e, Ord i) => SubViewSelector (IntervalSelector e i) where
  isSubView (IntervalSelector v) (IntervalSelector v') = IMap.coveredBy (IMap.coveringSet v') (IMap.coveringSet v)

instance
    ( Ord i, Ord e
    , FromJSON e, FromJSON i, FromJSONKey i
    , ToJSON e, ToJSON i, ToJSONKey i
    ) => ViewSelectorJSON (IntervalSelector e i) where
  viewSelectorIsFromJSON = Sub Dict
  viewIsFromJSON = Sub Dict
  viewSelectorIsToJSON = Sub Dict
  viewIsToJSON = Sub Dict


instance (Ord e, Ord i) => Align (ViewResult (IntervalSelector e i)) where
  nil = IntervalView (MMap.empty)
  alignWith :: forall a b c.
       (These a b -> c)
    -> ViewResult (IntervalSelector e i) a
    -> ViewResult (IntervalSelector e i) b
    -> ViewResult (IntervalSelector e i) c
  alignWith f (IntervalView xs) (IntervalView ys) = IntervalView $ alignWith f' xs ys
    where
      f' :: These (First (a, Interval e)) (First (b, Interval e))
         -> (First (c, Interval e))
      f' (This (First (x, i)))                 = First (f (This x), i)
      f' (That                 (First (y, j))) = First (f (That y), j)
      f' (These (First (x, i)) (First (y, j))) = First (f (These x y), unionIntervals i j)

instance (Ord e, Ord i) => Apply (ViewResult (IntervalSelector e i)) where
  liftF2
    :: forall a b c.
       (a -> b -> c)
    -> ViewResult (IntervalSelector e i) a
    -> ViewResult (IntervalSelector e i) b
    -> ViewResult (IntervalSelector e i) c
  liftF2 f (IntervalView xs) (IntervalView ys) = IntervalView $ MMap.intersectionWith f' xs ys
    where
      f' :: First (a, Interval e)
         -> First (b, Interval e)
         -> First (c, Interval e)
      f' (First (x, i)) (First (y, j)) = First (f x y, unionIntervals i j)

instance FunctorMaybe (ViewResult (IntervalSelector e i )) where
  fmapMaybe :: forall a b. (a -> Maybe b) -> ViewResult (IntervalSelector e i) a -> ViewResult (IntervalSelector e i) b
  fmapMaybe f = IntervalView . fmapMaybe (traverse . _1 $ f) . unIntervalView

instance FunctorWithIndex (Interval e) (ViewResult (IntervalSelector e i))
instance FoldableWithIndex (Interval e) (ViewResult (IntervalSelector e i))

instance TraversableWithIndex (Interval e) (ViewResult (IntervalSelector e i)) where
  itraverse :: forall f a b. Applicative f => (Interval e -> a -> f b) -> ViewResult (IntervalSelector e i) a -> f (ViewResult (IntervalSelector e i) b)
  itraverse f (IntervalView xs) = IntervalView <$> traverse f' xs
    where
      f' :: First (a, Interval e) -> f (First (b, Interval e))
      f' (First (x, i)) = First . (, i) <$> f i x

deriveShow1Methods [d|instance (Show i, Show e) => Show1 (ViewResult (IntervalSelector e i)) |]
deriveShow1Methods [d|instance (        Show e) => Show1             (IntervalSelector e i) |]

