{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Vassal.RangeSelector where

import Vassal.Class
import Vassal.Orphans ()

import Prelude hiding (lookup, null, (.))

import Vassal.AppendIntervalMap (AppendIntervalMap, WithInfinity (..))
import qualified Vassal.AppendIntervalMap as IMap
import Control.Category ((.))
import Control.Lens.Indexed (FoldableWithIndex, FunctorWithIndex, TraversableWithIndex, itraverse)
import Data.Aeson

import Data.Align
import Data.AppendMap ()
import Data.IntervalMap.Interval (Interval(..))
import Data.Constraint
import Data.Functor.Classes
import Data.Functor.Apply
import qualified Data.IntervalMap as BaseIMap
import Data.Map.Monoidal (MonoidalMap)
import Data.Maybe (isJust)
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup (Option (..), Semigroup)
import Data.Set (Set)
import qualified Data.Set as Set
import Reflex.FunctorMaybe

import Vassal.WrappedShow1


newtype RangeSelector e a = RangeSelector
  { unRangeSelector :: (AppendIntervalMap (Interval e)) a }
  deriving
    ( Eq, Eq1
    , Ord, Ord1
    , Show
    , Functor, Foldable, Traversable
    , Monoid, Semigroup
    , FromJSON, FromJSON1
    , ToJSON, ToJSON1
    , FunctorMaybe
    , Align
    , Apply
    )

type RangeSelector' e = RangeSelector (WithInfinity e)

viewRangeAll :: Bounded e => a -> RangeSelector e a
viewRangeAll = RangeSelector . IMap.singleton (ClosedInterval minBound maxBound)

viewRangeBetween :: (e, e) -> a -> RangeSelector e a
viewRangeBetween (k1, k2) = RangeSelector . IMap.singleton (ClosedInterval k1 k2)

viewRangeExactly :: e -> a -> RangeSelector e a
viewRangeExactly k = RangeSelector . IMap.singleton (ClosedInterval k k)

viewRangeSet :: Ord e => Set e -> a -> RangeSelector e a
viewRangeSet ks a = RangeSelector $ IMap.fromSet (const a) (Set.mapMonotonic eqK ks)
  where
    eqK k = ClosedInterval k k

-- type RangeView e v = View (RangeSelector e) v
-- type RangeView' e v = View (RangeSelector (WithInfinity e)) v

instance (Ord e) => ViewSelector (RangeSelector e) where
  newtype ViewResult (RangeSelector e) v = RangeView { unRangeView :: MonoidalMap e v }
    deriving (Eq, Ord, Eq1, Ord1, Show, Functor, Foldable, Traversable, Semigroup, Monoid, FunctorMaybe, FromJSON1, ToJSON1, FromJSON, ToJSON, Align, Apply)
  type ViewIndex (RangeSelector e) = e

  lookup k (RangeSelector xs) = getOption $ foldMap (Option . Just) $ IMap.containing xs k
  -- viewIsMonoid = Sub Dict
  -- viewIsSemigroup = Sub Dict
  viewSelectorIsSemigroup = Sub Dict

instance Ord e => SubViewSelector (RangeSelector e) where
  isSubView (RangeSelector v) (RangeSelector v') = IMap.coveredBy (IMap.coveringSet v') (IMap.coveringSet v)

instance (FromJSON e, FromJSONKey e, ToJSON e, ToJSONKey e, Ord e) => ViewSelectorJSON (RangeSelector e) where
  viewSelectorIsFromJSON = Sub Dict
  viewIsFromJSON = Sub Dict
  viewSelectorIsToJSON = Sub Dict
  viewIsToJSON = Sub Dict


-- deriving instance (Eq e, Eq v) => Eq1 (ViewResult (RangeSelector e) v)
--   -- liftEq f (RangeView p1) (RangeView p2) = p1 == p2 && liftEq f s1 s2
-- 
-- deriving instance (Ord e, Ord v) => Ord1 (ViewResult (RangeSelector e))
--   -- liftCompare f (RangeView s1 p1) (RangeView s2 p2) = compare p1 p2 <> liftCompare f s1 s2

-- instance (Semigroup a, Ord e) => Semigroup (ViewResult (RangeSelector e) v a) where
--   RangeView i1 xs1 <> RangeView i2 xs2 = RangeView i $ MMap.unionWith const xs1 $ iMapMaybe inI xs2
--     where
--       i = i1 <> i2
--       inI k v = if null $ IMap.containing i k then Nothing else Just v

-- instance (Semigroup a, Ord e) => Monoid (ViewResult (RangeSelector e) v a) where
--   mappend = (<>)
--   mempty = RangeView mempty MMap.empty

-- instance (Ord e) => FunctorMaybe (ViewResult (RangeSelector e) v) where
--   fmapMaybe f (RangeView i xs) = RangeView i' $ iMapMaybe inI xs
--     where
--       i' = fmapMaybe f i
--       inI k v = if null $ IMap.containing i' k then Nothing else Just v

instance Ord e => FunctorWithIndex e (ViewResult (RangeSelector e))
instance Ord e => FoldableWithIndex e (ViewResult (RangeSelector e))
-- This is not 100% cromulent, but i think it's "correct" for the way they get
-- used, which is poking around in Views to make FunctorMaybe trim out just enough data properly
instance Ord e => TraversableWithIndex e (ViewResult (RangeSelector e)) where
  itraverse f (RangeView xs) = RangeView <$> itraverse f xs

-- produce a view that covers a single point, useful for NotifyHandlers
-- toRangeView1 :: (Semigroup a, Ord e) => RangeSelector e a -> e -> Maybe v -> ViewResult (RangeSelector e) v a
-- toRangeView1 vs e xs = RangeView (IMap.fromList $ toList $ (k,) <$> vs') (MMap.fromList $ toList $ (,) <$> e' <*> xs)
--   where
--     k = ClosedInterval e e
--     vs' = lookup e vs
--     e' = e <$ vs'


-- toRangeView :: Ord e => RangeSelector e a -> [(e, v)] -> ViewResult (RangeSelector e) v a
-- toRangeView (RangeSelector vs) v = RangeView vs $ MMap.fromList v

-- these are a terrible hack to get through the release.  the real deal would be
-- to make the query just test each range
isCompleteSelector :: Ord k => RangeSelector' k a -> Bool
isCompleteSelector (RangeSelector (IMap.AppendIntervalMap vs)) = isJust $ BaseIMap.lookup (ClosedInterval LowerInfinity UpperInfinity) vs

deriveShow1Methods [d|instance (Show e) => Show1 (ViewResult (RangeSelector e))|]
deriveShow1Methods [d|instance (Show e) => Show1             (RangeSelector e ) |]

-- instance (FromJSON k, Ord k, FromJSON v, FromJSONKey k) => FromJSON1 (ViewResult (RangeSelector k)) where
--   liftParseJSON = $(mkLiftParseJSON defaultOptions 'RangeView)
-- instance (Ord k, Semigroup a, FromJSON k, FromJSON a, FromJSON v, FromJSONKey k) => FromJSON (ViewResult (RangeSelector k) v a) where
--   parseJSON = $(mkParseJSON defaultOptions 'RangeView)
-- 
-- instance (ToJSON k, Ord k, ToJSON v, ToJSONKey k) => ToJSON1 (ViewResult (RangeSelector k) v) where
--   liftToEncoding = $(mkLiftToEncoding defaultOptions 'RangeView)
--   liftToJSON = $(mkLiftToJSON defaultOptions 'RangeView)
-- instance (Ord k, Semigroup a, ToJSON k, ToJSON a, ToJSON v, ToJSONKey k) => ToJSON (ViewResult (RangeSelector k) v a) where
--   toEncoding = $(mkToEncoding defaultOptions 'RangeView)
--   toJSON = $(mkToJSON defaultOptions 'RangeView)
