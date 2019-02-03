{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Vassal.Class where

import Prelude hiding (lookup, null, filter, (.))

import Control.Category ((.))
import Control.Lens.Indexed (FunctorWithIndex, TraversableWithIndex, imap, itraverse)
import Control.Monad (join, void)
import Data.Aeson
import Data.Aeson.TH
import Data.Align
import Data.AppendMap ()
import Data.Bifunctor
import Data.Bool (bool)
import Data.Constraint
import Data.Foldable
import Data.Functor.Apply
import Data.Semigroup (Semigroup, First(..))
import Reflex.FunctorMaybe
import Data.These (These(..), these)

-- | this is the parametric replacement for *crop*.
-- chop :: forall a b c v x . (Semigroup a, ViewSelector v) => (a -> b -> Maybe c) -> v a -> View v x b -> View v x c

-- NOTE: for maximum sanity, the parameter 'v' is the instance itself, 'a' is
-- the query witness, 'b' is the value result of a query
data View v b a = View
  { _view_selector :: !(v a)
  , _view_result :: !(ViewResult v b)
  } deriving (Functor, Foldable, Traversable)

toView :: ViewSelector v => v a -> ViewResult v b -> View v b a
toView vs v = View vs $ v'
  where
    View _ v' = cropView (void vs) $ View vs v

instance ViewSelector v => Bifunctor (View v) where
  bimap f g (View vs v) = View (fmap g vs) (fmap f v)

deriving instance (Show (v a), Show (ViewResult v b)) => Show (View v b a)
deriving instance (Eq (v a), Eq (ViewResult v b)) => Eq (View v b a)
deriving instance (Ord (v a), Ord (ViewResult v b)) => Ord (View v b a)

instance ViewSelector v => FunctorMaybe (View v b) where
  fmapMaybe f (View vs v) = View vs' (iMapMaybe (\i x -> x <$ lookup i (fmap First vs)) v)
    where
      vs' = fmapMaybe f vs

instance (Semigroup a, ViewSelector v) => Semigroup (View v b a) where
  (<>) = appendViews

-- | combine views in a simple way where the View does not natively support deletes, but can do leftmost replacements
appendViewsConst :: forall v b a. (Semigroup a, ViewSelector v) => View v b a -> View v b a -> View v b a
appendViewsConst (View vs v) (View vs' v') = View (vs <> vs') (alignWith (these id id const) v v')
  \\ (viewSelectorIsSemigroup :: Semigroup a :- Semigroup (v a))

-- | combine View's in a delete-supporting way.  anything known to both views, even missing values, is taken from the leftmost view
appendViewsDefault :: forall v b a. (Semigroup a, ViewSelector v) => View v b a -> View v b a -> View v b a
appendViewsDefault (View vs v) (View vs' v') = View (fmap (these id id (<>)) vsA) $ iMapMaybe f vA
  where
    vsA = align vs vs'
    vA = align v v'

    f :: ViewIndex v -> These b b -> Maybe b
    f i0 = case lookup i0 vsA of
      Nothing -> const Nothing
      Just (This _) ->    these Just (const Nothing) (\x _ -> Just x) -- This/That is a bit nonsense, having data that you claim not to know is invalid.  this maybe should cause an exception?
      Just (That _) ->    these (const Nothing) Just (\_ y -> Just y)
      Just (These _ _) -> these (const Nothing) Just (\x _ -> Just x)

instance (Semigroup a, ViewSelector v) => Monoid (View v b a) where
  mempty = View nil nil

cropViewDefault :: (ViewSelector v, Semigroup a) => v a -> View v b a' -> View v b a
cropViewDefault vs (View vs' v) = (View (vs <. vs') (iMapMaybe (\i x -> x <$ lookup i vs) v))

class ( TraversableWithIndex (ViewIndex v) (ViewResult v)
      , FunctorMaybe (ViewResult v)
      , Align (ViewResult v)
      , Apply (ViewResult v)
      , FunctorMaybe v
      , Align v
      , Apply v
      ) => ViewSelector v where

  data ViewResult v :: * -> *
  type ViewIndex v

  -- we could do this with QuantifiedConstraints, in 8.6
  viewSelectorIsSemigroup :: Semigroup a :- Semigroup (v a)

  lookup :: Semigroup a => ViewIndex v -> v a -> Maybe a


  cropView :: forall a a' b . Semigroup a => v a -> View v b a' -> View v b a
  cropView = cropViewDefault

  appendViews :: forall b a. Semigroup a => View v b a -> View v b a -> View v b a
  appendViews = appendViewsDefault

class ViewSelector v => SubViewSelector v where
  -- TODO: I don't think i can make this a method exactly as this,  I don't see a way to write this for Compose.
  -- roughly  intersection x y == xx
  -- isSubView should evaluate to True if all of x is selected by y

  isSubView :: v a -> v b -> Bool

fromView :: SubViewSelector v => v a -> View v b a' -> Maybe (ViewResult v b)
fromView vs v@(View vs' _) =
  if vs `isSubView` vs'
  then Just $ _view_result $ cropView (void vs) v
  else Nothing


class ViewSelector v => ViewSelectorJSON v where
  viewSelectorIsFromJSON :: FromJSON a :- FromJSON (v a)
  viewSelectorIsToJSON   :: ToJSON a :- ToJSON (v a)
  viewIsFromJSON         :: FromJSON b :- FromJSON (ViewResult v b)
  viewIsToJSON           :: ToJSON b :- ToJSON (ViewResult v b)

-- because of the combining aspect of how these things get used, there will
-- also be an extra parameter that must be carried around with both queries and
-- their responses to tie response back to their queries.  That may be
-- explained in detail later, but for now, there will need to be some extra,
-- functorial data, that can usually be counted on to be a Semigroup

infixl 3 </\>, <\/>

-- these only satisfy lattice laws if a is an idempotent semigroup.
(</\>) :: (Semigroup a, Apply f) => f a -> f a -> f a
(</\>) = liftF2 (<>)
(<\/>) :: (Semigroup a, Align f) => f a -> f a -> f a
(<\/>) = alignWith (these id id (<>))

tightenView :: forall v b a. ViewSelector v => View v b a -> View v b a
tightenView v@(View vs _) = View vs v'
  where
    View _ v' = cropView (fmap First vs) v



viewSelects :: (Semigroup a, ViewSelector v) => ViewIndex v -> v a -> Bool
viewSelects i vs = not $ null $ lookup i vs

-- TODO: add to reflex and/or use Data.Witherable.Filterable
catMaybes :: FunctorMaybe f => f (Maybe a) -> f a
catMaybes = fmapMaybe id
{-# INLINE catMaybes #-}

filter :: FunctorMaybe f => (a -> Bool) -> f a -> f a
filter p = fmapMaybe (join $ bool (const Nothing) Just . p)
{-# INLINE filter #-}

iMapMaybe :: (FunctorWithIndex i t, FunctorMaybe t) => (i -> a -> Maybe b) -> t a -> t b
iMapMaybe f = catMaybes . imap f

iWither :: (TraversableWithIndex i t, FunctorMaybe t, Applicative f) => (i -> a -> f (Maybe b)) -> t a -> f (t b)
iWither f = fmap catMaybes . itraverse f

return []

instance (FromJSON a, FromJSON b, ViewSelectorJSON v) => FromJSON (View v b a) where
  parseJSON = $(mkParseJSON defaultOptions 'View)
    \\ (viewSelectorIsFromJSON :: FromJSON a :- FromJSON (v a))
    \\ (viewIsFromJSON :: FromJSON b :- FromJSON (ViewResult v b))

instance (ToJSON b, ToJSON a, ViewSelectorJSON v) => ToJSON (View v b a) where
  toEncoding = $(mkToEncoding defaultOptions 'View)
    \\ (viewSelectorIsToJSON :: ToJSON a :- ToJSON (v a))
    \\ (viewIsToJSON :: ToJSON b :- ToJSON (ViewResult v b))

  toJSON = $(mkToJSON defaultOptions 'View)
    \\ (viewSelectorIsToJSON :: ToJSON a :- ToJSON (v a))
    \\ (viewIsToJSON :: ToJSON b :- ToJSON (ViewResult v b))
