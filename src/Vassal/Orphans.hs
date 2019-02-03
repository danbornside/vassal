{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wall -Werror -Wno-orphans #-}

module Vassal.Orphans where

import Data.Functor.Apply
import Data.Aeson.TH
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Align
import Data.IntervalMap.Interval (Interval(..))
import Data.Functor.Compose (Compose (..))
import Data.Semigroup (Option (..))
import Data.These (These (..))
import Reflex.FunctorMaybe

-- horray, orphans!
-- more orphans!
-- deriving instance FunctorMaybe Option


-- "witherable" has this instance.  I think, if we had a use for this instance (other than "we could use it in theory") we really want:
-- instance (Foldable g, FunctorMaybe f, FunctorMaybe g) => FunctorMaybe (Compose f g)
-- so that we could cut things from f when the contained g's become empty (as in `Foldable.null`).  That's different from the obvious instance below, which corresponds to the other instances for `Compose` in "witherable" and "compactible"
instance (Functor f, FunctorMaybe g) => FunctorMaybe (Compose f g) where
  fmapMaybe f (Compose xs) = Compose (fmap (fmapMaybe f) xs)

instance (Align f, Align g) => Align (Compose f g) where
  nil = Compose nil
  alignWith f (Compose xs) (Compose ys) = Compose $ alignWith f' xs ys
    where
      f' = \case
        This ga -> fmap (f . This) ga
        That gb -> fmap (f . That) gb
        These ga gb -> alignWith f ga gb

deriving instance Align Option

-- deriving instance (Ord k, FromJSONKey k) => FromJSON1 (MonoidalMap k)
-- deriving instance ToJSONKey k => ToJSON1 (MonoidalMap k)

deriving instance Ord k => Apply (MonoidalMap k)

deriveJSON defaultOptions ''Interval
