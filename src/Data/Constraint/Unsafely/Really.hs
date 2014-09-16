{-# LANGUAGE FlexibleContexts, FlexibleInstances, PolyKinds, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, UndecidableInstances                  #-}
-- | Really unsafe module to provide internal interface.
-- This module should be imported if you wish to allow the unsafe computation globally.
--
-- But be careful: the unsafety contaminates entire the program once you write
-- the instance for 'ReallyUnsafe' somewhere.
module Data.Constraint.Unsafely.Really (
  ReallyUnsafely, Unsafely, unsafely
  ) where
import Unsafe.Coerce

-- | The trick type-class to prevent providing global instances for @Unsafely@.
-- This class is not exported in "Data.Constraint.Unsafely", so if you want to
-- permit unsafe operations globally, you should import this module directly.
class ReallyUnsafely t

-- | The constraint for the instances which might be /unsafe/ in some sence.
-- @t@ in @Unsafely t@ is the dummy tag for some series of /unsafe/ operations.
class ReallyUnsafely t => Unsafely (t :: k) where
  impossible :: Impossible t
instance ReallyUnsafely t => Unsafely t where
  impossible = Impossible
  
-- | Witness to allow /unsafe/ computation.
-- This type never be exported, which ensures that you can call @unsafely@ safely.
data Impossible (t :: k) = Impossible

newtype Thingy t a = Thingy (Unsafely t => a)

-- | Evaluate the value which might be unsafe.
unsafely :: forall proxy t a. proxy t -> (Unsafely t => a) -> a
unsafely _ f = unsafeCoerce (Thingy f :: Thingy t a) Impossible
