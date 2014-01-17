{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances, RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances, UndecidableInstances                      #-}
module Main where
import Data.Constraint.Unsafely
import Data.Proxy

class Semigroup a where
  -- | binary operation which satisifies associative law:
  --   @(a .+. b) .+. c == a .+. (b .+. c)@
  (.+.) :: a -> a -> a

infixl 6 .+.

-- | 'Int', 'Integer' and 'Rational' satisfies associative law.
instance Semigroup Int where
  (.+.) = (+)
instance Semigroup Integer where
  (.+.) = (+)
instance Semigroup Rational where
  (.+.) = (+)

-- | Dummy type indicating the computation which may /unsafely/ violates associative law.
data ViolateAssocLaw

-- | Helper function to use /unsafe/ instance for @Semigroup@
unsafelyViolate :: (Unsafely ViolateAssocLaw => a) -> a
unsafelyViolate = unsafely (Proxy :: Proxy ViolateAssocLaw)

-- | 'Double' doesn't satsfies associative law:
--
-- > (1.9546929672907305 + 2.0) + 0.14197132377740074 == 4.096664291068132
-- > 1.9546929672907305 + (2.0 + 0.14197132377740074) == 4.096664291068131
--
-- But sometimes there is the case the instance for @Semigroup@ for Double is required.
-- So we use @Unsafely@ to mark this instance is somewhat unsafe.
instance Unsafely ViolateAssocLaw => Semigroup Double where
  (.+.) = (+)

main :: IO ()
main = do
  print (1 .+. 2 :: Int)
  unsafelyViolate $ print (3 .+. 4 :: Double)
  -- You cannot done as above, if you drop @unsafelyViolate@.
  -- Uncommenting following line causes type error.
  -- print (5 .+. 6 :: Double)


