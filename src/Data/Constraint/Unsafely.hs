{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Data.Constraint.Unsafely (
  -- * Introduction
  -- $introduction

  -- * Interface
  Unsafely, unsafely,

  -- * Example: semigroup instance
  -- $semigroup

  -- ** Allow unsafe operations globally
  -- $really

  -- * Example: restrict the access for unsafe operations
  -- $restriction
) where
import Data.Constraint.Unsafely.Really

{- $introduction #introduction#
This module aims at providing simple interface to express some operations or instance are considered to be /unsafe/.

The motivation of this package is somewhat similar to
@<https://ghc.haskell.org/trac/ghc/ticket/7642 NullaryTypeClasses>@ extension,
but permits more flexible access control. See <#g:3 example> for more detail.
-}

{- $semigroup
The first example is about /unsafe/ instances. Below, we use @EmptyDataDecls@, @FlexibleContexts@, @FlexibleInstances@,
@RankNTypes@, @TypeSynonymInstances@ and @UndecidableInstances@ extensions.

Let's think about semigroups. Semigroups are sets equipped with binary relation, which satisfies assosiative law. i.e:

> a + (b + c) == (a + b) + c

We denote this binary operation as @.+.@.

@
class Semigroup a where
  (.+.) :: a -> a -> a

infixl 6 .+.
@

'Int', 'Integer' and 'Rational's are naturally semigroups with their ordinary addition:

@
instance Semigroup Int where
  (.+.) = (+)
instance Semigroup Integer where
  (.+.) = (+)
instance Semigroup Rational where
  (.+.) = (+)
@

But, unfortunately, 'Double' is not semigroup because it doesn't satisfies associative law.
For example:

@
(1.9546929672907305 + 2.0) + 0.14197132377740074 == 4.096664291068132
1.9546929672907305 + (2.0 + 0.14197132377740074) == 4.096664291068131 -- different!
@

But, in some cases, we need @Semigroup@ instance for 'Double' for convenience.
So we provide @Semigroup Double@ instance with the /unsafety/ precondition:

@
data ViolateAssocLaw

unsafelyViolate :: ('Unsafely' ViolateAssocLaw => a) -> a
unsafelyViolate = 'unsafely' ('Proxy' :: 'Proxy' ViolateAssocLaw)

instance 'Unsafely' ViolateAssocLaw => Semigroup Double where
  (.+.) = (+)
@

In above, @ViolateAssocLaw@ is the tag for unsafe @Semigroup@ instance which may violate associative law.
For the convenience, we also defined the convenient function @unsafelyViolate@.

With the code above, we get the following:

>>> 1 .+. 2 :: Int
3
>>> 2 .+. 0.5 :: Rational
5 % 2
>>> 3.0 .+. 0.4 :: Double
    No instance for (Data.Constraint.Unsafely.Really.ReallyUnsafely ViolateAssocLaw)
      arising from a use of `.+.'
    Possible fix:
      add an instance declaration for
      (Data.Constraint.Unsafely.Really.ReallyUnsafely ViolateAssocLaw)
    In the expression: 1.0 .+. 2.0 :: Double
    In an equation for `it': it = 1.0 .+. 2.0 :: Double
>>> unsafelyViolate $ 3.0 .+. 4.0 :: Double
7.0
-}

{- $really
Things seems to be good! But, what is the 'ReallyUnsafely' type-class?

This class prevents users from writing 'Unsafely' instace globaly and contaminating entire program.
This is because 'ReallyUnsafely' is not exported in this module and 'ReallyUnsafely' is the superclass of 'Unsafely'.

On the other hand, there is the case to permit /unsafe/ operations globally. In such situation, "Data.Constraint.Unsafely.Really" module enables you to write such an instace:

>>> 3 .+. 4 :: Double
<interactive>:4:3:
    No instance for (ReallyUnsafely * ViolateAssocLaw)
      arising from a use of `.+.'
    Possible fix:
      add an instance declaration for (ReallyUnsafely * ViolateAssocLaw)
    In the expression: 3 .+. 4 :: Double
    In an equation for `it': it = 3 .+. 4 :: Double
>>> import Data.Constraint.Unsafely.Really
>>> instance ReallyUnsafely ViolateAssocLaw
>>> 3 .+. 4 :: Double
7.0

As above, once you declare the 'ReallyUnsafely' instance for your tag, you can use unsafe operations anywhere in your code.
-}

{- $restriction
Another example is slightly safer 'unsafePerformIO'.
As above, we can provide the following interface:

@
module Main where
import Data.Constraint.Unsafely
import Data.IORef
import Data.Proxy
import System.IO.Unsafe

saferUnsafePerformIO :: Unsafely IO => IO a -> a
saferUnsafePerformIO = unsafePerformIO

unsafelyIO :: (Unsafely IO => a) -> a
unsafelyIO = unsafely (Proxy :: Proxy IO)
@

In this example, we use 'IO' type as the tag of unsafety precondition instead of defining new empty data-type.
'Unsafely' class is kind-polymorphic, so it can take any type of any kind as its parameter.

With this, we can create the global state, which is inaccessible without @Unsafely IO@.

@
global :: Unsafely IO => IORef Int
global = saferUnsafePerformIO $ newIORef 0

main :: IO ()
main = do
  unsafelyIO $ readIORef global
  return ()
@
-}
