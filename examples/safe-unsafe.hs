{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module Main where
import Data.Constraint.Unsafely
import Data.IORef
import Data.Proxy
import System.IO.Unsafe

saferUnsafePerformIO :: Unsafely IO => IO a -> a
saferUnsafePerformIO = unsafePerformIO

global :: Unsafely IO => IORef Int
global = saferUnsafePerformIO $ newIORef 0

unsafelyIO :: (Unsafely IO => a) -> a
unsafelyIO = unsafely (Proxy :: Proxy IO)

main :: IO ()
main = do
  unsafelyIO $ readIORef global
  return ()
