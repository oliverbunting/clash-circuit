{-|
Copyright  :  (C) 2021, Oliver Bunting
License    :  BSD2 (see the file LICENSE)
Maintainer :  Oliver Bunting <oliver.bunting@gmail.com>

Unsafe primitives
-}
module Clash.Circuit.Unsafe
(
  lower
)
where

import qualified Unsafe.Linear as Unsafe (toLinear, toLinear2)
import qualified System.IO.Unsafe as Unsafe
import           Data.IORef ( newIORef, readIORef, writeIORef )

-- NonLinear functions
import qualified Prelude as NonLinear


-- | Reduces the order of a high-order linear function argument
--
-- ** SAFETY **
--
-- If the result is consumed linearly, its believed this function is safe (inc threading ?).
-- If consumed non-linearly, it is highly unsafe, and likely to lead to runtime errors
--
-- ==== Example
--
-- -- The following is unsound, and will produce an error at runtime
-- > main :: IO ()
-- > main = putStrLn (snd (lower (\x -> x) False))
lower ::  ((a ⊸ b) ⊸ r) ⊸ b ⊸ (r,a)
lower = Unsafe.toLinear2 lower'
  where
    lower' :: ((a ⊸ b) ⊸ r) -> b -> (r, a)
    lower' f b = Unsafe.unsafePerformIO NonLinear.$ do
      ref <- newIORef NonLinear.undefined
      let !r = f (Unsafe.toLinear (\a -> Unsafe.unsafePerformIO (b NonLinear.<$ writeIORef ref a)))
      a <- readIORef ref
      NonLinear.pure (r, a)
{-# NOINLINE lower #-}
