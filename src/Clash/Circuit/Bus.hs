{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Copyright   : (c) Oliver Bunting, 2021
License     : BSD3
Maintainer  : oliverbunting@gmail.com
Stability   : experimental

Interfaces with bi-directional dataflow between 'Circuit's
|-}

module Clash.Circuit.Bus
(  -- * Class
  Bus(..),
  runC,

  -- * Type functions and aliases
  BusDir(..),
  RBusDir,
  FwdOf,
  BwdOf,

  -- * Data representations
  C(..),

  -- * Constrained type classes
  -- ** Functor
  fmapC,
  -- ** Applicative
  appC,
  -- ** Monad
  joinC,
)where

-- Linear functions
import           Prelude.Linear ((&), (.), id)
import qualified Unsafe.Linear as Unsafe


import qualified Data.Functor.Linear as Data
import qualified Control.Functor.Linear as Control
import Control.Monad.Constrained.FreeT.Linear ( FreeT(..) )
import Clash.Signal.Internal (Signal(..))
import Clash.Circuit.Unsafe

-- | Inner Circuit representation
--
-- A Bus must be able to lift into C via 'pureC'. This enables
-- 'C' to be embedded in 'Circuit'
data C a where
  C :: (Bus a) => (BwdOf a ⊸ FwdOf a) ⊸ C a

-- | Consume 'C'
--
-- Note: Record accessors are not linear
runC :: (Bus a) => C a ⊸ BwdOf a ⊸ FwdOf a
runC (C f) = f


-- | Instances of Bus are the types through which circuits are interfaced
--
class Bus a where
  -- | The type of the Forward and Backward unidirectional channels forming the bidirectional Bus
  type Channel (dir :: BusDir) a = r | r -> a dir

  -- | A bus must define how it is lifted into a circuit and
  --
  -- Todo: For expected instances the definition is identical, and a default implementation should be provided with generics.
  pureC :: a ⊸ C a

  -- | Users are not expected to implement this
  --
  -- The default method suffices for all instances except C (C a)
  --
  -- Todo: singletons for moving this out of the class definition?
  bindC :: (Bus b) => C a ⊸ (a ⊸ C b) ⊸ C b

  default bindC :: (Bus a, Bus b) => C a ⊸ (a ⊸ C b) ⊸ C b
  bindC m f = joinC (pureC f `appC` m)

-- | A constrained Functor instance is provided for all busses
fmapC :: (Bus a, Bus b) => (a ⊸ b) ⊸ C a ⊸ C b
fmapC f a = pureC f `appC` a

appC :: (Bus b) => C(a ⊸ b) ⊸ C a ⊸ C b
appC f a = C (Unsafe.toLinear3 appFn f a)
  where
    appFn :: C (a ⊸ b) -> C a -> BwdOf b -> FwdOf b
    appFn (C f') (C g') bB =
      let
        (Fn fB bA) = f' (Fn bB fA)
        fA = g' bA
      in fB

-- | Remove one level of monad structure
joinC :: Bus a => C (C a) ⊸ C a
joinC x = x `bindC` id

-- | Tag denoting direction of data flow in a bus
data BusDir = Forward | Backward

-- | Type function for reversing a bus direction
type family RBusDir (d :: BusDir) = r | r -> d where
  RBusDir 'Forward = 'Backward
  RBusDir 'Backward = 'Forward

type FwdOf a = Channel 'Forward a
type BwdOf a = Channel 'Backward a


-- **********************************************************
-- Instances
-- **********************************************************

-- | `C` may be applied some than once. The underlying bus representation is unchanged.
--
-- This allows us to provide a default bindC implementation for all other Bus instances
instance Bus a => Bus (C a) where
  type Channel d (C a) = CC d a

  pureC (C g) = C (\(CC bC) -> CC (g bC))

  bindC m f = f (g m)
    where
      g :: C (C a) ⊸ C a
      g (C h) = C (\b -> h (CC b) & \case (CC b') -> b')

newtype CC d a where
  CC :: Channel d a ⊸ CC d a

-- | The Unit bus
--
-- The type `a ⊸ Circuit ()` indicates a circuit is a slave to bus b, and is not a bus master.
instance Bus () where
  type Channel d () = NC d
  pureC () = C (\NCBwd -> NCFwd)

-- The Constructor `NC` comes from the commonly used schematic abbreviation for "not connected"
data NC d where
  NCFwd :: NC 'Forward
  NCBwd :: NC 'Backward


-- | Product of a Busses
instance (Bus a, Bus b) => Bus (a,b) where
  type Channel d (a,b) = (Channel d a, Channel d b)
  pureC (a,b) = C (\(x,y) -> (pureC a & \case C f -> f x, pureC b & \case C f -> f y))


-- | Busses may be higher order functions of Busses
data Fn d a b where
  Fn :: (Channel d b) ⊸ (Channel (RBusDir d) a) ⊸ Fn d b a

instance (Bus a, Bus b) => Bus (a ⊸ b) where
  type Channel d (a ⊸ b) = Fn d b a
  pureC f = C (\(Fn bwdB fwdA) -> lower (\x -> C x `bindC` (pureC . f)) fwdA `applyB` bwdB)
    where
      applyB :: (C b, BwdOf a) ⊸ BwdOf b ⊸ Channel 'Forward (a ⊸ b)
      applyB (cB, bwdA) bwdB = cB & \case (C g) -> Fn (g bwdB) bwdA

-- | Busses may be associated with a @Signal dom@
instance (Bus a) => Bus (Signal dom a) where
  type Channel d (Signal dom a) = Signal dom (Channel d a)
  pureC a = g a & \case (FreeT m) -> m pureC
    where
        g :: Signal dom a ⊸ FreeT Bus C (Signal dom a)
        g x = Data.traverse Control.pure x

-- Todo: These need defining, ideally upstream
instance Data.Functor (Signal dom) where
instance Data.Traversable (Signal dom) where

