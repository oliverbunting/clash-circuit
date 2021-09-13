{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- {-# OPTIONS_GHC -freduction-depth=0 #-}
{-|
Module      : Clash.Circuit
Description : High-level structural Circuit composition in Clash
Copyright   : (c) Oliver Bunting, 2021
License     : BSD3
Maintainer  : oliverbunting@gmail.com
Stability   : experimental
Portability : POSIX

= Introduction

The <https://clash-lang.org/ Clash > compiler translates Haskell descriptions of hardware
into `verilog`, `system-verilog`, or `VHDL` code.

This module provides a set of high-level abstractions for the structural composition
of hardware descriptions.

= Motivation

Hardware descriptions are typically extremely verbose. By the nature of the task, every physical
wire in a circuit is described, and requires its connectivity specified. This leads to an explosion in
code size when circuit fragments are composed. Consider a simple Axi4 memory-mapped RAM. In Verilog:

@
   // A module declaratino with a single Axi4 interface in verilog
   module example #(
      parameter ADDR = 64;
      parameter DATA = 64;
      parameter ID   = 4;
      parameter AW_USER = 1;
      parameter W_USER  = 1;
      parameter B_USER  = 1;
      parameter AR_USER = 1;
      parameter R_USER  = 1
    ) (
    input  wire     aClk;
    input  wire     aResetN;

    // Write address channel
    input  wire                 sAxi_awValid;
    output wire                 sAxi_awReady;
    input  wire [ADDR-1 : 0]    sAxi_awAddr;
    input  wire [7 : 0 ]        sAxi_awLen;
    input  wire [2 : 0]         sAxi_awSize;
    input  wire [1 : 0]         sAxi_awBurst;
    input  wire [ID-1 : 0]      sAxi_awId;
    input  wire [3 : 0]         sAxi_awCache;
    input  wire [2 : 0]         sAxi_awProt;
    input  wire [3 : 0]         sAxi_awQos;
    input  wire [AW_USER-1 : 0] sAxi_awUser;

    // Write data channel
    input  wire                 sAxi_wValid;
    output wire                 sAxi_wReady;
    input  wire [DATA-1 : 0]    sAxi_wData;
    input  wire [ID-1 : 0]      sAxi_wId;
    input  wire                 sAxi_wLast;
    input  wire [DATA/8-1 : 0]  sAxi_wStrb;
    input  wire [W_USER-1 : 0]  sAxi_wUser;

    // Write response channel
    output  wire                sAxi_bValid;
    input   wire                sAxi_bReady;
    output  wire [ID-1 : 0]     sAxi_bId;
    output  wire [1 : 0]        sAxi_bResp;
    output  wire [B_USER-1 : 0] sAxi_bUser;

    // Read address channel
    input  wire                 sAxi_arValid;
    output wire                 sAxi_arReady;
    input  wire [ADDR-1 : 0]    sAxi_arAddr;
    input  wire [7 : 0 ]        sAxi_arLen;
    input  wire [2 : 0]         sAxi_arSize;
    input  wire [1 : 0]         sAxi_arBurst;
    input  wire [ID-1 : 0]      sAxi_arId;
    input  wire [3 : 0]         sAxi_arCache;
    input  wire [2 : 0]         sAxi_arProt;
    input  wire [3 : 0]         sAxi_arQos;
    input  wire [AR_USER-1 : 0] sAxi_arUser;

    // Read data channel
    output  wire                sAxi_rValid;
    input   wire                sAxi_rReady;
    output  wire [ID-1 : 0]     sAxi_rId;
    output  wire [DATA-1 : 0]   sAxi_rData;
    output  wire [1 : 0]        sAxi_rResp;
    output  wire                sAxi_rLast;
    output  wire [R_USER-1 : 0] sAxi_rUser;

    // Low power interface
    input wire                  sAxi_cSysReq;
    output wire                 sAxi_cSysAck;
    input wire                  sAxi_cActive
   );
@

The module is necessarily verbose, in verilog at least. system-verilog and VHDL address this to some extent,
with interfaces and record types respectively. However, we aim to do better in Haskell, and this is my attempt at that.

= Examples

`Clash.Circuit` allows the previous verilog module to be declared as


> simple :: Axi4' ("Addr" ::: 64) ("Data" ::: 64) ("Id" ::: 4)) ⊸ Circuit ()
> simple = error "Undefined"


Even more compelling is composition:


> interconnect
>   :: forall n addr data id.
>   -> Axi4' addr data id
>   -> Circuit (Vec n (Axi4' addr data id))
> interconnect = error "Undefined"
>
> compose :: Axi4' 64 64 4 ⊸ Circuit ()
> compose x = do
>   (a :> b :> c :> d) <- interconnect x
>   simple a
>   simple b
>   simple c
>   simple d

-- Outstanding issues:
--
--  - [ ] Check the continuation magic in FreeT is ok...
--  - [ ] Clash need to support transformation of higher over linear arguments
--  - [ ] Implement some things, check its useable. Movable instances will be key to this.
--  - [ ] Thoughts on recursion:
--        ArrowLoop is ok? if the feedback loop is a linear function
--        loop :: ((a,(d ⊸ d')) ⊸ (b,(d ⊸ d'))) ⊸ a ⊸ b
-}
module Clash.Circuit
-- (
--   -- * Classes
--   Bus(..),

--   -- * Types
--   Circuit(..),
--   -- runCircuit,
--   -- CircuitArrow(..),

--   C(..),
--   -- runC,
-- )
where

-- Linear functions
import           Control.Arrow.Linear ( Kleisli(..), Arrow, ArrowApply )
import           Control.Category.Linear ( Category )
import qualified Control.Functor.Linear as Control
import           Control.Monad.Constrained.FreeT.Linear
import qualified Data.Functor.Linear as Data
import           Prelude.Linear ((&),($), (.), id, error, const)
import qualified Unsafe.Linear as Unsafe
import qualified System.IO.Unsafe as Unsafe
import           Data.IORef
-- NonLinear functions
-- Linear import
import GHC.Types (Type)
import qualified Prelude as NonLinear


-- import Prelude ((.))
-- import Data.Functor ((<$>))
-- import qualified Control.Applicative as NonLinear (Applicative(..))

import Clash.Signal.Internal (Signal(..))
import Clash.Prelude (Constraint)


-- Reading list:
--
-- http://www.cs.nott.ac.uk/~psxjb5/publications/2013-SculthorpeBrackerGiorgidzeGill-TheConstrainedMonadProblem.pdf


-- | Circuit representation
--
-- Provides an unconstrained instances of linear monad of C via FreeT
newtype Circuit a = Circuit (FreeT Bus C a)
    deriving (Control.Functor, Data.Functor) via FreeT Bus C
    deriving (Control.Applicative, Data.Applicative) via FreeT Bus C
    deriving (Control.Monad) via FreeT Bus C

-- | Lift C into Circuit
--
liftC :: (Bus a) => C a ⊸ Circuit a
liftC m = Circuit (FreeT (m `bindC`))

-- | Lower Circuit to C
lowerC :: (Bus a) =>  Circuit a ⊸ C a
lowerC (Circuit (FreeT m)) = m pureC


-- | Inner Circuit representation
--
-- A Bus must be able to lift into C via `pureC`. This enables
-- C to be embedded in `Circuit`
data C a where
  C :: (Bus a) => (BwdOf a ⊸ FwdOf a) ⊸ C a

-- | Consume C
--
-- Note: Record accessors are not linear
runC :: (Bus a) => C a ⊸ BwdOf a ⊸ FwdOf a
runC (C f) = f


-- | Instances of Bus are the types through which circuits are interfaced
--
class Bus a where
  -- | The type of the Forward and Backward unidirectional channels forming the bidirectional Bus
  type Channel (dir :: BusDir) a = r | r -> a

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


-- | A constrained fmap is provided for busses
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
newtype CC a = CC a

instance Bus a => Bus (C a) where
  type Channel 'Forward (C a) = CC (Channel 'Forward a)
  type Channel 'Backward (C a) = CC (Channel 'Backward a)
  pureC (C g) = C (\(CC bC) -> CC (g bC))
  bindC m f = f (g m)
    where
      g :: C (C a) ⊸ C a
      g (C h) = C (\b -> h (CC b) & \case (CC b') -> b')


-- | The Unit bus
--
-- The type `a ⊸ Circuit ()` indicates a circuit is a slave to bus b, and is not a bus master.
--
-- The Constructor `NC` comes from the commonly used schematic abbreviation for "not connected"
instance Bus () where
  type Channel d () = ()
  pureC () = C (\() -> ())


-- | Product of a Busses
instance (Bus a, Bus b) => Bus (a,b) where
  type Channel d (a,b) = (Channel d a, Channel d b)
  pureC (a,b) = C (\(x,y) -> (pureC a & \case C f -> f x, pureC b & \case C f -> f y))


-- | Busses may be higher order functions of Busses
data Fn a b = Fn a b

instance (Bus a, Bus b) => Bus (a ⊸ b) where
  type Channel d (a ⊸ b) = Fn (Channel d b) (Channel (RBusDir d) a)
  pureC f = C (\(Fn bwdB fwdA) -> lower (\x -> C x `bindC` (pureC . f)) fwdA `applyB` bwdB)
    where
      applyB :: (C b, BwdOf a) ⊸ BwdOf b ⊸ Channel 'Forward (a ⊸ b)
      applyB (cB, bwdA) bwdB = cB & \case (C g) -> Fn (g bwdB) bwdA


-- | Reduces the order of a high-order linear function argument
--
-- Todo: Will need special handling by clash, without doubt
-- defer until abstraction proven
--
-- ** SAFETY **
--
-- If the result is consumed linearly, its believed this function is safe (threads?).
-- If consumed non-linearly, it is highly unsafe.
lower ::  ((a ⊸ b) ⊸ r) ⊸ b ⊸ (r,a)
lower = Unsafe.toLinear2 lower'
  where
    lower' :: ((a ⊸ b) ⊸ r) -> b -> (r, a)
    lower' f b = Unsafe.unsafePerformIO $ do
      ref <- newIORef NonLinear.undefined
      let !r = f (Unsafe.toLinear (\a -> Unsafe.unsafePerformIO (b NonLinear.<$ writeIORef ref a)))
      a <- readIORef ref
      NonLinear.pure (r, a)
{-# NOINLINE lower #-}



































-- newtype Df a = Df (Bwd ⊸ Fwd a)
--     -- deriving Bus via (Bwd -> Fwd a)


-- -- | The forward component of the Df Bus
-- data Fwd a = Fwd
--     { _valid :: Bool  -- ^ Indicates `dat` is valid
--     , _dat   :: a     -- ^ The data transferred by the bus
--     }

-- -- | The backward component of the Df Bus
-- newtype Bwd = Bwd
--     {  _ready :: Bool  -- ^ Handshake signal
--     }








-- -- | A Signal of a Bus is a Bus
-- instance (Bus a) => Bus (Signal dom a) where
--   type FwdOf (Signal dom a) = Signal dom ( FwdOf a)
--   type BwdOf (Signal dom a) = Signal dom ( BwdOf a)
--   pureC f = C (\b -> (runC Data.<$> (pureC Data.<$> f)) Control.<*> b)




-- | Arrow representation of circuits
--
-- Allows the circuit designer to use arrow notation to define circuit connectivity
--
-- do-notation is in general a better user experience than arrow notation.
-- However, an implementation of ArrowLoop for CircuitArrow is (believed to be) realizable.
--
-- Its not clear that `MonadFix` is realizable, or even what a sensible definition of `MonadFix` is, although
-- `(a ⊸ m b) ⊸ m ()` is my suggestion.
--
-- Therefore, for recursive circuits, this is the best bet, currently.
-- newtype CircuitArrow a b = CircuitArrow (a ⊸ Circuit b)
--     deriving (Category, Arrow, ArrowApply) via (Kleisli (FreeT Bus C))


-- -- * Orphan instances (for clash)

-- -- Speak with christiaanb about these.
-- -- Signal needs careful handling.
-- -- pretty sure a heavy dose of unsafe is required here

-- instance Data.Functor (Signal dom) where
--   fmap _ = error "Todo"

-- instance Control.Functor (Signal dom) where
--   fmap f = error "Todo" f

-- instance Data.Applicative (Signal dom) where
--   pure = NonLinear.pure
--   (<*>) = error "Todo"

-- instance Control.Applicative (Signal dom) where
--   pure = error "Todo"
--   (<*>) = error "Todo"
