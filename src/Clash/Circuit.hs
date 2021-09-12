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
import           Prelude.Linear ((&),($), (.), id, error)
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


-- | Instances of Bus are the types through which circuits are interfaced
--
class (CMonad a) => Bus a where
  -- | The type of the Forward and Backward unidirectional channels forming the bidirectional Bus
  data family Channel (dir :: BusDir) a

data BusDir = Forward | Backward

type family RBusDir (d :: BusDir) = r | r -> d where
  RBusDir 'Forward = 'Backward
  RBusDir 'Backward = 'Forward

type FwdOf a = Channel 'Forward a
type BwdOf a = Channel 'Backward a

type family RChannel (d :: BusDir) a where
  RChannel 'Forward a = Channel 'Backward a
  RChannel 'Backward a = Channel 'Forward a


-- | Restricted Linear Functor over C
class CFunctor a where
  fmapC :: (Bus b) => (a ⊸ b) ⊸ C a ⊸ C b

  default fmapC :: (Bus a, Bus b) => (a ⊸ b) ⊸ C a ⊸ C b
  fmapC f a = pureC f `appC` a

-- | Restricted Linear Applicative over C
class CFunctor a => CApplicative a where
  pureC :: a ⊸ C a

  appC :: (Bus b) => C(a ⊸ b) ⊸ C a ⊸ C b

  default appC :: (Bus b) => C(a ⊸ b) ⊸ C a ⊸ C b
  appC f a = C (Unsafe.toLinear3 appFn f a)
    where
      appFn :: C (a ⊸ b) -> C a -> BwdOf b -> FwdOf b
      appFn (C f') (C g') bB =
        let
          (Fn fB bA) = f' (Fn bB fA)
          fA = g' bA
        in fB

-- | Restricted Linear Monad over C
class CApplicative a => CMonad a where
  bindC :: (Bus b) => C a ⊸ (a ⊸ C b) ⊸ C b

  default bindC :: (Bus a, Bus b) => C a ⊸ (a ⊸ C b) ⊸ C b
  bindC m f = joinC (pureC f `appC` m)


joinC :: Bus a => C (C a) ⊸ C a
joinC (C f) = C (\a -> f (CBwd a) & \case (CFwd b) -> b)

-- **********************************************************
-- Instances
-- **********************************************************

-- | `C` may be applied some than once. The underlying bus representation is unchanged.
--
-- This allows us to provide a default implementation for all other Bus instances
instance Bus a => Bus (C a) where
  data instance Channel 'Forward (C a) = CFwd (Channel 'Forward a)
  data instance Channel 'Backward (C a) = CBwd (Channel 'Backward a)

deriving instance (Bus a) => CFunctor (C a)

instance (Bus a) => CApplicative (C a) where
  pureC (C g) = C (\(CBwd bC) -> CFwd (g bC))

instance (Bus a) => CMonad (C a) where
  bindC m f = f (lowerC m)
    where
      lowerC :: C (C a) ⊸ C a
      lowerC (C g) = C (\b -> g (CBwd b) & \case (CFwd b') -> b')


-- | The Unit bus
--
-- The type `a ⊸ Circuit ()` indicates a circuit is a slave to bus b, and is not a bus master.
--
-- The Constructor `NC` comes from the commonly used schematic abbreviation for "not connected"
instance Bus () where
  data instance Channel d () = NC

deriving instance CFunctor ()
deriving instance CMonad ()
instance CApplicative () where
  pureC () = C (\NC -> NC)


-- | Product of a Busses
instance (Bus a, Bus b) => Bus (a,b) where
  data instance Channel d (a,b) = T2 (Channel d a) (Channel d b)

deriving instance (Bus a, Bus b) => CFunctor (a, b)
deriving instance (Bus a, Bus b) => CMonad (a, b)

instance (Bus a, Bus b) => CApplicative (a,b) where
  pureC (a,b) = C (\(T2 x y) -> T2 (pureC a & \case C f -> f x) (pureC b & \case C f -> f y))


-- | Busses may be higher order functions of Busses
instance (Bus a, Bus b) => Bus (a ⊸ b) where
  data instance Channel d (a ⊸ b) = Fn (Channel d b) (Channel (RBusDir d) a)

deriving instance (Bus a, Bus b) => CFunctor (a ⊸ b)
deriving instance (Bus a, Bus b) => CMonad (a ⊸ b)

instance (Bus a, Bus b) => CApplicative (a ⊸ b) where
  pureC f = C (\(Fn bwdB fwdA) -> lower (\x -> C x `bindC` (pureC . f)) fwdA `applyB` bwdB)
    where
      applyB :: (C b, BwdOf a) ⊸ BwdOf b ⊸ Channel 'Forward (a ⊸ b)
      applyB (cB, bwdA) bwdB = cB & \case (C g) -> Fn (g bwdB) bwdA


-- data (a :-> b) where
--   (:->):: (a ⊸ b) ⊸ (a :-> b)

-- instance Bus (a :-> b) where
--   type Channel 'Forward (a :-> b ) = b
--   type Channel 'Backward (a :-> b ) = a

-- instance BusFunctor (a :-> b) where
--   fmapC = \case ((:->) a) -> C a

-- instance BusApplicative (a :-> b) where
--   pureC = \case ((:->) a) -> C a
  -- appC f



data C a where
  C :: (Bus a) => (BwdOf a ⊸ FwdOf a) ⊸ C a

-- instance Control.Functor C where
--   fmap f a = pure f Control.<*> a

-- instance Control.Applicative C where
--   pure = pureC

-- | Consume C
--
-- Record accessors are not linear
runC :: (Bus a) => C a ⊸ BwdOf a ⊸ FwdOf a
runC (C f) = f



-- | Monadic representation of circuits
--
-- This type allows us to use do notation to define circuit connectivity.
newtype Circuit a = Circuit (FreeT Bus C a)
    deriving (Control.Functor, Data.Functor) via FreeT Bus C
    deriving (Control.Applicative, Data.Applicative) via FreeT Bus C
    deriving (Control.Monad) via FreeT Bus C


-- | Create Circuit
--
-- Record accessors are not linear
-- mkCircuit :: (Bus a) => C a ⊸ Circuit a
-- mkCircuit m = Circuit (FreeT (\k -> ))



-- | Reduces the order of a high-order linear function argument
--
-- Todo: Will need special handling by clash, without doubt
-- defer until abstraction proven
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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}



































-- | Consume Circuit
--
-- Record accessors are not linear
-- runCircuit :: (Bus a) => Circuit a ⊸ C a
-- runCircuit = \case (Circuit (FreeT f)) -> (f pureC)



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



-- | Internal representation of a circuit.
--
-- Refer to `Circuit` instead
--
-- This representation is chosen to enforce linear usage during composition
--
-- Most bus types will be instances of Movable


-- -- | Consume C
-- --
-- -- Record accessors are not linear
-- runC :: (Bus a) => C a ⊸ BwdOf a ⊸ FwdOf a
-- runC (C f) = f




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
