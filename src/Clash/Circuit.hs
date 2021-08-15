{-# OPTIONS_GHC -fno-warn-orphans #-}
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
module Clash.Circuit (
  -- * Classes
  Bus(..),

  -- * Types
  Circuit(..),
  runCircuit,
  CircuitArrow(..),

  C(..),
  runC,
) where

-- Linear functions
import           Control.Arrow.Linear ( Kleisli(..), Arrow, ArrowApply )
import           Control.Category.Linear ( Category )
import qualified Control.Functor.Linear as Control
import           Control.Monad.Constrained.FreeT.Linear ( FreeT(..) )
import qualified Data.Functor.Linear as Data
import           Prelude.Linear ((&), id, error)


-- NonLinear functions
-- Linear import
import GHC.Types (Type)
import qualified Prelude as NonLinear


-- import Prelude ((.))
-- import Data.Functor ((<$>))
-- import qualified Control.Applicative as NonLinear (Applicative(..))

import Clash.Signal.Internal (Signal(..))

-- | Instances of Bus are the types through which circuits are interfaced
--
-- A Bus, in general, represents bi-directional data transfer.
--
-- As can be seen in the implementation of Bus `(a->b)`, a the `FwdOf` component of a Bus is a function of
-- `BwdOf` data received.
--
-- The other notable set of instances are product types.
class Bus a where
  type FwdOf a :: Type
  type BwdOf a :: Type
  pureC :: a ⊸ C a

-- | The unit bus is used to indicate a `Circuit` produces no output
instance Bus () where
  type FwdOf () = ()
  type BwdOf () = ()
  pureC () = C id


-- | A Bus is a function, providing FwdOf data in response to BwdOf data
instance Bus (a ⊸ b) where
  type FwdOf (a ⊸ b) = b
  type BwdOf (a ⊸ b) = a
  pureC a = C a


-- | A Signal of a Bus is a Bus
instance (Bus a) => Bus (Signal dom a) where
  type FwdOf (Signal dom a) = Signal dom ( FwdOf a)
  type BwdOf (Signal dom a) = Signal dom ( BwdOf a)
  pureC f = C (\b -> (runC Data.<$> (pureC Data.<$> f)) Control.<*> b)


-- | A Product of a Bus is a Bus
instance (Bus b0, Bus b1) => Bus (b0,b1) where
   type FwdOf (b0,b1) = (FwdOf b0, FwdOf b1)
   type BwdOf (b0,b1) = (BwdOf b0, BwdOf b1)
   pureC (b0,b1) = C (\(x,y) -> (pureC b0 & \case C f -> f x, pureC b1 & \case C f -> f y))


-- | Internal representation of a circuit.
--
-- Refer to `Circuit` instead
--
-- This representation is chosen to enforce linear usage during composition
--
-- Most bus types will be instances of Movable
data C a where
  C :: (Bus a) => (BwdOf a ⊸ FwdOf a) ⊸ C a


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


-- | Consume Circuit
--
-- Record accessors are not linear
runCircuit :: (Bus a) => Circuit a ⊸ C a
runCircuit = \case (Circuit (FreeT f)) -> (f pureC)


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
newtype CircuitArrow a b = CircuitArrow (a ⊸ Circuit b)
    deriving (Category, Arrow, ArrowApply) via (Kleisli (FreeT Bus C))


-- * Orphan instances (for clash)

-- Speak with christiaanb about these.
-- Signal needs careful handling.
-- pretty sure a heavy dose of unsafe is required here

instance Data.Functor (Signal dom) where
  fmap _ = error "Todo"

instance Control.Functor (Signal dom) where
  fmap f = error "Todo" f

instance Data.Applicative (Signal dom) where
  pure = NonLinear.pure
  (<*>) = error "Todo"

instance Control.Applicative (Signal dom) where
  pure = error "Todo"
  (<*>) = error "Todo"
