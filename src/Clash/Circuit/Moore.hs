

module Clash.Circuit.Mealy where

import qualified Clash.Prelude as CP
import           Clash.Signal ( Signal, HiddenClockResetEnable )
import           Clash.XException           (NFDataX)

import Clash.Circuit ( Circuit, mkCircuit )
import Clash.Circuit.Bus ( BwdOf, FwdOf, Bus )
import Unsafe.Linear as Unsafe ( toLinear )

-- | Create a synchronous Circuit from a combinational function describing
-- a mealy machine
moore
  ::   forall dom s a.
    ( HiddenClockResetEnable dom
     , NFDataX s
     , Bus a
     )
  => s
  -- ^ Initial state
  -> (s ⊸ BwdOf a  ⊸ s)
  -- ^ 	Transfer function in moore machine form: state -> input -> new_state
  -> (s ⊸ FwdOf a)
  -- ^ Output function in moore machine form: state -> output
  -> Circuit (Signal dom a)
  -- ^ Synchronous circuit with output bus a
moore s f g = mkCircuit (Unsafe.toLinear (CP.moore (\s' i -> f s' i) (\s' -> g s') s))
{-# INLINE moore #-}
{-# ANN moore "HLint: ignore Avoid lambda" #-}