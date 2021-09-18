{-|
Copyright   : (c) Oliver Bunting, 2021
License     : BSD3
Maintainer  : oliverbunting@gmail.com
Stability   : experimental

Create 'Circuit' using Mealy machines
|-}


module Clash.Circuit.Mealy where

import qualified Clash.Prelude as CP
import           Clash.Signal ( Signal, HiddenClockResetEnable )
import           Clash.XException           (NFDataX)

import Clash.Circuit ( Circuit, mkCircuit )
import Clash.Circuit.Bus ( BwdOf, FwdOf, Bus )
import Unsafe.Linear as Unsafe ( toLinear )

-- | Create a synchronous Circuit from a combinational function describing
-- a mealy machine
mealy
  :: ( HiddenClockResetEnable dom
     , NFDataX s
     , Bus a
     )
  => s
  -- ^ Initial state
  -> (s ⊸ BwdOf a  ⊸ (s, FwdOf a))
  -- ^ Transfer function in mealy machine form: @state -> input -> (new_state,output)@
  -> Circuit (Signal dom a)
  -- ^ Synchronous circuit with output bus b
mealy s f = mkCircuit (Unsafe.toLinear (CP.mealy (\s' b -> f s' b) s))
{-# INLINE mealy #-}
{-# ANN mealy "HLint: ignore Avoid lambda" #-}