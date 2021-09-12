

module Clash.Circuit.Mealy where

import qualified Clash.Prelude as CP
import           Clash.Signal
import           Clash.XException           (NFDataX)
import Control.Functor.Linear (Applicative (pure))
import Clash.Circuit
import Prelude.Linear
import Data.Unrestricted.Linear
-- | Create a synchronous Circuit from a combinational function describing
-- a mealy machine
-- mealy
--   :: ( HiddenClockResetEnable dom
--      , NFDataX s
--      , Bus b
--      )
--   => s
--   -- ^ Initial state
--   -> (s -> BwdOf b -> (s, FwdOf b))
--   -- ^ Transfer function in mealy machine form: @state -> input -> (newstate,output)@
--   -> Circuit (Signal dom a)
--   -- ^ Synchronous circuit with output bus b
-- mealy s f = pure . pure (\bwd -> move bwd & \case (Ur a) -> CP.mealy f s)
-- {-# INLINE mealy #-}


-- foo :: (Bus a, Movable (BwdOf a)) => (BwdOf a -> FwdOf a) -> C a
-- foo f = C ((\case (Ur a') -> f a') . move)

-- bar :: C a -> Circuit a
-- bar = Circuit . pure