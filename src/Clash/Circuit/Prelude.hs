

module Clash.Circuit.Prelude (
    -- * Clash Circuit
    module Clash.Circuit,
    module Clash.Circuit.Bus,

    -- * Type Classes
    Functor(..), Applicative(..), Monad(..),
    Arrow(..), ArrowApply(..), ArrowLoop(..),

    -- * Linear functions
    (.), ($), (&),

    -- * Reexported from clash Prelude
    Signal,

) where

import Clash.Circuit ( liftC, lowerC, mkCircuit, Circuit(..) )
import Clash.Circuit.Bus
    ( BwdOf,
      FwdOf,
      RBusDir,
      BusDir(..),
      Bus(..),
      C(..))
import Clash.Prelude (Signal)

import Control.Arrow.Linear
import Control.Functor.Linear

-- import
import Prelude.Linear (($), (&), (.))