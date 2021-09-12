

module Clash.Circuit.Prelude (
    -- * Clash Circuit
    module Clash.Circuit,

    -- * Type Classes
    Functor(..), Applicative(..), Monad(..),
    Arrow(..), ArrowApply(..), ArrowLoop(..),

    -- * Linear functions
    (.), ($),

    -- * From clash Prelude
    Signal,

) where

import Clash.Circuit
import Clash.Prelude (Signal)

import Control.Arrow.Linear
import Control.Functor.Linear

-- import
import Prelude.Linear (($), (.))