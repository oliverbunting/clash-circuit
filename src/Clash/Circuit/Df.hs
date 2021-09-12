module Clash.Circuit.Df (

    -- -- * Types
    -- Df(..),
    -- Fwd(..),
    -- Bwd(..),

    -- -- * Lenses
    -- -- valid,
    -- -- ready,
    -- -- dat,
    -- -- dat',

    -- -- * Circuits
    -- broadcast,
    -- -- merge
)
where

import Clash.Circuit.Prelude

import Data.Bool ( Bool )
import qualified Prelude as NonLinear
import Prelude.Linear ((&), error)
import Control.Optics.Linear
-- import Control.Functor.Linear as Control
-- import Data.Functor

import Unsafe.Linear


-- | A general purpose dataflow interface
--
-- Dataflow transactions have `valid` + `ready` handshake semantics. This
-- supports back pressure between circuits
--
--  The laws of this Bus are:
--
--  - The assertion of `valid` must not depend on the state of `ready`
--  - `dat` is undefined when `valid` is low
--  - `ready` is undefined when valid is low
--  - `dat` must remain stable if `valid` is high and `ready` is low
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

-- instance Functor Df where
--     fmap f (Df a) = Df (\b -> f a)

-- instance Functor Fwd where
--     fmap f (Fwd {_valid=_valid,_dat=_dat}) = Fwd{_valid=_valid,_dat= f _dat}


-- -- Lenses

-- -- | Lens into the `valid` signal of a Df Bus
-- valid :: Lens' (Fwd a) Bool
-- valid = lens (\(Fwd {_valid=v, _dat=d}) -> (v, \v' -> Fwd {_valid=v', _dat=d}))

-- -- | Monomorphic Lens into the `dat` signal of a Df Bus
-- dat' :: Lens' (Fwd a) a
-- dat' = dat

-- -- | Lens into the `dat` signal of a Df Bus
-- dat :: Lens (Fwd a) (Fwd b) a b
-- dat = lens (\(Fwd {_valid=v, _dat=d}) -> (d, \d' -> Fwd {_valid=v, _dat=d'}))

-- -- | Lens into the `ready` signal of a Df Bus
-- ready :: Lens' Bwd Bool
-- ready = lens (\(Bwd {_ready=r}) -> (r, \r' -> Bwd {_ready=r'}))





-- * Circuits
-- These live elsewhere (Type and implementation should be easily separable)

-- mealyC  :: State s ⊸ (Bwd a -> Fwd a) ⊸ Circuit (Signal dom a)


-- idC :: Signal d (Df a) ⊸ Circuit (Signal d (Df (a,b)))
-- idC (Df a) = pure . toLinear $ a

-- -- | Broadcast @a@ to all downstream circuits
--


-- ** Routing

-- | Aggregates multiple Dataflow components
--
-- todo: Generalise to (Vec n (Signal d (Df a))) ⊸ Circuit (Vec n (Signal d (Df a)))
-- combine :: Signal d (Df a) ⊸ Signal d (Df b) ⊸ Circuit (Signal d (Df (a,b)))
-- combine busA busB = pure . Unsafe.toLinear $


    -- Fwd a <- purebusB

-- | Duplicates to all downstream slaves
--
-- -- todo: Generalise to (Vec n (Signal d (Df a))) ⊸ Circuit (Vec n (Signal d (Df a)))
-- broadcast :: Df a ⊸ Circuit (Df a, Df a)
-- broadcast = error "Undefined"
-- -- broadcast (Df f) = \(Bwd r0, Bwd r1) ->
-- --     let x = f

-- -- | Bus with Dataflow handshaking
-- instance Bus (Df a) where
--     type FwdOf (Df a) = Fwd a
--     type BwdOf (Df a) = Bwd
--     pureC (Df b) = C b
