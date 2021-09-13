{-|
Description : High-level structural Circuit composition in Clash
Copyright   : (c) Oliver Bunting, 2021
License     : BSD3
Maintainer  : oliverbunting@gmail.com
Stability   : experimental

A dataflow bus with back-pressure semantics
|-}

module Clash.Circuit.Bus.Df (

    -- -- * Types
    Df(..),
    Fwd(..),
    Bwd(..),

    -- -- * Lenses
    valid,
    ready,
    dat,
    dat',

    -- -- * Circuits
    -- broadcast,
    -- -- merge
)
where

import Data.Bool ( Bool )
import Control.Optics.Linear ( lens, Lens, Lens' )
import qualified Data.Functor.Linear as Data
import qualified Control.Functor.Linear as Control

import Unsafe.Linear as Unsafe ( coerce )

import Clash.Circuit.Bus

-- | A bus with unidirectional data flow, supporting back-pressure.
--
--  The laws of this Bus are:
--
--  - The assertion of `valid` must not depend on the state of `ready`
--  - `dat` is undefined when `valid` is low
--  - `ready` is undefined when valid is low
--  - `dat` must remain stable if `valid` is high and `ready` is low
newtype Df a where
  Df :: (Bwd a ⊸ Fwd a) ⊸ Df a

instance Bus (Df a) where
  type Channel 'Forward  (Df a) = Fwd a
  type Channel 'Backward (Df a) = Bwd a
  pureC (Df f) = C f

-- | Data flow Bus, master to slave
data Fwd a = Fwd
  { _valid :: Bool  -- ^ Indicates `dat` is valid
  , _dat   :: a     -- ^ The data transferred by the bus
  }

-- | Data flow Bus, slave to master
newtype Bwd a = Bwd
  {  _ready :: Bool  -- ^ Handshake signal
  }


instance Data.Functor Fwd where
  fmap f (Fwd {_valid=_valid,_dat=_dat}) = Fwd{_valid=_valid,_dat= f _dat}

instance Control.Functor Fwd where
  fmap f (Fwd {_valid=_valid,_dat=_dat}) = Fwd{_valid=_valid,_dat= f _dat}

instance Data.Functor Df where
  fmap f (Df g) = Df \b -> f Data.<$> g (Unsafe.coerce b)

instance Control.Functor Df where
  fmap f (Df g) = Df \b -> f Control.<$> g (Unsafe.coerce b)



-- Lenses

-- | Lens into the `valid` signal of a Df Bus
valid :: Lens' (Fwd a) Bool
valid = lens (\(Fwd {_valid=v, _dat=d}) -> (v, \v' -> Fwd {_valid=v', _dat=d}))

-- | Monomorphic Lens into the `dat` signal of a Df Bus
dat' :: Lens' (Fwd a) a
dat' = dat

-- | Lens into the `dat` signal of a Df Bus
dat :: Lens (Fwd a) (Fwd b) a b
dat = lens (\(Fwd {_valid=v, _dat=d}) -> (d, \d' -> Fwd {_valid=v, _dat=d'}))

-- | Lens into the `ready` signal of a Df Bus
ready :: Lens' (Bwd a) Bool
ready = lens (\(Bwd {_ready=r}) -> (r, \r' -> Bwd {_ready=r'}))





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
