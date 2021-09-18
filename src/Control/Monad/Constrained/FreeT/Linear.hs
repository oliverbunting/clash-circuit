{-# LANGUAGE RankNTypes #-}

{-|
Module      : Control.Monad.Constrained.FreeT.Linear
Description : A linearly typed FreeT
Copyright   : (c) Oliver Bunting, 2021
License     : MIT
Maintainer  : oliverbunting@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Control.Monad.Constrained.FreeT.Linear
(
    -- * Types
    FreeT(..),
)
where

import qualified Control.Functor.Linear as Control
import qualified Data.Functor.Linear as Data
import Prelude.Linear ( ($), (&), (.) )

-- | The free monad transformer, with constraint kinds
--
--  See
--
--     - http://okmij.org/ftp/Haskell/set-monad.html#set-cps
--     - https://doisinkidney.com/posts/2017-03-08-constrained-applicatives.html
--
data FreeT c m a where
       FreeT :: (forall r. c r => (a %1 -> m r) %1 ->  m r) %1 ->  FreeT c m a


instance Data.Functor (FreeT r m) where
    fmap f (FreeT m) = FreeT (\ c -> m (c . f))
    {-# INLINE fmap #-}

instance Control.Functor (FreeT r m) where
    fmap f (FreeT m) = FreeT (\ c -> m (c . f))
    {-# INLINE fmap #-}

instance Data.Applicative (FreeT r m) where
    pure x  = FreeT ($ x)
    {-# INLINE pure #-}
    (FreeT f) <*> (FreeT v) = FreeT (\ c -> f $ \ g -> v (c . g))
    {-# INLINE (<*>) #-}

instance Control.Applicative (FreeT r m) where
    pure x  = FreeT ($ x)
    {-# INLINE pure #-}
    (FreeT f) <*> (FreeT v) = FreeT (\ c -> f $ \ g -> v (c . g))
    {-# INLINE (<*>) #-}

instance Control.Monad (FreeT c m) where
    (FreeT m) >>= k = FreeT (\ c -> m (\ x -> k x & \case (FreeT c') -> c' c))
    {-# INLINE (>>=) #-}

