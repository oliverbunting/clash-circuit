module Control.Arrow.Linear (
  -- | Arrows
  Arrow(..),
  Kleisli(..),
  -- | Derived function
  (^>>), (>>^), (>>>), returnA,
  -- | Right-to-left variants
  (<<^), (^<<), (<<<),
  -- | Arrow Application
  ArrowApply(..),
  -- | Feedback
  ArrowLoop(..)

) where

import Data.Profunctor.Kleisli.Linear (Kleisli(..))
import Control.Category.Linear ( (<<<), (>>>), Category(..) )
import Data.Unrestricted.Linear ( Dupable(dup2) )
import Control.Functor.Linear as Control
-- Non Linear imports
import GHC.Exts(FUN)
import GHC.Types (Multiplicity(One))

-- | A linear Arrow
class Category a => Arrow a where
    -- | Lift function to an arrow
    arr    :: (b ⊸ c) ⊸ a b c

    -- | Send the first component of the input through the argument
    --   arrow, and copy the rest unchanged to the output.
    first  :: a b c ⊸ a (b,d) (c,d)
    first = (*** id)

    -- | A mirror image of 'first'.
    --
    --   The default definition may be overridden with a more efficient
    --   version if desired.
    second :: a b c ⊸ a (d,b) (d,c)
    second = (id ***)

    -- | Split the input between the two argument arrows and combine
    --   their output.  Note that this is in general not a functor.
    --
    --   The default definition may be overridden with a more efficient
    --   version if desired.
    (***)  :: a b c ⊸ a b' c' ⊸ a (b,b') (c,c')
    f *** g = first f >>> arr swap >>> first g >>> arr swap
      where
        swap :: (x,y) ⊸ (y,x)
        swap (x,y) = (y,x)
    infixr 3 ***

    -- | Fanout: send the input to both argument arrows and combine
    --   their output.
    --
    --   This requires the input to be `Dupable`
    (&&&) :: (Dupable b) => a b c ⊸ a b c' ⊸ a b (c,c')
    f &&& g = arr dup2 >>> (f *** g)

class Arrow a => ArrowLoop a where
    loop :: a (b,d) (c,d) ⊸ a b c


-- | Some arrows allow application of arrow inputs to other inputs.
-- Instances should satisfy the following laws:
--
--  * @'first' ('arr' (\\x -> 'arr' (\\y -> (x,y)))) >>> 'app' = 'id'@
--
--  * @'first' ('arr' (g >>>)) >>> 'app' = 'second' g >>> 'app'@
--
--  * @'first' ('arr' (>>> h)) >>> 'app' = 'app' >>> h@
--
-- Such arrows are equivalent to monads (see 'ArrowMonad').
class Arrow a => ArrowApply a where
    app :: a (a b c, b) c


-- Need category instance for (FUN 'Many)
returnA :: Arrow a => a b b
returnA = arr id

-- Derived combinators


-- | Pre-composition with a pure function.
(^>>) :: Arrow a => (b ⊸ c) ⊸ a c d ⊸ a b d
f ^>> a = arr f >>> a
infixr 1 ^>>

-- | Post-composition with a pure function.
(>>^) :: Arrow a => a b c ⊸ (c ⊸ d) ⊸ a b d
a >>^ f = a >>> arr f
infixr 1 >>^

-- Right to left variants

-- | Pre-composition with a pure function (right-to-left variant).
(<<^) :: Arrow a => a c d ⊸ (b ⊸ c) ⊸ a b d
a <<^ f = a <<< arr f
infixr 1 <<^

-- | Post-composition with a pure function (right-to-left variant).
(^<<) :: Arrow a => (c ⊸ d) ⊸ a b c ⊸ a b d
f ^<< a = arr f <<< a
infixr 1 ^<<


instance Arrow (FUN 'One) where
  arr f = f
  (f *** g) (x,y) = (f x, g y)
  -- | NOTE: `~(x,y)` is the NonLinear pattern match


instance ArrowApply (FUN 'One) where
  app (f,x) = f x


instance Monad m => Arrow (Kleisli m) where
    arr f = Kleisli (return . f)
    first (Kleisli f) = Kleisli (\ (b,d) -> f b >>= \c -> return (c,d))
    second (Kleisli f) = Kleisli (\ (d,b) -> f b >>= \c -> return (d,c))


instance Monad m => ArrowApply (Kleisli m) where
    app = Kleisli (\(Kleisli f, x) -> f x)
