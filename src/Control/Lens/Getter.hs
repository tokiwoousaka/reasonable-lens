{-# LANGUAGE DeriveFunctor, RankNTypes #-}
module Control.Lens.Getter where
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Applicative
import Unsafe.Coerce
import Data.Monoid
import Data.Foldable

infixl 8 ^.

type Getting r s a = (a -> Accessor r a) -> s -> Accessor r s
type Getter s a = forall r. Getting r s a

folded :: (Foldable f, Monoid r) => Getting r (f a) a
folded ar = unsafeCoerce `asTypeOf` (Accessor .)
  $ foldMap (unsafeCoerce `asTypeOf` (runAccessor .) $ ar)

views :: MonadReader s m => Getting r s a -> (a -> r) -> m r
views = asks unsafeCoerce
{-# INLINE views #-}

view :: MonadReader s m => Getting a s a -> m a
view l = views l id
{-# INLINE view #-}

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf = unsafeCoerce
{-# INLINE foldMapOf #-}

foldOf :: Getting a s a -> s -> a
foldOf l = foldMapOf l id
{-# INLINE foldOf #-}

to :: (s -> a) -> Getter s a
to f = \ar -> unsafeCoerce (ar . f)
{-# INLINE to #-}

(^.) :: s -> Getting a s a -> a
(^.) = flip foldOf
{-# INLINE (^.) #-}

----

uses :: MonadState s m => Getter s a -> (a -> r) -> m r
uses g f = get >>= foldMapOf g (return . f)
{-# INLINE uses #-}

use :: MonadState s m => Getter s a -> m a
use g = uses g id
{-# INLINE use #-}

----

newtype Accessor r a = Accessor { runAccessor :: r }
  deriving (Show, Read, Eq, Ord, Functor)

instance Monoid r => Applicative (Accessor r) where
  pure _ = Accessor mempty
  {-# INLINE pure #-}
  Accessor a <*> Accessor b = Accessor (mappend a b)
  {-# INLINE (<*>) #-}
