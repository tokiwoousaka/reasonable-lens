{-# LANGUAGE DeriveFunctor, RankNTypes #-}
module Control.Lens.Getter where
import Control.Monad.State.Class
import Unsafe.Coerce
infixl 8 ^.

type Getting r s a = (a -> Accessor r a) -> s -> Accessor r s
type Getter s a = forall r. Getting r s a


foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l r = runAccessor . l (Accessor . r)

foldOf :: Getter s a -> s -> a
foldOf l v = foldMapOf l id v

to :: (s -> a) -> Getting r s a
to f g = unsafeCoerce . g . f

(^.) = flip foldOf

----

use :: MonadState s m => Getter s a -> m a
use g = do
  state <- get
  return $ state^.g

----

newtype Accessor r a = Accessor { runAccessor :: r }
  deriving (Show, Read, Eq, Ord, Functor)

