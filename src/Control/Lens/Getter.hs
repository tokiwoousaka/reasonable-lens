{-# LANGUAGE DeriveFunctor #-}
module Control.Lens.Getter where
import Control.Monad.State.Class
infixl 8 ^.

type Getting r s a = (a -> Accessor r a) -> s -> Accessor r s

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l r = runAccessor . l (Accessor . r)

foldOf :: Getting a s a -> s -> a
foldOf l v = foldMapOf l id v

(^.) = flip foldOf

----

use :: MonadState s m => Getting a s a -> m a
use g = do
  state <- get
  return $ state^.g

----

newtype Accessor r a = Accessor { runAccessor :: r }
  deriving (Show, Read, Eq, Ord, Functor)

