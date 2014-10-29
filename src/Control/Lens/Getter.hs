{-# LANGUAGE DeriveFunctor #-}
module Control.Lens.Getter where
infixl 8 ^.

type Getting r s a = (a -> Accessor r a) -> s -> Accessor r s

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l r = runAccessor . l (Accessor . r)

foldOf :: Getting a s a -> s -> a
foldOf l v = foldMapOf l id v

(^.) = flip foldOf

----

newtype Accessor r a = Accessor { runAccessor :: r }
  deriving (Show, Read, Eq, Ord, Functor)

