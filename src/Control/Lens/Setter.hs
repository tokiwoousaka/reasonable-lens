{-# LANGUAGE DeriveFunctor #-}
module Control.Lens.Setter where
import Control.Monad.State.Class

infixl 4 .=, %=
infixr 4 .~, %~, +~, -~, *~, /~
infixl 1 &

type Setter s t a b = (a -> Mutator b) -> s -> Mutator t

over :: Setter s t a b -> (a -> b) -> s -> t
over l f = runMutator . l (Mutator . f)

set :: Setter s t a b -> b -> s -> t
set a v = over a $ const v

(.~) = set
(&) = flip ($)

----

(%~) :: Setter s t a b -> (a -> b) -> s -> t
(%~) = over

(+~) :: Num a => Setter s t a a -> a -> s -> t
s +~ x = over s (+ x)

(-~) :: Num a => Setter s t a a -> a -> s -> t
s -~ x = over s (subtract x)

(*~) :: Num a => Setter s t a a -> a -> s -> t
s *~ x = over s (* x)

(/~) :: Fractional a => Setter s t a a -> a -> s -> t
s /~ x = over s (/ x)

----

(%=) :: MonadState s m => Setter s s a a -> (a -> a) -> m ()
s %= f = do
  state <- get
  put $ state&s %~ f

(.=) :: MonadState s m => Setter s s a a -> a -> m ()
s .= v = s %= (const v)

(+=) :: (Num a, MonadState s m) => Setter s s a a -> a -> m ()
s += x = s %= (+ x)

(-=) :: (Num a, MonadState s m) => Setter s s a a -> a -> m ()
s -= x = s %= (subtract x)

(*=) :: (Num a, MonadState s m) => Setter s s a a -> a -> m ()
s *= x = s %= (* x)

(//=) :: (Fractional a, MonadState s m) => Setter s s a a -> a -> m ()
s //= x = s %= (/ x)

----

newtype Mutator a = Mutator { runMutator :: a }
  deriving (Show ,Read, Eq, Ord, Functor)

