{-# LANGUAGE DeriveFunctor #-}
module Control.Lens.Setter where
import Control.Monad.State.Class
import Control.Applicative
import Unsafe.Coerce

infixl 4 .=, %=, +=, -=, *=, //=
infixr 4 .~, %~, +~, -~, *~, /~
infixl 1 &

type Setter s t a b = (a -> Mutator b) -> s -> Mutator t

over :: Setter s t a b -> (a -> b) -> s -> t
over = unsafeCoerce
{-# INLINE over #-}

set :: Setter s t a b -> b -> s -> t
set a v = over a $ const v
{-# INLINE set #-}

(.~) :: Setter s t a b -> b -> s -> t
(.~) = set
{-# INLINE (.~) #-}

(&) = flip ($)
{-# INLINE (&) #-}

----

(%~) :: Setter s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE (%~) #-}

(+~) :: Num a => Setter s t a a -> a -> s -> t
s +~ x = over s (+ x)
{-# INLINE (+~) #-}

(-~) :: Num a => Setter s t a a -> a -> s -> t
s -~ x = over s (subtract x)
{-# INLINE (-~) #-}

(*~) :: Num a => Setter s t a a -> a -> s -> t
s *~ x = over s (* x)
{-# INLINE (*~) #-}

(/~) :: Fractional a => Setter s t a a -> a -> s -> t
s /~ x = over s (/ x)
{-# INLINE (/~) #-}

----

(%=) :: MonadState s m => Setter s s a a -> (a -> a) -> m ()
s %= f = modify (s %~ f)
{-# INLINE (%=) #-}

(.=) :: MonadState s m => Setter s s a a -> a -> m ()
s .= v = s %= const v
{-# INLINE (.=) #-}

(+=) :: (Num a, MonadState s m) => Setter s s a a -> a -> m ()
s += x = s %= (+ x)
{-# INLINE (+=) #-}

(-=) :: (Num a, MonadState s m) => Setter s s a a -> a -> m ()
s -= x = s %= (subtract x)
{-# INLINE (-=) #-}

(*=) :: (Num a, MonadState s m) => Setter s s a a -> a -> m ()
s *= x = s %= (* x)
{-# INLINE (*=) #-}

(//=) :: (Fractional a, MonadState s m) => Setter s s a a -> a -> m ()
s //= x = s %= (/ x)
{-# INLINE (//=) #-}

----

newtype Mutator a = Mutator { runMutator :: a }
  deriving (Show, Read, Eq, Ord, Functor)

instance Applicative Mutator where
  pure = Mutator
  {-# INLINE pure #-}
  Mutator f <*> Mutator a = Mutator (f a)
  {-# INLINE (<*>) #-}
