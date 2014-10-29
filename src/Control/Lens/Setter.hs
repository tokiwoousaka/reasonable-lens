{-# LANGUAGE DeriveFunctor #-}
module Control.Lens.Setter where
infixr 4 .~
infixl 1 &

type Setter s t a b = (a -> Mutator b) -> s -> Mutator t

over :: Setter s t a b -> (a -> b) -> s -> t
over l f = runMutator . l (Mutator . f)

set :: Setter s t a b -> b -> s -> t
set a v = over a $ const v

(.~) = set
(&) = flip ($)

----

newtype Mutator a = Mutator { runMutator :: a }
  deriving (Show ,Read, Eq, Ord, Functor)

