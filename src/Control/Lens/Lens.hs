{-# LANGUAGE RankNTypes #-}
module Control.Lens.Lens 
  ( module Control.Lens.Getter
  , module Control.Lens.Setter
  , Lens(..)
  ) where
import Control.Lens.Getter
import Control.Lens.Setter

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

----


