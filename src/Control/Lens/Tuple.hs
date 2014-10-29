{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Lens.Tuple where
import Control.Lens.Lens

class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _1 :: Lens s t a b

class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _2 :: Lens s t a b

class Field3 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _3 :: Lens s t a b

----

instance Field1 (a, v) (b, v) a b where
  _1 f (x, y) = fmap (\x' -> (x', y)) (f x)

instance Field2 (v, a) (v, b) a b where
  _2 f (x, y) = fmap (\y' -> (x, y')) (f y)
