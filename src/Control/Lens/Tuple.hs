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

class Field4 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _4 :: Lens s t a b

-- 以下、TemplateHaskell採用を検討中...

----

instance Field1 (a, v) (b, v) a b where
  _1 f (a, b) = fmap (\a' -> (a', b)) (f a)

instance Field2 (v, a) (v, b) a b where
  _2 f (a, b) = fmap (\b' -> (a, b')) (f b)

----

instance Field1 (a, v, w) (b, v, w) a b where
  _1 f (a, b, c) = fmap (\a' -> (a', b, c)) (f a)

instance Field2 (v, a, w) (v, b, w) a b where
  _2 f (a, b, c) = fmap (\b' -> (a, b', c)) (f b)

instance Field3 (v, w, a) (v, w, b) a b where
  _3 f (a, b, c) = fmap (\c' -> (a, b, c')) (f c)

----

instance Field1 (a, v, w, x) (b, v, w, x) a b where
  _1 f (a, b, c, d) = fmap (\a' -> (a', b, c, d)) (f a)

instance Field2 (v, a, w, x) (v, b, w, x) a b where
  _2 f (a, b, c, d) = fmap (\b' -> (a, b', c, d)) (f b)

instance Field3 (v, w, a, x) (v, w, b, x) a b where
  _3 f (a, b, c, d) = fmap (\c' -> (a, b, c', d)) (f c)

instance Field4 (v, w, x, a) (v, w, x, b) a b where
  _4 f (a, b, c, d) = fmap (\d' -> (a, b, c, d')) (f d)
