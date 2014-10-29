{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Lens.TH

data Piyo = Piyo 
  { _hoge :: String
  , _piyo :: Int
  , fuga :: Bool
  } deriving (Show, Read, Eq, Ord)

data Neko = Neko 
  { _mike :: Int 
  , _kuro :: Int
  , shiba :: Bool
  } deriving (Show, Read, Eq, Ord)

data HogeHoge = String

makeLenses ''Piyo
makeLenses ''Neko
--makeLenses ''HogeHoge
