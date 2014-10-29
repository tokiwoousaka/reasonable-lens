{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Control.Lens.TH
  (makeLenses
  ) where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List.Split

data Hoge = Hoge
  { _foo :: String
  , _bar :: Int
  , buz :: Int
  } deriving (Show, Read, Eq, Ord)

----

makeLenses :: Name -> DecsQ
makeLenses n = do
  info <- reify n
  case info2Records info n of 
    Left xs -> fmap concat . sequence $ map createLensFunction xs
    Right x -> error x

info2Records :: Info -> Name -> Either [VarStrictType] String
info2Records (TyConI (DataD _ _ _ (RecC _ xs:_) _)) _ = Left xs
info2Records _ name = Right $ "Type \"" ++ show name ++ "\" have not records."

createLensFunction :: VarStrictType -> DecsQ
createLensFunction (v, s, t) = do 
  name <- return $ getFuncName v
  case name of
    Just n -> do
      exp <- [| putStrLn ("function " ++ n ++ " called!") |] --TODO
      sequence 
        [ funD (mkName n) [return $ Clause [] (NormalB exp) []]
        ]
    Nothing -> return []

getFuncName :: Name -> Maybe String
getFuncName n = getn . last . endBy "." $ show n
  where
    getn :: String -> Maybe String
    getn ('_':s) = Just s -- TODO : 一文字目を小文字にする処理
    getn _ = Nothing

----

makeClassy :: Name -> DecsQ
makeClassy = undefined
