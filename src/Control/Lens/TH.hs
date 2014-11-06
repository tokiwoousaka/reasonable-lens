{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
module Control.Lens.TH
  ( makeLenses
  ) where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List.Split
import Data.Char
import Control.Lens.Lens

makeLenses :: Name -> DecsQ
makeLenses n = do
  info <- reify n
  case info2Records info n of 
    Left xs -> fmap concat . sequence $ map (createLensFunction n) xs
    Right x -> error x

info2Records :: Info -> Name -> Either [VarStrictType] String
info2Records (TyConI (DataD _ _ _ (RecC _ xs:_) _)) _ = Left xs
info2Records _ name = Right $ "Type \"" ++ show name ++ "\" have not records."

createLensFunction :: Name -> VarStrictType -> DecsQ
createLensFunction n (v, s, t) = do 
  name <- return $ getFuncName v
  case name of
    Just nm -> do
      exp <- createLensExp v 
      funName <- return $ mkName nm
      sequence 
        [ sigD funName $ createLensTypeSig n t
        , funD funName [return $ Clause [] (NormalB exp) []]
        ]
    Nothing -> return []

getFuncName :: Name -> Maybe String
getFuncName n = getn . last . endBy "." $ show n
  where
    getn :: String -> Maybe String
    getn ('_':s:xs) = Just $ toLower s : xs
    getn _ = Nothing

----
-- create expression
-- TODO : refactor

--createLensSig :: Name -> Name -> Type -> ExpQ
--createLensSig fid tn ft = do
--  exp <- createLensExp fid
--  ts <- createLensTypeSig tn ft
--  return  $ SigE exp ts

-- \fld -> (\f v -> fmap (\a -> v {fld = a} ) (f (fld v)))
createLensExp :: Name -> ExpQ
createLensExp fld = do
  f <- newName "f"
  v <- newName "v"
  updFunc <- makeUpdFunc v fld
  return . LamE [VarP f, VarP v] $ makeAppFmap updFunc (makeComp f fld v)

-- \f v -> fmap f v
makeAppFmap :: Exp -> Exp -> Exp
makeAppFmap f v = AppE (AppE (VarE 'fmap) f) v

-- \r f -> (\a -> r { f = a })
makeUpdFunc :: Name -> Name -> ExpQ
makeUpdFunc r f = do
  a <- newName "a"
  return . LamE [VarP a] $ makeUpd r f (VarE a)

-- \r f a -> r { f = a }
makeUpd :: Name -> Name -> Exp -> Exp
makeUpd r f a = RecUpdE (VarE r) [(f, a)]

-- \f g v -> f (g v)
makeComp :: Name -> Name -> Name -> Exp
makeComp f g v =  AppE (VarE f) (AppE (VarE g) (VarE v))

-- type
createLensTypeSig :: Name -> Type -> TypeQ
createLensTypeSig tn ft = do
  runQ [t| Lens $(n2ct tn) $(n2ct tn) $(return ft) $(return ft) |]
    where
      n2ct = return . ConT

---------------------------------------------------------------------------------------------------
-- makeClassy

makeClassy :: Name -> DecsQ
makeClassy = undefined

