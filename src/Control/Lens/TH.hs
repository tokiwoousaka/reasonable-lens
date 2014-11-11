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
    Left (tvbs, xs) -> fmap concat . sequence $ map (createLensFunction tvbs n) xs
    Right x -> error x

info2Records :: Info -> Name -> Either ([TyVarBndr], [VarStrictType]) String
info2Records (TyConI (DataD _ _ tvbs (RecC _ xs:_) _)) _ = Left (tvbs, xs)
info2Records _ name = Right $ "Type \"" ++ show name ++ "\" have not records."

createLensFunction :: [TyVarBndr] -> Name -> VarStrictType -> DecsQ
createLensFunction tvbs n (v, s, t) = do 
  name <- return $ getFuncName v
  case name of
    Just nm -> do
      exp <- createLensExp v 
      funName <- return $ mkName nm
      sequence 
        [ sigD funName $ createLensTypeSig tvbs n t
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
makeComp f g v =  AppE (VarE f) $ AppE (VarE g) (VarE v)

----
-- types

createLensTypeSig :: [TyVarBndr] -> Name -> Type -> TypeQ
createLensTypeSig [] tn ft = runQ [t| Lens $(conT tn) $(conT tn) $(return ft) $(return ft) |]
createLensTypeSig tvbs tn (VarT ft) = let

  makeNameList :: TyVarBndr -> Q (TyVarBndr, Name)
  makeNameList t = newName "t" >>= \x -> return (t, x)

  judgeType :: Name -> (TyVarBndr, Name) -> TypeQ
  judgeType n (PlainTV p, m) = varT $ if ft == p then n else m

  nameList :: Name -> [(TyVarBndr, Name)] -> [TypeQ]
  nameList n xs = map (judgeType n) xs

  appCon :: Name -> [TypeQ] -> TypeQ
  appCon n xs = foldl1 appT $ conT n : xs 

  typeCon :: Name -> Name -> [(TyVarBndr, Name)] -> TypeQ
  typeCon n m = appCon n . nameList m 

  in do
    a <- newName "a"
    b <- newName "b"
    qnl <- sequence $ map makeNameList tvbs
    res <- runQ [t| Lens $(typeCon tn a qnl) $(typeCon tn b qnl) $(varT a) $(varT b) |]
    forallT ((map (PlainTV . snd) qnl) ++ PlainTV a:PlainTV b:[]) (return []) $ return res

---------------------------------------------------------------------------------------------------
-- makeClassy

makeClassy :: Name -> DecsQ
makeClassy = undefined

