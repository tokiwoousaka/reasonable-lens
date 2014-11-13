{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
module Control.Lens.TH
  ( makeLenses
  ) where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List.Split
import Data.Char
import Data.List
import Control.Lens.Lens
import Control.Lens.Util.TH 

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

-- \r f -> (\x -> r { f = x })
makeUpdFunc :: Name -> Name -> ExpQ
makeUpdFunc r f = do
  x <- newName "x"
  return . LamE [VarP x] $ makeUpd r f (VarE x)

-- \r f a -> r { f = a }
makeUpd :: Name -> Name -> Exp -> Exp
makeUpd r f a = RecUpdE (VarE r) [(f, a)]

-- \f g v -> f (g v)
makeComp :: Name -> Name -> Name -> Exp
makeComp f g v =  AppE (VarE f) $ AppE (VarE g) (VarE v)

----
-- types

createLensTypeSig :: [TyVarBndr] -> Name -> Type -> TypeQ
createLensTypeSig tvbs tn ty = do
  --runIO $ trace1 tvbs tn ty
  -- new var names
  let an = type2List ty
  nt1 <- mkNp "t1"
  nt2 <- mkNp "t2" >>= \xs -> 
    return (map (jgName an) $ zip nt1 (map snd xs))
  -- lens args
  let lensArg1 = mkCon tn nt1
  let lensArg2 = mkCon tn nt2
  let lensArg3 = repNp ty nt1
  let lensArg4 = repNp ty nt2
  --runIO $ trace2 an nt1 nt2 lensArg1 lensArg2 lensArg3 lensArg4
  -- make result
  res <- runQ [t| Lens $(return lensArg1) $(return lensArg2) $(return lensArg3) $(return lensArg4) |]
  forallT (map (PlainTV . snd) $ nub (nt1 ++ nt2)) (return []) $ return res
    where
      mkNp :: String -> Q [(Name, Name)]
      mkNp s = mapM (\_ -> newName s) tvbs >>= return . zip (map bndrName tvbs)

      repNp :: Type -> [(Name, Name)] -> Type
      repNp t ns = foldr (.) id (map mkf ns) $ t
        where
          mkf :: (Name, Name) -> Type -> Type
          mkf nt = uncurry replaceTypeVar $ nt

      mkCon :: Name -> [(Name, Name)] -> Type
      mkCon n t = foldl1 AppT $ ConT n : map (VarT . snd) t

      jgName :: [Name] -> ((Name, Name), Name) -> (Name, Name)
      jgName xs ((n1, n2), n3) = if elem n1 xs then (n1, n3) else (n1, n2)

trace1 tvbs tn ty = do
      putStrLn "------------------------"
      putStrLn $ "tvbs     = " ++ show tvbs
      putStrLn $ "tn       = " ++ show tn 
      putStrLn $ "ty       = " ++ show ty
trace2 an nt1 nt2 lensArg1 lensArg2 lensArg3 lensArg4 = do
      putStrLn "------"
      putStrLn $ "an       = " ++ show an
      putStrLn $ "nt1      = " ++ show nt1
      putStrLn $ "nt2      = " ++ show nt2 
      putStrLn $ "lensArg1 = " ++ show lensArg1
      putStrLn $ "lensArg2 = " ++ show lensArg2
      putStrLn $ "lensArg3 = " ++ show lensArg3
      putStrLn $ "lensArg4 = " ++ show lensArg4

---------------------------------------------------------------------------------------------------
-- makeClassy

makeClassy :: Name -> DecsQ
makeClassy = undefined

