module Control.Lens.Util.TH where
import Language.Haskell.TH

bndrName :: TyVarBndr -> Name
bndrName (PlainTV n) = n
bndrName (KindedTV n _) = n

----
-- replace

replaceTypeVar :: Name -> Name -> Type -> Type
replaceTypeVar l m (VarT n) = VarT $ judgeName l m n
replaceTypeVar l m (ForallT tvbs c t) = 
  ForallT (replaceTvbsVar l m tvbs) (replaceCxtVar l m c) (replaceTypeVar l m t)
replaceTypeVar l m (AppT t1 t2) = AppT (replaceTypeVar l m t1) (replaceTypeVar l m t2)
replaceTypeVar l m (SigT t k) = SigT (replaceTypeVar l m t) k
replaceTypeVar _ _ t = t

replaceCxtVar :: Name -> Name -> Cxt -> Cxt
replaceCxtVar l m = map rep
  where
    rep :: Pred -> Pred
    rep (ClassP n ts) = ClassP n $ map (replaceTypeVar l m) ts
    rep (EqualP t1 t2) = EqualP (replaceTypeVar l m t1) (replaceTypeVar l m t2)

replaceTvbsVar :: Name -> Name -> [TyVarBndr] -> [TyVarBndr]
replaceTvbsVar l m = map rep
  where
    rep :: TyVarBndr -> TyVarBndr 
    rep (PlainTV n) = PlainTV $ judgeName l m n
    rep (KindedTV n k) = KindedTV (judgeName l m n) k

judgeName :: Name -> Name -> Name -> Name
judgeName l m n = if l == n then m else n

----
-- to name list

type2List :: Type -> [Name]
type2List (VarT n) = [n]
type2List (ForallT tvbs c t) = tvbs2List tvbs ++ cxt2List c ++ type2List t
type2List (AppT t1 t2) = type2List t1 ++ type2List t2
type2List (SigT t _) = type2List t
type2List t = []

cxt2List :: Cxt -> [Name]
cxt2List = concatMap rep
  where
    rep :: Pred -> [Name]
    rep (ClassP n ts) = concatMap type2List ts
    rep (EqualP t1 t2) = type2List t1 ++ type2List t2

tvbs2List :: [TyVarBndr] -> [Name]
tvbs2List = map rep
  where
    rep :: TyVarBndr -> Name
    rep (PlainTV n) = n
    rep (KindedTV n k) = n

