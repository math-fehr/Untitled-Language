module IRUtils where

import IR

-- Substitute a De Bruijn variable to an expression
subst :: Int -> Expr -> Expr -> Expr
subst idx sub_e (LocalVar s idx') =
  case compare idx idx' of
    EQ -> sub_e
    LT -> LocalVar s (idx' - 1)
    GT -> LocalVar s idx'
subst _ _ (Def s) = Def s
subst _ _ (IntConst i) = IntConst i
subst _ _ (BoolConst b) = BoolConst b
subst _ _ (PrimitiveType s) = PrimitiveType s
subst idx sub_e (Assign s e1 e2) = Assign s (subst idx sub_e e1) (subst (idx + 1) sub_e e2)
subst idx sub_e (IfThenElse e1 e2 e3) = IfThenElse (subst idx sub_e e1) (subst idx sub_e e2) (subst idx sub_e e3)
subst idx sub_e (Call e1 e2) = Call (subst idx sub_e e1) (subst idx sub_e e2)
subst idx sub_e (Arrow e1 e2) = Arrow (subst idx sub_e e1) (subst idx sub_e e2)
subst _ _ Type = Type

