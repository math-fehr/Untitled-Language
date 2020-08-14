module Error where

import IR

data Error =
  DefsNotFound [String]
  | ShouldBeType Expr Expr Expr
  | NotAFunction Expr Expr

instance Show Error where
  show (DefsNotFound defs) = "Undefined references to " ++ show defs
  show (ShouldBeType expr typ expected_typ) = show expr ++ " should is of type " ++ show typ ++ " but should be of type " ++ show expected_typ
  show (NotAFunction expr typ) = "Expression " ++ show expr ++ " of type " ++ show typ ++ " is not a function"
