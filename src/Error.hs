module Error where

import IR

data Error =
  DuplicateDefinition String
  | UndefinedReference String
  | ShouldBeType Expr Expr Expr
  | NotAFunction Expr Expr

instance Show Error where
  show (DuplicateDefinition ident) = "Duplicate definition of " ++ ident
  show (UndefinedReference def) = "Undefined reference to " ++ show def
  show (ShouldBeType expr typ expected_typ) = show expr ++ " should is of type " ++ show typ ++ " but should be of type " ++ show expected_typ
  show (NotAFunction expr typ) = "Expression " ++ show expr ++ " of type " ++ show typ ++ " is not a function"
