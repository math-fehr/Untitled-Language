module Error where

import IR

data Error
  = DuplicateDefinition String
  | UndefinedReference String
  | ShouldBeType Expr Expr Expr
  | NotAFunction Expr Expr
  | ExpectedConstrutor String
  | NoCases
  | MatchHeterogeneous
  | DuplicateCase String
  | MissingCase String

instance Show Error where
  show (DuplicateDefinition ident) = "Duplicate definition of " ++ ident
  show (UndefinedReference def) = "Undefined reference to " ++ show def
  show (ShouldBeType expr typ expected_typ) =
    show expr ++
    " is of type " ++ show typ ++ " but should be of type " ++ show expected_typ
  show (NotAFunction expr typ) =
    "Expression " ++
    show expr ++ " of type " ++ show typ ++ " is not a function"
  show (ExpectedConstrutor expr) =
    "Identifier " ++ expr ++ " is expected to be a constructor"
  show NoCases = "Pattern matching should contain at least one case"
  show MatchHeterogeneous =
    "Found pattern matching with constructors of different types"
  show (DuplicateCase s) =
    "Duplicate case in pattern-matching: branch with constructor " ++
    show s ++ " defined twice."
  show (MissingCase s) =
    "Missing case with constructor " ++ s ++ " in pattern matching"
