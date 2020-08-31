module Error where

import IR

data Error
  = ExpectedType Expr Type Type -- Expression, expected type, and real type
  | ExpectedIntType Expr Type -- Expression, and real type
  | ReusedLinear String Expr -- Variable name, expression,
                             -- and expression in which it's used
  | UnusedLinear String
  | UndefinedVariable String Expr -- Same
  | UndefinedVariableInterpreter Variable
  | NotAType Expr -- Expression is not a type
  | ValueNotAType Value
  | UnknownVariable Variable
  | LastScope
  -- UnknownBuiltin Builtins
  | DifferringRessources Expr Expr
  | LinearUseIllegal String Expr
  | TypingCycle -- There is an unsolvable cyclic dependency for typing TODO: Print the cycle
  | InternalError String
  | NotAnArrow Type -- The type is not an arrow
  | DeclarationTypeIsNotAType
  | DeclarationFunctionTypeArgumentNumberMismatch
  | NotYetTyped Variable
  | DivisionByZero
  | Unimplemented String
  | TypeSystemUnsound String
  | IncompatibleTypes Type Type
  | LastShouldBeType IR.Expr
  | NotEnoughArgs
  | TooManyArgs
  | TypeShouldHaveUnrArrows
  deriving (Eq, Show)
