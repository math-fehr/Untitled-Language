module IR where

import Data.Map (Map)

data PrimitiveDataType =
  IntType
  | BoolType
  deriving(Show, Eq)

data Expr =
  LocalVar String Int -- De Bruijn index, and name for debug purposes
  | Def String -- Definition
  | IntConst Int
  | BoolConst Bool
  | PrimitiveType PrimitiveDataType
  | Assign String Expr Expr -- Name for debug purposes
  | IfThenElse Expr Expr Expr
  | Call Expr Expr
  | Lambda String Expr Expr
  | Arrow Expr Expr
  | Type
  deriving(Show)

instance Eq Expr where
  LocalVar _ i == LocalVar _ i' = i == i'
  Def s == Def s' = s == s'
  IntConst i == IntConst i' = i == i'
  BoolConst b == BoolConst b' = b == b'
  PrimitiveType t == PrimitiveType t' = t == t'
  Assign _ e1 e2 == Assign _ e1' e2' = e1 == e1' && e2 == e2'
  IfThenElse cond e1 e2 == IfThenElse cond' e1' e2' = e1 == e1' && e2 == e2' && cond == cond'
  Call e1 e2 == Call e1' e2' = e1 == e1' && e2 == e2'
  Arrow e1 e2 == Arrow e1' e2' = e1 == e1' && e2 == e2'
  Lambda _ e1 e2 == Lambda _ e1' e2' = e1 == e1' && e2 == e2'
  Type == Type = True
  _ == _ = False

data Definition =
  Definition { fun_name :: String
           , fun_args :: [(String, Expr)] -- Names for debug purposes
           , fun_type :: Expr
           , fun_body :: Expr
           } deriving(Show)

type Program = Map String Definition
