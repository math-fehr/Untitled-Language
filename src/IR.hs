module IR where

import Data.Map (Map)

data PrimitiveDataType =
  IntType
  | BoolType
  deriving(Show)

data Expr =
  LocalVar String Int -- De Bruijn index, and name for debug purposes
  | Def String -- Definition
  | IntConst Int
  | BoolConst Bool
  | PrimitiveType PrimitiveDataType
  | Assign String Expr Expr -- Name for debug purposes
  | IfThenElse Expr Expr Expr
  | Call Expr Expr
  | Arrow Expr Expr
  | Type
  deriving(Show)

data Definition =
  Definition { fun_name :: String
           , fun_args :: [(String, Expr)] -- Names for debug purposes
           , fun_type :: Expr
           , fun_body :: Expr
           } deriving(Show)

type Program = Map String Definition
