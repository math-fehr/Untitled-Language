module IR where

data Expr =
  LocalVar String Int -- De Bruijn index, and name for debug purposes
  | Def String -- Definition
  | IntConst Int
  | BoolConst Bool
  | PrimitiveType String -- String representation of a primitive type
  | Assign String Expr Expr -- Name for debug purposes
  | IfThenElse Expr Expr Expr
  | Call Expr Expr
  deriving(Show)

data Function =
  Function { fun_name :: String
           , fun_args :: [(String, Expr)] -- Names for debug purposes
           , fun_type :: Expr
           , fun_body :: Expr
           } deriving(Show)

data Program = Map String Function
