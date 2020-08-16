module IR where

import Data.Map

data ConstType
  = IntConst Int
  | BoolConst Bool
  | BoolType
  | IntType
  deriving (Show, Eq)

data Expr
  = LocalVar String Int -- De Bruijn index, and name for debug purposes
  | Def String -- Definition
  | InductiveType String
  | Constructor String Int
  | Const ConstType
  | Assign String Expr Expr -- Name for debug purposes
  | IfThenElse Expr Expr Expr
  | Call Expr Expr
  | Lambda String Expr Expr -- Name for debug purposes
  | Arrow Expr Expr
  | Type
  deriving (Show)

instance Eq Expr where
  LocalVar _ i == LocalVar _ i' = i == i'
  Def s == Def s' = s == s'
  Constructor i c == Constructor i' c' = i == i' && c == c'
  Const c == Const c' = c == c'
  Assign _ e1 e2 == Assign _ e1' e2' = e1 == e1' && e2 == e2'
  IfThenElse cond e1 e2 == IfThenElse cond' e1' e2' = e1 == e1' && e2 == e2' && cond == cond'
  Call e1 e2 == Call e1' e2' = e1 == e1' && e2 == e2'
  Arrow e1 e2 == Arrow e1' e2' = e1 == e1' && e2 == e2'
  Lambda _ e1 e2 == Lambda _ e1' e2' = e1 == e1' && e2 == e2'
  Type == Type = True
  _ == _ = False

data Definition = Definition
  { def_name :: String
  , def_args :: [(String, Expr)] -- Names for debug purposes
  , def_type :: Expr
  , def_body :: Expr
  } deriving(Show)

data InductiveConstructor = InductiveConstructor
  { constr_name :: String
  , constr_args :: [(String, Expr)] }

data Inductive = Inductive
  { ind_name   :: String
  , ind_args   :: [(String, Expr)]
  , ind_constr :: [InductiveConstructor] }

data Program = Program
  { prog_defs :: Map String Definition
  , prog_inds :: Map String Inductive }

insertDefinition :: Definition -> Program -> Program
insertDefinition def (Program defs inds) = Program (insert (def_name def) def defs) inds

insertInductive :: Inductive -> Program -> Program
insertInductive ind (Program defs inds) = Program defs (insert (ind_name ind) ind inds)

getDefinition :: String -> Program -> Definition
getDefinition ident (Program defs _) = defs ! ident

getInductive :: String -> Program -> Inductive
getInductive ident (Program _ inds) = inds ! ident
