module IR where

import Data.Map

data ConstType
  = IntConst Int
  | BoolConst Bool
  | BoolType
  | IntType
  deriving (Show, Eq)

data DebugInfo a = DI a
instance Eq (DebugInfo a) where
  (==) = const $ const True
instance Show a => Show (DebugInfo a) where
  show (DI x) = show x

data Expr
  = LocalVar (DebugInfo String) Int
  | Def String -- Definition
  | InductiveType String
  | Constructor String Int
  | Const ConstType
  | Assign (DebugInfo String) Expr Expr
  | IfThenElse Expr Expr Expr
  | Call Expr Expr
  | Lambda (DebugInfo String) Expr Expr
  | Arrow Expr Expr
  | Type
  deriving (Eq,Show)

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
