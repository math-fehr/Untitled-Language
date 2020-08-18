{-# LANGUAGE TemplateHaskell #-}

module IR where

import Data.Map
import Control.Lens

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

data MatchCase = MatchCase
  { case_args   :: [DebugInfo String],
    case_expr   :: Expr
  } deriving (Eq, Show)

data Expr
  = LocalVar (DebugInfo String) Int
  | Def String -- Definition
  | InductiveType String
  | Constructor String Int
  | Const ConstType
  | Assign (DebugInfo String) Expr Expr
  | IfThenElse Expr Expr Expr
  | Match Expr String [MatchCase]
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
  { _prog_defs :: Map String Definition
  , _prog_inds :: Map String Inductive }

makeLenses ''Program

insertDefinition :: Definition -> Program -> Program
--insertDefinition def (Program defs inds) = Program (insert (def_name def) def defs) inds
insertDefinition def = prog_defs %~ insert (def_name def) def

insertInductive :: Inductive -> Program -> Program
insertInductive ind = prog_inds %~ insert (ind_name ind) ind

getDefinition :: String -> Program -> Definition
getDefinition ident p = (p^.prog_defs) ! ident

getInductive :: String -> Program -> Inductive
getInductive ident p = (p^.prog_inds) ! ident
