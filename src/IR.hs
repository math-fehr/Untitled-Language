{-# LANGUAGE TemplateHaskell #-}

module IR where

import Control.Lens
import qualified Data.Map as M
import Data.Map (Map)

data ConstType
  = IntConst Int
  | BoolConst Bool
  | BoolType
  | IntType
  deriving (Show, Eq)

data DebugInfo a =
  DI a

instance Eq (DebugInfo a) where
  (==) = const $ const True

instance Show a => Show (DebugInfo a) where
  show (DI x) = show x

data Builtins = Product
              | Sum
              | Ampersand
              | LinearArrow
              | UnrestrictedArrow
              deriving (Eq, Show)

data Expr
  = LocalVar (DebugInfo String) Int
  | Builtin Builtins
  | Def String -- Definition
  | Const ConstType
  | Assign (DebugInfo String) Expr Expr
  | IfThenElse Expr Expr Expr
  | Call Expr Expr
  | Lambda (DebugInfo String) Arg Expr
  | Type
  deriving (Eq,Show)

data Arg = UnrestrictedArg Expr
         | LinearArg       Expr
         deriving (Eq,Show)

data Definition = Definition
  { def_name :: String
  , def_args :: [(DebugInfo String, Arg)]
  , def_type :: Expr
  , def_body :: Expr
  } deriving(Eq,Show)

data Program = Program
  { _prog_defs :: Map String Definition
  } deriving(Show)

makeLenses ''Program

insertDefinition :: Definition -> Program -> Program
--insertDefinition def (Program defs inds) = Program (insert (def_name def) def defs) inds
insertDefinition def = prog_defs %~ M.insert (def_name def) def

getDefinition :: String -> Program -> Definition
getDefinition ident p = (p^.prog_defs) M.! ident
