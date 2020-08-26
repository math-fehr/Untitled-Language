{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module IR where

import Control.Lens
import qualified Data.Map as M
import Data.Map (Map)

newtype DebugInfo a =
  DI a

instance Eq (DebugInfo a) where
  (==) = const $ const True

instance Show a => Show (DebugInfo a) where
  show (DI x) = show x

instance Ord (DebugInfo a) where
  _ <= _ = True

data IntType =
  IntType
    { size :: Int
    , signed :: Bool
    , wrapping :: Bool
    }
  deriving (Eq, Show, Ord)

-- | The *concrete* values for types.
data TypeBase
  = TVar (DebugInfo String) Int
  -- ^ Type variable bounded by a Forall (de Bruijn)
  | TBool
  | TType
  -- ^ The type of a type
  | TInt IntType
  | TByte
  | TTuple [Type]
  | TArray Type Int
  | TChoice [Type]
  | TSum [(String, Type)]
  | TStruct [(String, Type)]
  | TLinArrow Type Type
  | TUnrArrow Type Type
  | TForallArrow (DebugInfo String) Type Type
  | TNewType String [TValue] Type
  -- ^ A type constructor name, it's arguments if any and the underlying type
  --   The arguments must have an equality. For now only bool, int and type are supported
  deriving (Show, Eq, Ord)

data Type = Type
  { comptime :: Bool
  , base :: TypeBase
  }
  deriving (Show, Eq, Ord)

void :: Type
void = Type {comptime = True, base = TSum []}

unit :: Type
unit = Type {comptime = True, base = TTuple []}

data Operator
  = Plus
  | Minus
  | Times
  | Div
  | Hat
  | Ampersand
  | Bar
  | Arrow
  | LinArrow
  deriving (Show, Eq, Ord)

-- | Type for concrete comptime values in the interpreter
data Value
  = VUnit
  | VInt Int
    -- ^ Can be of any integer size and a byte
  | VBool Bool
  | VType Type
  | VStruct [(String, Value)]
  | VTuple [Value]
  | VConstr String Value
  | VFun TExpr
  -- ^ The function is evaluated by binding the Bruijn index 0 and evaluating
  | VOperator Operator
  deriving (Show, Eq, Ord)

-- | Typed value
data TValue =
  TValue Value Type
  deriving (Show, Eq, Ord)

data ExprT typ expr
  = LocalVar (DebugInfo String) Int
  | Def String
  -- ^ Unresolved Definition.
  | IntConst Int
  | Let (DebugInfo String) (Maybe typ) expr
  | IfThenElse expr expr expr
  | Call expr expr
    -- ^ Many Op are called on a big tuple
  | Operator Operator
  | Tuple [expr]
  | Lambda
      { name :: DebugInfo String
      , linear :: Bool
      , argtyp :: typ
      , body :: expr
      }
  | ForAll (DebugInfo String) typ expr
    -- ^ ForAll (name of var) (type of var) (type of the rest of the arrow)
  deriving (Eq, Show, Ord)

data SourcePos = SourcePos
  deriving (Eq, Show, Ord)

data Expr =
  Expr SourcePos (ExprT Expr Expr)
  deriving (Eq, Show, Ord)

data TExpr =
  TExpr Type (ExprT Type Expr)
  deriving (Eq, Show, Ord)

data DefT typ expr =
  DefT
    { def_name :: String
    , def_type :: typ
    , def_args :: [DebugInfo String]
    , def_body :: expr
    }
  deriving (Eq, Show, Ord)

type Def = DefT Expr Expr

type TDef = DefT Type TExpr

data DeclT typ expr
  = DDef (DefT typ expr)
  | DEnum
      { ename :: String
      , constructors :: [(String, typ)]
      }
  | DStruct
      { sname :: String
      , fields :: [(String, typ)]
      }
  deriving (Eq, Show, Ord)

declName :: DeclT typ expr -> String
declName (DDef DefT {def_name}) = def_name
declName DEnum {ename} = ename
declName DStruct {sname} = sname

type Decl = DeclT Expr Expr

type TDecl = DeclT Type TExpr

newtype ProgramT typ expr =
  ProgramT
    { prog_defs :: Map String (DeclT typ expr)
    }
  deriving (Show, Eq, Ord)

type Program = ProgramT Expr Expr

type TProgram = ProgramT Type Expr

insertDefinition :: DefT typ expr -> ProgramT typ expr -> ProgramT typ expr
insertDefinition def prog =
  ProgramT {prog_defs = M.insert (def_name def) (DDef def) (prog_defs prog)}

insertDeclaration :: DeclT typ expr -> ProgramT typ expr -> ProgramT typ expr
insertDeclaration decl prog =
  ProgramT {prog_defs = M.insert (declName decl) decl (prog_defs prog)}

getDeclaration :: String -> ProgramT typ expr -> DeclT typ expr
getDeclaration ident p = prog_defs p M.! ident
