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

data Variable
  = DeBruijn Int
  | Global String
  deriving (Eq, Ord, Show)

-- | The *concrete* values for types.
data Type
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
  | TSum [Value] [String]
  | TStruct [(String, Type)]
  | TLinArrow Type Type
  | TUnrArrow Type Type
  | TForallArrow (DebugInfo String) Type Type
  | TNewType String [TValue] Type
  -- ^ A type constructor name, it's arguments if any and the underlying type
  --   The arguments must have an equality. For now only bool, int and type are supported
  deriving (Show, Eq, Ord)

void :: Type
void = TSum [] []

unit :: Type
unit = TTuple []

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
  | Eq
  | Neq
  | Gt
  | Lt
  | Gteq
  | Lteq
  deriving (Show, Eq, Ord)

-- | Type for concrete comptime values in the interpreter
data Value
  = VUnit
  | VInt Integer
    -- ^ Can be of any integer size and a byte
  | VBool Bool
  | VType Type
  | VStruct [(String, Value)]
  | VTuple [Value]
  | VEnum String [Value] [Value]
  | VFun [Value] Int TExpr
  -- ^ The argument is at De Bruijn index 0 and the context in the list is above 0.
  --   The integer is the number of expected arguments before reduction is possible
  | VForall [TValue] Int Type Expr
  -- ^ Function whose body type depends on the argument.
  deriving (Show, Eq, Ord)

-- | Typed value
data TValue =
  TValue Value Type
  deriving (Show, Eq, Ord)

data ExprT typ expr
  = LocalVar (DebugInfo String) Int
  | Def String
  -- ^ Unresolved Definition.
  | Constructor String
  | Value TValue
  | Let
      { name :: DebugInfo String
      , val :: expr
      , vartyp :: (Maybe typ)
      , body :: expr
      }
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

data SourcePos =
  SourcePos
  deriving (Eq, Show, Ord)

data Expr =
  Expr SourcePos (ExprT Expr Expr)
  deriving (Eq, Show, Ord)

data TExpr =
  TExpr Type (ExprT Type TExpr)
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
      , eargs :: [(DebugInfo String, typ)]
      , constructors :: [String]
      }
  | DConstr String typ
  | DStruct
      { sname :: String
      , fields :: [(String, typ)]
      }
  deriving (Eq, Show, Ord)

declName :: DeclT typ expr -> String
declName (DDef DefT {def_name}) = def_name
declName DEnum {ename} = ename
declName DStruct {sname} = sname
declName (DConstr name _) = name

type Decl = DeclT Expr Expr

type TDecl = DeclT Type TExpr

newtype Program =
  Program
    { prog_defs :: Map String (DeclT Expr Expr)
    }
  deriving (Show, Eq, Ord)

type TProgram = Map String TValue

insertDefinition :: Def -> Program -> Program
insertDefinition def prog =
  Program {prog_defs = M.insert (def_name def) (DDef def) (prog_defs prog)}

insertDeclaration :: Decl -> Program -> Program
insertDeclaration decl prog =
  Program {prog_defs = M.insert (declName decl) decl (prog_defs prog)}

getDeclaration :: String -> Program -> Decl
getDeclaration ident p = prog_defs p M.! ident
