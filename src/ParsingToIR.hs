{-# LANGUAGE TemplateHaskell #-}

module ParsingToIR where

import Control.Lens
import Control.Monad
import Data.List
import qualified Data.List as L
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Error
import IR
import Parser
import Utils

-- Local variable context
data PIRContext =
  PIRContext
    { _pirctx_local :: [String]
    }

makeLenses ''PIRContext

-- An empty context
emptyCtx :: PIRContext
emptyCtx = PIRContext []

-- Binary operations
binOpBuiltins :: Map BinOp Operator
binOpBuiltins =
  M.fromList
    [ (BPlus, IR.Plus)
    , (BMinus, IR.Minus)
    , (BTimes, IR.Times)
    , (BDiv, IR.Div)
    , (BUnrestrictedArrow, IR.Arrow)
    , (BLinearArrow, IR.LinArrow)
    ]

-- Manyary operations
manyOpBuiltins :: Map ManyOp Operator
manyOpBuiltins =
  M.fromList [(MAmpersand, IR.Ampersand), (MBar, IR.Bar), (MHat, IR.Hat)]

-- Get an expression from an identifier
getExprFromIdent :: String -> PIRContext -> Maybe IR.Expr
getExprFromIdent str (PIRContext local)
  | str `elem` local =
    Expr SourcePos <$> LocalVar (DI str) <$> elemIndex str local
  | otherwise = Just $ Expr SourcePos $ Def str

-- Add a local variable with de bruijn index 0 to the context
addLocalVar :: String -> PIRContext -> PIRContext
addLocalVar str = pirctx_local %~ (str :)

-- Transform a parsed expression to an IR expression
exprToIr :: Parser.Expr -> PIRContext -> Either Error IR.Expr
exprToIr (Var "Int") _ = return $ Expr SourcePos $ Def "Int"
exprToIr (Var str) ctx =
  case getExprFromIdent str ctx of
    Just res -> return res
    Nothing -> Left $ UndefinedReference str
exprToIr (Parser.IntConst i) _ = return $ Expr SourcePos $ Value $ VInt i
exprToIr (Parser.Let var typ val body) ctx = do
  typ' <- percolateMaybe $ flip exprToIr ctx <$> typ
  val' <- exprToIr val ctx
  body' <- exprToIr body (addLocalVar var ctx)
  return $ Expr SourcePos $ (IR.Let (DI var) val' typ' body')
exprToIr (Parser.Call fun arg) ctx = do
  fun' <- exprToIr fun ctx
  arg' <- exprToIr arg ctx
  return $ Expr SourcePos $ IR.Call fun' arg'
exprToIr (BinOp binop e1 e2) ctx = do
  e1' <- exprToIr e1 ctx
  e2' <- exprToIr e2 ctx
  let op = binOpBuiltins ! binop
  return $
    Expr SourcePos $
    IR.Call
      (Expr SourcePos $ IR.Call (Expr SourcePos $ Value (VOperator op)) e1')
      e2'
exprToIr (ManyOp mop es) ctx = do
  let op = manyOpBuiltins ! mop
  es' <- mapM (flip exprToIr ctx) es
  return $
    Expr SourcePos $
    IR.Call (Expr SourcePos $ Operator op) (Expr SourcePos $ IR.Tuple es')
exprToIr (Forall var e1 e2) ctx = do
  e1' <- exprToIr e1 ctx
  e2' <- exprToIr e2 (addLocalVar var ctx)
  return $ Expr SourcePos $ IR.ForAll (DI var) e1' e2'
exprToIr (Parser.IfThenElse cond e1 e2) ctx = do
  cond' <- exprToIr cond ctx
  e1' <- exprToIr e1 ctx
  e2' <- exprToIr e2 ctx
  return $ Expr SourcePos $ IR.IfThenElse cond' e1' e2'
exprToIr (Parser.Lambda name arg body) ctx = do
  arg' <- exprToIr arg ctx
  body' <- exprToIr body (addLocalVar name ctx)
  return $ Expr SourcePos $ IR.Lambda (DI name) True arg' body'
exprToIr (Parser.Parens e) ctx = exprToIr e ctx

-- Transform a parsed function to an IR function
defToIr :: PDefinition -> PIRContext -> Either Error Def
defToIr (PDefinition name args body typ) ctx = do
  let args' = DI <$> args
  typ' <- exprToIr typ ctx
  let ctx' = foldl (flip addLocalVar) ctx args
  body' <- exprToIr body ctx
  return $ DefT name typ' args' body'

-- Parse a parsed program to an IR program
parsedProgramToIr :: Parser.Program -> Either Error IR.Program
parsedProgramToIr p = do
  foldM
    (\p' decl ->
       case decl of
         DefDecl def -> flip insertDefinition p' <$> defToIr def emptyCtx
         IndDecl _ -> return $ p')
    (IR.ProgramT M.empty)
    p
