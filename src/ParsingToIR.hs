{-# LANGUAGE TemplateHaskell #-}

module ParsingToIR where

import Control.Lens
import Control.Monad
import Data.List
import qualified Data.List as L
import Data.Map (Map)
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
    , _pirctx_def :: Set String
    }

makeLenses ''PIRContext

-- An empty context
emptyCtx :: PIRContext
emptyCtx = PIRContext [] S.empty

-- Builtins operations
builtinsOp :: Map String Builtins
builtinsOp =
  M.fromList
    [ ("*", IR.Product)
    , ("+", IR.Plus)
    , ("&", IR.Ampersand)
    , ("->", IR.UnrestrictedArrow)
    , ("-o", IR.LinearArrow)
    ]

-- Get an expression from an identifier
getExprFromIdent :: String -> PIRContext -> Maybe IR.Expr
getExprFromIdent str (PIRContext local def)
  | str `elem` local = LocalVar (DI str) <$> elemIndex str local
  | str `S.member` def = Just $ Def str
  | otherwise = IR.Builtin <$> M.lookup str builtinsOp

-- Add a local variable with de bruijn index 0 to the context
addLocalVar :: String -> PIRContext -> PIRContext
addLocalVar str = pirctx_local %~ (str :)

-- Add a definition to the context
addDef :: String -> PIRContext -> Either Error PIRContext
addDef str ctx
  | isNothing $ getExprFromIdent str ctx =
    return $ pirctx_def %~ (S.insert str) $ ctx
  | otherwise = Left $ DuplicateDefinition str

-- Add all definitions and declarations to the context
getProgramCtx :: Parser.Program -> Either Error PIRContext
getProgramCtx [] = return emptyCtx
getProgramCtx (d:p) = getProgramCtx p >>= addDef (pdef_name d)

-- Transform a parsed expression to an IR expression
exprToIr :: Parser.Expr -> PIRContext -> Either Error IR.Expr
exprToIr (Var "bool") _ = return $ IR.Const BoolType
exprToIr (Var "int") _ = return $ IR.Const IntType
exprToIr (Var str) ctx =
  case getExprFromIdent str ctx of
    Just res -> return res
    Nothing -> Left $ UndefinedReference str
exprToIr (TypedVar _ typ) ctx = exprToIr typ ctx
exprToIr (Parser.IntConst i) _ = return $ IR.Const (IR.IntConst i)
exprToIr (Parser.BoolConst b) _ = return $ IR.Const (IR.BoolConst b)
exprToIr (Parser.Assign s expr body) ctx = do
  expr' <- exprToIr expr ctx
  body' <- exprToIr body (addLocalVar s ctx)
  return $ IR.Assign (DI s) expr' body'
exprToIr (Parser.Call fun arg) ctx = do
  fun' <- exprToIr fun ctx
  arg' <- exprToIr arg ctx
  return $ IR.Call fun' arg'
exprToIr (Parser.IfThenElse c e1 e2) ctx = do
  c' <- exprToIr c ctx
  e1' <- exprToIr e1 ctx
  e2' <- exprToIr e2 ctx
  return $ IR.IfThenElse c' e1' e2'
exprToIr (Parser.Lambda s e1 e2) ctx = do
  e1' <- exprToIr e1 ctx
  e2' <- exprToIr e2 (addLocalVar s ctx)
  return $ IR.Lambda (DI s) (LinearArg e1') e2'

argsToIr ::
     [(String, Parser.Expr, Bool)]
  -> PIRContext
  -> Either Error ([(DebugInfo String, IR.Arg)], PIRContext)
argsToIr args ctx =
  foldM
    (\(l, ctx') (arg_name, arg_typ, b) -> do
       expr <- exprToIr arg_typ ctx'
       return (l ++ [(DI arg_name, makeArg b expr)], addLocalVar arg_name ctx'))
    ([], ctx)
    args
  where
    makeArg :: Bool -> IR.Expr -> Arg
    makeArg True = LinearArg
    makeArg False = UnrestrictedArg

argsFromType :: Parser.Expr -> [(String, Parser.Expr, Bool)]
argsFromType (Parser.Call (Parser.Call (Parser.Var "->") (Parser.TypedVar name typ)) expr) =
  (name, typ, False) : argsFromType expr
argsFromType (Parser.Call (Parser.Call (Parser.Var "-o") (Parser.TypedVar name typ)) expr) =
  (name, typ, True) : argsFromType expr
argsFromType _ = []

-- Transform a parsed function to an IR function
defToIr :: PDefinition -> PIRContext -> Either Error Definition
defToIr (PDefinition name body typ) ctx = do
  let args = argsFromType typ
  (args', ctx') <- argsToIr args ctx
  typ' <- exprToIr typ ctx'
  body' <- exprToIr body ctx'
  return $ Definition name args' typ' body'

-- Parse a parsed program to an IR program
parsedProgramToIr :: Parser.Program -> Either Error IR.Program
parsedProgramToIr p = do
  ctx <- getProgramCtx p
  foldM
    (\p' def -> flip insertDefinition p' <$> defToIr def ctx)
    (IR.Program M.empty)
    p
