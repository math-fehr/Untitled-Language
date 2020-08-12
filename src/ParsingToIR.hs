module ParsingToIR where

import IR
import Parser
import Data.List
import Data.Maybe

-- Local variable context
newtype PIRContext = PIRContext [String]

-- A context with no variables
emptyCtx :: PIRContext
emptyCtx = PIRContext []

-- Find a variable and retrieve its Bruijn index
findVar :: String -> PIRContext -> Maybe Int
findVar str (PIRContext ctx) = elemIndex str ctx

-- Add a variable with index 0
addVar :: String -> PIRContext -> PIRContext
addVar str (PIRContext ctx) = PIRContext (str : ctx)


-- Transform a parsed expression to an IR expression
exprToIr :: Parser.Expr -> PIRContext -> IR.Expr
exprToIr (Var "bool") _ = IR.PrimitiveType "bool"
exprToIr (Var "int") _ = IR.PrimitiveType "int"
exprToIr (Var str) ctx = fromMaybe (Def str) $ fmap (LocalVar str) $ findVar str ctx
exprToIr (Parser.IntConst i) _ = IR.IntConst i
exprToIr (Parser.BoolConst b) _ = IR.BoolConst b
exprToIr (Parser.Assign s expr body) ctx = IR.Assign s (exprToIr expr ctx) (exprToIr body (addVar s ctx))
exprToIr (Parser.Call fun arg) ctx = IR.Call (exprToIr fun ctx) (exprToIr arg ctx)

-- Transform a parsed function to an IR function
funToIr :: PFunction -> Function
funToIr (PFunction name args body typ) =
  let (args', ctx') = foldr (\(arg_name, arg_typ) (l, ctx) -> ((arg_name, exprToIr arg_typ ctx) : l, addVar arg_name ctx)) ([], emptyCtx) args in
  let typ' = exprToIr typ ctx' in
  let body' = exprToIr body ctx' in
  Function name args' typ' body'

-- Parse a parsed program to an IR program
parsedProgramToIr :: [PFunction] -> [Function]
parsedProgramToIr = map funToIr
