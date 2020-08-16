module ParsingToIR where

import IR
import Parser
import Data.List
import Data.Map
import Data.Set
import Error
import Control.Monad

-- Local variable context
data PIRContext = PIRContext
  { pirctx_local :: [String]
  , pirctx_def :: Set String
  , pirctx_ind :: Set String
  , pirctx_constr :: Map String (String, Int) }

-- An empty context
emptyCtx :: PIRContext
emptyCtx = PIRContext [] Data.Set.empty Data.Set.empty Data.Map.empty

-- Get an expression from an identifier
getExprFromIdent :: String -> PIRContext -> Maybe IR.Expr
getExprFromIdent str ctx
  | elem str (pirctx_local ctx) = (LocalVar str) <$> elemIndex str (pirctx_local ctx)
  | Data.Set.member str (pirctx_def ctx) = Just $ Def str
  | Data.Set.member str (pirctx_ind ctx) = Just $ InductiveType str
  | Data.Map.member str (pirctx_constr ctx) =
    let (constr, idx) = ((pirctx_constr ctx) ! str) in
      Just $ Constructor constr idx
  | otherwise = Nothing

-- Add a local variable with de bruijn index 0 to the context
addLocalVar :: String -> PIRContext -> PIRContext
addLocalVar str (PIRContext local def ind constr) = PIRContext (str : local) def ind constr

-- Add a definition to the context
addDef :: String -> PIRContext -> Either Error PIRContext
addDef str (PIRContext local def ind constr)
  | getExprFromIdent str (PIRContext local def ind constr) == Nothing =
    return $ PIRContext local (Data.Set.insert str def) ind constr
  | otherwise = Left $ DuplicateDefinition str

-- Add an inductive type to the context
addIndType :: String -> PIRContext ->  Either Error PIRContext
addIndType str (PIRContext local def ind constr)
  | getExprFromIdent str (PIRContext local def ind constr) == Nothing =
    return $ PIRContext local def (Data.Set.insert str ind) constr
  | otherwise = Left $ DuplicateDefinition str

-- Add an inductive constructor to the context
addIndConstr :: String -> String -> Int -> PIRContext -> Either Error PIRContext
addIndConstr ind constr idx (PIRContext local defs inds constrs)
  | getExprFromIdent constr (PIRContext local defs inds constrs)  == Nothing =
      return $ PIRContext local defs inds (Data.Map.insert constr (ind, idx) constrs)
  | otherwise = Left $ DuplicateDefinition constr

-- Add an inductive type and its constructor to the context
addInd :: PInductive -> PIRContext -> Either Error PIRContext
addInd (PInductive name args ind_constrs) ctx = do
  ctx_with_ind <- addIndType name ctx
  let ctx_with_args = Prelude.foldl (\ctx' (arg_name, _) -> addLocalVar arg_name ctx') ctx_with_ind args
  PIRContext _ def inds constrs <-
    foldM (\ctx' (idx, PInductiveConstructor constr _) ->
              addIndConstr name constr idx ctx')
    ctx_with_args (zip [0..] ind_constrs)
  return $ PIRContext (pirctx_local ctx) def inds constrs

-- Add all definitions and declarations to the context
getProgramCtx :: Parser.Program -> Either Error PIRContext
getProgramCtx [] = return emptyCtx
getProgramCtx (InductiveDecl ind : p) = getProgramCtx p >>= addInd ind
getProgramCtx (DefinitionDecl d : p) = getProgramCtx p >>= addDef (pdef_name d)

-- Transform a parsed expression to an IR expression
exprToIr :: Parser.Expr -> PIRContext -> Either Error IR.Expr
exprToIr (Var "bool") _ = return $ IR.Const BoolType
exprToIr (Var "int") _ = return $ IR.Const IntType
exprToIr (Var str) ctx =
  case getExprFromIdent str ctx of
    Just res -> return res
    Nothing -> Left $ UndefinedReference str
exprToIr (Parser.IntConst i) _ = return $ IR.Const (IR.IntConst i)
exprToIr (Parser.BoolConst b) _ = return $ IR.Const (IR.BoolConst b)
exprToIr (Parser.Assign s expr body) ctx =
  do expr' <- exprToIr expr ctx
     body' <- exprToIr body (addLocalVar s ctx)
     return $ IR.Assign s expr' body'
exprToIr (Parser.Call fun arg) ctx =
  do fun' <- exprToIr fun ctx
     arg' <- exprToIr arg ctx
     return $ IR.Call fun' arg'
exprToIr (Parser.IfThenElse c e1 e2) ctx =
  do c' <- exprToIr c ctx
     e1' <- exprToIr e1 ctx
     e2' <- exprToIr e2 ctx
     return $ IR.IfThenElse c' e1' e2'
exprToIr (Parser.Arrow e1 e2) ctx =
  do e1' <- exprToIr e1 ctx
     e2' <- exprToIr e2 ctx
     return $ IR.Arrow e1' e2'
exprToIr (Parser.Lambda s e1 e2) ctx =
  do e1' <- exprToIr e1 ctx
     e2' <- exprToIr e2 (addLocalVar s ctx)
     return $ IR.Lambda s e1' e2'

argsToIr :: [(String, Parser.Expr)] -> PIRContext -> Either Error ([(String, IR.Expr)], PIRContext)
argsToIr args ctx =
  foldM
    (\(l, ctx') (arg_name, arg_typ) ->
        do expr <- exprToIr arg_typ ctx'
           return $ (l ++ [(arg_name, expr)], addLocalVar arg_name ctx'))
    ([], ctx) args

-- Transform a parsed function to an IR function
defToIr :: PDefinition -> PIRContext -> Either Error Definition
defToIr (PDefinition name args body typ) ctx = do
  (args', ctx') <- argsToIr args ctx
  typ' <- exprToIr typ ctx'
  body' <- exprToIr body ctx'
  return $ Definition name args' typ' body'

-- Transform a parsed constructor declaration to an IR constructor declaration
constrToIr :: PInductiveConstructor -> PIRContext -> Either Error InductiveConstructor
constrToIr (PInductiveConstructor name args) ctx = do
  (args', _) <- argsToIr args ctx
  return $ InductiveConstructor name args'

-- Transform a parsed inductive to an IR inductive
indToIr :: PInductive -> PIRContext -> Either Error Inductive
indToIr (PInductive name args constr) ctx = do
  (args', ctx') <- argsToIr args ctx
  constr' <- foldM (\l constr' -> do constr'' <- constrToIr constr' ctx'
                                     return $ constr'' : l) [] constr
  return $ Inductive name args' constr'

-- Parse a parsed program to an IR program
parsedProgramToIr :: Parser.Program -> Either Error IR.Program
parsedProgramToIr p =
  do ctx <- getProgramCtx p
     foldM (\p' decl ->
              case decl of
                InductiveDecl ind -> do ind' <- indToIr ind ctx
                                        return $ insertInductive ind' p'
                DefinitionDecl def -> do def' <- defToIr def ctx
                                         return $ insertDefinition def' p')
       (IR.Program Data.Map.empty Data.Map.empty) p
