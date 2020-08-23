{-# LANGUAGE TemplateHaskell #-}

module ParsingToIR where

import           IR
import           Parser
import           Data.List
import qualified Data.List     as L
import           Data.Map         (Map)
import qualified Data.Map      as M
import           Data.Set         (Set)
import qualified Data.Set      as S
import           Data.Maybe
import           Error
import           Control.Monad
import           Control.Lens
import           Utils

-- Local variable context
data PIRContext = PIRContext
  { _pirctx_local  :: [String]
  , _pirctx_def    :: Set String
  , _pirctx_ind    :: Map String [String]
  , _pirctx_constr :: Map String (String, Int) }

makeLenses ''PIRContext

-- An empty context
emptyCtx :: PIRContext
emptyCtx = PIRContext [] S.empty M.empty M.empty

-- Get an expression from an identifier
getExprFromIdent :: String -> PIRContext -> Maybe IR.Expr
getExprFromIdent str (PIRContext local def ind constrs)
  | str `elem`     local  = LocalVar (DI str) <$> elemIndex str local
  | str `S.member` def    = Just $ Def str
  | str `M.member` ind    = Just $ InductiveType str
  | str `M.member` constrs =
    let (constr, idx) = constrs M.! str in
      Just $ Constructor constr idx
  | otherwise = Nothing

-- Get the type of a constructor, and its constructor id
getConstructorId :: String -> PIRContext -> Maybe (String, Int)
getConstructorId str ctx = M.lookup str (ctx^.pirctx_constr)

-- Get the constructors of an inductive
getConstructors :: String -> PIRContext -> [String]
getConstructors str ctx = (ctx^.pirctx_ind) M.! str

-- Add a local variable with de bruijn index 0 to the context
addLocalVar :: String -> PIRContext -> PIRContext
addLocalVar str = pirctx_local %~ (str :)

-- Add a definition to the context
addDef :: String -> PIRContext -> Either Error PIRContext
addDef str ctx
  | isNothing $ getExprFromIdent str ctx =
    return $ pirctx_def %~ (S.insert str) $ ctx
  | otherwise = Left $ DuplicateDefinition str

-- Add an inductive type to the context
addIndType :: String -> [String] -> PIRContext ->  Either Error PIRContext
addIndType str constrs ctx
  | isNothing $ getExprFromIdent str ctx =
    return $ pirctx_ind %~ (M.insert str constrs) $ ctx
  | otherwise = Left $ DuplicateDefinition str

-- Add an inductive constructor to the context
addIndConstr :: String -> String -> Int -> PIRContext -> Either Error PIRContext
addIndConstr ind constr idx ctx
  | isNothing $ getExprFromIdent constr ctx =
      return $ pirctx_constr %~ (M.insert constr (ind, idx)) $ ctx
  | otherwise = Left $ DuplicateDefinition constr

-- Add an inductive type and its constructor to the context
addInd :: PInductive -> PIRContext -> Either Error PIRContext
addInd (PInductive name args ind_constrs) ctx = do
  ctx_with_ind <- addIndType name ((^.pconstr_name) <$> ind_constrs) ctx
  let ctx_with_args = Prelude.foldl (\ctx' (arg_name, _) -> addLocalVar arg_name ctx') ctx_with_ind args
  ctx' <-
    foldM (\ctx' (idx, PInductiveConstructor constr _) ->
              addIndConstr name constr idx ctx')
          ctx_with_args (zip [0..] ind_constrs)
  return $ pirctx_local .~ (ctx^.pirctx_local) $ ctx'

-- Add all definitions and declarations to the context
getProgramCtx :: Parser.Program -> Either Error PIRContext
getProgramCtx [] = return emptyCtx
getProgramCtx (InductiveDecl  ind : p) = getProgramCtx p >>= addInd ind
getProgramCtx (DefinitionDecl d   : p) = getProgramCtx p >>= addDef (pdef_name d)

-- Transform a case in a match to an IR case, with a constructor identifier
matchCaseToIr :: PMatchCase -> PIRContext -> Either Error (String, Int, MatchCase)
matchCaseToIr (PMatchCase constr args expr) ctx =
  case getConstructorId constr ctx of
    Just (typ, idx) -> let ctx' = L.foldl (flip addLocalVar) ctx args in
                         do expr' <- exprToIr expr ctx'
                            return $ (typ, idx, MatchCase (DI <$> args) expr')
    Nothing -> Left $ ExpectedConstrutor constr

-- Check that a pattern match has the correct number of constructors
checkMatchList :: [(String, Int, MatchCase)] -> PIRContext -> Either Error (String, [MatchCase])
checkMatchList [] _ = Left NoCases
checkMatchList cases ctx
  | not $ all ((== ind_name) <$> (^._1)) cases = Left MatchHeterogeneous
  | Just i <- missingInRange constrs_id n_constrs = Left $ MissingCase (constrs !! i)
  | Just i <- duplicate constrs_id = Left $ DuplicateCase (constrs !! i)
  | otherwise = return (ind_name, (^._3) <$> fromJust <$> (\x -> find ((==x) <$> (^._2)) cases) <$> [0..(n_constrs - 1)])
  where (ind_name, _, _) = head cases
        constrs_id = (^._2) <$> cases
        n_constrs = length constrs_id
        constrs = getConstructors ind_name ctx

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
     return $ IR.Assign (DI s) expr' body'
exprToIr (Parser.Call fun arg) ctx =
  do fun' <- exprToIr fun ctx
     arg' <- exprToIr arg ctx
     return $ IR.Call fun' arg'
exprToIr (Parser.IfThenElse c e1 e2) ctx =
  do c' <- exprToIr c ctx
     e1' <- exprToIr e1 ctx
     e2' <- exprToIr e2 ctx
     return $ IR.IfThenElse c' e1' e2'
exprToIr (Parser.Match e cases) ctx =
  do e' <- exprToIr e ctx
     cases' <- mapM (flip matchCaseToIr ctx) cases
     (ind_type, ordered_cases) <- checkMatchList cases' ctx
     return $ IR.Match e' ind_type ordered_cases
exprToIr (Parser.Arrow e1 e2) ctx =
  do e1' <- exprToIr e1 ctx
     e2' <- exprToIr e2 ctx
     return $ IR.Arrow e1' e2'
exprToIr (Parser.Lambda s e1 e2) ctx =
  do e1' <- exprToIr e1 ctx
     e2' <- exprToIr e2 (addLocalVar s ctx)
     return $ IR.Lambda (DI s) e1' e2'

argsToIr :: [(String, Parser.Expr)] -> PIRContext -> Either Error ([(String, IR.Expr)], PIRContext)
argsToIr args ctx =
  foldM
    (\(l, ctx') (arg_name, arg_typ) ->
        do expr <- exprToIr arg_typ ctx'
           return (l ++ [(arg_name, expr)], addLocalVar arg_name ctx'))
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
  constr' <- foldM (\l constr' -> (:l) <$> constrToIr constr' ctx')
                   [] constr
  return $ Inductive name args' (reverse constr')

-- Parse a parsed program to an IR program
parsedProgramToIr :: Parser.Program -> Either Error IR.Program
parsedProgramToIr p =
  do ctx <- getProgramCtx p
     foldM (\p' decl ->
              case decl of
                InductiveDecl  ind -> flip insertInductive  p' <$> indToIr ind ctx
                DefinitionDecl def -> flip insertDefinition p' <$> defToIr def ctx)
       (IR.Program M.empty M.empty) p
