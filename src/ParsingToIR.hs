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
    , (BEq, IR.Eq)
    , (BNeq, IR.Neq)
    , (BGt, IR.Gt)
    , (BGteq, IR.Gteq)
    , (BLt, IR.Lt)
    , (BLteq, IR.Lteq)
    ]

-- Manyary operations
manyOpBuiltins :: Map ManyOp Operator
manyOpBuiltins =
  M.fromList [(MAmpersand, IR.Ampersand), (MBar, IR.Bar), (MHat, IR.Hat)]

-- Get an expression from an identifier
getExprFromIdent :: String -> PIRContext -> IR.Expr
getExprFromIdent str (PIRContext local) =
  case elemIndex str local of
    Just pos -> Expr SourcePos $ LocalVar (DI str) pos
    Nothing -> Expr SourcePos $ Def str

-- Add a local variable with de bruijn index 0 to the context
addLocalVar :: String -> PIRContext -> PIRContext
addLocalVar str = pirctx_local %~ (str :)

ttypeExpr :: IR.Expr
ttypeExpr = IR.Expr SourcePos $ Value $ TValue (VType TType) TType

-- Transform a parsed expression to an IR expression
exprToIr :: Parser.Expr -> PIRContext -> Either Error IR.Expr
exprToIr EType _ = return ttypeExpr
exprToIr (Var str) ctx = return $ getExprFromIdent str ctx
exprToIr (Parser.IntConst i) _ =
  return $
  Expr SourcePos $ Value $ TValue (VInt i) $ TInt $ IntType 32 True False
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
    IR.Call (Expr SourcePos $ IR.Call (Expr SourcePos $ Operator op) e1') e2'
exprToIr (ManyOp MComma es) ctx =
  Expr SourcePos <$> Tuple <$>
  foldM (\lst e -> (: lst) <$> exprToIr e ctx) [] es
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
defToIr :: PDefinition -> PIRContext -> Either Error Decl
defToIr (PDefinition name args body typ) ctx = do
  let args' = DI <$> args
  typ' <- exprToIr typ ctx
  let ctx' = foldl (flip addLocalVar) ctx args
  body' <- exprToIr body ctx'
  return $ DDef $ DefT name typ' args' body'

indTypToIr ::
     Parser.Expr
  -> [String]
  -> PIRContext
  -> Either Error ([(DebugInfo String, IR.Expr)], PIRContext)
indTypToIr (BinOp BLinearArrow _ _) _ _ = do
  Left $ TypeShouldHaveUnrArrows
indTypToIr (BinOp BUnrestrictedArrow e1 e2) (arg:args) ctx = do
  e1' <- exprToIr e1 ctx
  let ctx' = addLocalVar "_" ctx
  (e2', ctx'') <- indTypToIr e2 args ctx'
  return ((DI arg, e1') : e2', ctx'')
indTypToIr (BinOp BUnrestrictedArrow _ _) [] ctx = Left $ NotEnoughArgs
indTypToIr (Forall name e1 e2) (arg:args) ctx = do
  e1' <- exprToIr e1 ctx
  let ctx' = addLocalVar name ctx
  (e2', ctx'') <- indTypToIr e2 args ctx'
  return ((DI arg, e1') : e2', ctx'')
indTypToIr (Forall _ _ _) [] ctx = Left $ NotEnoughArgs
indTypToIr e1 (_:_) ctx = Left $ TooManyArgs
indTypToIr e1 [] ctx = do
  e1' <- exprToIr e1 ctx
  if e1' == ttypeExpr
    then return ([], ctx)
    else Left $ LastShouldBeType e1'

indToIr :: PInductive -> PIRContext -> Either Error Decl
indToIr (PInductive name typ args constrs) ctx = do
  (typ', ctx') <- indTypToIr typ args ctx
  constrs' <-
    mapM
      (\(c_name, typ) -> do
         typ' <- exprToIr typ ctx'
         return (c_name, typ'))
      constrs
  return $ DEnum name typ' constrs'

structToIr :: PStruct -> PIRContext -> Either Error Decl
structToIr (PStruct name fields) ctx = do
  fields' <-
    mapM
      (\(f_name, typ) -> do
         typ' <- exprToIr typ ctx
         return (f_name, typ'))
      fields
  return $ DStruct name fields'

declToIr :: PDeclaration -> PIRContext -> Either Error Decl
declToIr (DefDecl d) ctx = defToIr d ctx
declToIr (IndDecl d) ctx = indToIr d ctx
declToIr (StructDecl s) ctx = structToIr s ctx

-- Parse a parsed program to an IR program
parsedProgramToIr :: Parser.Program -> Either Error IR.Program
parsedProgramToIr =
  foldM
    (\p' decl -> flip insertDeclaration p' <$> declToIr decl emptyCtx)
    (IR.Program M.empty)
