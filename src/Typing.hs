module Typing where

import IR
import Error
import Data.Foldable

-- A context of variable types
newtype TypingContext = TypingContext [Expr]

-- A context with no loval variables
emptyCtx :: TypingContext
emptyCtx = TypingContext []

-- Find a variable type in a context given its De Bruijn index
getVarType :: Int -> TypingContext -> Expr
getVarType idx (TypingContext ctx) = ctx !! idx

-- Add a variable type to the top of the context
addVarType :: Expr -> TypingContext -> TypingContext
addVarType e (TypingContext l) = TypingContext $ e : l

-- Check and get the term of a succession of arrows represented by an expression list
-- [bool; int; string] is representing bool -> (int -> string)
checkBigArrow :: TypingContext -> Program -> [Expr] -> Either Error Expr
checkBigArrow _ _ [] = error "Compiler error: call of checkBigArrow with an empty list"
checkBigArrow ctx p [e] = checkExprWellTypedHasType ctx p e Type >> return e
checkBigArrow ctx p (e1 : e2 : es) =
  do checkExprWellTypedHasType ctx p e1 Type
     esArrow <- checkBigArrow (addVarType e1 ctx) p (e2 : es)
     return $ Arrow e1 esArrow

-- Check that the type of a definition is well typed, and return the type
-- Does not check if the body is well typed
checkDefTypeWellTyped :: Definition -> Program -> Either Error Expr
checkDefTypeWellTyped (Definition _ args typ _) p =
  checkBigArrow emptyCtx p (fmap snd args ++ [typ])

-- Check that an expression is well typed, and that its type is equal to
-- some given type
checkExprWellTypedHasType :: TypingContext -> Program -> Expr -> Expr -> Either Error ()
checkExprWellTypedHasType ctx p e expected_typ =
  do typ <- checkExprWellTyped ctx p e
     if expected_typ == typ then
       return ()
     else
       Left $ ShouldBeType e typ expected_typ

-- Get the type of a Const
getConstType :: ConstType -> Expr
getConstType (BoolConst _) = Const BoolType
getConstType (IntConst _) = Const IntType
getConstType IntType = Type
getConstType BoolType = Type

-- Check that the term is well typed, and return its type
checkExprWellTyped :: TypingContext -> Program -> Expr -> Either Error Expr
checkExprWellTyped ctx _ (LocalVar _ idx) = return $ getVarType idx ctx
checkExprWellTyped _ p (Def d) = checkDefTypeWellTyped (getDefinition d p) p
checkExprWellTyped _ _ (InductiveType _) = return Type
checkExprWellTyped _ _ (Constructor s _) = return $ InductiveType s
checkExprWellTyped _ _ (Const c) = return $ getConstType c
checkExprWellTyped ctx p (Assign _ sub_e e) =
  do sub_e_type <- checkExprWellTyped ctx p sub_e
     checkExprWellTyped (addVarType sub_e_type ctx) p e
checkExprWellTyped ctx p (IfThenElse cond e1 e2) =
  do checkExprWellTypedHasType ctx p cond (Const BoolType)
     e1_type <- checkExprWellTyped ctx p e1
     checkExprWellTypedHasType ctx p e2 e1_type
     return $ e1_type
checkExprWellTyped ctx p (Call fun arg) =
  do fun_typ <- checkExprWellTyped ctx p fun
     case fun_typ of
       Arrow arg_type res_type ->
           do checkExprWellTypedHasType ctx p arg arg_type
              return $ res_type
       typ -> Left $ NotAFunction fun typ
checkExprWellTyped ctx p (Arrow e1 e2) =
  do checkExprWellTypedHasType ctx p e1 Type
     checkExprWellTypedHasType ctx p e2 Type
     return Type
checkExprWellTyped ctx p (Lambda _ e1 e2) =
  do checkExprWellTypedHasType ctx p e1 Type
     typ <- checkExprWellTyped (addVarType e1 ctx) p e2
     return $ Arrow e1 typ
checkExprWellTyped _ _ Type = return Type

-- Check that given a context, the definition is well typed, and return its type
checkDefWellTyped' :: TypingContext -> Program -> Definition -> Either Error Expr
checkDefWellTyped' ctx p (Definition _ [] typ body) =
  do checkExprWellTypedHasType ctx p typ Type
     checkExprWellTypedHasType ctx p body typ
     return typ
checkDefWellTyped' ctx p (Definition name ((_, arg) : args) typ body) =
  do checkExprWellTypedHasType ctx p arg Type
     def_typ <- checkDefWellTyped' (addVarType arg ctx) p (Definition name args typ body)
     return $ Arrow arg def_typ

-- Check that the definition is well typed, and return its type
checkDefWellTyped :: Program -> Definition -> Either Error Expr
checkDefWellTyped = checkDefWellTyped' emptyCtx

-- Check that a program is well typed
checkProgramWellTyped :: Program -> Either Error ()
checkProgramWellTyped p = traverse_ (checkDefWellTyped p) (prog_defs p)
