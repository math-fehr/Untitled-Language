{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Typing where

import Control.Lens hiding (Const(..))
import Control.Lens.Indexed
import Control.Monad
import Control.Monad.Except
import Control.Monad.HT (nest)
import Control.Monad.State hiding (get, put, state)
import Control.Monad.State.Class
import Data.Foldable
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Error
import IR
import Interpreter hiding (UndefinedVariable, interpret)

--   _____            _               __  __                       _
--  |_   _|   _ _ __ (_)_ __   __ _  |  \/  | ___  _ __   __ _  __| |
--    | || | | | '_ \| | '_ \ / _` | | |\/| |/ _ \| '_ \ / _` |/ _` |
--    | || |_| | |_) | | | | | (_| | | |  | | (_) | | | | (_| | (_| |
--    |_| \__, | .__/|_|_| |_|\__, | |_|  |_|\___/|_| |_|\__,_|\__,_|
--        |___/|_|            |___/
-- Abstract typing monad
--     _   _       _               _
--    /_\ | |__ __| |_ _ _ __ _ __| |_
--   / _ \| '_ (_-<  _| '_/ _` / _|  _|
--  /_/ \_\_.__/__/\__|_| \__,_\__|\__|
--
-- | Status of individual linear variable during typing
data VarStatus
  = Unrestricted
  | LinearUsed
  | LinearFree
  | LinearHidden
  | Undefined
  deriving (Eq, Ord, Show)

-- | Status of global binding in the program during typing
data GlobalStatus
  = Undeclared Decl
  | DeclInProgress
  | Declared Type Decl
  | DefInProgress
  | Defined TValue
  deriving (Eq, Ord, Show)

data TypingError
  = ExpectedType Expr Type Type -- Expression, expected type, and real type
  | ReusedLinear String Expr -- Variable name, expression,
                             -- and expression in which it's used
  | UnusedLinear String Expr -- Same
  | UndefinedVariable String Expr -- Same
  | NotAType Expr -- Expression is not a type
  | UnknownVariable Variable
  | LastScope
  -- UnknownBuiltin Builtins
  | DifferringRessources Expr Expr
  | LinearUseIllegal String Expr
  | RuntimeError RuntimeError -- The typer required an evaluation which failed.
  | TypingCycle -- There is an unsolvable cyclic dependency for typing TODO: Print the cycle
  | InternalError String
  | NotAnArrow TypeBase -- The type is not an arrow
  | DeclarationTypeIsNotAType
  | DeclarationFunctionTypeArgumentNumberMismatch
  deriving (Eq, Show)

-- Abstract Typing Monad definition
class MonadError TypingError m =>
      TypingMonad m
  where
  register :: String -> Decl -> m ()
  decl :: String -> (Decl -> m Type) -> m Type
  -- ^ Declare a global.
  -- While the internal computation is in progress, The variable is marked as `InProgress`
  def :: String -> (Decl -> Type -> m TValue) -> m TValue
  addLinear :: String -> Type -> Maybe Value -> m ()
  addUnrestricted :: String -> Type -> Maybe Value -> m ()
  getValue :: Variable -> m (Maybe Value)
  leaveScope :: m String -- Name of variable which scope we're leaving
  variableStatus :: Variable -> m VarStatus
  globalStatus :: String -> m GlobalStatus
  -- State manipulation
  saveState :: m a -> m a
  hideLinear :: m a -> m a
  -- The following three might throw UnknownVariable
  useVar :: Variable -> m ()
  varName :: Variable -> m String
  varType :: Variable -> m Type
  freeVariables :: m (Set Variable)
  interpret :: TExpr -> m TValue
  runTyping :: m a -> Either TypingError a

-- Concrete typing monad
--    ___                     _
--   / __|___ _ _  __ _ _ ___| |_ ___
--  | (__/ _ \ ' \/ _| '_/ -_)  _/ -_)
--   \___\___/_||_\__|_| \___|\__\___|
--
data LocalVariable =
  LVar
    { _lv_name :: String
    , _lv_typ :: Type
    , _lv_isLinear :: Bool
    , _lv_isUsed :: Bool
    , _lv_compTime :: Maybe Value
    }
  deriving (Eq, Show)

makeLenses ''LocalVariable

data TypingState =
  TpState
    { _ts_global :: Map String GlobalStatus
    , _ts_local :: Vector LocalVariable
    , _ts_hidden :: Int
    }
  deriving (Eq, Show)

makeLenses ''TypingState

type ConcreteTypingMonad_ a
   = StateT TypingState (ExceptT TypingError Identity) a

newtype ConcreteTypingMonad a =
  CTM
    { unCTM :: ConcreteTypingMonad_ a
    }

instance Functor ConcreteTypingMonad where
  fmap f (CTM x) = CTM $ f <$> x

instance Applicative ConcreteTypingMonad where
  pure = CTM . pure
  (CTM f) <*> (CTM x) = CTM $ f <*> x

instance Monad ConcreteTypingMonad where
  (CTM x) >>= f = CTM $ x >>= (unCTM . f)

instance MonadError TypingError ConcreteTypingMonad where
  throwError = CTM . throwError
  catchError (CTM def) handler = CTM $ catchError def $ unCTM . handler

instance MonadState TypingState ConcreteTypingMonad where
  get = CTM get
  put = CTM . put

-- lookupVar :: TypingState -> Variable -> Maybe (Either Type (LocalVariable, Int))
-- lookupVar (TpState globals _ _) (Global name) =
--   M.lookup name globals >>= return . Left
-- lookupVar (TpState _ locals _) (DeBruijn id) =
--   locals V.!? id >>= return . Right . (, id)
instance TypingMonad ConcreteTypingMonad where
  register name decl = ts_global %= M.insert name (Undeclared decl)
  --
  decl = undefined
  -- decl name action = do
  --   globals <- use ts_global
  --   case M.lookup name globals of
  --     Just (Undeclared decl) -> do
  --       ts_global %= M.insert name DeclInProgress
  --       typ <- action decl
  --       ts_global %= M.insert name (Declared typ)
  --       return typ
  --     Just (Declared typ _) -> typ
  --     Just (Defined (TValue typ value)) -> typ
  --     Just a -> throwError TypingCycle
  --     Nothing ->
  --       throwError (InternalError "Trying to declare an unregistred variable")
    --
  def = undefined
  -- def name action = do
  --   globals <- use ts_global
  --   typ <-
  --     case M.lookup name globals of
  --       Just (Declared typ) -> return typ
  --       Just a ->
  --         throwError
  --           (InternalError
  --              ("Defining var" ++ name ++ "but it is in state" ++ show a))
  --       Nothing ->
  --         throwError (InternalError "Trying to declare an unregistred variable")
  --   return ()
    --
  addLinear name typ mvalue = ts_local %= cons (LVar name typ True False mvalue)
    --
  addUnrestricted name typ mvalue =
    ts_local %= cons (LVar name typ False False mvalue)
  --
  leaveScope = do
    name <-
      preuse (ts_local . ix 0) >>=
      maybe (throwError $ LastScope) (return . (^. lv_name))
    ts_local %= V.tail
    return name
  --
  variableStatus = undefined
  saveState = undefined
  hideLinear = undefined
  useVar (DeBruijn id) = do
    var <-
      preuse (ts_local . ix id) >>=
      maybe (throwError $ UnknownVariable $ DeBruijn id) return
    let usedVar = var & lv_isUsed .~ True
    ts_local . ix id .= usedVar
  varName = undefined
  varType = undefined
  freeVariables = undefined
  runTyping = undefined
  -- variableStatus var = do
  --   state <- get
  --   return $
  --     case lookupVar state var of
  --       Just (Left _) -> Unrestricted
  --       Just (Right (lv, id)) ->
  --         if not (lv ^. lv_isLinear)
  --           then Unrestricted
  --           else if id + state ^. ts_hidden >= length (state ^. ts_local)
  --                  then LinearHidden
  --                  else if lv ^. lv_isUsed
  --                         then LinearUsed
  --                         else LinearFree
  --       Nothing -> Undefined
  -- saveState action = do
  --   state <- get
  --   result <- action
  --   put state
  --   return result
  -- hideLinear action = do
  --   state <- get
  --   let hidden = state ^. ts_hidden
  --   let size = V.length $ state ^. ts_local
  --   ts_hidden .= size
  --   result <- action
  --   ts_hidden .= hidden
  --   return result
  -- useVar (Global _) = return ()
  -- useVar (DeBruijn id) = do
  --   var <-
  --     preuse (ts_local . ix id) >>=
  --     maybe (throwError $ UnknownVariable $ DeBruijn id) return
  --   let usedVar = var & lv_isUsed .~ True
  --   ts_local . ix id .= usedVar
  -- varName var =
  --   case var of
  --     Global name -> return name
  --     DeBruijn id -> do
  --       state <- get
  --       case lookupVar state (DeBruijn id) of
  --         Just (Right (lv, _)) -> return $ lv ^. lv_name
  --         _ -> throwError $ UnknownVariable $ DeBruijn id
  -- varType var = do
  --   state <- get
  --   case lookupVar state var of
  --     Just (Left typ) -> return typ
  --     Just (Right (lv, _)) -> return $ lv ^. lv_typ
  --     Nothing -> throwError $ UnknownVariable var
  -- freeVariables = do
  --   (TpState _ local _) <- get
  --   let free =
  --         filter (not . (^. lv_isUsed) . snd) $
  --         V.ifoldr (\ix x l -> (ix, x) : l) [] local
  --   return $ S.fromList $ map (DeBruijn . fst) free
  -- runTyping (CTM ctm) = runExcept $ evalStateT ctm initState
  --   where
  --     initState :: TypingState
  --     initState = TpState M.empty V.empty 0

-- Utils
--   _   _ _   _ _
--  | | | | |_(_) |___
--  | |_| |  _| | (_-<
--   \___/ \__|_|_/__/
--
(>>=^) :: Monad m => m a -> (a -> m b) -> m a
x >>=^ f = x >>= \y -> f y >> return y

-- -- Typing algorithm
-- --   _____            _
-- --  |_   _|   _ _ __ (_)_ __   __ _
-- --    | || | | | '_ \| | '_ \ / _` |
-- --    | || |_| | |_) | | | | | (_| |
-- --    |_| \__, | .__/|_|_| |_|\__, |
-- --        |___/|_|            |___/
-- --      _    _                  _ _   _
-- --     / \  | | __ _  ___  _ __(_) |_| |__  _ __ ___
-- --    / _ \ | |/ _` |/ _ \| '__| | __| '_ \| '_ ` _ \
-- --   / ___ \| | (_| | (_) | |  | | |_| | | | | | | | |
-- --  /_/   \_\_|\__, |\___/|_|  |_|\__|_| |_|_| |_| |_|
-- --             |___/
typeProgram :: Program -> Either TypingError TProgram
typeProgram (Program decls) =
  runT $ do
    forM_ decls registerDecl
    return M.empty
  where
    runT :: ConcreteTypingMonad a -> Either TypingError a
    runT = runTyping

registerDecl :: TypingMonad m => Decl -> m ()
registerDecl decl =
  case decl of
    DDef def -> register (def_name def) decl
    _ -> throwError (InternalError "Enum and struct decl unimplemented")

-- compute the type of the declaration, potentatially by using other decl or defs.
declDecl :: TypingMonad m => String -> m Type
declDecl name =
  decl name $ \case
    DDef DefT {def_name, def_type, def_args, def_body} -> do
      texpr <- typeExpr def_type
      vtype <- interpret texpr
      case vtype of
        TValue (VType typ) _ -> return typ
        _ -> throwError DeclarationTypeIsNotAType
    _ -> throwError (InternalError "Enum and struct decl unimplemented")

desugarDef :: TypingMonad m => Type -> [DebugInfo String] -> Expr -> m Expr
desugarDef typ l body =
  case l of
    [] -> return body
    h:t ->
      case base typ of
        TLinArrow arg tbody -> do
          body <- desugarDef tbody t body
          return
            (Expr SourcePos $
             Lambda
               { name = h
               , linear = True
               , argtyp =
                   Expr SourcePos $ Value $ TValue (VType arg) $ Type True TType
               , body
               })
        TUnrArrow arg tbody -> do
          body <- desugarDef tbody t body
          return
            (Expr SourcePos $
             Lambda
               { name = h
               , linear = False
               , argtyp =
                   Expr SourcePos $ Value $ TValue (VType arg) $ Type True TType
               , body
               })
        TForallArrow argname arg tbody -> do
          body <- desugarDef tbody t body
          return
            (Expr SourcePos $
             ForAll
               h
               (Expr SourcePos $ Value $ TValue (VType arg) $ Type True TType)
               body)
        _ -> throwError DeclarationFunctionTypeArgumentNumberMismatch

-- compute the value of a declaration
defDecl :: TypingMonad m => String -> m TValue
defDecl name =
  def name $ \decl typ ->
    case decl of
      DDef DefT {def_name, def_type, def_args, def_body} -> do
        expr <- desugarDef typ def_args def_body
        texpr <- typeExpr expr
        interpret texpr
      _ -> throwError (InternalError "Enum and struct decl unimplemented")

typeVariable :: TypingMonad m => String -> Expr -> Variable -> m Type
typeVariable name e var = do
  state <- variableStatus var
  case state of
    LinearUsed -> throwError $ ReusedLinear name e
    LinearHidden -> throwError $ LinearUseIllegal name e
    Undefined -> throwError $ Typing.UndefinedVariable name e
    _ -> useVar var >> varType var

typeExpr :: TypingMonad m => Expr -> m TExpr
typeExpr e = do
  te@(TExpr (Type comptime typ) e') <- typeExpr' e
  if comptime
    then do
      v <- Typing.interpret te
      return $ TExpr (Type True typ) (Value v)
    else return te

typeExpr' :: TypingMonad m => Expr -> m (TExpr)
typeExpr' e@(Expr _ (LocalVar (DI name) id)) =
  TExpr <$> (typeVariable name e $ DeBruijn id) <*>
  (return $ LocalVar (DI name) id)
typeExpr' e@(Expr _ (Def def)) =
  TExpr <$> (typeVariable def e $ Global def) <*> (return (Def def))
typeExpr' (Expr _ (Value (TValue val typ))) =
  return $ TExpr typ (Value (TValue val typ))
typeExpr' (Expr _ (Let (DI name) val valtyp body)) = do
  teval@(TExpr tval _) <- typeExpr val
  addLinear name tval Nothing
  tebody@(TExpr tbody _) <- typeExpr body
  _ <- leaveScope
  case valtyp of
    Just valtyp -> do
      tvaltyp <- typeExprToType valtyp
      when (tvaltyp /= tval) $ throwError $ ExpectedType val tvaltyp tval
      return $ TExpr tbody $ Let (DI name) teval (Just tval) tebody
    _ -> return $ TExpr tbody $ Let (DI name) teval (Just tval) tebody
typeExpr' (Expr _ (IfThenElse cond thenE elseE)) = do
  tecond@(TExpr ctyp _) <- typeExpr cond
  when (ctyp /= (Type False TBool)) $ throwError $
    ExpectedType cond ctyp (Type False TBool)
  (tethen@(TExpr thenT _), thenV) <-
    saveState $ (,) <$> typeExpr thenE <*> freeVariables
  (teelse@(TExpr elseT _), elseV) <-
    saveState $ (,) <$> typeExpr elseE <*> freeVariables
  when (thenV /= elseV) $ throwError $ DifferringRessources thenE elseE
  when (thenT /= elseT) $ throwError $ ExpectedType elseE thenT elseT
  return $ TExpr thenT $ IfThenElse tecond tethen teelse
typeExpr' (Expr _ (Call funE argE)) = do
  tefun@(TExpr (Type _ tfun) _) <- typeExpr funE
  case tfun of
    TLinArrow expectedArgT bodyT -> do
      tearg@(TExpr targ _) <- typeExpr argE
      when (targ /= expectedArgT) $ throwError $
        ExpectedType argE expectedArgT targ
      return $ TExpr bodyT $ Call tefun tearg
    TUnrArrow expectedArgT bodyT -> do
      tearg@(TExpr targ _) <- hideLinear $ typeExpr argE
      when (targ /= expectedArgT) $ throwError $
        ExpectedType argE expectedArgT targ
      return $ TExpr bodyT $ Call tefun tearg
    _ -> throwError $ NotAnArrow tfun
typeExpr' (Expr _ (Tuple es)) = do
  tees <- mapM typeExpr es
  let comptime = all (\(TExpr (Type b _) _) -> b) tees
  let typs = (\(TExpr typ _) -> typ) <$> tees
  return $ TExpr (Type comptime $ TTuple typs) $ (Tuple tees)
typeExpr' (Expr _ (Lambda (DI name) linear typ body)) = do
  typ <- typeExprToType typ
  (if linear
     then addLinear
     else addUnrestricted)
    name
    typ
    Nothing
  tebody@(TExpr tbody _) <- typeExpr body
  _ <- leaveScope
  return $
    TExpr
      (Type
         True
         ((if linear
             then TLinArrow
             else TUnrArrow)
            typ
            tbody)) $
    Lambda (DI name) linear typ tebody
typeExpr' (Expr _ (ForAll (DI name) typ expr)) = do
  typ' <- typeExprToType typ
  addUnrestricted name typ' Nothing
  expr' <- typeExprToType expr
  _ <- leaveScope
  return $ TExpr (Type True TType) $ Value $
    TValue (VType $ Type True $ TForallArrow (DI name) typ' expr') $
    Type True TType
typeExpr' (Expr _ (Operator _)) =
  throwError (InternalError "typing of operators unimplemented")

typeExprToType :: TypingMonad m => Expr -> m (Type)
typeExprToType e = do
  te@(TExpr (Type _ etyp) _) <- typeExpr e
  when (etyp /= TType) $ throwError $ NotAType e
  (TValue val _) <- Typing.interpret te
  case val of
    (VType val) -> return val
    _ ->
      throwError $
      InternalError
        "interpreter didn't returned a type when given an expression of type Type"
-- checkProgramWellTyped :: Program -> Either TypingError ()
-- checkProgramWellTyped program =
--   runT $ do
--     deftypes <- forM definitions pretypeDefinition
--     let deftyped = zip definitions deftypes
--     forM_ deftyped $ uncurry typeDefinition
--   where
--     runT :: ConcreteTypingMonad () -> Either TypingError ()
--     runT = runTyping
--     definitions :: [Definition]
--     definitions = fmap snd $ M.toList $ program ^. prog_defs
-- checkExprWellTyped :: Expr -> Either TypingError Type
-- checkExprWellTyped = runT . typeExpr
--   where
--     runT :: ConcreteTypingMonad Type -> Either TypingError Type
--     runT = runTyping
-- pretypeDefinition :: TypingMonad m => Definition -> m ([Type], Type)
-- pretypeDefinition (Definition name args typ _) = do
--   args_checked <- mapM processArg args
--   checked <- processType typ
--   replicateM_ (length args) leaveScope
--   let ftype = foldr buildFunType checked args_checked
--   addGlobal name ftype
--   return (fmap snd args_checked, checked)
--   where
--     processArg :: TypingMonad m => (DebugInfo String, Arg) -> m (Bool, Type)
--     processArg (DI name, LinearArg typ) =
--       (processType typ >>=^ addLinear name) >>= return . (True, )
--     processArg (DI name, UnrestrictedArg typ) =
--       (processType typ >>=^ addUnrestricted name) >>= return . (False, )
--     buildFunType :: (Bool, Type) -> Type -> Type
--     buildFunType (True, arg) result = LinArrow arg result
--     buildFunType (False, arg) result = UnrArrow arg result
-- typeDefinition :: TypingMonad m => Definition -> ([Type], Type) -> m ()
-- typeDefinition (Definition name args _ body) (args_type, ret_type) = do
--   forM_ (zip args args_type) addArg
--   checked_type <- typeExpr body
--   when (checked_type /= ret_type) $ throwError $
--     ExpectedType body checked_type ret_type
--   replicateM_ (length args) leaveScope
--   where
--     addArg :: TypingMonad m => ((DebugInfo String, Arg), Type) -> m ()
--     addArg ((DI name, LinearArg _), typ) = addLinear name typ
--     addArg ((DI name, UnrestrictedArg _), typ) = addUnrestricted name typ
-- processType :: TypingMonad m => Expr -> m Type
-- processType (Call (Call (Builtin Plus) e1) e2) =
--   Sum <$> processType e1 <*> processType e2
-- processType (Call (Call (Builtin Product) e1) e2) =
--   Prod <$> processType e1 <*> processType e2
-- processType (Call (Call (Builtin Ampersand) e1) e2) =
--   Choice <$> processType e1 <*> processType e2
-- processType (Call (Call (Builtin LinearArrow) e1) e2) =
--   LinArrow <$> processType e1 <*> processType e2
-- processType (Call (Call (Builtin UnrestrictedArrow) e1) e2) =
--   UnrArrow <$> processType e1 <*> processType e2
-- processType (Const IntType) = return Int
-- processType (Const BoolType) = return Bool
-- processType expr = throwError $ NotAType expr
-- typeExpr :: TypingMonad m => Expr -> m Type
-- typeExpr e@(LocalVar (DI name) id) = typeVariable name e $ DeBruijn id
-- typeExpr e@(Def def) = typeVariable def e $ Global def
-- typeExpr e@(Builtin b) =
--   return $
--   case b of
--     Product -> uarrow
--     Plus -> uarrow
--     Ampersand -> uarrow
--     LinearArrow -> uarrow
--     UnrestrictedArrow -> uarrow
--   where
--     uarrow :: Type
--     uarrow = UnrArrow Universe (UnrArrow Universe Universe)
-- typeExpr e@(Const ct) =
--   return $
--   case ct of
--     IntConst _ -> Int
--     BoolConst _ -> Bool
--     IntType -> Universe
--     BoolType -> Universe
-- typeExpr e@(Assign (DI name) val body) = do
--   vtyp <- typeExpr val
--   addLinear name vtyp
--   rtyp <- typeExpr body
--   leaveScope
--   return rtyp
-- typeExpr e@(IfThenElse cond thenE elseE) = do
--   ctyp <- typeExpr cond
--   when (ctyp /= Bool) $ throwError $ ExpectedType cond ctyp Bool
--   (thenT, thenV) <- saveState $ (,) <$> typeExpr thenE <*> freeVariables
--   (elseT, elseV) <- saveState $ (,) <$> typeExpr elseE <*> freeVariables
--   when (thenV /= elseV) $ throwError $ DifferringRessources thenE elseE
--   when (thenT /= elseT) $ throwError $ ExpectedType elseE thenT elseT
--   return thenT
-- typeExpr e@(Call funE argE) = do
--   funT <- typeExpr funE
--   case funT of
--     LinArrow expectedArgT bodyT -> do
--       argT <- typeExpr argE
--       when (argT /= expectedArgT) $ throwError $
--         ExpectedType argE expectedArgT argT
--       return bodyT
--     UnrArrow expectedArgT bodyT -> do
--       argT <- hideLinear $ typeExpr argE
--       when (argT /= expectedArgT) $ throwError $
--         ExpectedType argE expectedArgT argT
--       return bodyT
-- typeExpr e@(Lambda (DI name) var body) = do
--   argT <-
--     case var of
--       LinearArg typ -> processType typ >>=^ addLinear name
--       UnrestrictedArg typ -> processType typ >>=^ addUnrestricted name
--   bodyT <- typeExpr body
--   leaveScope
--   return $
--     case var of
--       LinearArg _ -> LinArrow argT bodyT
--       UnrestrictedArg _ -> UnrArrow argT bodyT
-- typeExpr Type = return Universe
-- typeVariable :: TypingMonad m => String -> Expr -> Variable -> m Type
-- typeVariable name e var = do
--   state <- variableStatus var
--   case state of
--     LinearUsed -> throwError $ ReusedLinear name e
--     LinearHidden -> throwError $ LinearUseIllegal name e
--     Undefined -> throwError $ UndefinedVariable name e
--     _ -> use var >> varType var
-- -- >>> checkExprWellTyped $ Call (Lambda (DI "x") (LinearArg $ IR.Const BoolType) (IfThenElse (LocalVar (DI "x") 0) (IR.Const (BoolConst False)) (IR.Const (BoolConst True)))) (IR.Const (BoolConst True))
-- -- Right Bool
-- -- >>> checkExprWellTyped $ Call (Lambda (DI "x") (LinearArg $ IR.Const BoolType) (IfThenElse (LocalVar (DI "x") 0) (IR.Const (BoolConst False)) (LocalVar (DI "x") 0))) (IR.Const (BoolConst True))
-- -- Left (ReusedLinear "x" (LocalVar "x" 0))
-- -- >>> checkExprWellTyped $ (Lambda (DI "x") (LinearArg $ IR.Const BoolType) (IfThenElse (LocalVar (DI "x") 0) (IR.Const (BoolConst False)) (IR.Const (BoolConst True))))
-- -- Right (LinArrow Bool Bool)
-- -- >>> checkExprWellTyped $ Call (Lambda (DI "x") (LinearArg $ IR.Const BoolType) (IfThenElse (IR.Const $ BoolConst True) (IR.Const (BoolConst False)) (LocalVar (DI "x") 0))) (IR.Const (BoolConst True))
-- -- Left (DifferringRessources (Const (BoolConst False)) (LocalVar "x" 0))
-- -- >>> checkExprWellTyped $ Call (Lambda (DI "x") (LinearArg $ IR.Const BoolType) (IfThenElse (IR.Const $ BoolConst True) (LocalVar (DI "x") 0) (LocalVar (DI "x") 0))) (IR.Const (BoolConst True))
-- -- Right Bool
