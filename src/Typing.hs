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

-- import Interpreter hiding (UndefinedVariable, interpret)
import qualified Interpreter

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
  = Undeclared
  | Registered Decl
  | DeclInProgress
  | Declared Type Decl
  | DefInProgress Type
  | Defined TValue
  deriving (Eq, Ord, Show)

-- Abstract Typing Monad definition
class MonadError Error m =>
      TypingMonad m
  where
  register :: String -> Decl -> m ()
  decl :: String -> (Decl -> m Type) -> m Type
  -- ^ Declare a global.  While the internal computation is in
  -- progress, The variable is marked as `DeclInProgress`.  If the
  -- variable has already been declared, does nothing. In case the
  -- declaration fails, the variable will still be marked as
  -- registered.
  def :: String -> (Decl -> Type -> m TValue) -> m TValue
  -- ^ Define a global. While the internal computation is in progress,
  -- the variable is marked as `DefInProgress`. If the variable has
  -- already been defined, does nothing. If the definition fails, the
  -- variable will still be marked as declared.
  addLinear :: String -> Type -> Maybe Value -> m ()
  addUnrestricted :: String -> Type -> Maybe Value -> m ()
  leaveScope :: m String -- Name of variable which scope we're leaving
  variableStatus :: Variable -> m VarStatus
  globalStatus :: String -> m GlobalStatus
  -- State manipulation
  saveState :: m a -> m a
  hideLinear :: m a -> m a
  useVar :: Variable -> m ()
  varName :: Variable -> m String
  varType :: Variable -> m Type
  getValue :: Variable -> m (Maybe Value)
  freeVariables :: m (Set Variable)
  interpret :: TExpr -> m TValue
  runTyping :: m a -> Either Error a

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

emptyLV :: LocalVariable
emptyLV = LVar undefined undefined undefined undefined undefined

makeLenses ''LocalVariable

data TypingState =
  TpState
    { _ts_global :: Map String GlobalStatus
    , _ts_local :: Vector LocalVariable
    , _ts_hidden :: Int
    }
  deriving (Eq, Show)

makeLenses ''TypingState

type ConcreteTypingMonad_ a = StateT TypingState (ExceptT Error Identity) a

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

instance MonadError Error ConcreteTypingMonad where
  throwError = CTM . throwError
  catchError (CTM def) handler = CTM $ catchError def $ unCTM . handler

instance MonadState TypingState ConcreteTypingMonad where
  get = CTM get
  put = CTM . put

-- <<<<<<< HEAD
-- lookupVar :: TypingState -> Variable -> Maybe (Either Type (LocalVariable, Int))
-- lookupVar (TpState globals _ _) (Global name) =
-- --   M.lookup name globals >>= return . Left
-- -- lookupVar (TpState _ locals _) (DeBruijn id) =
-- --   locals V.!? id >>= return . Right . (, id)
-- instance TypingMonad ConcreteTypingMonad where
--   register name decl = ts_global %= M.insert name (Undeclared decl)
--   --
--   decl = undefined
--   -- decl name action = do
--   --   globals <- use ts_global
--   --   case M.lookup name globals of
--   --     Just (Undeclared decl) -> do
--   --       ts_global %= M.insert name DeclInProgress
--   --       typ <- action decl
--   --       ts_global %= M.insert name (Declared typ)
--   --       return typ
--   --     Just (Declared typ _) -> typ
--   --     Just (Defined (TValue typ value)) -> typ
--   --     Just a -> throwError TypingCycle
--   --     Nothing ->
--   --       throwError (InternalError "Trying to declare an unregistred variable")
--     --
--   def = undefined
--   -- def name action = do
--   --   globals <- use ts_global
--   --   typ <-
--   --     case M.lookup name globals of
--   --       Just (Declared typ) -> return typ
--   --       Just a ->
--   --         throwError
--   --           (InternalError
--   --              ("Defining var" ++ name ++ "but it is in state" ++ show a))
--   --       Nothing ->
--   --         throwError (InternalError "Trying to declare an unregistred variable")
--   --   return ()
--     --
--   addLinear name typ mvalue = ts_local %= cons (LVar name typ True False mvalue)
--     --
--   addUnrestricted name typ mvalue =
--     ts_local %= cons (LVar name typ False False mvalue)
--   --
-- =======
collapseGStatus :: Maybe GlobalStatus -> GlobalStatus
collapseGStatus Nothing = Undeclared
collapseGStatus (Just s) = s

lookupVar ::
     TypingState
  -> Variable
  -> (Either GlobalStatus (Maybe (LocalVariable, Int)))
lookupVar (TpState globals _ _) (Global name) =
  Left $ collapseGStatus $ M.lookup name globals
lookupVar (TpState _ locals _) (DeBruijn id) = Right $ (, id) <$> locals V.!? id

lookupVarE ::
     Variable -> ConcreteTypingMonad (Either GlobalStatus (LocalVariable, Int))
lookupVarE var = do
  state <- get
  let result = Typing.lookupVar state var
  case result of
    Left Undeclared -> throwError $ UnknownVariable var
    Left gs -> return $ Left gs
    Right Nothing -> throwError $ UnknownVariable var
    Right (Just lv) -> return $ Right lv

addLV :: Bool -> String -> Type -> Maybe Value -> ConcreteTypingMonad ()
addLV linear name typ mval =
  ts_local %=
  V.cons
    (emptyLV & lv_name .~ name & lv_typ .~ typ & lv_isLinear .~ linear &
     lv_isUsed .~
     False &
     lv_compTime .~
     mval)

makeGlobalContext :: TypingState -> Interpreter.GlobalContext
makeGlobalContext (TpState globals locals _) =
  M.foldrWithKey addGlobal (V.ifoldr addLocal emptyGC locals) globals
  where
    addGlobal ::
         String
      -> GlobalStatus
      -> Interpreter.GlobalContext
      -> Interpreter.GlobalContext
    addGlobal name (Defined value) (Interpreter.GlobalContext globals locals) =
      Interpreter.GlobalContext (M.insert name value globals) locals
    addGlobal _ _ gc = gc
    addLocal ::
         Int
      -> LocalVariable
      -> Interpreter.GlobalContext
      -> Interpreter.GlobalContext
    addLocal id lv gc@(Interpreter.GlobalContext globals locals) =
      case lv ^. lv_compTime of
        Nothing -> gc
        Just val ->
          Interpreter.GlobalContext globals $
          M.insert id (TValue val $ lv ^. lv_typ) locals
    emptyGC :: Interpreter.GlobalContext
    emptyGC = Interpreter.GlobalContext M.empty M.empty

instance TypingMonad ConcreteTypingMonad where
  register name decl = ts_global %= M.insert name (Registered decl)
  decl name declare = do
    globals <- use ts_global
    case collapseGStatus $ M.lookup name globals of
      Undeclared ->
        throwError $ InternalError $ "Trying to declare unregistered \"" ++ name ++
        "\""
      DeclInProgress ->
        throwError $ InternalError $ "Trying to declare \"" ++ name ++
        "\" while it is already being declared"
      Registered d -> do
        ts_global %= M.insert name DeclInProgress
        typ <- declare d
        catchError
          (ts_global %= M.insert name (Declared typ d))
          (\e -> ts_global %= M.insert name (Registered d) >> throwError e)
        return typ
      Declared typ _ -> return typ
      DefInProgress typ -> return typ
      Defined (TValue _ typ) -> return typ
  def name define = do
    globals <- use ts_global
    case collapseGStatus $ M.lookup name globals of
      Undeclared ->
        throwError $ InternalError $ "Trying to define unregistered \"" ++ name ++
        "\""
      Registered _ ->
        throwError $ InternalError $ "Trying to define undeclared \"" ++ name ++
        "\""
      DeclInProgress ->
        throwError $ InternalError $ "Cannot define \"" ++ name ++
        "\" while it is being declared"
      DefInProgress _ ->
        throwError $ InternalError $ "Cannot define \"" ++ name ++
        "\" while it is already being defined"
      Declared typ d -> do
        ts_global %= M.insert name (DefInProgress typ)
        value <- define d typ
        catchError
          (ts_global %= M.insert name (Defined value))
          (\e -> ts_global %= M.insert name (Declared typ d) >> throwError e)
        return value
      Defined val -> return val
  addLinear = addLV True
  addUnrestricted = addLV False
  leaveScope = do
    locals <- use ts_local
    when (V.null locals) $ throwError LastScope
    ts_local %= V.tail
    return $ (V.head locals) ^. lv_name
  variableStatus var = do
    state <- get
    return $
      case Typing.lookupVar state var of
        Left Undeclared -> Undefined
        Left _ -> Unrestricted
        Right Nothing -> Undefined
        Right (Just (lv, id)) ->
          if not (lv ^. lv_isLinear)
            then Unrestricted
            else if id + state ^. ts_hidden >= length (state ^. ts_local)
                   then LinearHidden
                   else if lv ^. lv_isUsed
                          then LinearUsed
                          else LinearFree
  globalStatus name = do
    state <- get
    case Typing.lookupVar state (Global name) of
      Left gs -> return gs
      Right _ -> throwError $ InternalError "Reached impossible to reach branch"
  -- State manipulation
  saveState action = do
    state <- get
    result <- action
    put state
    return result
  hideLinear action = do
    state <- get
    let hidden = state ^. ts_hidden
    let size = V.length $ state ^. ts_local
    ts_hidden .= size
    result <- action
    ts_hidden .= hidden
    return result
  -- The following three might throw UnknownVariable
  useVar (Global _) = return ()
  useVar (DeBruijn id) = do
    var <-
      preuse (ts_local . ix id) >>=
      maybe (throwError $ UnknownVariable $ DeBruijn id) return
    ts_local . ix id .= (var & lv_isUsed .~ True)
  varName var =
    case var of
      Global name -> return name
      DeBruijn id ->
        lookupVarE var >>= \case
          Right (lv, _) -> return $ lv ^. lv_name
          _ -> throwError $ InternalError "Reached impossible to reach branch"
  varType var =
    lookupVarE var >>= \case
      Left (Declared typ _) -> return typ
      Left (DefInProgress typ) -> return typ
      Left (Defined (TValue _ typ)) -> return typ
      Left _ -> throwError $ NotYetTyped var
      Right (lv, _) -> return $ lv ^. lv_typ
  getValue var =
    lookupVarE var >>= \case
      Left (Defined (TValue value _)) -> return $ Just value
      Right (lv, _) -> return $ lv ^. lv_compTime
      _ -> return Nothing
  freeVariables = do
    local <- use ts_local
    let free =
          filter (not . (^. lv_isUsed) . snd) $
          V.ifoldr (\ix x l -> (ix, x) : l) [] local
    return $ S.fromList $ map (DeBruijn . fst) free
  interpret expr = do
    state <- get
    let result = Interpreter.interpret (makeGlobalContext state) expr
    case result of
      Left runErr -> throwError $ runErr
      Right tval -> return tval
  runTyping (CTM ctm) = runExcept $ evalStateT ctm initState
    where
      initState :: TypingState
      initState = TpState M.empty V.empty 0

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
typeProgram :: Program -> Either Error TProgram
typeProgram (Program decls) =
  runT $ do
    forM_ decls registerDecl
    state <- get
    return $ Interpreter.globals $ makeGlobalContext state
  where
    runT :: ConcreteTypingMonad a -> Either Error a
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
    Undefined -> throwError $ UndefinedVariable name e
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
typeExpr' (Expr _ (Call (Expr _ (Operator Plus)) arg)) = typeCallOp Plus arg
typeExpr' (Expr _ (Call (Expr _ (Operator Minus)) arg)) = typeCallOp Minus arg
typeExpr' (Expr _ (Call (Expr _ (Operator Times)) arg)) = typeCallOp Times arg
typeExpr' (Expr _ (Call (Expr _ (Operator Div)) arg)) = typeCallOp Div arg
typeExpr' (Expr _ (Call funE argE)) = do
  tefun@(TExpr (Type ctfun tfun) _) <- typeExpr funE
  case tfun of
    TLinArrow expectedArgT (Type ctbody tbody) -> do
      tearg@(TExpr targ _) <- typeExpr argE
      when (targ /= expectedArgT) $ throwError $
        ExpectedType argE expectedArgT targ
      return $ TExpr (Type (ctbody && ctfun) tbody) $ Call tefun tearg
    TUnrArrow expectedArgT (Type ctbody tbody) -> do
      tearg@(TExpr targ _) <- hideLinear $ typeExpr argE
      when (targ /= expectedArgT) $ throwError $
        ExpectedType argE expectedArgT targ
      return $ TExpr (Type (ctbody && ctfun) tbody) $ Call tefun tearg
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
typeExpr' (Expr _ (Operator Arrow)) =
  let ttype = Type True TType
      arrowtype = TUnrArrow ttype $ Type True $ (TUnrArrow ttype ttype)
   in return $ TExpr (Type True arrowtype) (Operator Arrow)
typeExpr' (Expr _ (Operator LinArrow)) =
  let ttype = Type True TType
      arrowtype = TUnrArrow ttype $ Type True $ (TUnrArrow ttype ttype)
   in return $ TExpr (Type True arrowtype) (Operator LinArrow)
typeExpr' (Expr _ (Operator op)) =
  throwError (InternalError $ "Operator" ++ show op ++ "implemented")

typeCallOp :: TypingMonad m => Operator -> Expr -> m TExpr
typeCallOp op arg = do
  tearg@(TExpr targ@(Type ctarg tbarg) varg) <- typeExpr arg
  case op of
    _
      | op == Plus || op == Minus || op == Times || op == Div ->
        case tbarg of
          TInt intType ->
            let funType = binopType True (TInt intType)
             in return $ TExpr (typeAfterCall ctarg funType) $
                Call (TExpr funType (Operator Plus)) tearg
          _ -> throwError (ExpectedIntType arg targ)

binopType :: Bool -> TypeBase -> Type
binopType comptime typ =
  Type
    comptime
    (TLinArrow
       (Type False typ)
       (Type True $ TLinArrow (Type False typ) (Type False typ)))

typeAfterCall :: Bool -> Type -> Type
typeAfterCall comptime (Type comptimeFun (TLinArrow arg (Type _ body))) =
  Type (comptime && comptimeFun) body

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
