{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Typing where

import Control.Lens hiding (Const(..))
import Control.Lens.Indexed
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.HT (nest)
import Control.Monad.IO.Class
import Control.Monad.State hiding (get, put, state)
import Control.Monad.State.Class
import Data.Foldable
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Error
import IR
import ULPrelude (intt, prelude)

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
data MetaData =
  MtDt
    { _mtdt_comptime :: Bool
    , _mtdt_interp :: Int
    }
  deriving (Eq, Show, Ord)

makeLenses ''MetaData

instance Semigroup MetaData where
  (MtDt comp1 interp1) <> (MtDt comp2 interp2) =
    MtDt (comp1 && comp2) (max interp1 interp2)

defMtdt = MtDt True (-1)

data MetaType =
  MtType
    { _mtype_data :: MetaData
    , _mtype_type :: Type
    }
  deriving (Eq, Show, Ord)

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
  leaveScope :: m VarStatus
  -- ^ Throws UnusedLinear if variable is LinearFree, otherwise
  -- returns status of variable which scope we're leaving.
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
  getTProgram :: m TProgram
  loadTProgram :: TProgram -> m ()

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

type ConcreteTypingMonad_ m a = StateT TypingState (ExceptT Error m) a

newtype ConcreteTypingMonadT m a =
  CTM
    { unCTM :: ConcreteTypingMonad_ m a
    }

instance Functor f => Functor (ConcreteTypingMonadT f) where
  fmap f (CTM x) = CTM $ f <$> x

instance Monad m => Applicative (ConcreteTypingMonadT m) where
  pure = CTM . pure
  (CTM f) <*> (CTM x) = CTM $ f <*> x

instance Monad m => Monad (ConcreteTypingMonadT m) where
  (CTM x) >>= f = CTM $ x >>= (unCTM . f)

instance Monad m => MonadError Error (ConcreteTypingMonadT m) where
  throwError = CTM . throwError
  catchError (CTM def) handler = CTM $ catchError def $ unCTM . handler

instance Monad m => MonadState TypingState (ConcreteTypingMonadT m) where
  get = CTM get
  put = CTM . put

instance MonadTrans ConcreteTypingMonadT where
  lift = CTM . lift . lift

instance MonadIO m => MonadIO (ConcreteTypingMonadT m) where
  liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (ConcreteTypingMonadT m) where
  throwM = CTM . throwM

instance MonadCatch m => MonadCatch (ConcreteTypingMonadT m) where
  catch (CTM x) handler = CTM $ catch x $ unCTM . handler

instance MonadMask m => MonadMask (ConcreteTypingMonadT m) where
  mask f = CTM $ mask $ \g -> unCTM $ f (CTM . g . unCTM)
  uninterruptibleMask f =
    CTM $ uninterruptibleMask $ \g -> unCTM $ f (CTM . g . unCTM)
  generalBracket (CTM x) release inner =
    CTM $ generalBracket x (\a -> unCTM . release a) (unCTM . inner)

collapseGStatus :: Maybe GlobalStatus -> GlobalStatus
collapseGStatus Nothing = Undeclared
collapseGStatus (Just s) = s

interpretAndDef :: TypingMonad m => TExpr -> m TValue
interpretAndDef te@(TExpr _ e) = do
  forM_ (getDefsInTExpr e) defDecl
  interpret te

lookupVar ::
     TypingState
  -> Variable
  -> (Either GlobalStatus (Maybe (LocalVariable, Int)))
lookupVar (TpState globals _ _) (Global name) =
  Left $ collapseGStatus $ M.lookup name globals
lookupVar (TpState _ locals _) (DeBruijn id) = Right $ (, id) <$> locals V.!? id

lookupVarE ::
     Monad m
  => Variable
  -> ConcreteTypingMonadT m (Either GlobalStatus (LocalVariable, Int))
lookupVarE var = do
  state <- get
  let result = Typing.lookupVar state var
  case result of
    Left Undeclared -> throwError $ UnknownVariable var
    Left gs -> return $ Left gs
    Right Nothing -> throwError $ UnknownVariable var
    Right (Just lv) -> return $ Right lv

addLV ::
     Monad m
  => Bool
  -> String
  -> Type
  -> Maybe Value
  -> ConcreteTypingMonadT m ()
addLV linear name typ mval =
  ts_local %=
  V.cons
    (emptyLV & lv_name .~ name & lv_typ .~ typ & lv_isLinear .~ linear &
     lv_isUsed .~
     False &
     lv_compTime .~
     mval)

makeTProgram :: TypingState -> TProgram
makeTProgram (TpState globals _ _) = M.foldrWithKey addGlobal M.empty globals
  where
    addGlobal :: String -> GlobalStatus -> TProgram -> TProgram
    addGlobal name (Defined tvalue) g = M.insert name tvalue g
    addGlobal _ _ gc = gc

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

instance Monad m => TypingMonad (ConcreteTypingMonadT m) where
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
    status <- variableStatus $ DeBruijn 0
    locals <- use ts_local
    when (V.null locals) $ throwError LastScope
    ts_local %= V.tail
    let lv = V.head locals
    when (status == LinearFree) $ throwError $ UnusedLinear (lv ^. lv_name)
    return status
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
  getTProgram = makeTProgram <$> get
  loadTProgram tprogram = do
    globals <- use ts_global
    ts_global .= M.foldrWithKey addGlobal globals tprogram
    return ()
    where
      addGlobal name tvalue = M.insert name (Defined tvalue)

type ConcreteTypingMonad = ConcreteTypingMonadT Identity

runTypingT :: Monad m => ConcreteTypingMonadT m a -> m (Either Error a)
runTypingT (CTM action) = runExceptT $ evalStateT action initState
  where
    initState :: TypingState
    initState = TpState M.empty V.empty 0

runTyping :: ConcreteTypingMonad a -> Either Error a
runTyping = runIdentity . runTypingT

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
    loadTProgram prelude
    forM_ decls registerDecl
    forM_ names declDecl
    forM_ names defDecl
    getTProgram
  where
    runT :: ConcreteTypingMonad a -> Either Error a
    runT = runTyping
    names :: [String]
    names = M.keys decls

registerDecl :: TypingMonad m => Decl -> m ()
registerDecl decl =
  case decl of
    DDef def -> register (def_name def) decl
    DEnum name _ _ -> register name decl
    DConstr name _ -> register name decl
    _ -> throwError (InternalError "struct register unimplemented")

checkConstrType :: TypingMonad m => Type -> m Type
checkConstrType = return . id

-- compute the type of the declaration, potentially by using other decl or defs.
declDecl :: TypingMonad m => String -> m Type
declDecl name =
  decl name $ \case
    DDef DefT {def_name, def_type, def_args, def_body} ->
      typeExprToType def_type
    DEnum e_name e_args e_constructor -> getTypeFromEnum e_args
    DConstr name typ -> do
      nt <- typeExprToType typ
      checkConstrType nt
      return nt
    _ -> throwError (InternalError "struct decl unimplemented")

desugarDef :: TypingMonad m => Type -> [DebugInfo String] -> Expr -> m Expr
desugarDef typ l body =
  case l of
    [] -> return body
    h:t ->
      case typ of
        TLinArrow arg tbody -> do
          body <- desugarDef tbody t body
          return
            (Expr SourcePos $
             Lambda
               { name = h
               , linear = True
               , argtyp = Expr SourcePos $ Value $ TValue (VType arg) TType
               , body
               })
        TUnrArrow arg tbody -> do
          body <- desugarDef tbody t body
          return
            (Expr SourcePos $
             Lambda
               { name = h
               , linear = False
               , argtyp = Expr SourcePos $ Value $ TValue (VType arg) TType
               , body
               })
        TForallArrow argname arg tbody -> do
          body <- desugarDef tbody t body
          return
            (Expr SourcePos $
             ForAll h (Expr SourcePos $ Value $ TValue (VType arg) TType) body)
        _ -> throwError DeclarationFunctionTypeArgumentNumberMismatch

countArguments :: Type -> Int
countArguments (TLinArrow _ t) = 1 + countArguments t
countArguments (TUnrArrow _ t) = 1 + countArguments t
countArguments (TForallArrow _ _ t) = 1 + countArguments t
countArguments _ = 0

-- compute the value of a declaration
defDecl :: TypingMonad m => String -> m TValue
defDecl name =
  def name $ \decl typ ->
    case decl of
      DDef DefT {def_name, def_type, def_args, def_body} -> do
        expr <- desugarDef typ def_args def_body
        texpr@(TExpr found_typ _) <- fst <$> typeExprAndEval expr
        expectType typ def_body found_typ
        interpretAndDef texpr
      DEnum enum_name enum_args constructors ->
        let n = length enum_args
         in if n == 0
              then return $ TValue (VType $ TSum [] constructors) TType
              else return $ flip TValue typ $
                   VForall [] (length enum_args) TType $
                   Expr SourcePos $
                   Value $
                   TValue (VType $ TSum [] constructors) TType
      DConstr constr_name constr_type ->
        let n = countArguments typ
         in if n == 0
              then return $ flip TValue typ $ VEnum constr_name [] []
              else return $ flip TValue typ $ VFun [] (countArguments typ) $
                   TExpr typ $
                   Constructor constr_name
      _ -> throwError (InternalError "Struct decl unimplemented")

typeVariable :: TypingMonad m => String -> Expr -> Variable -> m Type
typeVariable name e var = do
  state <- variableStatus var
  case state of
    LinearUsed -> throwError $ ReusedLinear name e
    LinearHidden -> throwError $ LinearUseIllegal name e
    Undefined -> throwError $ UndefinedVariable name e
    _ -> useVar var >> varType var

expectType :: TypingMonad m => Type -> Expr -> Type -> m ()
expectType expected_type expr actual_type =
  when (expected_type /= actual_type) $ throwError $
  ExpectedType expr expected_type actual_type

mergeType :: TypingMonad m => Type -> Type -> m Type
mergeType t t' = do
  when (t /= t') $ throwError $ IncompatibleTypes t t'
  return t

extractVal :: TValue -> Value
extractVal (TValue val _) = val

typeExpr :: TypingMonad m => Expr -> m (TExpr, MetaData)
typeExpr e@(Expr _ (LocalVar (DI name) id)) =
  (, defMtdt & mtdt_interp .~ id) <$> texpr
  where
    texpr =
      TExpr <$> (typeVariable name e $ DeBruijn id) <*>
      (return $ LocalVar (DI name) id)
typeExpr e@(Expr _ (Def def)) = do
  declDecl def
  (, defMtdt) <$> texpr
  where
    texpr = TExpr <$> (typeVariable def e $ Global def) <*> (return (Def def))
typeExpr (Expr _ (Value (TValue val typ))) =
  return $ (TExpr typ (Value (TValue val typ)), defMtdt)
typeExpr (Expr _ (Let (DI name) var vartyp body)) = do
  (tevar@(TExpr tvar _), mtdt) <- typeExprAndEval var
  typ <-
    case vartyp of
      Just vartyp -> do
        tvartyp <- typeExprToType vartyp
        expectType tvartyp var tvar
        return tvartyp
      _ -> return tvar
  vval <-
    if mtdt ^. mtdt_interp < 0
      then Just <$> extractVal <$> interpretAndDef tevar
      else return Nothing
  (tebody@(TExpr tbody _), body_mtdt) <- typeExpr body
  leaveScope
  return $
    ( TExpr tbody $ Let (DI name) tevar (Just typ) tebody
    , body_mtdt & mtdt_interp %~ (\x -> x - 1))
typeExpr (Expr _ (IfThenElse cond thenE elseE)) = do
  (tecond@(TExpr ctyp _), cond_mtdt) <- typeExpr cond
  expectType TBool cond ctyp
  ((tethen@(TExpr then_type _), then_mtdt), then_free_vars) <-
    saveState $ (,) <$> typeExpr thenE <*> freeVariables
  ((teelse@(TExpr else_type _), else_mtdt), else_free_vars) <-
    (,) <$> typeExpr elseE <*> freeVariables
  when (then_free_vars /= else_free_vars) $ throwError $
    DifferringRessources thenE elseE
  expectType then_type elseE else_type
  return $
    ( TExpr then_type $ IfThenElse tecond tethen teelse
    , cond_mtdt <> then_mtdt <> else_mtdt)
typeExpr (Expr _ (Call (Expr _ (Operator Plus)) arg)) = typeCallOp Plus arg
typeExpr (Expr _ (Call (Expr _ (Operator Minus)) arg)) = typeCallOp Minus arg
typeExpr (Expr _ (Call (Expr _ (Operator Times)) arg)) = typeCallOp Times arg
typeExpr (Expr _ (Call (Expr _ (Operator Div)) arg)) = typeCallOp Div arg
typeExpr (Expr _ (Call (Expr _ (Operator Eq)) arg)) = typeCallOp Eq arg
typeExpr (Expr _ (Call (Expr _ (Operator Neq)) arg)) = typeCallOp Neq arg
typeExpr (Expr _ (Call (Expr _ (Operator Gteq)) arg)) = typeCallOp Gteq arg
typeExpr (Expr _ (Call (Expr _ (Operator Gt)) arg)) = typeCallOp Gt arg
typeExpr (Expr _ (Call (Expr _ (Operator Lt)) arg)) = typeCallOp Lt arg
typeExpr (Expr _ (Call (Expr _ (Operator Lteq)) arg)) = typeCallOp Lteq arg
typeExpr (Expr _ (Call (Expr _ (Operator Array)) arg)) = typeCallOp Array arg
typeExpr (Expr _ (Call (Expr _ (Operator Ampersand)) arg)) =
  typeCallOp Ampersand arg
typeExpr (Expr _ (Call (Expr _ (Operator Hat)) arg)) = typeCallOp Hat arg
typeExpr (Expr _ (Call (Expr _ (Call (Expr _ (Operator Index)) index)) obj)) = do
  (teobj@(TExpr tobj vobj), mtdt) <- typeExpr obj
  case tobj of
    ttuple@(TTuple typs) -> do
      let n = length typs
      val_index <- typeExprAndInterpret index
      case val_index of
        TValue (VInt i) _ ->
          if fromInteger i < n
            then do
              let typ = typs !! fromInteger i
              let partial_index =
                    TExpr (TLinArrow ttuple typ) $ Value $
                    TValue (VOperator [VInt i] 1 Index) (TLinArrow ttuple typ)
              return $ (, mtdt) $ TExpr typ $ Call partial_index teobj
            else throwError $ IndexingError "Out of bound tuple indexing"
        _ -> throwError $ IndexingError "Trying to index a tuple with a non-int"
    tarr@(TArray typ num) -> do
      (teind@(TExpr tind vind), mtdtind) <- typeExpr index
      let op = TExpr (TLinArrow intt (TLinArrow tarr typ)) $ Operator Index
      let partial_index = TExpr (TLinArrow tarr typ) (Call op teind)
      return $ (, mtdt <> mtdtind) $ TExpr typ (Call partial_index teobj)
    t -> throwError $ IndexingError ("Can't index in type " ++ show t)
typeExpr (Expr _ (Call funE argE)) = do
  (tefun@(TExpr tfun _), fun_mtdt) <- typeExpr funE
  let called_mtdt = fun_mtdt & mtdt_interp %~ (\x -> x - 1)
  case tfun of
    TLinArrow expectedArgT tbody -> do
      (tearg@(TExpr targ _), arg_mtdt) <- typeExpr argE
      expectType expectedArgT argE targ
      return $ (TExpr tbody $ Call tefun tearg, called_mtdt <> arg_mtdt)
    TUnrArrow expectedArgT tbody -> do
      (tearg@(TExpr targ _), arg_mtdt) <- hideLinear $ typeExpr argE
      expectType expectedArgT argE targ
      return $ (TExpr tbody $ Call tefun tearg, called_mtdt <> arg_mtdt)
    _ -> throwError $ NotAnArrow tfun
typeExpr (Expr _ (Tuple es)) = do
  tees <- mapM typeExpr es
  let typs = fmap (\(TExpr typ _, _) -> typ) tees
  let mtdt = foldr (<>) defMtdt $ fmap snd tees
  return $ (, mtdt) $ TExpr (TTuple typs) $ Tuple $ fmap fst tees
typeExpr (Expr _ (Lambda (DI name) linear typ body)) = do
  typ <- typeExprToType typ
  (if linear
     then addLinear
     else addUnrestricted)
    name
    typ
    Nothing
  (tebody@(TExpr tbody _), body_mtdt) <- typeExpr body
  _ <- leaveScope
  let result_mtdt = body_mtdt & mtdt_interp %~ (\x -> x - 1)
  return $ (, result_mtdt) $
    TExpr
      ((if linear
          then TLinArrow
          else TUnrArrow)
         typ
         tbody) $
    Lambda (DI name) linear typ tebody
typeExpr (Expr _ (ForAll (DI name) typ expr)) = do
  typ' <- typeExprToType typ
  addUnrestricted name typ' Nothing
  expr' <- typeExprToType expr
  _ <- leaveScope
  return $ (, defMtdt) $ TExpr TType $ Value $
    TValue (VType $ TForallArrow (DI name) typ' expr') TType
typeExpr (Expr _ (Operator Arrow)) =
  let arrowtype = TUnrArrow TType $ TUnrArrow TType TType
   in return $ (, defMtdt) $ TExpr arrowtype $ Operator Arrow
typeExpr (Expr _ (Operator LinArrow)) =
  let arrowtype = TUnrArrow TType $ TUnrArrow TType TType
   in return $ (, defMtdt) $ TExpr arrowtype (Operator LinArrow)
typeExpr (Expr _ (Operator op)) =
  throwError (InternalError $ "Operator " ++ show op ++ " unimplemented")

typeCallOp :: TypingMonad m => Operator -> Expr -> m (TExpr, MetaData)
typeCallOp op arg = do
  (tearg@(TExpr targ _), mtdt) <- typeExpr arg
  case op of
    _
      | op == Plus || op == Minus || op == Times || op == Div ->
        case targ of
          TInt intType ->
            let funType = binopType (TInt intType)
             in return $ (, mtdt) $ TExpr (typeAfterCall funType) $
                Call (TExpr funType (Operator op)) tearg
          _ -> throwError (ExpectedIntType arg targ)
    _
      | op == Eq || op == Neq ->
        return $ (, mtdt) $ TExpr typafter $
        Call (TExpr funType (Operator op)) tearg
      where funType = compType targ
            typafter = typeAfterCall funType
    _
      | op == Gt || op == Lt || op == Gteq || op == Lteq ->
        case targ of
          TInt intType ->
            let funType = compType (TInt intType)
             in return $ (, mtdt) $ TExpr (typeAfterCall funType) $
                Call (TExpr funType (Operator op)) tearg
          _ -> throwError (ExpectedIntType arg targ)
    Array ->
      case targ of
        TTuple (h:l) -> do
          forM_ l asserth
          return $ (, mtdt) $
            TExpr array_type (Call (TExpr fun_type $ Operator Array) tearg)
          where n = 1 + length l
                asserth t = when (t /= h) $ throwError $ ArrayNotSameType h t
                array_type = TArray h n
                fun_type = TLinArrow targ array_type
        _ ->
          throwError (InternalError "Array operator was applied to a non tuple")
    Ampersand ->
      case targ of
        TTuple typs -> do
          forM_ typs asserttyp
          return $ (, mtdt) $ TExpr TType $
            Call (TExpr fun_type $ Operator Ampersand) tearg
          where n = length typs
                asserttyp t = when (t /= TType) $ throwError $ NotAType arg
                fun_type = TLinArrow targ TType

binopType :: Type -> Type
binopType typ = TLinArrow typ (TLinArrow typ typ)

typeAfterCall :: Type -> Type
typeAfterCall (TLinArrow arg body) = body

compType :: Type -> Type
compType typ = TLinArrow typ $ TLinArrow typ TBool

-- | Call typeExpr, and if the resulting expression can be evaluated,
-- do it.
typeExprAndEval :: TypingMonad m => Expr -> m (TExpr, MetaData)
typeExprAndEval expr = do
  (typed_expr@(TExpr typ _), metadata) <- typeExpr expr
  result_expr <-
    if metadata ^. mtdt_comptime && metadata ^. mtdt_interp < 0
      then TExpr typ <$> Value <$> interpretAndDef typed_expr
      else return typed_expr
  return (result_expr, metadata)

typeExprAndInterpret :: TypingMonad m => Expr -> m TValue
typeExprAndInterpret expr = do
  te <- fst <$> typeExpr expr
  Typing.interpret te

typeExprToType :: TypingMonad m => Expr -> m Type
typeExprToType e = do
  te@(TExpr etyp _) <- fst <$> typeExpr e
  when (etyp /= TType) $ throwError $ NotAType e
  (TValue val _) <- interpretAndDef te
  case val of
    (VType val) -> return val
    _ -> throwError $ ValueNotAType val

getTypeFromEnum :: TypingMonad m => [(DebugInfo String, Expr)] -> m Type
getTypeFromEnum [] = return TType
getTypeFromEnum ((_, e):es) = do
  e' <- typeExprToType e
  addUnrestricted "_" e' Nothing
  es' <- getTypeFromEnum es
  return $ TForallArrow (DI "_") e' es'
