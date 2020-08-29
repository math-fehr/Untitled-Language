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
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Error
import IR
import ULPrelude (prelude)

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
    _ -> throwError (InternalError "Enum and struct decl unimplemented")

-- compute the type of the declaration, potentatially by using other decl or defs.
declDecl :: TypingMonad m => String -> m Type
declDecl name =
  decl name $ \case
    DDef DefT {def_name, def_type, def_args, def_body} ->
      typeExprToType def_type
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

expectType :: TypingMonad m => Type -> Expr -> Type -> m ()
expectType expected_type expr actual_type =
  case (expected_type, actual_type) of
    (Type True etyp, Type True atyp) -> when (etyp /= atyp) fail
    (Type False etyp, Type _ atyp) -> when (etyp /= atyp) fail
    _ -> fail
  where
    fail = throwError $ ExpectedType expr expected_type actual_type

mergeType :: TypingMonad m => Type -> Type -> m Type
mergeType t@(Type comptime base) t'@(Type comptime' base') = do
  when (base /= base') $ throwError $ IncompatibleTypes t t'
  return (Type (comptime && comptime') base)

typeExpr :: TypingMonad m => Expr -> m TExpr
typeExpr e = do
  te@(TExpr (Type comptime typ) e') <- typeExpr' e
  if comptime
    then do
      v <- interpret te
      return $ TExpr (Type True typ) (Value v)
    else return te

typeExpr' :: TypingMonad m => Expr -> m TExpr
typeExpr' e@(Expr _ (LocalVar (DI name) id)) =
  TExpr <$> (typeVariable name e $ DeBruijn id) <*>
  (return $ LocalVar (DI name) id)
typeExpr' e@(Expr _ (Def def)) =
  TExpr <$> (typeVariable def e $ Global def) <*> (return (Def def))
typeExpr' (Expr _ (Value (TValue val typ))) =
  return $ TExpr typ (Value (TValue val typ))
typeExpr' (Expr _ (Let (DI name) val valtyp body)) = do
  teval@(TExpr tval@(Type comptime _) _) <- typeExpr val
  typ <-
    case valtyp of
      Just valtyp -> do
        tvaltyp <- typeExprToType valtyp
        expectType tvaltyp val tval
      -- when (tvaltyp /= tval) $ throwError $ ExpectedType val tvaltyp tval
        return tvaltyp
      _ -> return tval
  if comptime
    then do
      (TValue vval _) <- interpret teval
      addLinear name tval (Just vval)
    else addLinear name tval Nothing
  tebody@(TExpr tbody _) <- typeExpr body
  leaveScope
  return $ TExpr tbody $ Let (DI name) teval (Just typ) tebody
typeExpr' (Expr _ (IfThenElse cond thenE elseE)) = do
  tecond@(TExpr ctyp _) <- typeExpr cond
  expectType (Type False TBool) cond ctyp
  (tethen@(TExpr then_type _), then_free_vars) <-
    saveState $ (,) <$> typeExpr thenE <*> freeVariables
  (teelse@(TExpr else_type _), else_free_vars) <-
    saveState $ (,) <$> typeExpr elseE <*> freeVariables
  when (then_free_vars /= else_free_vars) $ throwError $
    DifferringRessources thenE elseE
  commontyp <- mergeType then_type else_type
  return $ TExpr commontyp $ IfThenElse tecond tethen teelse
typeExpr' (Expr _ (Call (Expr _ (Operator Plus)) arg)) = typeCallOp Plus arg
typeExpr' (Expr _ (Call (Expr _ (Operator Minus)) arg)) = typeCallOp Minus arg
typeExpr' (Expr _ (Call (Expr _ (Operator Times)) arg)) = typeCallOp Times arg
typeExpr' (Expr _ (Call (Expr _ (Operator Div)) arg)) = typeCallOp Div arg
typeExpr' (Expr _ (Call funE argE)) = do
  tefun@(TExpr (Type ctfun tfun) _) <- typeExpr funE
  case tfun of
    TLinArrow expectedArgT (Type ctbody tbody) -> do
      tearg@(TExpr targ _) <- typeExpr argE
      expectType expectedArgT argE targ
      return $ TExpr (Type (ctbody && ctfun) tbody) $ Call tefun tearg
    TUnrArrow expectedArgT (Type ctbody tbody) -> do
      tearg@(TExpr targ _) <- hideLinear $ typeExpr argE
      expectType expectedArgT argE targ
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
