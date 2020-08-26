{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Typing where

import Control.Lens hiding (Const(..), use)
import Control.Lens.Indexed
import Control.Monad
import Control.Monad.Except
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
data VarStatus
  = Unrestricted
  | LinearUsed
  | LinearFree
  | LinearHidden
  | Undefined
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
  | UnknownBuiltin Builtins
  | DifferringRessources Expr Expr
  | LinearUseIllegal String Expr
  deriving (Eq, Show)

-- Abstract Typing Monad definition
class MonadError TypingError m =>
      TypingMonad m
  where
  addGlobal :: String -> Type -> m ()
  addLinear :: String -> Type -> m ()
  addUnrestricted :: String -> Type -> m ()
  leaveScope :: m String -- Name of variable which scope we're leaving
  variableStatus :: Variable -> m VarStatus
  -- State manipulation
  saveState :: m a -> m a
  hideLinear :: m a -> m a
  -- The following three might throw UnknownVariable
  use :: Variable -> m ()
  varName :: Variable -> m String
  varType :: Variable -> m Type
  freeVariables :: m (Set Variable)
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
    }
  deriving (Eq, Show)

makeLenses ''LocalVariable

data TypingState =
  TpState
    { _ts_global :: Map String Type
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

lookupVar :: TypingState -> Variable -> Maybe (Either Type (LocalVariable, Int))
lookupVar (TpState globals _ _) (Global name) =
  M.lookup name globals >>= return . Left
lookupVar (TpState _ locals _) (DeBrujin id) =
  locals V.!? id >>= return . Right . (, id)

instance TypingMonad ConcreteTypingMonad where
  addGlobal name typ = ts_global %= M.insert name typ
  addLinear name typ = ts_local %= cons (LVar name typ True False)
  addUnrestricted name typ = ts_local %= cons (LVar name typ False False)
  leaveScope = do
    name <-
      preuse (ts_local . ix 0) >>=
      maybe (throwError $ LastScope) (return . (^. lv_name))
    ts_local %= V.tail
    return name
  variableStatus var = do
    state <- get
    return $
      case lookupVar state var of
        Just (Left _) -> Unrestricted
        Just (Right (lv, id)) ->
          if not (lv ^. lv_isLinear)
            then Unrestricted
            else if id + state ^. ts_hidden >= length (state ^. ts_local)
                   then LinearHidden
                   else if lv ^. lv_isUsed
                          then LinearUsed
                          else LinearFree
        Nothing -> Undefined
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
  use (Global _) = return ()
  use (DeBrujin id) = do
    var <-
      preuse (ts_local . ix id) >>=
      maybe (throwError $ UnknownVariable $ DeBrujin id) return
    let usedVar = var & lv_isUsed .~ True
    ts_local . ix id .= usedVar
  varName var =
    case var of
      Global name -> return name
      DeBrujin id -> do
        state <- get
        case lookupVar state (DeBrujin id) of
          Just (Right (lv, _)) -> return $ lv ^. lv_name
          _ -> throwError $ UnknownVariable $ DeBrujin id
  varType var = do
    state <- get
    case lookupVar state var of
      Just (Left typ) -> return typ
      Just (Right (lv, _)) -> return $ lv ^. lv_typ
      Nothing -> throwError $ UnknownVariable var
  freeVariables = do
    (TpState _ local _) <- get
    let free =
          filter (not . (^. lv_isUsed) . snd) $
          V.ifoldr (\ix x l -> (ix, x) : l) [] local
    return $ S.fromList $ map (DeBrujin . fst) free
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

-- Typing algorithm
--   _____            _             
--  |_   _|   _ _ __ (_)_ __   __ _ 
--    | || | | | '_ \| | '_ \ / _` |
--    | || |_| | |_) | | | | | (_| |
--    |_| \__, | .__/|_|_| |_|\__, |
--        |___/|_|            |___/ 
--      _    _                  _ _   _               
--     / \  | | __ _  ___  _ __(_) |_| |__  _ __ ___  
--    / _ \ | |/ _` |/ _ \| '__| | __| '_ \| '_ ` _ \ 
--   / ___ \| | (_| | (_) | |  | | |_| | | | | | | | |
--  /_/   \_\_|\__, |\___/|_|  |_|\__|_| |_|_| |_| |_|
--             |___/                                  
checkProgramWellTyped :: Program -> Either TypingError ()
checkProgramWellTyped program =
  runT $ do
    deftypes <- forM definitions pretypeDefinition
    let deftyped = zip definitions deftypes
    forM_ deftyped $ uncurry typeDefinition
  where
    runT :: ConcreteTypingMonad () -> Either TypingError ()
    runT = runTyping
    definitions :: [Definition]
    definitions = fmap snd $ M.toList $ program ^. prog_defs

checkExprWellTyped :: Expr -> Either TypingError Type
checkExprWellTyped = runT . typeExpr
  where
    runT :: ConcreteTypingMonad Type -> Either TypingError Type
    runT = runTyping

pretypeDefinition :: TypingMonad m => Definition -> m ([Type], Type)
pretypeDefinition (Definition name args typ _) = do
  args_checked <- mapM processArg args
  checked <- processType typ
  replicateM_ (length args) leaveScope
  let ftype = foldr buildFunType checked args_checked
  addGlobal name ftype
  return (fmap snd args_checked, checked)
  where
    processArg :: TypingMonad m => (DebugInfo String, Arg) -> m (Bool, Type)
    processArg (DI name, LinearArg typ) =
      (processType typ >>=^ addLinear name) >>= return . (True, )
    processArg (DI name, UnrestrictedArg typ) =
      (processType typ >>=^ addUnrestricted name) >>= return . (False, )
    buildFunType :: (Bool, Type) -> Type -> Type
    buildFunType (True, arg) result = LinArrow arg result
    buildFunType (False, arg) result = UnrArrow arg result

typeDefinition :: TypingMonad m => Definition -> ([Type], Type) -> m ()
typeDefinition (Definition name args _ body) (args_type, ret_type) = do
  forM_ (zip args args_type) addArg
  checked_type <- typeExpr body
  when (checked_type /= ret_type) $ throwError $
    ExpectedType body checked_type ret_type
  replicateM_ (length args) leaveScope
  where
    addArg :: TypingMonad m => ((DebugInfo String, Arg), Type) -> m ()
    addArg ((DI name, LinearArg _), typ) = addLinear name typ
    addArg ((DI name, UnrestrictedArg _), typ) = addUnrestricted name typ

processType :: TypingMonad m => Expr -> m Type
processType (Call (Call (Builtin Plus) e1) e2) =
  Sum <$> processType e1 <*> processType e2
processType (Call (Call (Builtin Product) e1) e2) =
  Prod <$> processType e1 <*> processType e2
processType (Call (Call (Builtin Ampersand) e1) e2) =
  Choice <$> processType e1 <*> processType e2
processType (Call (Call (Builtin LinearArrow) e1) e2) =
  LinArrow <$> processType e1 <*> processType e2
processType (Call (Call (Builtin UnrestrictedArrow) e1) e2) =
  UnrArrow <$> processType e1 <*> processType e2
processType (Const IntType) = return Int
processType (Const BoolType) = return Bool
processType expr = throwError $ NotAType expr

typeExpr :: TypingMonad m => Expr -> m Type
typeExpr e@(LocalVar (DI name) id) = typeVariable name e $ DeBrujin id
typeExpr e@(Def def) = typeVariable def e $ Global def
typeExpr e@(Builtin b) =
  return $
  case b of
    Product -> uarrow
    Plus -> uarrow
    Ampersand -> uarrow
    LinearArrow -> uarrow
    UnrestrictedArrow -> uarrow
  where
    uarrow :: Type
    uarrow = UnrArrow Universe (UnrArrow Universe Universe)
typeExpr e@(Const ct) =
  return $
  case ct of
    IntConst _ -> Int
    BoolConst _ -> Bool
    IntType -> Universe
    BoolType -> Universe
typeExpr e@(Assign (DI name) val body) = do
  vtyp <- typeExpr val
  addLinear name vtyp
  rtyp <- typeExpr body
  leaveScope
  return rtyp
typeExpr e@(IfThenElse cond thenE elseE) = do
  ctyp <- typeExpr cond
  when (ctyp /= Bool) $ throwError $ ExpectedType cond ctyp Bool
  (thenT, thenV) <- saveState $ (,) <$> typeExpr thenE <*> freeVariables
  (elseT, elseV) <- saveState $ (,) <$> typeExpr elseE <*> freeVariables
  when (thenV /= elseV) $ throwError $ DifferringRessources thenE elseE
  when (thenT /= elseT) $ throwError $ ExpectedType elseE thenT elseT
  return thenT
typeExpr e@(Call funE argE) = do
  funT <- typeExpr funE
  case funT of
    LinArrow expectedArgT bodyT -> do
      argT <- typeExpr argE
      when (argT /= expectedArgT) $ throwError $
        ExpectedType argE expectedArgT argT
      return bodyT
    UnrArrow expectedArgT bodyT -> do
      argT <- hideLinear $ typeExpr argE
      when (argT /= expectedArgT) $ throwError $
        ExpectedType argE expectedArgT argT
      return bodyT
typeExpr e@(Lambda (DI name) var body) = do
  argT <-
    case var of
      LinearArg typ -> processType typ >>=^ addLinear name
      UnrestrictedArg typ -> processType typ >>=^ addUnrestricted name
  bodyT <- typeExpr body
  leaveScope
  return $
    case var of
      LinearArg _ -> LinArrow argT bodyT
      UnrestrictedArg _ -> UnrArrow argT bodyT
typeExpr Type = return Universe

typeVariable :: TypingMonad m => String -> Expr -> Variable -> m Type
typeVariable name e var = do
  state <- variableStatus var
  case state of
    LinearUsed -> throwError $ ReusedLinear name e
    LinearHidden -> throwError $ LinearUseIllegal name e
    Undefined -> throwError $ UndefinedVariable name e
    _ -> use var >> varType var
-- >>> checkExprWellTyped $ Call (Lambda (DI "x") (LinearArg $ IR.Const BoolType) (IfThenElse (LocalVar (DI "x") 0) (IR.Const (BoolConst False)) (IR.Const (BoolConst True)))) (IR.Const (BoolConst True))
-- Right Bool
-- >>> checkExprWellTyped $ Call (Lambda (DI "x") (LinearArg $ IR.Const BoolType) (IfThenElse (LocalVar (DI "x") 0) (IR.Const (BoolConst False)) (LocalVar (DI "x") 0))) (IR.Const (BoolConst True))
-- Left (ReusedLinear "x" (LocalVar "x" 0))
-- >>> checkExprWellTyped $ (Lambda (DI "x") (LinearArg $ IR.Const BoolType) (IfThenElse (LocalVar (DI "x") 0) (IR.Const (BoolConst False)) (IR.Const (BoolConst True))))
-- Right (LinArrow Bool Bool)
-- >>> checkExprWellTyped $ Call (Lambda (DI "x") (LinearArg $ IR.Const BoolType) (IfThenElse (IR.Const $ BoolConst True) (IR.Const (BoolConst False)) (LocalVar (DI "x") 0))) (IR.Const (BoolConst True))
-- Left (DifferringRessources (Const (BoolConst False)) (LocalVar "x" 0))
-- >>> checkExprWellTyped $ Call (Lambda (DI "x") (LinearArg $ IR.Const BoolType) (IfThenElse (IR.Const $ BoolConst True) (LocalVar (DI "x") 0) (LocalVar (DI "x") 0))) (IR.Const (BoolConst True))
-- Right Bool
