{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter where

import Control.Arrow ((***), first, second)
import Control.Lens hiding (Const(..))
import Control.Lens.Indexed
import Control.Monad
import Control.Monad.Except
import Control.Monad.State hiding (get, put, state)
import Control.Monad.State.Class
import Data.Bits
import Data.Foldable
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple
import Data.Vector (Vector)
import qualified Data.Vector as V
import Error
import IR

-- MonadError
--   __  __                       _ _____                     
--  |  \/  | ___  _ __   __ _  __| | ____|_ __ _ __ ___  _ __ 
--  | |\/| |/ _ \| '_ \ / _` |/ _` |  _| | '__| '__/ _ \| '__|
--  | |  | | (_) | | | | (_| | (_| | |___| |  | | | (_) | |   
--  |_|  |_|\___/|_| |_|\__,_|\__,_|_____|_|  |_|  \___/|_|   
--                                                            
-- Abstract
--     _   _       _               _   
--    /_\ | |__ __| |_ _ _ __ _ __| |_ 
--   / _ \| '_ (_-<  _| '_/ _` / _|  _|
--  /_/ \_\_.__/__/\__|_| \__,_\__|\__|
--                                     
data GlobalContext =
  GlobalContext
    { globals :: Map String TValue
    , locals :: Map Int TValue
    }
  deriving (Show, Eq)

class MonadError Error m =>
      InterpreterMonad m
  where
  getValue :: Variable -> m Value
  withValue :: Value -> m b -> m b

-- Concrete
--    ___                     _       
--   / __|___ _ _  __ _ _ ___| |_ ___ 
--  | (__/ _ \ ' \/ _| '_/ -_)  _/ -_)
--   \___\___/_||_\__|_| \___|\__\___|
--                                    
data InterpreterState =
  ItState
    { _is_global :: Map String Value
    , _is_local :: Vector (Maybe Value)
    }
  deriving (Eq, Show)

makeLenses ''InterpreterState

type ConcreteInterpreterMonad_ m a = StateT InterpreterState (ExceptT Error m) a

newtype ConcreteInterpreterMonadT m a =
  CIM
    { unCIM :: ConcreteInterpreterMonad_ m a
    }

instance Functor m => Functor (ConcreteInterpreterMonadT m) where
  fmap f = CIM . fmap f . unCIM

instance Monad m => Applicative (ConcreteInterpreterMonadT m) where
  pure = CIM . pure
  (CIM f) <*> (CIM x) = CIM $ f <*> x

instance Monad m => Monad (ConcreteInterpreterMonadT m) where
  (CIM x) >>= f = CIM $ x >>= (unCIM . f)

instance Monad m => MonadError Error (ConcreteInterpreterMonadT m) where
  throwError = CIM . throwError
  catchError (CIM def) handler = CIM $ catchError def $ unCIM . handler

instance Monad m =>
         MonadState InterpreterState (ConcreteInterpreterMonadT m) where
  get = CIM get
  put = CIM . put

instance MonadTrans ConcreteInterpreterMonadT where
  lift = CIM . lift . lift

lookupVar :: InterpreterState -> Variable -> Maybe Value
lookupVar (ItState globals _) (Global name) = M.lookup name globals
lookupVar (ItState _ locals) (DeBruijn id) = join $ locals V.!? id

instance Monad m => InterpreterMonad (ConcreteInterpreterMonadT m) where
  getValue var =
    get >>=
    maybe (throwError $ UndefinedVariableInterpreter var) return .
    flip lookupVar var
  withValue val action = do
    is_local %= cons (Just val)
    result <- action
    is_local %= V.tail
    return result

runInterpreterT ::
     Monad m
  => GlobalContext
  -> ConcreteInterpreterMonadT m a
  -> m (Either Error a)
runInterpreterT (GlobalContext globals ids) (CIM action) =
  runExceptT $ evalStateT action initState
  where
    initState :: InterpreterState
    initState = ItState (fmap extractV globals) stack
    stack :: Vector (Maybe Value)
    stack = fmap extractV <$> V.generate size (flip M.lookup ids)
    size :: Int
    size = M.foldrWithKey (\k -> const $ max k) 0 ids

type ConcreteInterpreterMonad a = ConcreteInterpreterMonadT Identity a

runInterpreter :: GlobalContext -> ConcreteInterpreterMonad a -> Either Error a
runInterpreter gc = runIdentity . runInterpreterT gc

-- Utils
--   _   _ _   _ _     
--  | | | | |_(_) |___ 
--  | | | | __| | / __|
--  | |_| | |_| | \__ \
--   \___/ \__|_|_|___/
--                     
liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe error = maybe (throwError error) return

extractV :: TValue -> Value
extractV (TValue val _) = val

data InspectCapturedState =
  ICState
    { _ic_remaping :: Map Int Int
    , _ic_stack :: [Value]
    }
  deriving (Eq, Show)

type ICMonad m = StateT InspectCapturedState m

makeLenses ''InspectCapturedState

--   ___       _                           _            
--  |_ _|_ __ | |_ ___ _ __ _ __  _ __ ___| |_ ___ _ __ 
--   | || '_ \| __/ _ \ '__| '_ \| '__/ _ \ __/ _ \ '__|
--   | || | | | ||  __/ |  | |_) | | |  __/ ||  __/ |   
--  |___|_| |_|\__\___|_|  | .__/|_|  \___|\__\___|_|   
--                         |_|                          
interpret :: GlobalContext -> TExpr -> Either Error TValue
interpret gc expr@(TExpr typ _) =
  flip TValue typ <$> (rinter $ interpretTExpr expr)
  where
    rinter :: ConcreteInterpreterMonad a -> Either Error a
    rinter = runInterpreter gc

interpretTExpr :: InterpreterMonad m => TExpr -> m Value
interpretTExpr (TExpr typ (Operator op)) = return $ makeOperatorClosure typ op
interpretTExpr (TExpr typ expr) = interpretExpr expr

interpretExpr :: InterpreterMonad m => ExprT Type TExpr -> m Value
interpretExpr (LocalVar (DI name) id) = getValue $ DeBruijn id
interpretExpr (Def name) = getValue $ Global name
interpretExpr (Let name val vartyp body) =
  interpretTExpr val >>= flip withValue (interpretTExpr body)
interpretExpr (IfThenElse cond thenE elseE) =
  interpretTExpr cond >>= \case
    (VBool b) ->
      if b
        then interpretTExpr thenE
        else interpretTExpr elseE
    _ ->
      throwError $
      TypeSystemUnsound "Condition in if-then-else should have been bool"
interpretExpr (Tuple exprs) = VTuple <$> mapM interpretTExpr exprs
interpretExpr (Lambda name _ _ body) = do
  (captured, nbody) <- findCapturedVariables body
  return $ VFun captured 1 nbody
interpretExpr (ForAll _ _ _) =
  throwError $ Unimplemented "Forall interpretation"
interpretExpr (Value (TValue v t)) = return v
interpretExpr (Call fun arg) = do
  argV <- interpretTExpr arg
  funV <- interpretTExpr fun
  callFun funV argV

callFun :: InterpreterMonad m => Value -> Value -> m Value
callFun (VFun context arity (TExpr typ body)) arg =
  if arity < 1
    then throwError $ TypeSystemUnsound "Too many arguments"
    else if arity > 1
           then return $ VFun (arg : context) (arity - 1) $ TExpr typ body
           else evaluateFun (reverse $ arg : context) body
callFun _ _ =
  throwError $ TypeSystemUnsound "Calling something that is not a function !"

evaluateFun :: InterpreterMonad m => [Value] -> ExprT Type TExpr -> m Value
evaluateFun args (Operator op) = evaluateOperator op args
evaluateFun (arg:ctx) body = withValue arg $ evaluateFun ctx body
evaluateFun [] body = interpretExpr body

makeOperatorClosure :: Type -> Operator -> Value
makeOperatorClosure tp Plus = binOp tp Plus
makeOperatorClosure tp Minus = binOp tp Minus
makeOperatorClosure tp Times = binOp tp Times
makeOperatorClosure tp Div = binOp tp Div
makeOperatorClosure tp Hat = unOp tp Hat
makeOperatorClosure tp Ampersand = unOp tp Ampersand
makeOperatorClosure tp Bar = unOp tp Bar
makeOperatorClosure tp Arrow = binOp tp Arrow
makeOperatorClosure tp LinArrow = binOp tp LinArrow

binOp :: Type -> Operator -> Value
binOp tp op = VFun [] 2 $ TExpr tp $ Operator op

unOp :: Type -> Operator -> Value
unOp tp op = VFun [] 1 $ TExpr tp $ Operator op

evaluateOperator :: InterpreterMonad m => Operator -> [Value] -> m Value
evaluateOperator Plus [VInt i1, VInt i2] = return $ VInt $ i1 + i2
evaluateOperator Plus _ =
  throwError $ TypeSystemUnsound "Invalid \"+\" arguments"
evaluateOperator Minus [VInt i1, VInt i2] = return $ VInt $ i1 - i2
evaluateOperator Minus _ =
  throwError $ TypeSystemUnsound "Invalid \"-\" arguments"
evaluateOperator Times [VInt i1, VInt i2] = return $ VInt $ i1 * i2
evaluateOperator Times _ =
  throwError $ TypeSystemUnsound "Invalid \"*\" arguments"
evaluateOperator Div [VInt i1, VInt i2] = return $ VInt $ (i1 `div` i2)
evaluateOperator Div _ =
  throwError $ TypeSystemUnsound "Invalid \"/\" arguments"
evaluateOperator Hat [VTuple (VInt start:args)] =
  VInt <$> foldM (\acc v -> xor acc <$> extractVInt "\"^\"" v) start args
evaluateOperator Hat [VTuple (VType start:args)] =
  VType <$> uncurry Type <$> extractVTypes "\"^\"" (VType start : args) TChoice
evaluateOperator Hat _ =
  throwError $ TypeSystemUnsound "Invalid \"^\" arguments"
evaluateOperator Ampersand [VTuple (VInt start:args)] =
  VInt <$> foldM (\acc v -> (acc .&.) <$> extractVInt "\"&\"" v) start args
evaluateOperator Ampersand [VTuple (VType start:args)] =
  VType <$> uncurry Type <$> extractVTypes "\"&\"" (VType start : args) TTuple
evaluateOperator Bar [VTuple (VInt start:args)] =
  VInt <$> foldM (\acc v -> (acc .|.) <$> extractVInt "\"&\"" v) start args
evaluateOperator Bar _ =
  throwError $ TypeSystemUnsound "Invalid \"|\" arguments"
evaluateOperator Arrow [VType t1, VType t2@(Type comptime _)] =
  return $ VType $ Type comptime $ TUnrArrow t1 t2
evaluateOperator Arrow _ =
  throwError $ TypeSystemUnsound "Invalid \"->\" arguments"
evaluateOperator LinArrow [VType t1, VType t2@(Type comptime _)] =
  return $ VType $ Type comptime $ TLinArrow t1 t2
evaluateOperator LinArrow _ =
  throwError $ TypeSystemUnsound "Invalid \"-o\" arguments"

extractVInt :: InterpreterMonad m => String -> Value -> m Integer
extractVInt _ (VInt i) = return i
extractVInt opname _ =
  throwError $ TypeSystemUnsound $ opname <> " extected an integer"

extractVType :: InterpreterMonad m => String -> Value -> m Type
extractVType _ (VType t) = return t
extractVType opname _ =
  throwError $ TypeSystemUnsound $ opname <> " extected a type"

extractVTypes ::
     forall m. InterpreterMonad m
  => String
  -> [Value]
  -> ([Type] -> TypeBase)
  -> m (Bool, TypeBase)
extractVTypes opname vals f = second f <$> foldM accumulator (True, []) vals
  where
    accumulator :: (Bool, [Type]) -> Value -> m (Bool, [Type])
    accumulator (is_comptime, acc) val =
      extractVType opname val >>= \(Type cp rt) ->
        return (cp && is_comptime, Type cp rt : acc)

findCapturedVariables ::
     forall m. InterpreterMonad m
  => TExpr
  -> m ([Value], TExpr)
findCapturedVariables body =
  (_ic_stack *** id) <$> swap <$> runStateT action initState
  where
    action :: ICMonad m TExpr
    action = extractCapturedT 1 body
    initState :: InspectCapturedState
    initState = ICState M.empty []

extractCapturedT :: InterpreterMonad m => Int -> TExpr -> ICMonad m TExpr
extractCapturedT depth (TExpr typ expr) =
  TExpr typ <$> extractCaptured depth expr

rename ::
     MonadState InspectCapturedState m
  => Int -- ^ The depth in the lambda
  -> Int -- ^ The De Bruijn index of the variable
  -> m a -- ^ What to do when the variable is local
  -> (Int -> m a) -- ^ What to do when the variable has already been captured,
                       --   gets the replaced id
  -> (Int -> m a) -- ^ What to do when the variable should be captured
                       --   gets a newly allocated id
  -> m a
rename depth id local captured capture =
  if id < depth
    then local
    else let topid = id - depth
          in do state <- use ic_remaping
                case M.lookup topid state of
                  Nothing ->
                    let newid = M.size state
                     in (ic_remaping .= M.insert id newid state) >>
                        capture (newid + depth)
                  Just newid -> captured $ newid + depth

doCapture :: InterpreterMonad m => Int -> Int -> ICMonad m (Int, Maybe Value)
doCapture depth id =
  rename
    depth
    id
    (return (id, Nothing))
    (return . (, Nothing))
    (\nid -> lift (getValue (DeBruijn $ nid - 1)) >>= return . (nid, ) . Just)

extractCaptured ::
     InterpreterMonad m
  => Int
  -> ExprT Type TExpr
  -> ICMonad m (ExprT Type TExpr)
extractCaptured depth (LocalVar dname id) = do
  (nid, mcaptured) <- doCapture depth id
  ic_stack %= \c -> maybe c (: c) mcaptured
  return $ LocalVar dname nid
extractCaptured depth e@(Def _) = return e
extractCaptured depth e@(Value v) = return e
extractCaptured depth (Let name val typ body) = do
  nval <- extractCapturedT depth val
  nbody <- extractCapturedT (depth + 1) body
  return $ Let name nval typ nbody
extractCaptured depth (IfThenElse cond ifE thenE) =
  IfThenElse <$> extractCapturedT depth cond <*> extractCapturedT depth ifE <*>
  extractCapturedT depth thenE
extractCaptured depth (Call body arg) =
  Call <$> extractCapturedT depth body <*> extractCapturedT depth arg
extractCaptured depth e@(Operator _) = return e
extractCaptured depth (Tuple lexpr) =
  Tuple <$> (forM lexpr $ extractCapturedT depth)
extractCaptured depth (Lambda name linear typ body) =
  Lambda name linear typ <$> extractCapturedT (depth + 1) body
extractCaptured depth (ForAll name typ body) =
  ForAll name typ <$> extractCapturedT (depth + 1) body
