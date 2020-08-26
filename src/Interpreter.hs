{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Interpreter where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import IR

data RuntimeError
  = DivisionByZero
  | Unimplemented
  | TypeSystemUnsound
  | UndefinedValue
  deriving (Eq, Show)

data GlobalContext =
  GlobalContext
    { globals :: Map String TValue
    , locals :: Map Int TValue
    }

class MonadError RuntimeError m =>
      InterpreterMonad m
  where
  runInterpreter :: GlobalContext -> m a -> Either RuntimeError a
  getValue :: Variable -> m TValue
  withValue :: TValue -> m b -> m b

interpret :: InterpreterMonad m => TExpr -> m TValue
interpret = undefined
