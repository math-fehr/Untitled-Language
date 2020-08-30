module Repl where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans
import qualified Data.List as L
import qualified Data.Map as M
import qualified Error as Err
import IR
import qualified Interpreter as Ir
import qualified Parser as Pr
import qualified ParsingToIR as PtIR
import System.Console.Haskeline
import System.Console.Repline hiding (Command)
import System.IO.Error
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import qualified Typing as Ty
import ULPrelude (prelude)

type ReplM = HaskelineT (Ty.ConcreteTypingMonadT IO)

runReplM :: ReplM a -> IO (Either Err.Error a)
runReplM action = Ty.runTypingT $ runHaskelineT defaultSettings action

-- Commands
--    ____                                          _     
--   / ___|___  _ __ ___  _ __ ___   __ _ _ __   __| |___ 
--  | |   / _ \| '_ ` _ \| '_ ` _ \ / _` | '_ \ / _` / __|
--  | |__| (_) | | | | | | | | | | | (_| | | | | (_| \__ \
--   \____\___/|_| |_| |_|_| |_| |_|\__,_|_| |_|\__,_|___/
data CmdError
  = CvtError Err.Error
  | ParseError ParseError
  deriving (Show)

data Command
  = CmdTopLevel Decl
  | CmdExpr Expr
  | CmdError CmdError
  deriving (Show)

cmdParser :: Parser Command
cmdParser =
  (either (CmdError . CvtError) CmdTopLevel <$>
   flip PtIR.declToIr (PtIR.PIRContext []) <$>
   (Pr.declarationParser <* eof)) <|>
  (either (CmdError . CvtError) CmdExpr <$>
   flip PtIR.exprToIr (PtIR.PIRContext []) <$>
   (Pr.exprParser <* eof))

getCommand :: String -> Command
getCommand str =
  case parse cmdParser "repl" str of
    Left err -> CmdError $ ParseError err
    Right cmd -> cmd

executeCommand :: Command -> ReplM ()
executeCommand (CmdError (CvtError err)) =
  liftIO $ putStrLn $ "Error interpreting expression : " ++ show err
executeCommand (CmdError (ParseError err)) =
  liftIO $ putStrLn $ "Error parsing command : " ++ show err
executeCommand (CmdTopLevel decl) =
  lift $
  catchError action (\e -> liftIO $ putStrLn $ "Couldn't define : " ++ show e)
  where
    name = declName decl
    action = do
      Ty.registerDecl decl
      Ty.declDecl name
      Ty.defDecl name
      liftIO $ putStrLn $ "Defined " ++ declName decl
executeCommand (CmdExpr expr) =
  lift $
  catchError action (\e -> liftIO $ putStrLn $ "Couldn't evaluate : " ++ show e)
  where
    action = do
      texpr <- Ty.typeExpr expr
      value <- Ty.interpret texpr
      liftIO $ displayTValue value

-- Pretty Printing
--   ____           _   _         ____       _       _   _             
--  |  _ \ _ __ ___| |_| |_ _   _|  _ \ _ __(_)_ __ | |_(_)_ __   __ _ 
--  | |_) | '__/ _ \ __| __| | | | |_) | '__| | '_ \| __| | '_ \ / _` |
--  |  __/| | |  __/ |_| |_| |_| |  __/| |  | | | | | |_| | | | | (_| |
--  |_|   |_|  \___|\__|\__|\__, |_|   |_|  |_|_| |_|\__|_|_| |_|\__, |
--                          |___/                                |___/ 
displayTValue :: TValue -> IO ()
displayTValue (TValue value (Type _ typ)) = do
  putStr $ displayValue value
  putStr " : "
  putStr $ displayType typ
  putStr "\n"

displayValue :: Value -> String
displayValue VUnit = "()"
displayValue (VInt i) = show i
displayValue (VBool b) =
  if b
    then "true"
    else "false"
displayValue (VType (Type _ typ)) = displayType typ
displayValue (VStruct []) = "{ }"
displayValue (VStruct (fld1:flds)) =
  "{ " ++
  foldl (\str fld -> str ++ ", " ++ displayField fld) (displayField fld1) flds ++
  " }"
displayValue (VTuple []) = "()"
displayValue (VTuple (v1:vs)) =
  "(" ++
  foldl (\str v -> str ++ ", " ++ displayValue v) (displayValue v1) vs ++ ")"
displayValue (VConstr name value) = name ++ " (" ++ displayValue value ++ ")"
displayValue (VFun _ i _) = "<lambda#" ++ show i ++ ">"
displayValue (VForall _ (Type _ typ) _) = "<forall : " ++ displayType typ ++ ">"

displayField :: (String, Value) -> String
displayField (name, value) = name ++ " = " ++ displayValue value

unType :: Type -> TypeBase
unType (Type _ typ) = typ

displayType :: TypeBase -> String
displayType (TVar (DI name) id) = name ++ "<" ++ show id ++ ">"
displayType TBool = "Bool"
displayType TType = "Type"
displayType (TInt it) = displayIntType it
displayType TByte = "Byte"
displayType (TTuple []) = "(&)"
displayType (TTuple (t1:ts)) =
  "(" ++
  foldl
    (\str t -> str ++ " & " ++ displayType t)
    (displayType $ unType t1)
    (map unType ts) ++
  ")"
displayType (TArray t size) =
  "Array<" ++ displayType (unType t) ++ "," ++ show size ++ ">"
displayType (TChoice []) = "(^)"
displayType (TChoice (t1:ts)) =
  "(" ++
  foldl
    (\str t -> str ++ " ^ " ++ displayType t)
    (displayType $ unType t1)
    (map unType ts) ++
  ")"
displayType (TSum []) = "[|]"
displayType (TSum (t1:ts)) =
  "[ " ++
  foldl (\str t -> str ++ " | " ++ displayTName t) (displayTName t1) ts ++ " ]"
displayType (TStruct []) = "{&}"
displayType (TStruct (t1:ts)) =
  "{ " ++
  foldl (\str t -> str ++ " & " ++ displayTName t) (displayTName t1) ts ++ " }"
displayType (TLinArrow arg ret) =
  if isArrow arg
    then "(" ++ displayType (unType arg) ++ ") -@ " ++ displayType (unType ret)
    else displayType (unType arg) ++ " -@ " ++ displayType (unType ret)
displayType (TUnrArrow arg ret) =
  if isArrow arg
    then "(" ++ displayType (unType arg) ++ ") -> " ++ displayType (unType ret)
    else displayType (unType arg) ++ " -> " ++ displayType (unType ret)
displayType (TForallArrow (DI name) argt rett) =
  "forall (" ++
  name ++
  " : " ++ displayType (unType argt) ++ "). " ++ displayType (unType rett)
displayType (TNewType name [] typ) =
  "<" ++ name ++ " := " ++ displayType (unType typ) ++ ">"
displayType (TNewType name (a1:as) typ) =
  "<" ++
  name ++
  " := " ++
  foldl
    (\str (TValue a _) -> str ++ " (" ++ displayValue a ++ ")")
    ("(" ++ displayType (unType typ) ++ ")")
    as ++
  ">"

isArrow :: Type -> Bool
isArrow (Type _ (TLinArrow _ _)) = True
isArrow (Type _ (TUnrArrow _ _)) = True
isArrow (Type _ (TForallArrow _ _ _)) = True
isArrow _ = False

displayTName :: (String, Type) -> String
displayTName (name, Type _ typ) = name ++ " : " ++ displayType typ

displayIntType :: IntType -> String
displayIntType (IntType size True True) = "Int" ++ show size
displayIntType (IntType size False True) = "Uint" ++ show size
displayIntType (IntType size True False) = "Wuint" ++ show size
displayIntType (IntType size False False) = "Wint" ++ show size

-- Repl
--   ____            _ 
--  |  _ \ ___ _ __ | |
--  | |_) / _ \ '_ \| |
--  |  _ <  __/ |_) | |
--  |_| \_\___| .__/|_|
--            |_|      
getContext :: Monad m => Ty.ConcreteTypingMonadT m Ir.GlobalContext
getContext = do
  globals <- use Ty.ts_global
  let nglobs = M.mapMaybe globalValue globals
  return $ Ir.GlobalContext nglobs M.empty
  where
    globalValue :: Ty.GlobalStatus -> Maybe TValue
    globalValue (Ty.Defined val) = Just val
    globalValue _ = Nothing

names :: Monad m => Ty.ConcreteTypingMonadT m [String]
names = do
  (Ir.GlobalContext globals _) <- getContext
  return $ M.keys globals

completer :: WordCompleter (Ty.ConcreteTypingMonadT IO)
completer n = filter (L.isPrefixOf n) <$> names

info :: String -> ReplM ()
info _ = liftIO $ putStrLn "Unimplemented"

load :: String -> ReplM ()
load file = do
  prog <-
    liftIO $
    catchIOError
      (Pr.parseFile file)
      (\e -> (putStrLn $ "Couldn't read program : " ++ show e) >> return [])
  case PtIR.parsedProgramToIr prog of
    Left e -> liftIO $ putStrLn $ "Couldn't parse file : " ++ show e
    Right program ->
      case Ty.typeProgram program of
        Left e -> liftIO $ putStrLn $ "Couldn't type file : " ++ show e
        Right tprogram -> lift $ Ty.loadTProgram tprogram

opts :: [(String, String -> ReplM ())]
opts = [("info", info), ("i", info), ("load", load), ("l", load)]

ini :: ReplM ()
ini = do
  lift $ Ty.loadTProgram prelude
  liftIO $
    putStrLn
      "Welcome to the Untitled Language Repl !\n\
          \        _   _ _     __  __ \n\
          \       | | | | |   |  \\/  |\n\
          \       | | | | |   | |\\/| |\n\
          \       | |_| | |___| |  | |\n\
          \        \\___/|_____|_|  |_|\n"

prompt :: MultiLine -> ReplM String
prompt MultiLine = return "\\-> "
prompt SingleLine = return ">>> "

cmd :: String -> ReplM ()
cmd = executeCommand . getCommand

leaving :: ReplM ExitDecision
leaving = do
  liftIO $ putStrLn "Good bye world!"
  return Exit

repl :: IO ()
repl =
  discard $
  Ty.runTypingT $
  evalRepl prompt cmd opts (Just ':') Nothing (Word0 completer) ini leaving
  where
    discard :: Monad m => m a -> m ()
    discard = (>> return ())