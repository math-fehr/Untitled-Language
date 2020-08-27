{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens ((^.))
import Control.Monad
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as M
import System.Directory
import Text.Parsec (parse)

import IR
import Interpreter
import qualified Parser
import ParsingToIR

main :: IO ()
main = do
  ran_tests <- tests
  defaultMain ran_tests

-- | All tests
tests :: IO TestTree
tests = do
  file_tests <- interpreter_file_tests
  return $ testGroup "Interpreter" [interpreter_ast_tests, file_tests]

interpret :: TExpr -> Either RuntimeError Value
interpret expr = rinter $ interpretTExpr expr
  where
    rinter :: ConcreteInterpreterMonad a -> Either RuntimeError a
    rinter = runInterpreter (GlobalContext M.empty M.empty)

deftyp :: Type
deftyp = Type True TType

ast_tests_data :: [(TExpr, Value)]
ast_tests_data =
  [ ( TExpr
        deftyp
        (Call
           (TExpr
              deftyp
              (Call
                 (TExpr deftyp (Operator Minus))
                 (TExpr deftyp (Value $ VInt 2))))
           (TExpr deftyp (Value $ VInt 3)))
    , VInt (-1))
  , ( TExpr
        deftyp
        (Let
           (DI "x")
           (TExpr deftyp (Value $ VInt 2))
           Nothing
           (TExpr
              deftyp
              (Call
                 (TExpr
                    deftyp
                    (Call
                       (TExpr deftyp (Operator Minus))
                       (TExpr deftyp (LocalVar (DI "x") 0))))
                 (TExpr deftyp (Value $ VInt 3)))))
    , VInt (-1))
  , ( let lambda =
            TExpr
              deftyp
              (Lambda
                 (DI "x")
                 True
                 deftyp
                 (TExpr
                    deftyp
                    (Lambda
                       (DI "y")
                       True
                       deftyp
                       (TExpr
                          deftyp
                          (Call
                             (TExpr
                                deftyp
                                (Call
                                   (TExpr deftyp (Operator Minus))
                                   (TExpr deftyp (LocalVar (DI "y") 0))))
                             (TExpr deftyp (LocalVar (DI "x") 1)))))))
       in TExpr
            deftyp
            (Let
               (DI "f")
               lambda
               Nothing
               (TExpr
                  deftyp
                  (Call
                     (TExpr
                        deftyp
                        (Call
                           (TExpr deftyp (LocalVar (DI "f") 0))
                           (TExpr deftyp (Value $ VInt 2))))
                     (TExpr deftyp (Value $ VInt 3)))))
    , VInt 1)
  ]

interpreter_ast_tests :: TestTree
interpreter_ast_tests =
  testGroup "Expr" $
  map (\(e, r) -> testCase (show r) $ interpret e @?= Right r) ast_tests_data

file_tests_data :: [(FilePath, Value)]
file_tests_data =
  [ ("examples/interp/const.ulm", VInt 42)
  , ("examples/interp/lambda.ulm", VInt 42)
  ]

interpreter_file_tests :: IO TestTree
interpreter_file_tests =
  forM file_tests_data (uncurry file_make_test) >>= return . testGroup "File"

mockTyper :: Expr -> TExpr
mockTyper (Expr _ expr) = TExpr deftyp $ mockTyperT expr

mockTyperT :: ExprT Expr Expr -> ExprT Type TExpr
mockTyperT (LocalVar name id) = LocalVar name id
mockTyperT (Def name) = Def name
mockTyperT (Value val) = Value val
mockTyperT (Let name val _ body) =
  Let name (mockTyper val) Nothing (mockTyper body)
mockTyperT (IfThenElse cond ifE elseE) =
  IfThenElse (mockTyper cond) (mockTyper ifE) (mockTyper elseE)
mockTyperT (Call fun arg) = Call (mockTyper fun) (mockTyper arg)
mockTyperT (Operator op) = Operator op
mockTyperT (Tuple exprs) = Tuple $ fmap mockTyper exprs
mockTyperT (Lambda name linear _ body) =
  Lambda name linear undefined $ mockTyper body
mockTyperT (ForAll name _ body) = ForAll name undefined $ mockTyper body

mockTypeFile :: Parser.Program -> GlobalContext
mockTypeFile prog =
  flip GlobalContext M.empty $
  foldr addGlobal (M.fromList [("true", VBool True), ("false", VBool False)]) $
  prog_defs defs
  where
    addGlobal :: Decl -> Map String Value -> Map String Value
    addGlobal (DDef (DefT name _ args body)) globals =
      M.insert name (VFun [] (length args) $ mockTyper body) globals
    addGlobal _ globals = globals
    defs :: Program
    Right defs = parsedProgramToIr prog

file_expression :: TExpr
file_expression =
  TExpr
    undefined
    (Call (TExpr undefined (Def "main")) (TExpr undefined (Value $ VInt 0)))

file_make_test :: FilePath -> Value -> IO TestTree
file_make_test path expected = do
  contents <- readFile path
  return $
    testCase ("Parsing " ++ show path) $
    case parse Parser.programParser "" contents of
      Left e -> assertFailure $ "Parsing error : " ++ show e
      Right expr ->
        let ctx = mockTypeFile expr
         in case rinter ctx file_expression of
              Left e -> assertFailure $ "Runtime error : " ++ show e
              Right val -> val @?= expected
  where
    rinter :: GlobalContext -> TExpr -> Either RuntimeError Value
    rinter ctx expr =
      runInterpreter ctx (interpretTExpr expr :: ConcreteInterpreterMonad Value)
