{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.ParserCombinators.Parsec

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens ((^.))
import Data.List
import Data.Map ((!))
import System.Directory

import Error
import IR
import IRUtils
import Parser
import ParsingToIR
import Typing

main :: IO ()
main = do
  tests <- main_tests
  defaultMain tests

main_tests :: IO TestTree
main_tests = do
  ran_tests <- run_tests
  return $ testGroup "Tests" [parser_tests, ran_tests]

parser_tests :: TestTree
parser_tests = testGroup "Parser" [parser_expr_tests]

parser_expr_examples :: [(Parser.Expr, String)]
parser_expr_examples =
  [ (Parser.IntConst 5, "5")
  , (Parser.BoolConst True, "true")
  , (Parser.BoolConst False, "false")
  ]

parser_make_test :: (Show a, Eq a) => Parser a -> (a, String) -> TestTree
parser_make_test parser (expected, src) =
  testCase ("Parsing \"" ++ show expected ++ "\"") $
  case parse parser "test" src of
    Right result -> result @?= expected
    Left err -> assertFailure $ "Failed to parse :" ++ show err

parser_expr_tests :: TestTree
parser_expr_tests =
  testGroup "Expr" $
  fmap (parser_make_test Parser.exprParser) parser_expr_examples

-- Check a program
checkAndType :: Parser.Program -> Either Error IR.Program
checkAndType parsed_ir = do
  ir <- parsedProgramToIr parsed_ir
  checkProgramWellTyped ir
  return ir

-- Parse and check a file
parseCheckAndType :: FilePath -> IO (Either Error IR.Program)
parseCheckAndType path = do
  parsed_ir <- Parser.parseFile path
  return $ checkAndType parsed_ir

-- Parse and type a file, execute the main function, and check that the result
-- is equal to the integer 0
run_make_test :: FilePath -> IO TestTree
run_make_test path = do
  prog <- parseCheckAndType path
  case prog of
    Right p ->
      let res = callByValue p $ getExprFromDef $ (p ^. prog_defs) ! "main"
       in return $
          testCase ("Running" ++ show path) $ res @?= Const (IR.IntConst 0)
    Left err -> assertFailure $ "Error : " ++ show err

-- Run all main function in files in "./examples/run"
run_tests :: IO TestTree
run_tests = do
  files <- listDirectory "./examples/run"
  let tests_files = filter (isSuffixOf ".ulm") files
      tests_paths = ("./examples/run/" ++) <$> tests_files
      ran_tests = run_make_test <$> tests_paths
  testGroup "Run" <$> sequence ran_tests
