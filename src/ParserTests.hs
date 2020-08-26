{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.ParserCombinators.Parsec

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens ((^.))
import Data.List
import Data.Map ((!))
import System.Directory

import Parser

main :: IO ()
main = do
  ran_tests <- tests
  defaultMain ran_tests

-- | All tests
tests :: IO TestTree
tests = do
  ran_file_tests <- file_tests
  return $ testGroup "Parser" [parser_expr_tests, ran_file_tests]

-- | A list of examples
parser_expr_examples :: [(Parser.Expr, String)]
parser_expr_examples = [(Parser.IntConst 5, "5"), (Parser.IntConst 42, "42")]

-- | Parse an element and check the equality against an expected element
parser_make_test :: (Show a, Eq a) => Parser a -> (a, String) -> TestTree
parser_make_test parser (expected, src) =
  testCase ("Parsing \"" ++ show expected ++ "\"") $
  case parse parser "test" src of
    Right result -> result @?= expected
    Left err -> assertFailure $ "Failed to parse :" ++ show err

-- | Parse some expressions and check that they are correctly parsed
parser_expr_tests :: TestTree
parser_expr_tests =
  testGroup "Expr" $
  fmap (parser_make_test Parser.exprParser) parser_expr_examples

-- Parse a file
file_make_test :: FilePath -> IO TestTree
file_make_test path = do
  contents <- readFile path
  return $
    testCase ("Parsing" ++ show path) $
    case parse programParser "" contents of
      Left e -> assertFailure $ "Parsing error : " ++ show e
      Right _ -> True @?= True

-- Parse all files in "./examples/run"
file_tests :: IO TestTree
file_tests = do
  files <- listDirectory "./examples/run"
  let tests_files = filter (isSuffixOf ".ulm") files
      tests_paths = ("./examples/run/" ++) <$> tests_files
      ran_tests = file_make_test <$> tests_paths
  testGroup "Files" <$> sequence ran_tests
