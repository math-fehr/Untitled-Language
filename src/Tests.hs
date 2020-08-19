
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Parser
import           Text.ParserCombinators.Parsec
  
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
  
main :: IO ()
main = defaultMain main_tests

main_tests :: TestTree
main_tests = testGroup "Tests" [parser_tests]

parser_tests :: TestTree
parser_tests = testGroup "Parser" [parser_expr_tests]

parser_expr_examples :: [(Parser.Expr, String)]
parser_expr_examples = 
  [ (Parser.IntConst 5, "5")
  , (Parser.BoolConst True,  "true")
  , (Parser.BoolConst False, "false")
  ]
  
parser_make_test :: (Show a, Eq a) => Parser a -> (a, String) -> TestTree
parser_make_test parser (expected, src) = 
  testCase ("Parsing \"" ++ show expected ++ "\"")
         $ case parse parser "test" src of
             Right result -> result @?= expected
             Left error   -> assertFailure $ "Failed to parse :" ++ show error


parser_expr_tests :: TestTree
parser_expr_tests = testGroup "Expr" $ fmap (parser_make_test Parser.exprParser) parser_expr_examples

