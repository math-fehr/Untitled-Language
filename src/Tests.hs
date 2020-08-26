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
  return $ testGroup "Tests" [ran_tests]

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
