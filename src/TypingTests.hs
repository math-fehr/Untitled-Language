{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main where

import Text.ParserCombinators.Parsec

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens ((^.))
import Data.List
import Data.Map ((!))
import System.Directory

import IR
import Typing
import Error

main :: IO ()
main = do
  ran_tests <- tests
  defaultMain ran_tests

-- | All tests
tests :: IO TestTree
tests = do
  return $ testGroup "Typing" [typing_monad_tests]

-- | TypingMonad tests
data TypingMonadTest m where
  TMTestEq :: (Eq a, Show a) => String -> a -> m a -> TypingMonadTest m
  TMTestFailure
    :: (Show a) => String -> (Error -> Bool) -> m a -> TypingMonadTest m

runTest :: TypingMonad m => TypingMonadTest m -> TestTree
runTest (TMTestEq name expected action) =
  testCase name $
  case runTyping action of
    Left err -> assertFailure $ "Typing failed with " ++ show err
    Right x -> x @?= expected
runTest (TMTestFailure name checkErr action) =
  testCase name $
  case runTyping action of
    Right x -> assertFailure $ "Expected failure, got \"" ++ show x ++ "\""
    Left err ->
      if checkErr err
        then True @?= True
        else assertFailure $ "Expected another failure, got " ++ show err

internalP :: Error -> Bool
internalP (InternalError _) = True
internalP _ = False

unknownP :: Error -> Bool
unknownP (UnknownVariable _) = True
unknownP _ = False

reuseP :: Error -> Bool
reuseP (ReusedLinear _ _) = True
reuseP _ = False

typing_monad_tests_data :: TypingMonad m => [TypingMonadTest m]
typing_monad_tests_data =
  [ TMTestEq "Registering" (Registered d) $
    register "main" d >> globalStatus "main"
  , TMTestEq "Not registered" Undeclared $ globalStatus "main"
  , TMTestFailure "Decl unregistered" internalP $ decl "main" (\d -> return t)
  , TMTestEq "Declaring" (Declared et d) $
    register "main" d >> decl "main" (const $ return et) >> globalStatus "main"
  , TMTestEq "Defining" (Defined $ TValue (VInt 42) et) $
    register "main" d >> decl "main" (const $ return et) >>
    def "main" (const $ const $ return $ TValue (VInt 42) et) >>
    globalStatus "main"
  , TMTestEq "Add linear" "x" $ addLinear "x" et Nothing >> leaveScope
  , TMTestEq "Add unrestricted" "x" $
    addUnrestricted "x" et Nothing >> leaveScope
  , TMTestFailure "Use undefined" unknownP $ useVar $ DeBruijn 0
  , TMTestEq "Use" LinearUsed $
    let v = DeBruijn 0
     in addLinear "x" et Nothing >> useVar v >> variableStatus v
  , TMTestEq "Double use ok" () $
    addLinear "x" et Nothing >> (useVar $ DeBruijn 0) >> (useVar $ DeBruijn 0)
  ]
  where
    d = DDef (DefT "main" (Expr SourcePos $ Value $ TValue (VType et) t) [] e)
    t = Type True TType
    et = Type True $ TInt $ IntType 64 False True
    e = Expr SourcePos $ Value $ TValue (VInt 42) et

typing_monad_tests :: TestTree
typing_monad_tests =
  testGroup
    "Monad"
    [ testGroup "ConcreteTypingMonad" $
      fmap
        runTest
        (typing_monad_tests_data :: [TypingMonadTest ConcreteTypingMonad])
    ]
