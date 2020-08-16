module BoolTest where

import IR
import IRUtils
import Control.Monad

-- Check if a definition has only boolean parameters and results
isBooleanDef :: Definition -> Bool
isBooleanDef (Definition _ args typ _) =
  typ == Const BoolType &&
  all ((== Const BoolType) . snd) args

-- Generate all possible boolean lists of a given size
generateAllBoolCombinations :: Int -> [[Bool]]
generateAllBoolCombinations 0 = [[]]
generateAllBoolCombinations n =
  let combs = generateAllBoolCombinations (n-1) in
    fmap (True :) combs ++ fmap (False :) combs

-- Call with a list of boolean arguments
callBoolList :: IR.Expr -> [Bool] -> IR.Expr
callBoolList fun args = callArgumentList fun (fmap (Const . IR.BoolConst) args)

-- Cast a constant boolean expression into a boolean
toBoolean :: IR.Expr -> Bool
toBoolean (Const (IR.BoolConst b)) = b
toBoolean e = error $ "toBoolean was called with " ++ show e

-- Run a definition with all possible combinations of arguments
runBooleanDef :: Program -> Definition -> IO ()
runBooleanDef p def =
  if not (isBooleanDef def) then
    return ()
  else
    let all_booleans = generateAllBoolCombinations (length $ def_args def) in
    let exprs = fmap (callBoolList (getExprFromDef def)) all_booleans in
    let results = zip all_booleans $ fmap (callByValue p) exprs in
    let print_results = foldM (\_ (inputs, output) -> (putStrLn $ show inputs ++ " " ++ show (toBoolean output))) () results in
      print_results

-- Run every boolean definition with all possible combinations of arguments
runBooleanProgram :: Program -> IO ()
runBooleanProgram p = foldM (\_ def -> (putStrLn $ "Function " ++ show (def_name def)) >> runBooleanDef p def >> putStrLn "") () (prog_defs p)
