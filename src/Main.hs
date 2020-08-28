import Parser
import ParsingToIR
import System.Exit
import Typing

-- checkAndType parsed_ir = do
--   ir <- parsedProgramToIr parsed_ir
--   checkProgramWellTyped ir
--   return ir
main = do
  parsed_ir <- Parser.parseFile "examples/test.ulm"
  ir <-
    case parsedProgramToIr parsed_ir of
      Left err -> do
        putStrLn $ "Error: " ++ show err
        exitFailure
      Right res -> return res
  case typeProgram ir of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      exitFailure
    Right res -> putStrLn $ show res
