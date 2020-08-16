import Parser
import ParsingToIR
import Typing
import BoolTest

checkAndType parsed_ir =
    do ir <- parsedProgramToIr parsed_ir
       checkProgramWellTyped ir
       return ir

main = do parsed_ir <- Parser.parseFile "examples/test.ulm"
          case checkAndType parsed_ir of
            Left err -> putStrLn $ "Error: " ++ show err
            Right res -> runBooleanProgram res
