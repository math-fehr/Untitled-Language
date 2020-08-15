import Parser
import ParsingToIR
import IRCheck
import Typing

checkAndType parsed_ir =
    do let ir = parsedProgramToIr parsed_ir
       checkIR ir
       checkProgramWellTyped ir
       return ir

main = do parsed_ir <- Parser.parseFile "examples/test-parser.ulm"
          case checkAndType parsed_ir of
            Left err -> putStrLn $ "Error: " ++ show err
            Right res -> putStrLn $ "Result: " ++ show res
