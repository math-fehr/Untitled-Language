import Parser
import ParsingToIR
import IRCheck
import IRUtils
import Typing
import IR
import BoolTest

import Control.Monad
import Data.Map

checkAndType parsed_ir =
    do let ir = parsedProgramToIr parsed_ir
       checkIR ir
       checkProgramWellTyped ir
       return ir

main = do parsed_ir <- Parser.parseFile "examples/test.ulm"
          case checkAndType parsed_ir of
            Left err -> putStrLn $ "Error: " ++ show err
            Right res -> runBooleanProgram res
