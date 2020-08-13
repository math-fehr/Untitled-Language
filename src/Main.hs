import Parser
import ParsingToIR
import IRCheck

main = do parsed_ir <- Parser.parseFile "examples/test-parser.ulm"
          let ir = parsedProgramToIr parsed_ir in
            case checkIR ir of
              Nothing -> putStrLn . show $ ir
              Just err -> putStrLn . show $ err
