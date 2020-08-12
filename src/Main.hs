import Parser
import ParsingToIR

main = do result <- Parser.parseFile "examples/test-parser.ulm"
          putStrLn $ show $ parsedProgramToIr result
