import Parser

main = do result <- Parser.parseFile "examples/test-parser.ulm"
          putStrLn $ show result