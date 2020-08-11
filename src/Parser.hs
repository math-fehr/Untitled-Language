module Parser where

import System.IO()
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr()
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Parsed AST
data Expr = Var String
  | IntConst Integer
  | BoolConst Bool
  | Assign String Expr Expr
  | Call Expr Expr
  deriving(Show)

data Function = Function { fun_name :: String
                         , fun_args :: [(String, Expr)]
                         , fun_body :: Expr
                         } deriving(Show)

-- Lexer
languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "let"
                                     , "in"
                                     , "true"
                                     , "false"
                                     , "fun"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                     , "<", ">"
                                     ]
           }

lexer = Token.makeTokenParser languageDef

-- Useful tools for parsing
identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
semicolon  = Token.semi       lexer
whiteSpace = Token.whiteSpace lexer

-- Parse assignments
assignParser :: Parser Expr
assignParser =
  do reserved "let"
     var <- identifier
     reservedOp ":="
     expr <- exprParser
     reserved "in"
     body <- exprParser
     return $ Assign var expr body

callParser :: Parser Expr
callParser = do fun <- expr2Parser
                arg <- expr1Parser
                return $ Call fun arg

boolParser :: Parser Bool
boolParser = (reserved "true" >> return True)
             <|> (reserved "false" >> return False)

-- Parse expressions with precedence 2
expr2Parser :: Parser Expr
expr2Parser = parens exprParser
              <|> liftM Var identifier
              <|> liftM IntConst integer
              <|> liftM BoolConst boolParser

-- Parse expressions with precedence 1
expr1Parser :: Parser Expr
expr1Parser = try callParser
              <|> expr2Parser

-- Parse expressions with precedence 0
exprParser :: Parser Expr
exprParser = assignParser <|>
             expr1Parser

-- Parse a variable that has a type
typedVarParser :: Parser (String, Expr)
typedVarParser = parens (do name <- identifier
                            reservedOp ":"
                            typ <- exprParser
                            return (name, typ))

functionParser :: Parser Function
functionParser = do reserved "fun"
                    name <- identifier
                    args <- (many typedVarParser)
                    reservedOp ":="
                    body <- exprParser
                    return $ Function name args body

-- Main parser
programParser :: Parser [Function]
programParser = whiteSpace >> many functionParser

-- Parse a file
parseFile :: String -> IO [Function]
parseFile file = do program <- readFile file
                    case parse programParser "" program of
                      Left e -> print e >> fail "parse error"
                      Right r -> return r
