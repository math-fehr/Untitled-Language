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
whiteSpace = Token.whiteSpace lexer

-- Parse assignments
assignParser :: Parser Expr
assignParser =
  do reserved "let"
     var <- identifier
     reservedOp ":="
     expr <- expr0Parser
     reserved "in"
     body <- expr0Parser
     return $ Assign var expr body

callParser :: Parser Expr
callParser = do fun <- expr2Parser
                arg <- expr1Parser
                return $ Call fun arg

-- Parse booleans
boolParser :: Parser Bool
boolParser = (reserved "true" >> return True)
             <|> (reserved "false" >> return False)

expr2Parser :: Parser Expr
expr2Parser = liftM Var identifier
              <|> liftM IntConst integer
              <|> liftM BoolConst boolParser

-- Parse expressions
expr1Parser :: Parser Expr
expr1Parser = try callParser
              <|> expr2Parser

-- Parse expressions
expr0Parser :: Parser Expr
expr0Parser = assignParser <|>
              expr1Parser

-- Parser that exclude whitespaces
parser :: Parser Expr
parser = whiteSpace >> expr0Parser

-- Parse a file
parseFile :: String -> IO Expr
parseFile file = do program <- readFile file
                    case parse parser "" program of
                      Left e -> print e >> fail "parse error"
                      Right r -> return r
