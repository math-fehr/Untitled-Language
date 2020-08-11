module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Parsed AST
data Expr = Var String
  | IntConst Integer
  | BoolConst Bool
  | Assign String Expr Expr
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
     expr <- exprParser
     reserved "in"
     body <- exprParser
     return $ Assign var expr body

-- Parse booleans
boolParser :: Parser Bool
boolParser = (reserved "true" >> return True)
             <|> (reserved "false" >> return False)

-- Parse expressiosn
exprParser :: Parser Expr
exprParser = assignParser
             <|> liftM Var identifier
             <|> liftM IntConst integer
             <|> liftM BoolConst boolParser

-- Parser that exclude whitespaces             
parser :: Parser Expr
parser = whiteSpace >> exprParser

-- Parse a file
parseFile :: String -> IO Expr
parseFile file = do program <- readFile file
                    case parse parser "" program of
                      Left e -> print e >> fail "parse error"
                      Right r -> return r