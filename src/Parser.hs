module Parser where

import System.IO()
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Parsed AST
data Expr =
  Var String
  | IntConst Int
  | BoolConst Bool
  | Assign String Expr Expr
  | Call Expr Expr
  | IfThenElse Expr Expr Expr
  | Arrow Expr Expr
  deriving(Show)

data PFunction = PFunction
  { pfun_name :: String
  , pfun_args :: [(String, Expr)]
  , pfun_body :: Expr
  , pwfun_type :: Expr
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
                                     , "if"
                                     , "then"
                                     , "else"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                     , "<", ">", "->"
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

ifParser :: Parser Expr
ifParser = do reserved "if"
              cond <- exprParser
              reserved "then"
              trueCase <- exprParser
              reserved "else"
              falseCase <- exprParser
              return $ IfThenElse cond trueCase falseCase

opParser :: Parser Expr
opParser = buildExpressionParser operatorsList expr3Parser

operatorsList = [ [ Infix  (reservedOp "->" >> return Arrow) AssocLeft ] ]

-- Parse expressions with precedence 3
expr3Parser :: Parser Expr
expr3Parser = parens exprParser
              <|> liftM Var identifier
              <|> liftM (\x -> IntConst $ fromInteger x) integer
              <|> liftM BoolConst boolParser

-- Parse expressions with precedence 2
expr2Parser :: Parser Expr
expr2Parser = opParser
              <|> expr3Parser

-- Parse expressions with precedence 1
expr1Parser :: Parser Expr
expr1Parser = try callParser
              <|> expr2Parser

-- Parse expressions with precedence 0
exprParser :: Parser Expr
exprParser = ifParser <|>
             assignParser <|>
             expr1Parser

-- Parse a variable that has a type
typedVarParser :: Parser (String, Expr)
typedVarParser = parens (do name <- identifier
                            reservedOp ":"
                            typ <- exprParser
                            return (name, typ))

functionParser :: Parser PFunction
functionParser = do reserved "fun"
                    name <- identifier
                    args <- (many typedVarParser)
                    reservedOp ":"
                    typ <- exprParser
                    reservedOp ":="
                    body <- exprParser
                    return $ PFunction name args body typ

-- Main parser
programParser :: Parser [PFunction]
programParser = whiteSpace >> many functionParser

-- Parse a file
parseFile :: String -> IO [PFunction]
parseFile file = do program <- readFile file
                    case parse programParser "" program of
                      Left e -> print e >> fail "parse error"
                      Right r -> return r


