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
  | Lambda String Expr Expr
  deriving(Show)

data PDefinition = PDefinition
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
                                     , "def"
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

-- Parse lambda functions
lambdaParser :: Parser Expr
lambdaParser =
  do reserved "fun"
     (var, typ) <- typedVarParser
     reserved "=>"
     body <- exprParser
     return $ Lambda var typ body

exprListToCall :: [Expr] -> Expr
exprListToCall [e] = e
exprListToCall (e1 : e2 : es) = exprListToCall ((Call e1 e2) : es)

callParser :: Parser Expr
callParser = do exprs <- sepBy1 expr1Parser (return $ ())
                return $ exprListToCall exprs

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
opParser = buildExpressionParser operatorsList expr2Parser

operatorsList = [ [ Infix  (reservedOp "->" >> return Arrow) AssocLeft ] ]

-- Parse expressions with precedence 2
expr2Parser :: Parser Expr
expr2Parser = parens exprParser
              <|> liftM Var identifier
              <|> liftM (\x -> IntConst $ fromInteger x) integer
              <|> liftM BoolConst boolParser

-- Parse expressions with precedence 1
expr1Parser :: Parser Expr
expr1Parser = opParser
              <|> expr2Parser

-- Parse expressions with precedence 0
exprParser :: Parser Expr
exprParser = ifParser <|>
             assignParser <|>
             lambdaParser <|>
             callParser

-- Parse a variable that has a type
typedVarParser :: Parser (String, Expr)
typedVarParser = parens (do name <- identifier
                            reservedOp ":"
                            typ <- exprParser
                            return (name, typ))

definitionParser :: Parser PDefinition
definitionParser = do reserved "def"
                      name <- identifier
                      args <- (many typedVarParser)
                      reservedOp ":"
                      typ <- exprParser
                      reservedOp ":="
                      body <- exprParser
                      return $ PDefinition name args body typ

-- Main parser
programParser :: Parser [PDefinition]
programParser = whiteSpace >> many definitionParser

-- Parse a file
parseFile :: String -> IO [PDefinition]
parseFile file = do program <- readFile file
                    case parse programParser "" program of
                      Left e -> print e >> fail "parse error"
                      Right r -> return r


