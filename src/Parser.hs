{-# LANGUAGE TemplateHaskell #-}

module Parser where

import Control.Lens
import Control.Lens
import Control.Monad
import System.IO ()
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data BinOp
  = BPlus
  | BTimes
  | BAmpersand
  | BUnrestrictedArrow
  | BLinearArrow
  deriving (Show, Eq)

-- Parsed AST
data Expr
  = Var String
  | TypedVar String Expr
  | IntConst Int
  | BoolConst Bool
  | Assign String Expr Expr
  | Call Expr Expr
  | IfThenElse Expr Expr Expr
  | Lambda String Expr Expr
  deriving (Show, Eq)

data PDefinition =
  PDefinition
    { pdef_name :: String
    , pdef_body :: Expr
    , pdef_type :: Expr
    }
  deriving (Show, Eq)

type Program = [PDefinition]

-- Lexer
languageDef =
  emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "//"
    , Token.identStart = letter
    , Token.identLetter = alphaNum
    , Token.reservedNames =
        [ "let"
        , "in"
        , "true"
        , "false"
        , "def"
        , "fun"
        , "if"
        , "then"
        , "else"
        , "match"
        , "with"
        , "end"
        , "enum"
        ]
    , Token.reservedOpNames =
        ["+", "-", "*", "/", ":=", "&", "<", ">", "->", "|", "=>", "-o"]
    }

lexer = Token.makeTokenParser languageDef

-- Useful tools for parsing
identifier = Token.identifier lexer

reserved = Token.reserved lexer

reservedOp = Token.reservedOp lexer

parens = Token.parens lexer

integer = Token.integer lexer

semicolon = Token.semi lexer

whiteSpace = Token.whiteSpace lexer

-- Parse assignments
assignParser :: Parser Expr
assignParser = do
  reserved "let"
  var <- identifier
  reservedOp ":="
  expr <- exprParser
  reserved "in"
  body <- exprParser
  return $ Assign var expr body

-- Parse lambda functions
lambdaParser :: Parser Expr
lambdaParser = do
  reserved "fun"
  (var, typ) <- parens typedVarParser
  reservedOp "=>"
  body <- exprParser
  return $ Lambda var typ body

exprListToCall :: [Expr] -> Expr
exprListToCall = foldl1 Call

callParser :: Parser Expr
callParser = exprListToCall <$> sepBy1 expr1Parser (return ())

boolParser :: Parser Bool
boolParser =
  (reserved "true" >> return True) <|> (reserved "false" >> return False)

ifParser :: Parser Expr
ifParser = do
  reserved "if"
  cond <- exprParser
  reserved "then"
  trueCase <- exprParser
  reserved "else"
  falseCase <- exprParser
  return $ IfThenElse cond trueCase falseCase

opParser :: Parser Expr
opParser = buildExpressionParser operatorsList expr2Parser

builtinOp :: String -> Parser (Expr -> Expr -> Expr)
builtinOp op = reservedOp op >> return (\x1 x2 -> Call (Call (Var op) x1) x2)

operatorsList =
  [ [ Infix (builtinOp "+") AssocLeft
    , Infix (builtinOp "*") AssocLeft
    , Infix (builtinOp "&") AssocLeft
    ]
  , [Infix (builtinOp "->") AssocRight, Infix (builtinOp "-o") AssocRight]
  ]

varParser :: Parser Expr
varParser =
  identifier >>= \name ->
    (TypedVar name <$> (reservedOp ":" >> exprParser)) <|> return (Var name)

-- Parse expressions with precedence 2
expr2Parser :: Parser Expr
expr2Parser =
  parens exprParser <|> varParser <|> (IntConst . fromInteger) <$> integer <|>
  BoolConst <$> boolParser

-- Parse expressions with precedence 1
expr1Parser :: Parser Expr
expr1Parser = opParser <|> expr2Parser

-- Parse expressions with precedence 0
exprParser :: Parser Expr
exprParser =
  callParser <|> assignParser <|> ifParser <|> lambdaParser <|> expr1Parser

-- Parse a variable that has a type
typedVarParser :: Parser (String, Expr)
typedVarParser = do
  name <- identifier
  reservedOp ":"
  typ <- exprParser
  return (name, typ)

definitionParser :: Parser PDefinition
definitionParser = do
  reserved "def"
  name <- identifier
  reservedOp ":"
  typ <- exprParser
  reservedOp ":="
  body <- exprParser
  return $ PDefinition name body typ

-- Main parser
programParser :: Parser Program
programParser = whiteSpace >> many definitionParser

-- Parse a file
parseFile :: String -> IO Program
parseFile file = do
  program <- readFile file
  case parse programParser "" program of
    Left e -> print e >> fail "parse error"
    Right r -> return r
