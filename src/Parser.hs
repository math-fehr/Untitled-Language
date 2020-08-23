{-# LANGUAGE TemplateHaskell #-}

module Parser where

import Control.Lens
import Control.Monad
import System.IO ()
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data PMatchCase =
  PMatchCase
    { pcase_constr :: String
    , pcase_args :: [String]
    , pcase_expr :: Expr
    }
  deriving (Show, Eq)

-- Parsed AST
data Expr
  = Var String
  | IntConst Int
  | BoolConst Bool
  | Assign String Expr Expr
  | Call Expr Expr
  | IfThenElse Expr Expr Expr
  | Arrow Expr Expr
  | Lambda String Expr Expr
  | Match Expr [PMatchCase]
  deriving (Show, Eq)

data PDefinition =
  PDefinition
    { pdef_name :: String
    , pdef_args :: [(String, Expr)]
    , pdef_body :: Expr
    , pdef_type :: Expr
    }
  deriving (Show, Eq)

data PInductiveConstructor =
  PInductiveConstructor
    { _pconstr_name :: String
    , _pconstr_args :: [(String, Expr)]
    }

data PInductive =
  PInductive
    { _pind_name :: String
    , _pind_args :: [(String, Expr)]
    , _pind_constr :: [PInductiveConstructor]
    }

makeLenses ''PInductive

makeLenses ''PInductiveConstructor

data DeclarationType
  = InductiveDecl PInductive
  | DefinitionDecl PDefinition

type Program = [DeclarationType]

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
        ["+", "-", "*", "/", ":=", "<", ">", "->", "|", "=>"]
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
  (var, typ) <- typedVarParser
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

-- Parse a case in a pattern match
caseParser :: Parser PMatchCase
caseParser = do
  reserved "|"
  (constr:args) <- many1 identifier
  reservedOp "=>"
  expr <- exprParser
  return $ PMatchCase constr args expr

-- parse a pattern matching expression
matchParser :: Parser Expr
matchParser = do
  reserved "match"
  expr <- exprParser
  reserved "with"
  cases <- many caseParser
  reserved "end"
  return $ Match expr cases

opParser :: Parser Expr
opParser = buildExpressionParser operatorsList expr2Parser

operatorsList = [[Infix (reservedOp "->" >> return Arrow) AssocLeft]]

-- Parse expressions with precedence 2
expr2Parser :: Parser Expr
expr2Parser =
  parens exprParser <|> Var <$> identifier <|>
  (IntConst . fromInteger) <$> integer <|>
  BoolConst <$> boolParser

-- Parse expressions with precedence 1
expr1Parser :: Parser Expr
expr1Parser = opParser <|> expr2Parser

-- Parse expressions with precedence 0
exprParser :: Parser Expr
exprParser =
  callParser <|> assignParser <|> ifParser <|> matchParser <|> lambdaParser

-- Parse a variable that has a type
typedVarParser :: Parser (String, Expr)
typedVarParser =
  parens
    (do name <- identifier
        reservedOp ":"
        typ <- exprParser
        return (name, typ))

definitionParser :: Parser DeclarationType
definitionParser = do
  reserved "def"
  name <- identifier
  args <- (many typedVarParser)
  reservedOp ":"
  typ <- exprParser
  reservedOp ":="
  body <- exprParser
  return $ DefinitionDecl $ PDefinition name args body typ

-- An inductive constructor parser
inductiveConstructorParser :: Parser PInductiveConstructor
inductiveConstructorParser = do
  reservedOp "|"
  name <- identifier
  args <- (many typedVarParser)
  return $ PInductiveConstructor name args

-- An inductive parser
inductiveParser :: Parser DeclarationType
inductiveParser = do
  reserved "enum"
  name <- identifier
  args <- (many typedVarParser)
  reservedOp ":="
  constructors <- many inductiveConstructorParser
  return $ InductiveDecl $ PInductive name args constructors

-- Main parser
programParser :: Parser Program
programParser = whiteSpace >> many (definitionParser <|> inductiveParser)

-- Parse a file
parseFile :: String -> IO Program
parseFile file = do
  program <- readFile file
  case parse programParser "" program of
    Left e -> print e >> fail "parse error"
    Right r -> return r
