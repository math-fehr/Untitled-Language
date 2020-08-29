--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Parser where

import Control.Lens
import Control.Monad
import System.IO ()
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data BinOp
  = BPlus
  | BMinus
  | BTimes
  | BDiv
  | BUnrestrictedArrow
  | BLinearArrow
  deriving (Ord, Show, Eq)

str2binOp :: String -> BinOp
str2binOp "+" = BPlus
str2binOp "-" = BMinus
str2binOp "*" = BTimes
str2binOp "/" = BDiv
str2binOp "->" = BUnrestrictedArrow
str2binOp "-@" = BLinearArrow

data ManyOp
  = MAmpersand
  | MBar
  | MHat
  | MComma
  deriving (Ord, Show, Eq)

str2manyOp :: String -> ManyOp
str2manyOp "&" = MAmpersand
str2manyOp "|" = MBar
str2manyOp "^" = MHat
str2manyOp "," = MComma

-- Parsed AST
data Expr
  = Var String
  -- TypedVar String Expr
  | IntConst Integer
  | Let
      { var :: String
      , typ :: Maybe Expr
      , val :: Expr
      , body :: Expr
      }
  | Call Expr Expr
  | BinOp BinOp Expr Expr
  | ManyOp ManyOp [Expr]
  | Forall String Expr Expr
  | IfThenElse Expr Expr Expr
  | Lambda String Expr Expr
  | Parens Expr
  deriving (Show, Eq)

data PDefinition =
  PDefinition
    { pdef_name :: String
    , pdef_args :: [String]
    , pdef_body :: Expr
    , pdef_type :: Expr
    }
  deriving (Show, Eq)

data PIndConstructor =
  PIndConstructor
    { pconstr_name :: String
    , pconstr_type :: Expr
    }
  deriving (Show, Eq)

data PInductive =
  PInductive
    { pind_name :: String
    , pind_constrs :: [PIndConstructor]
    }
  deriving (Show, Eq)

data PStruct =
  PStruct
    { ps_name :: String
    , ps_fields :: [(String, Expr)]
    }
  deriving (Show, Eq)

data PDeclaration
  = DefDecl PDefinition
  | IndDecl PInductive
  | StructDecl PStruct
  deriving (Show, Eq)

type Program = [PDeclaration]

-- * Lexer
languageDef =
  emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "//"
    , Token.identStart = letter
    , Token.identLetter = alphaNum
    , Token.nestedComments = True
    , Token.reservedNames =
        [ "let"
        , "in"
        , "def"
        , "fun"
        , "if"
        , "then"
        , "else"
        , "match"
        , "with"
        , "end"
        , "enum"
        , "forall"
        , "of"
        , "struct"
        ]
    , Token.reservedOpNames =
        [ "+"
        , "-"
        , "*"
        , "/"
        , ":="
        , "&"
        , "<"
        , ">"
        , "->"
        , "|"
        , "=>"
        , "-@"
        , "^"
        , ","
        , "{"
        , "}"
        , ";"
        ]
    }

lexer = Token.makeTokenParser languageDef

-- * Atomic parsers
identifier = Token.identifier lexer

reserved = Token.reserved lexer

reservedOp = Token.reservedOp lexer

parens = Token.parens lexer

integer = Token.integer lexer

semicolon = Token.semi lexer

whiteSpace = Token.whiteSpace lexer

varParser :: Parser Expr
varParser = Var <$> identifier

{- * Expression parser
   The precedence are:
   - 4: atomic elements and parenthesised expressions
   - 3: Arithmetic and related binary operations ( + - * / )
   - 2: Many arity operation (^ & | ,)
   - 1: Low precedence binary operators ( -> -@ = > <)
   - 0: Function call

   TODO: Handle precedence below function call
-}
-- | Parse expressions with precedence 4
expr4Parser :: Parser Expr
expr4Parser =
  Parens <$> parens exprParser <|> varParser <|>
  IntConst . fromInteger <$> integer

-- | Parse expressions with precedence 3
expr3Parser :: Parser Expr
expr3Parser = opParser3 <|> expr4Parser

builtinBinOp :: String -> Parser (Expr -> Expr -> Expr)
builtinBinOp op = reservedOp op >> return (BinOp $ str2binOp op)

builtinManyOp :: String -> Parser (Expr -> Expr -> Expr)
builtinManyOp op =
  reservedOp op >>
  return
    (\x y ->
       let currentmo = str2manyOp op
        in case x of
             ManyOp mop es
               | mop == currentmo -> ManyOp currentmo (es ++ [y])
             _ -> ManyOp currentmo ([x, y]))

forallParser :: Parser (Expr -> Expr)
forallParser = do
  reserved "forall"
  (name, typ) <- parens typedVarParser
  reservedOp "->"
  return $ Forall name typ

operatorsList3 =
  [ [Infix (builtinBinOp "*") AssocLeft, Infix (builtinBinOp "/") AssocLeft]
  , [Infix (builtinBinOp "+") AssocLeft, Infix (builtinBinOp "-") AssocLeft]
  , [Prefix forallParser]
  , [Infix (builtinBinOp "->") AssocRight, Infix (builtinBinOp "-@") AssocRight]
  ]

opParser3 :: Parser Expr
opParser3 = buildExpressionParser operatorsList3 expr4Parser

operatorsListMany2 =
  [[Infix (builtinManyOp "&") AssocLeft], [Infix (builtinManyOp "^") AssocLeft]]

manyOpParser1 :: Parser Expr
manyOpParser1 = buildExpressionParser operatorsListMany1 expr3Parser

operatorsListMany1 =
  [[Infix (builtinManyOp "|") AssocLeft], [Infix (builtinManyOp ",") AssocLeft]]

manyOpParser2 :: Parser Expr
manyOpParser2 = buildExpressionParser operatorsListMany2 expr3Parser

-- | Parse expressions with precedence 2
expr2Parser :: Parser Expr
expr2Parser = manyOpParser2 <|> expr3Parser

-- | Parse expressions with precedence 1
expr1Parser :: Parser Expr
expr1Parser = manyOpParser1 <|> expr2Parser

-- | Parse expressions with precedence 0
exprParser :: Parser Expr
exprParser =
  callParser <|> letParser <|> ifParser <|> lambdaParser <|> expr1Parser

-- | Parse let bindings
letParser :: Parser Expr
letParser = do
  reserved "let"
  var <- identifier
  typ <- option Nothing (Just <$> (reservedOp ":" >> exprParser))
  reservedOp ":="
  val <- exprParser
  reserved "in"
  body <- exprParser
  return $ Let {var, typ, val, body}

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

ifParser :: Parser Expr
ifParser = do
  reserved "if"
  cond <- exprParser
  reserved "then"
  trueCase <- exprParser
  reserved "else"
  falseCase <- exprParser
  return $ IfThenElse cond trueCase falseCase

-- Parse a variable that has a type
typedVarParser :: Parser (String, Expr)
typedVarParser = do
  name <- identifier
  reservedOp ":"
  typ <- exprParser
  return (name, typ)

declParser :: Parser (String, Expr)
declParser = do
  reserved "decl"
  name <- identifier
  reservedOp ":"
  typ <- exprParser
  return $ (name, typ)

definitionParser :: Parser PDefinition
definitionParser = do
  (name, typ) <- declParser
  reserved "def"
  args <- many identifier
  reservedOp ":="
  body <- exprParser
  return $ PDefinition name args body typ

indConstructorParser :: Parser PIndConstructor
indConstructorParser = do
  name <- identifier
  reserved "of"
  typ <- expr2Parser
  return $ PIndConstructor name typ

inductiveParser :: Parser PInductive
inductiveParser = do
  reserved "enum"
  name <- identifier
  reservedOp ":="
  reservedOp "{"
  cases <- sepBy indConstructorParser (reservedOp "|")
  reservedOp "}"
  return $ PInductive name cases

structParser :: Parser PStruct
structParser = do
  reserved "struct"
  name <- identifier
  reservedOp ":="
  reservedOp "{"
  fields <- sepBy typedVarParser (reservedOp ";")
  reservedOp "}"
  return $ PStruct name fields

declarationParser :: Parser PDeclaration
declarationParser =
  (DefDecl <$> definitionParser) <|> (IndDecl <$> inductiveParser) <|>
  (StructDecl <$> structParser)

-- Main parser
programParser :: Parser Program
programParser = do
  prog <- whiteSpace >> many declarationParser
  eof
  return $ prog

parseExprFromString :: String -> IO Expr
parseExprFromString str =
  case parse exprParser "" str of
    Left e -> print e >> fail "parse error"
    Right r -> return r

parseFromString :: String -> IO Program
parseFromString str =
  case parse programParser "" str of
    Left e -> print e >> fail "parse error"
    Right r -> return r

-- Parse a file
parseFile :: String -> IO Program
parseFile file = do
  program <- readFile file
  case parse programParser "" program of
    Left e -> print e >> fail "parse error"
    Right r -> return r
