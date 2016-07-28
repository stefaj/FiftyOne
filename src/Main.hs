{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L

-- Boolean - For added Boolean Blindness
data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
     deriving (Show)

-- Binary operators
data BBinOp = And | Or
     deriving (Show)

data RBinOp = Greater | Less
     deriving (Show)

-- Arithmetic
data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
      deriving (Show)

-- Arithmetic operators
data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
      deriving (Show)

-- Statements
data Stmt = Seq [Stmt]
          | Assign String AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
      deriving (Show)

-- LEXING BOYS ----------------------------------------------------------------------
-- Space consumer
sc :: Parser ()
sc = L.space (void spaceChar) lineComment blockComment
  where lineComment = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.integer

semi :: Parser String
semi = symbol ";"

-- Never say the r-word
rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

-- Reserved words
rws :: [String]
rws = ["if","then","else","while","do","skip","true","false","not","and","or"]

identifier :: Parser String
identifier = lexeme (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x


-- PARSING BOYS --------------------------------------------------------------------------

whileParser :: Parser Stmt
whileParser = sc *> stmt <* eof

stmt :: Parser Stmt
stmt = parens stmt <|> stmtSeq

stmt' :: Parser Stmt
stmt' = ifStmt <|> whileStmt <|> skipStmt <|> assignStmt

stmtSeq :: Parser Stmt
stmtSeq = f <$> sepBy1 stmt' semi
  where f l@(length -> 1) = head l
        f l = Seq l

ifStmt :: Parser Stmt
ifStmt = do
  rword "if"
  cond <- bExpr
  rword "then"
  stmt1 <- stmt
  rword "else"
  stmt2 <- stmt
  return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt = do
  rword "while"
  cond <- bExpr
  rword "do"
  stmt1 <- stmt
  return $ While cond stmt1

assignStmt :: Parser Stmt
assignStmt = do
  var <- identifier
  void $ symbol ":="
  expr <- aExpr
  return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = Skip <$ rword "skip"

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (symbol "-" *> pure Neg) ]
  , [ InfixL (symbol "*" *> pure (ABinary Multiply))
    , InfixL (symbol "/" *> pure (ABinary Divide)) ]
  , [ InfixL (symbol "+" *> pure (ABinary Add))
    , InfixL (symbol "-" *> pure (ABinary Subtract)) ]
  ]

bOperators :: [[Operator Parser BExpr]]
bOperators =
  [ [Prefix (rword "not" *> pure Not) ]
  , [InfixL (rword "and" *> pure (BBinary And))
    , InfixL (rword "or" *> pure (BBinary Or)) ]
  ]

aTerm :: Parser AExpr
aTerm = parens aExpr
     <|> Var <$> identifier
     <|> IntConst <$> integer


bTerm :: Parser BExpr
bTerm = parens bExpr
     <|> (rword "true" *> pure (BoolConst True))
     <|> (rword "false" *> pure (BoolConst False))
     <|> rExpr

rExpr :: Parser BExpr
rExpr =
  do a1 <- aExpr
     op <- relation
     a2 <- aExpr
     return $ RBinary op a1 a2

relation :: Parser RBinOp
relation =  (symbol ">" *> pure Greater)
        <|> (symbol "<" *> pure Less)

main = do
  f <- getContents
  parseTest whileParser f
