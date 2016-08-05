{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser (BExpr(..)
              ,RBinOp(..)
              ,BBinOp(..)
              ,AExpr(..)
              ,ABinOp(..)
              ,Register(..)
              ,Stmt(..)
              ,whileParser
              )
  where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String 
import qualified Data.Text as T
import qualified Text.Megaparsec.Lexer as L
import Data.List (intercalate)

-- PARSING EXPRESSION TREE -----------------------------

-- Boolean - For added Boolean Blindness
data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr

-- Binary operators
data BBinOp = And | Or

data RBinOp = Greater | Less

instance Show RBinOp where
  show Greater = ">"
  show Less = "<"

instance Show BBinOp where
  show And = "and"
  show Or = "or"

instance Show BExpr where
  show (BoolConst b) = show b
  show (Not b) = "not"
  show (BBinary binop expr1 expr2) = unwords [show expr1, show binop, show expr2]
  show (RBinary binop expr1 expr2) = unwords [show expr1, show binop, show expr2]

-- Arithmetic
data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr

instance Show AExpr where
  show (Var s) = s
  show (IntConst i) = show i
  show (Neg a) = "-" ++ show a 
  show (ABinary abinop a1 a2) = unwords [show a1, show abinop, show a2]

-- Arithmetic operators
data ABinOp = Add
            | Subtract
            | Multiply
            | Divide

data Register = Port1

instance Show ABinOp where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"

-- Statements
data Stmt = Seq [Stmt]
          | Assign String AExpr
          | Write String Register
          | Read String Register
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip

instance Show Stmt where
  show Skip = "Skip"
  show (While bexpr stmt) = unwords ["While ", show bexpr, " do ", show stmt, " end"]
  show (If bexpr stmt1 stmt2) = unwords ["If ", show bexpr, " then ", show stmt1
                                        , " else ", show stmt2, " end"]
  show (Assign var expr) = unwords [var, " <- ", show expr]
  show (Seq stmts) = unwords ["(", intercalate "; " $ map show stmts, ")"]

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
rws = ["if","then","else","while","do","skip","true","false","not","and","or", "var", "end"]

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
stmt' = ifStmt <|> whileStmt <|> skipStmt <|> assignStmt <|> readStmt <|> writeStmt

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
  rword "end"
  return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt = do
  rword "while"
  cond <- bExpr
  rword "do"
  stmt1 <- stmt
  rword "end"
  return $ While cond stmt1

assignStmt :: Parser Stmt
assignStmt = do
  var <- identifier
  void $ symbol "<-"
  expr <- aExpr
  return $ Assign var expr

-- <<

parsePort :: Parser Register
parsePort = do
  rword "port1"
  return $ Port1

writeStmt :: Parser Stmt
writeStmt = do
  var <- identifier
  void $ symbol ">>"
  reg <- parsePort
  return $ Write var reg

readStmt :: Parser Stmt
readStmt = do
  var <- identifier
  void $ symbol "<<"
  reg <- parsePort
  return $ Read var reg

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



