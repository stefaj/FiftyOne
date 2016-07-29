{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String 
import qualified Data.Text as T
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map as M
import qualified Control.Monad.State.Lazy as S
import Control.Monad.State.Lazy (get,put)

-- FIFTY ONE: A simple language for a simple microcontroller

-- PARSING EXPRESSION TREE -----------------------------

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

instance Show ABinOp where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"

-- Statements
data Stmt = Seq [Stmt]
          | Assign String AExpr
          | Declare String AExpr
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
rws = ["if","then","else","while","do","skip","true","false","not","and","or", "var"]

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
stmt' = ifStmt <|> whileStmt <|> skipStmt <|> declareStmt <|> assignStmt

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
  void $ symbol "<-"
  expr <- aExpr
  return $ Assign var expr

declareStmt :: Parser Stmt
declareStmt = do
  rword "var"
  var <- identifier
  void $ symbol "<-"
  expr <- aExpr
  return $ Declare var expr



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

-- Code Generation Boys

type Code = String

lookupReg :: String -> S.State (M.Map String Int) Int
lookupReg var = do
  m <- get
  case M.lookup var m of
    Just pos -> return pos
    Nothing -> do
                  put $ M.insert var (M.size m) m
                  return $ M.size m

-- AExpr = Var String
--            | IntConst Integer
--            | Neg AExpr
--            | ABinary ABinOp AExpr AExpr
-- 
-- ABinOp = Add
--             | Subtract
--             | Multiply--             | Divide


generateA (Var s) = do
                      reg <- lookupReg s
                      return $ unlines ["MOV R1, " ++ show reg
                                       ,"MOV R0, @R1"]

generateA (IntConst int) = return $ "MOV R0, #" ++ show int
-- generateA (Negate ----)  TODO
generateA (ABinary Add a b) = do 
                                t1 <- generateA a
                                t2 <- generateA b
                                return $ unlines [
                                        t1 -- Result is in R0
                                       ,"MOV A, R0" -- t1 is now in A
                                       ,t2 -- t2 is now in R0
                                       ,"ADD A, R0" -- Add t1 and t2
                                       ,"MOV R0, A"] -- Store result in R0
generateA x = return $ show x

-- Stmt = Seq [Stmt]
--           | Assign String AExpr
--           | If BExpr Stmt Stmt
--           | While BExpr Stmt
--           | Skip
generate :: Stmt -> S.State (M.Map String Int) String
generate (Seq stmts) = unlines <$> mapM generate stmts
generate Skip = return $ "NOP"
generate e@(Assign str expr) = do
  reg <- lookupReg str
  t1 <- generateA expr
  return $ unlines [";; " ++ show e
                   ,"MOV R1, #" ++ show reg
                   ,t1 -- generate expression and store result in R0
                   ,"MOV @R1, R0"]
generate (Declare str expr) = generate (Assign str expr) -- same as above for now

generate other = return $ show other


generateMain ast = S.evalState (generate ast) $ M.empty



main = do
  f <- getContents
--  putStrLn "Parse:"
  case parse whileParser "" f of
    Left err -> print err
    Right ast -> do
--      print ast
--      putStrLn ""
--      putStrLn "OUT:"
      putStrLn $ generateMain ast
