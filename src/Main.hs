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
import Data.List (intercalate)

-- FIFTY ONE: A simple language for a simple microcontroller

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

type VarAddr = M.Map String Int
type BranchInd = Int
newtype StateData = StateData (VarAddr,BranchInd)

lookupReg :: String -> S.State StateData Int
lookupReg var = do
  StateData (m, bind) <- get
  case M.lookup var m of
    Just pos -> return pos
    Nothing -> do
                  put $ StateData $ (M.insert var (M.size m) m, bind)
                  return $ M.size m

getBranchId :: S.State StateData Int
getBranchId = do
  StateData (m, bind) <- get
  put $ StateData (m, bind+1)
  return bind

-- AExpr = Var String
--            | IntConst Integer
--            | Neg AExpr
--            | ABinary ABinOp AExpr AExpr
-- 
-- ABinOp = Add
--             | Subtract
--             | Multiply--             | Divide

-- Stores result in R0
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


-- BOOLEAN BINARY THINGS
-- BExpr = BoolConst Bool
--            | Not BExpr
--            | BBinary BBinOp BExpr BExpr
--            | RBinary RBinOp AExpr AExpr
-- 
-- BBinOp = And | Or
-- 
-- RBinOp = Greater | Less

-- 0 is true
-- Store binary stuff in R2

generateBBinOp (And) = return "ANL"
generateBBinOp (Or)  = return "ORL"

generateB (BoolConst b) = return $ if b then "MOV R2, #0"
                                   else "MOV R2, #1"
generateB (Not b) = do
  t1 <- generateB b
  return $ unlines $ [t1 -- Store in R2
                     ,"MOV A, R2"
                     ,"XRL A #1" -- Make false
                     ,"MOV R2, A"] -- Result in R2

generateB (BBinary binop expr1 expr2) = do
  t1 <- generateB expr1 
  t2 <- generateB expr2
  op <- generateBBinOp binop
  return $ unlines $ [t1
                     ,"MOV A, R2" -- Store t1 in A
                     ,t2 -- t2 is in R2
                     ,op ++ "A, R2"
                     ,"MOV R2, A"] -- Result in R2


generateB (RBinary Greater expr1 expr2) = do
  t1 <- generateA expr1
  t2 <- generateA expr2
  return $ unlines ["SETB C 0"
                   ,t1 
                   ,"MOV A, R0" -- t1 in A
                   ,t2 -- t2 in R0
                   ,"SUBB A, R0" -- Set c1 if t2>t1
                   ,"MOV R2, C"] -- Save result in R0

generateB (RBinary Less expr1 expr2) = do
  t1 <- generateA expr1
  t2 <- generateA expr2
  return $ unlines ["SETB C 0"
                   ,t2 
                   ,"MOV A, R0" -- t2 in A
                   ,t1 -- t1 in R0
                   ,"SUBB A, R0" -- Set c1 if t1>t2
                   ,"MOV R2, C"] -- Save result in R0


-- Stmt = Seq [Stmt]
--           | Assign String AExpr
--           | If BExpr Stmt Stmt
--           | While BExpr Stmt
--           | Skip
generate :: Stmt -> S.State StateData String
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



-- If BExpr Stmt Stmt
generate e@(If bexpr stmt1 stmt2) = do
  b <- generateB bexpr
  s1 <- generate stmt1 -- True
  s2 <- generate stmt2 -- False
  ind <- getBranchId
  return $ unlines [";; " ++ show e
                   ,b -- Store in R2, 0 if true
                   ,"MOVE A, R2"
                   ,"JNZ FALSE" ++ show ind  -- goto false if b not true
                   ,"TRUE" ++ show ind ++ ": "
                   ,s1 -- True code
                   ,"SJMP ENDB" ++ show ind -- dont do false as well, jump to end
                   ,"FALSE" ++ show ind ++ ": "
                   ,s2
                   ,"ENDB" ++ show ind ++ ": "]

generate e@(While bexpr stmt) = do
  b <- generateB bexpr
  s <- generate stmt -- True
  ind <- getBranchId
  return $ unlines [";; " ++ show e
                   ,"WHILE" ++ show ind ++ ": "
                   ,b -- Store in R2, 0 if true
                   ,"MOVE A, R2"
                   ,"JNZ ENDB" ++ show ind  -- goto false if b not true
                   ,s -- True code
                   ,"SJMP WHILE" ++ show ind -- dont do false as well, jump to end
                   ,"ENDB" ++ show ind ++ ": "]



generate other = error "Not yet implemented"


generateMain ast = S.evalState (generate ast) $ StateData (M.empty, 0)



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
