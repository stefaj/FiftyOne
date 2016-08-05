module CodeGen (generateMain)
  where

import qualified Data.Map as M
import qualified Control.Monad.State.Lazy as S
import Control.Monad.State.Lazy (get,put)
import Data.List (intercalate)
import Parser 

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


generate e@(Write var reg) = do
  let port = regToPortName reg  
  reg <- lookupReg var
  return $ unlines [";;" ++ show e
                   ,"MOV R1, #" ++ show reg
                   ,"MOV " ++ port ++ ", " ++ "@R1"]

generate e@(Read var reg) = do
  let port = regToPortName reg  
  reg <- lookupReg var
  return $ unlines [";;" ++ show e
                   ,"MOV R1, " ++ port
                   ,"MOV R0, #" ++ show reg
                   ,"MOV @R0, R1"]


generate _ = error "Not yet implemented"

regToPortName :: Register -> Code
regToPortName (Port1) = "P1"


generateMain ast = S.evalState (generate ast) $ StateData (M.empty, 0)
