{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad.State
import qualified Data.Char
import qualified Data.String.Here as Here

import qualified LLVM.AST as AST
import LLVM.AST( Named( (:=) ) )
import qualified LLVM.Quote.LLVM as Quote.LLVM
import qualified LLVM.Module as Module
import qualified LLVM.Context as Context
import qualified LLVM.AST.Type as AST.Type
import qualified LLVM.AST.Constant as AST.Constant
import qualified LLVM.AST.Global as AST.Global


-- ====== Global Language Settings ======
nIntBits = 32


-- TODO: Move datatypes to a single file

-- | Literal
data Lit =
  IntLit Int   |
  CharLit Char |
  BoolLit Bool |
  UnitLit
  deriving (Show)

-- | Identifier
data Ident = Ident String
  deriving (Show)

-- | Type
data Ty =
  IntTy  |
  CharTy |
  BoolTy |
  UnitTy
  deriving (Show)

-- | Bind (definition)
data Bind = Bind {ident :: Ident, ty :: Ty, bodyExpr :: Expr}
  deriving (Show)

-- | Expression
data Expr =
  LitExpr Lit |
  IdentExpr Ident |
  IfExpr {condExpr :: Expr, thenExpr :: Expr, elseExpr :: Expr} |
  ApplyExpr {calleeExpr :: Expr, params :: [Expr]} | -- NOTE: params can be Expr instead of [Expr] in the future
  LetExpr {binds :: [Bind], inExpr :: Expr}
  deriving (Show)

-- | Parameter
data Param = Param {ident :: Ident, ty :: Ty}

-- Global definition
data Gdef =
  LetGdef  {bind :: Bind} |
  FuncGdef {ident :: Ident, params :: [Param], retTy :: Ty, bodyExpr :: Expr}






data ExprCodegenEnv =
 ExprCodegenEnv {
   basicBlocks :: [AST.Global.BasicBlock]
 , count       :: Int
 }
 deriving (Show)

-- TODO: Change better type
type ErrorType = String

newtype ExprCodegen a = ExprCodegen {runExprCodegen :: StateT ExprCodegenEnv (Either ErrorType) a}
  deriving (Functor, Applicative, Monad, MonadState ExprCodegenEnv)

getFreshCount :: ExprCodegen Int
getFreshCount = do
  c <- gets count
  let newC = c + 1
  modify (\env -> env {count = newC})
  return newC


-- | Lit => Operand
litToOperand :: Lit -> AST.Operand
litToOperand (IntLit i)   = AST.ConstantOperand AST.Constant.Int {AST.Constant.integerBits=nIntBits, AST.Constant.integerValue=toInteger i}
litToOperand (CharLit ch) = AST.ConstantOperand AST.Constant.Int {AST.Constant.integerBits=8, AST.Constant.integerValue=toInteger $ Data.Char.ord ch}
litToOperand (BoolLit b)  = AST.ConstantOperand AST.Constant.Int {AST.Constant.integerBits=1, AST.Constant.integerValue=if b then 1 else 0}
litToOperand (UnitLit)    = AST.ConstantOperand AST.Constant.Int {AST.Constant.integerBits=1, AST.Constant.integerValue=1}

-- | Add a basic block
addBasicBlock :: AST.Global.BasicBlock -> ExprCodegen ()
addBasicBlock bb = do
  -- Get current basic blocks
  bbs <- gets basicBlocks
  -- Add a basic block
  modify (\env -> env {basicBlocks = bbs++[bb]})


-- TODO: Impl
exprToExprCodegen :: Expr -> ExprCodegen AST.Operand
exprToExprCodegen (LitExpr lit) = return (litToOperand lit)
exprToExprCodegen (IfExpr {condExpr, thenExpr, elseExpr}) = do
  -- Get fresh count for prefix of label
  freshCount <- getFreshCount
  -- Label prefix
  let labelPrefix = [Here.i|$$if${freshCount}|] :: String
      ifcondValueName  = AST.Name [Here.i|${labelPrefix}/cond|]
      thenValueName    = AST.Name [Here.i|${labelPrefix}/thenRes|]
      elseValueName    = AST.Name [Here.i|${labelPrefix}/elseRes|]
      endValueName     = AST.Name [Here.i|${labelPrefix}/endRes|]

      startLabel   = AST.Name [Here.i|${labelPrefix}/start|]
      thenLabel   = AST.Name [Here.i|${labelPrefix}/then|]
      elseLabel   = AST.Name [Here.i|${labelPrefix}/else|]
      endLabel    = AST.Name [Here.i|${labelPrefix}/end|]

  -- Eval condExpr to Operand
  condOperand <- exprToExprCodegen condExpr

  -- FIXME: In the future, llbb may be added, then rewrite
  let AST.GlobalDefinition AST.Global.Function{basicBlocks=[ifStartBlock]} = [Quote.LLVM.lldef|
    define void @_(){
    $id:startLabel
      $id:ifcondValueName = $opr:condOperand
      br i1 %ifcond, label $id:thenLabel, label $id:elseLabel
    }
  |]
  -- Add then block
  addBasicBlock ifStartBlock

  -- Eval thenExpr to Operand
  -- (NOTE: ORDER IS IMPORTANT: exprToExprCodegen can add new Basic Blocks)
  thenOperand <- exprToExprCodegen thenExpr

  -- Create elseBlock
  -- FIXME: In the future, llbb may be added, then rewrite
  let AST.GlobalDefinition AST.Global.Function{basicBlocks=[thenBlock]} = [Quote.LLVM.lldef|
    define void @_(){
    $id:elseLabel
      $id:thenValueName = $opr:thenOperand
      br label $id:endLabel
    }
  |]
  -- Add thenblock
  addBasicBlock thenBlock

  -- Eval elseExpr to Operand
  -- (NOTE: ORDER IS IMPORTANT: exprToExprCodegen can add new Basic Blocks)
  elseOperand <- exprToExprCodegen elseExpr

  -- Create elseBlock
  -- FIXME: In the future, llbb may be added, then rewrite
  let AST.GlobalDefinition AST.Global.Function{basicBlocks=[elseBlock]} = [Quote.LLVM.lldef|
    define void @_(){
    $id:elseLabel
      $id:elseValueName = $opr:elseOperand
      br label $id:endLabel
    }
  |]
  -- Add elseblock
  addBasicBlock elseBlock

  -- Create ifend
  -- FIXME: In the future, llbb may be added, then rewrite
  let AST.GlobalDefinition AST.Global.Function{basicBlocks=[endBlock]} = [Quote.LLVM.lldef|
    define void @_(){
    $id:endLabel
      $id:endValueName = phi i32 [$id:thenValueName, $id:thenLabel], [$id:thenValueName, $id:elseLabel]
    }
  |]
  -- Add endblock
  addBasicBlock endBlock

  let ty = AST.Type.i32 -- TODO: Implement (Find type from then-operand or other information...)
  return $ AST.LocalReference ty endValueName


-- | Expr => Operand
exprToOperandEither :: Expr -> Either ErrorType (AST.Operand, ExprCodegenEnv)
exprToOperandEither expr = runStateT (runExprCodegen $ exprToExprCodegen expr) initEnv
  where
    initEnv = ExprCodegenEnv {
                basicBlocks = []
              , count       = 0
              }

-- TODO: Implement
main :: IO ()
main = do
  let expr1 = LitExpr (IntLit 1515)
  let Right (operand1, exprCodeEnv1) = exprToOperandEither expr1
  print operand1
  print exprCodeEnv1

  let expr2 = IfExpr (LitExpr $ BoolLit True) (LitExpr $ IntLit 81818) (LitExpr $ IntLit 23232)
  let Right (operand2, exprCodeEnv2) = exprToOperandEither expr2
  print operand2
  print exprCodeEnv2
