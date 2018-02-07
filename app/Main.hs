{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.State
import qualified LLVM.AST as AST
import LLVM.AST( Named( (:=) ) )
import qualified LLVM.Quote.LLVM as Quote.LLVM
import qualified LLVM.Module as Module
import qualified LLVM.Context as Context
import qualified LLVM.AST.Type as AST.Type
import qualified LLVM.AST.Constant as AST.Constant
import qualified LLVM.AST.Global as AST.Global

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
 }

-- TODO: Change better type
type ErrorType = String

newtype ExprCodegen a = ExprCodegen {runExprCodegen :: StateT ExprCodegenEnv (Either ErrorType) a}
  deriving (Functor, Applicative, Monad, MonadState ExprCodegenEnv)


-- TODO: Impl
exprToCodegen :: Expr -> ExprCodegen AST.Operand
exprToCodegen (LitExpr lit) = return (AST.ConstantOperand AST.Constant.Int {AST.Constant.integerBits=32, AST.Constant.integerValue=868686}) -- TODO: impl


-- | Expr => Operand
exprToOperandEither :: Expr -> Either ErrorType AST.Operand
exprToOperandEither expr = evalStateT (runExprCodegen $ exprToCodegen expr) initEnv
  where
    initEnv = ExprCodegenEnv {
                 basicBlocks = []
              }

-- TODO: Implement
main :: IO ()
main = do
  let expr1 = LitExpr (IntLit 1515)
  let a = exprToOperandEither expr1
  print a
