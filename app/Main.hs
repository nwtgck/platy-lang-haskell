{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad.State
import qualified Data.Char
import qualified Data.String.Here as Here
import Data.String.Interpolate as Interpolate
import Control.Monad (mapM_)
import qualified Data.ByteString.Char8 as BS
import Data.String      (IsString(..))
import Data.ByteString.Short
import qualified Data.Text.Lazy.IO as TIO

import qualified LLVM.AST as AST
import LLVM.AST( Named( (:=) ) )
import qualified LLVM.Quote.LLVM as Quote.LLVM
import qualified LLVM.Module as Module
import qualified LLVM.Context as Context
import qualified LLVM.AST.Type as AST.Type
import qualified LLVM.AST.Constant as AST.Constant
import qualified LLVM.AST.Global as AST.Global
import qualified LLVM.Pretty


import Debug.Trace

-- ====== Global Language Settings ======
nIntBits = 32
globalInitFuncName = AST.Name "PLATY_GLOBALS_INIT"


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
  deriving (Show)

-- Global definition
data Gdef =
  LetGdef  {bind :: Bind} |
  FuncGdef {ident :: Ident, params :: [Param], retTy :: Ty, bodyExpr :: Expr}
  deriving (Show)

-- | Ty => LLVM Type
tyToLLVMTy :: Ty -> AST.Type
tyToLLVMTy IntTy  = AST.IntegerType {typeBits=nIntBits}
tyToLLVMTy CharTy = AST.IntegerType {typeBits=8}
tyToLLVMTy BoolTy = AST.IntegerType {typeBits=1}
tyToLLVMTy UnitTy = AST.IntegerType {typeBits=1}

-- | Lit => LLVM Type
litToLLVMType :: Lit -> AST.Type
litToLLVMType (IntLit _)  = AST.IntegerType {typeBits=nIntBits}
litToLLVMType (CharLit _) = AST.IntegerType {typeBits=8}
litToLLVMType (BoolLit _) = AST.IntegerType {typeBits=1}
litToLLVMType (UnitLit)   = AST.IntegerType {typeBits=1}

-- | Expr => LLVM Type
exprToLLVMType :: Expr -> AST.Type
exprToLLVMType (LitExpr lit)       = litToLLVMType lit
exprToLLVMType (IdentExpr _)       = undefined -- TODO impl
exprToLLVMType (IfExpr {thenExpr}) = exprToLLVMType thenExpr
exprToLLVMType (ApplyExpr{})       = undefined -- TODO impl
exprToLLVMType (LetExpr{inExpr})   = exprToLLVMType inExpr



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
      ifcondValueName  = AST.Name (strToShort [Here.i|${labelPrefix}/cond|])
      thenValueName    = AST.Name (strToShort [Here.i|${labelPrefix}/thenRes|])
      elseValueName    = AST.Name (strToShort [Here.i|${labelPrefix}/elseRes|])
      endValueName     = AST.Name (strToShort [Here.i|${labelPrefix}/endRes|])

      startLabel  = AST.Name (strToShort [Here.i|${labelPrefix}/start|])
      thenLabel   = AST.Name (strToShort [Here.i|${labelPrefix}/then|])
      elseLabel   = AST.Name (strToShort [Here.i|${labelPrefix}/else|])
      endLabel    = AST.Name (strToShort [Here.i|${labelPrefix}/end|])

  -- Eval condExpr to Operand
  condOperand <- exprToExprCodegen condExpr

  -- FIXME: In the future, llbb may be added, then rewrite
  let AST.GlobalDefinition AST.Global.Function{basicBlocks=[ifStartBlock]} = [Quote.LLVM.lldef|
    define void @_(){
    $id:startLabel
      $id:ifcondValueName = $opr:condOperand
      br i1 $id:ifcondValueName, label $id:thenLabel, label $id:elseLabel
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
    $id:thenLabel
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
      $id:endValueName = phi i32 [$id:thenValueName, $id:thenLabel], [$id:elseValueName, $id:elseLabel]
    }
  |]
  -- Add endblock
  addBasicBlock endBlock

  let ty = exprToLLVMType thenExpr
  return $ AST.LocalReference ty endValueName


-- | Expr => Operand
exprToOperandEither :: Expr -> Either ErrorType (AST.Operand, ExprCodegenEnv)
exprToOperandEither expr = runStateT (runExprCodegen $ exprToExprCodegen expr) initEnv
  where
    initEnv = ExprCodegenEnv {
                basicBlocks = []
              , count       = 0
              }

-- (for preventing "\22" in [Here.i])
strToShort :: String -> ShortByteString
strToShort = (toShort . BS.pack)

data GdefCodegenEnv =
 GdefCodegenEnv {
   definitions         :: [AST.Definition]
 , globalInitFuncs     :: [AST.Definition]
 }
 deriving (Show)

newtype GdefCodegen a = GdefCodegen {runGdefCodegen :: StateT GdefCodegenEnv (Either ErrorType) a}
  deriving (Functor, Applicative, Monad, MonadState GdefCodegenEnv)


-- Add a definition
addDefinition :: AST.Definition -> GdefCodegen ()
addDefinition def = do
  defs <- gets definitions
  modify (\env -> env{definitions=defs++[def]})

-- Add an init-funcitoin
addInitFunc :: AST.Definition -> GdefCodegen ()
addInitFunc initFunc = do
  funcs <- gets globalInitFuncs
  modify (\env -> env{globalInitFuncs=funcs++[initFunc]})

-- TODO: impl
gdefToGdefCodegen :: Gdef -> GdefCodegen ()
gdefToGdefCodegen (LetGdef (Bind {ident=Ident name, ty, bodyExpr})) = do
  let globalName   = AST.Name (strToShort [Here.i|${name}/ptr|])
      initFuncName = AST.Name (strToShort [Here.i|$$PLATY_INIT/${name}|])
      llvmTy       = tyToLLVMTy ty
      llvmPtrTy    = AST.Type.ptr llvmTy
  let globalDef = [Quote.LLVM.lldef| $gid:globalName = global $type:llvmTy undef |]
  -- Add the definition
  addDefinition globalDef

  case exprToOperandEither bodyExpr of
    Right (bodyOperand, ExprCodegenEnv{basicBlocks}) -> do
      -- NOTE: Should avoid to using PLATY_GLOBAL_RES if an user uses this name then fail
      let funcDef   =
           -- (this if is for just avoid empty $bbs)
           if Prelude.null basicBlocks
            then
              [Quote.LLVM.lldef|
                define void $gid:initFuncName(){
                entry:
                  %PLATY_GLOBAL_RES = $opr:bodyOperand
                  store $type:llvmTy %PLATY_GLOBAL_RES, $type:llvmPtrTy $gid:globalName
                  ret void
                }
              |]
            else
             [Quote.LLVM.lldef|
              define void $gid:initFuncName(){
              entry:
                $bbs:basicBlocks
                ;%PLATY_GLOBAL_RES = $opr:bodyOperand
                ;store $type:llvmTy %PLATY_GLOBAL_RES, $type:llvmPtrTy $gid:globalName
                ret void
              }
            |]
      -- Add the init-function
      addInitFunc funcDef

      return ()

    Left error -> fail error



-- | [Gdef] => AST.Module
gdefsToModule :: [Gdef] -> Either ErrorType AST.Module
gdefsToModule gdefs = do
  let initEnv = GdefCodegenEnv {
                  definitions         = []
                , globalInitFuncs     = []
                }
  GdefCodegenEnv{definitions, globalInitFuncs} <- execStateT (runGdefCodegen $ mapM_ gdefToGdefCodegen gdefs) initEnv

  let globalCtorsDef = [Quote.LLVM.lldef|
   @llvm.global_ctors = appending global [1 x { i32, void ()*, i8* }]
    [{ i32, void ()*, i8* } { i32 65535, void ()* $gid:globalInitFuncName, i8* null }]
  |]

  let -- Names of global init-funcs
      globalInitFuncNames :: [AST.Name]
      globalInitFuncNames = [name | AST.GlobalDefinition AST.Global.Function{name} <- globalInitFuncs]

      -- Call instructions of init-funcs
      initFuncCalls :: [AST.Named AST.Instruction]
      initFuncCalls = fmap (\name -> AST.Do [Quote.LLVM.lli| call void $gid:name() |]) globalInitFuncNames

      -- (FIXME: It's better to use $instrs instead of %bb, but currently $instrs doesn't work well)
      globalInitFuncBlock = AST.Global.BasicBlock (AST.Name "entry") initFuncCalls (AST.Do AST.Ret {AST.returnOperand = Nothing, AST.metadata' = []})
      globalInitFuncDef = [Quote.LLVM.lldef|
        define void $gid:globalInitFuncName(){
          $bb:globalInitFuncBlock
        }
      |]

  return AST.defaultModule{
    AST.moduleDefinitions =
      [globalCtorsDef, globalInitFuncDef]++ definitions ++ globalInitFuncs
  }


toLLVM :: AST.Module -> IO ()
toLLVM mod = Context.withContext $ \ctx -> do
  llvm <- Module.withModuleFromAST ctx mod Module.moduleLLVMAssembly
  BS.putStrLn llvm

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

  putStrLn("====================================")
  let gdef1 = LetGdef {bind=Bind {ident=Ident "myint", ty=IntTy, bodyExpr=IfExpr (LitExpr $ BoolLit True) (LitExpr $ IntLit 81818) (LitExpr $ IntLit 23232)}}
  let gdef2 = LetGdef {bind=Bind {ident=Ident "myint2", ty=IntTy, bodyExpr=IfExpr (LitExpr $ BoolLit True) (LitExpr $ IntLit 7117) (LitExpr $ IntLit 9889)}}
  let Right mod1 = gdefsToModule [gdef1, gdef2]
--  TIO.putStrLn (LLVM.Pretty.ppllvm mod1)
  putStrLn("----------------------------------")

  toLLVM mod1

  let gdef2 = LetGdef {bind=Bind {ident=Ident "myint", ty=IntTy, bodyExpr=LitExpr $ IntLit 2929}}
  let mod2Either = gdefsToModule [gdef2]
  let Right mod2 = mod2Either
--  print mod2
  putStrLn("----------------------------------")
--  TIO.putStrLn (LLVM.Pretty.ppllvm mod2)
--  putStrLn("----------------------------------")
--  TIO.putStrLn (LLVM.Pretty.ppll mod2)
--  putStrLn("----------------------------------")
  toLLVM mod2

