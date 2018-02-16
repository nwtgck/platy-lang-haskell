{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

-- TODO: Don't expose all function
module Platy.Codegen where

import Control.Monad.State
import qualified Data.Char
import qualified Data.String.Here as Here
import Control.Monad (mapM_)
import Control.Monad as Monad
import Data.String      (IsString(..))
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Foldable as Foldable
import qualified Control.Monad.Trans as Monad.Trans
import Control.Applicative ((<|>))
import qualified Data.Either.Utils as Either.Utils

import qualified LLVM.AST as AST
import LLVM.AST( Named( (:=) ) )
import qualified LLVM.Quote.LLVM as Quote.LLVM
import qualified LLVM.Module as Module
import qualified LLVM.Context as Context
import qualified LLVM.AST.Type as AST.Type
import qualified LLVM.AST.Constant as AST.Constant
import qualified LLVM.AST.Global as AST.Global
import qualified LLVM.AST.AddrSpace as AST.AddrSpace
import qualified LLVM.AST.CallingConvention as AST.CallingConvention

import Debug.Trace

import Platy.Constants
import Platy.Utils
import Platy.Datatypes
import Platy.CodegenUtils
import qualified Platy.NativeStdlib as  NativeStdlib

data IdentInfo =
  GVarIdentInfo {ty :: Ty, globalPtrName :: AST.Name} |
  LVarIdentInfo {ty :: Ty, localName :: AST.Name} |
  FuncIdentInfo {retTy :: Ty, paramTys :: [Ty], funcName :: AST.Name}
  deriving (Show)

type VarTable = Map Ident IdentInfo

data ExprCodegenEnv =
 ExprCodegenEnv {
   basicBlocks     :: [AST.Global.BasicBlock]
 , stackedLabels   :: [AST.Name]                  -- latest is in first
 , stackedInstrs   :: [AST.Named AST.Instruction] -- latest is in last
 , globalVarTable  :: VarTable
 , localVarTables  :: [VarTable]
 , count           :: Int
 }
 deriving (Show)

type CodegenError = ()

newtype ExprCodegen a = ExprCodegen {runExprCodegen :: StateT ExprCodegenEnv (Either CodegenError) a}
  deriving (Functor, Applicative, Monad, MonadState ExprCodegenEnv)

-- | Get fresh int count (zero-origin)
getFreshCount :: ExprCodegen Int
getFreshCount = do
  c <- gets count
  modify (\env -> env {count = c+1})
  return c


-- | Set current label
addLabel :: AST.Name -> ExprCodegen ()
addLabel label = do
  modify (\env@ExprCodegenEnv{stackedLabels} -> env{stackedLabels=label:stackedLabels})


-- | Set terminate a block with terminator
setTerminator :: Named AST.Terminator -> ExprCodegen ()
setTerminator terminator = do
  -- Get current label
  label:_ <- gets stackedLabels
  -- Pop the first label
  modify (\env@ExprCodegenEnv{stackedLabels=_:rest} -> env{stackedLabels=rest})
  -- Get stacked instructions
  sInstrs <- gets stackedInstrs
  -- Create a basic block with stacked instructions
  let bb = AST.Global.BasicBlock label sInstrs terminator
  -- Clear stacked instructions
  modify (\env -> env{stackedInstrs=[]})
  -- Add a basic block
  modify (\env@ExprCodegenEnv{basicBlocks} -> env {basicBlocks = basicBlocks++[bb]})


-- | Stack an instruction
stackInstruction :: Named AST.Instruction -> ExprCodegen ()
stackInstruction instr = do
  modify (\env@ExprCodegenEnv{stackedInstrs} -> env{stackedInstrs=stackedInstrs++[instr]})


-- | Push & Pop a local variable map
withLVarTable :: Map Ident IdentInfo -> ExprCodegen a -> ExprCodegen a
withLVarTable lVarMap f = do
  -- Push `lVarMap` to  `localVarTables`
  modify (\env@ExprCodegenEnv{localVarTables} -> env{localVarTables=lVarMap:localVarTables})
  ret <- f
  -- Pop lVarMap` from  `localVarTables`
  modify (\env@ExprCodegenEnv{localVarTables=_:rest} -> env{localVarTables=rest})
  return ret

-- | Expr => Operand
exprToExprCodegen :: Expr Ty -> ExprCodegen AST.Operand
exprToExprCodegen (LitExpr {lit}) = return (litToOperand lit)
exprToExprCodegen (IdentExpr {ident=ident@(Ident name)}) = do
  -- Get local variable tables
  lVarTables <- gets localVarTables
  -- Get global variable table
  gVarTable  <- gets globalVarTable
  -- Find ident from tables (NOTE: Semantic analysis keeps it safe)
  let Just indentInfo = lookupMaps ident lVarTables <|> Map.lookup ident gVarTable
  case indentInfo of
    GVarIdentInfo{ty, globalPtrName} -> do
      let llvmTy    = tyToLLVMTy ty
          llvmPtrTy = AST.Type.ptr llvmTy
      -- Get fresh count for prefix of loaded value of global variable
      freshCount <- getFreshCount
      let loadedGVarName = AST.Name (strToShort [Here.i|$$global${freshCount}|])
      let callInstr = [Quote.LLVM.lli| load $type:llvmPtrTy $gid:globalPtrName |]
      -- Stack the call instruction
      stackInstruction (loadedGVarName := callInstr)
      return $ AST.LocalReference llvmTy loadedGVarName

    LVarIdentInfo{ty, localName} -> do
      let llvmTy    = tyToLLVMTy ty
      return $ AST.LocalReference llvmTy localName

    FuncIdentInfo{} -> fail [Here.i|Unexpected function name '${name}'|]
exprToExprCodegen (LetExpr {binds, inExpr}) = do
  -- Get fresh prefix number
  prefixNumber <- getFreshCount

  -- TODO: Rename better
  let f :: Map Ident IdentInfo -> Bind Ty -> ExprCodegen (Map Ident IdentInfo)
      f lVarMap (Bind {ident=ident@(Ident name), ty, bodyExpr}) = do
        withLVarTable lVarMap $ do -- NOTE: push & pop (lVarMap)
          -- Eval bodyExpr to operand
          bodyOpr <- exprToExprCodegen bodyExpr
          -- Local variable name
          let localName = AST.Name (strToShort [Here.i|$$local${prefixNumber}/${name}|])
          let newlVarMap = Map.insert ident (LVarIdentInfo{ty=ty, localName=localName}) lVarMap
          -- Stack an instruction of local definition
          stackInstruction (localName := [Quote.LLVM.lli| $opr:bodyOpr |])
          return newlVarMap

  -- Define all binds
  localVariableMap <- Foldable.foldlM f (Map.empty :: Map Ident IdentInfo) binds
  inOpr <- withLVarTable localVariableMap $ do -- NOTE: push & pop (localVariableMap)
    -- Eval inExpr of local `let` to operand
    exprToExprCodegen inExpr

  return inOpr
exprToExprCodegen (ApplyExpr {calleeIdent=calleeIdent@(Ident calleeName), argExprs}) = do
  -- Get fresh prefix number
  prefixNumber <- getFreshCount
  -- Get global variable table
  gVarTable <- gets globalVarTable
  -- Result name
  let resName = AST.Name (strToShort [Here.i|$$apply_res${prefixNumber}|])
  -- Get identifier information (NOTE: Semantic analysis keeps it safe)
  let Just identInfo = Map.lookup calleeIdent gVarTable
  case identInfo of
    FuncIdentInfo{retTy, paramTys, funcName} -> do
      -- Get type
      let llvmRetTy = tyToLLVMTy retTy
      -- Create function LLVM type
      let llvmFuncTy = AST.Type.ptr
                         AST.FunctionType {
                             AST.resultType    = llvmRetTy
                           , AST.argumentTypes = fmap tyToLLVMTy paramTys
                           , AST.isVarArg      = False
                           }

      -- Eval argExprs to operands
      argOprs <- mapM exprToExprCodegen argExprs

      -- Get LLVM arguments
      -- NOTE: [] may be hard code
      let llvmArgs = fmap (\argExpr -> (argExpr, [])) argOprs

      -- Call instruction
      -- NOTE: Why llvm-hs-quote isn't used here? => Because llvm-has-quote doesn't have an antiquote for argument (Issue: https://github.com/llvm-hs/llvm-hs-quote/issues/18)
      let callInst = resName := AST.Call {
                                  AST.tailCallKind       = Nothing,
                                  AST.callingConvention  = AST.CallingConvention.C,
                                  AST.returnAttributes   = [],
                                  AST.function           = Right $ AST.ConstantOperand $ AST.Constant.GlobalReference llvmFuncTy funcName,
                                  AST.arguments          = llvmArgs,
                                  AST.functionAttributes = [],
                                  AST.metadata           = []
                                }

      -- Stack the call instruction
      stackInstruction callInst

      return $ AST.LocalReference llvmRetTy resName
    _  -> fail ([Here.i|Should be function identifier |])

exprToExprCodegen (ifexpr@IfExpr {anno, condExpr, thenExpr, elseExpr}) = do
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

  -- Get global variable table
  gVarTable  <- gets globalVarTable
  -- Get local variable tables
  lVarTables <- gets localVarTables
  -- Get type
  let ty     =  anno
  -- Get LLVM type
  let llvmTy = tyToLLVMTy ty

  -- ==== Condition ====
  -- Eval condExpr to Operand
  condOperand <- exprToExprCodegen condExpr
  stackInstruction (ifcondValueName := [Quote.LLVM.lli|$opr:condOperand|])
  setTerminator (AST.Do [Quote.LLVM.llt|br i1 $id:ifcondValueName, label $id:thenLabel, label $id:elseLabel|])


  -- ==== Then Block ====
  -- Set then-label
  addLabel (thenLabel)
  -- Eval thenExpr to Operand
  -- (NOTE: ORDER IS IMPORTANT: exprToExprCodegen can add new Basic Blocks)
  thenOperand <- exprToExprCodegen thenExpr
  -- Add a instruction
  stackInstruction (thenValueName := [Quote.LLVM.lli|$opr:thenOperand|])
  -- Set terminator of then
  setTerminator (AST.Do [Quote.LLVM.llt|br label $id:endLabel|])

  -- ==== Then Block ====
  -- Set else-label
  addLabel (elseLabel)
  -- Eval elseExpr to Operand
  -- (NOTE: ORDER IS IMPORTANT: exprToExprCodegen can add new Basic Blocks)
  elseOperand <- exprToExprCodegen elseExpr
  -- Add a instruction
  stackInstruction (elseValueName := [Quote.LLVM.lli|$opr:elseOperand|])
  -- Set terminator of then
  setTerminator (AST.Do [Quote.LLVM.llt|br label $id:endLabel|])

  -- ==== End-if ====
  addLabel (endLabel)
  stackInstruction (endValueName := [Quote.LLVM.lli|phi $type:llvmTy [$id:thenValueName, $id:thenLabel], [$id:elseValueName, $id:elseLabel]|])

  return $ AST.LocalReference llvmTy endValueName


-- | Expr Ty => Operand
exprToOperandEither :: VarTable -> [VarTable] -> Expr Ty -> Either CodegenError (AST.Operand, ExprCodegenEnv)
exprToOperandEither globalVarTable localVarTables expr = runStateT (runExprCodegen $ exprToExprCodegen expr) initEnv
  where
    initEnv = ExprCodegenEnv {
                basicBlocks    = []
              , stackedLabels  = [AST.Name "entry"]
              , stackedInstrs  = []
              , globalVarTable = globalVarTable
              , localVarTables = localVarTables
              , count          = 0
              }

data GdefCodegenEnv =
 GdefCodegenEnv {
   definitions         :: [AST.Definition]
 , globalInitFuncs     :: [AST.Definition]
 }
 deriving (Show)

newtype GdefCodegen a = GdefCodegen {runGdefCodegen :: StateT GdefCodegenEnv (Either CodegenError) a}
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

-- | Generate global ptr name
genGlobalVarName :: Ident -> AST.Name
genGlobalVarName (Ident name) = AST.Name (strToShort [Here.i|${name}/ptr|])

-- | Generate function name
genFuncName :: Ident -> AST.Name
genFuncName (Ident name) = AST.Name (strToShort name)

-- | Generate parameter name
genParamName :: Ident -> AST.Name
genParamName (Ident name) = AST.Name (strToShort [Here.i|$$param/${name}|])

-- Gdef => GdefCodegen
gdefToGdefCodegen :: VarTable -> Gdef Ty -> GdefCodegen ()
gdefToGdefCodegen globalVarTable (LetGdef (Bind {ident=ident@(Ident name), ty, bodyExpr})) = do
  let globalName   = genGlobalVarName ident
      initFuncName = AST.Name (strToShort [Here.i|$$PLATY_INIT/${name}|])
      llvmTy       = tyToLLVMTy ty
      llvmPtrTy    = AST.Type.ptr llvmTy
  let globalDef = [Quote.LLVM.lldef| $gid:globalName = global $type:llvmTy undef |]
  -- Add the definition
  addDefinition globalDef
  let operandEither = exprToOperandEither globalVarTable [] bodyExpr
  -- Get operand and env
  (bodyOperand, ExprCodegenEnv{basicBlocks, stackedInstrs, stackedLabels=[lastLabel]}) <- GdefCodegen (Monad.Trans.lift operandEither)
  -- NOTE: Should avoid to using PLATY_GLOBAL_RES if an user uses this name then fail
  let funcDef = AST.GlobalDefinition AST.Global.functionDefaults
        { AST.Global.name        = initFuncName
        , AST.Global.parameters  =([], False)
        , AST.Global.returnType  = AST.Type.void
        , AST.Global.basicBlocks = basicBlocks ++ [restBasicBlock]
        }
        where
          restBasicBlock = AST.Global.BasicBlock
              lastLabel
              -- NOTE: stackedInstrs added
              (stackedInstrs ++ [
                AST.Name "PLATY_GLOBAL_RES" := [Quote.LLVM.lli| $opr:bodyOperand |],
                AST.Do [Quote.LLVM.lli| store $type:llvmTy %PLATY_GLOBAL_RES, $type:llvmPtrTy $gid:globalName |]
              ])
              (AST.Do [Quote.LLVM.llt| ret void |])

  -- (This comment is for the future fundDef)
  -- (Issue about $instrs: https://github.com/llvm-hs/llvm-hs-quote/issues/16)
  -- (Issue about Empty Basic Block: https://github.com/llvm-hs/llvm-hs-quote/issues/17)
--       [Quote.LLVM.lldef|
--        define void $gid:initFuncName(){
--        entry:
--          $bbs:basicBlocks
--          $instrs:stackedInstrs
--          %PLATY_GLOBAL_RES = $opr:bodyOperand
--          store $type:llvmTy %PLATY_GLOBAL_RES, $type:llvmPtrTy $gid:globalName
--          ret void
--        }
--      |]
  -- Add the init-function
  addInitFunc funcDef

  return ()

gdefToGdefCodegen globalVarTable (FuncGdef {ident=ident@(Ident name), params, retTy, bodyExpr}) = do
  -- Variable of parameters
  let paramVarTable = Map.fromList [(ident, LVarIdentInfo{ty=ty, localName=genParamName ident}) | Param{ident, ty} <- params]
  -- Generate LLVM parameters
  let llvmParams    = [AST.Global.Parameter (tyToLLVMTy ty) (genParamName ident) [] | Param{ident, ty} <- params]
  -- Evaluate bodyExpr
  let operandEither = exprToOperandEither globalVarTable [paramVarTable] bodyExpr
  -- Get operand and env
  (bodyOperand, ExprCodegenEnv{basicBlocks, stackedInstrs, stackedLabels=[lastLabel]}) <- GdefCodegen (Monad.Trans.lift operandEither)

  let funcName  = if (ident == entrypointFuncIdent)
                    then langEntrypointFuncName
                    else genFuncName ident
      llvmRetTy = tyToLLVMTy retTy

  -- NOTE: Should avoid to using PLATY_GLOBAL_RES if an user uses this name then fail
  let funcDef = AST.GlobalDefinition AST.Global.functionDefaults
        { AST.Global.name        = funcName
        , AST.Global.parameters  =(llvmParams, False)
        , AST.Global.returnType  = llvmRetTy
        , AST.Global.basicBlocks = basicBlocks ++ [restBasicBlock]
        }
        where
          restBasicBlock = AST.Global.BasicBlock
              lastLabel
              -- NOTE: stackedInstrs added
              (stackedInstrs ++ [
                AST.Name "PLATY_GLOBAL_RES" := [Quote.LLVM.lli| $opr:bodyOperand |]
              ])
              (AST.Do [Quote.LLVM.llt| ret $type:llvmRetTy %PLATY_GLOBAL_RES |])

  -- (This comment is for the future fundDef)
--         [Quote.LLVM.lldef|
--          define $type:llvmRetTy $gid:funcName(){
--          entry:
--            $bbs:basicBlocks
--            %PLATY_GLOBAL_RES = $opr:bodyOperand
--            ret $type:llvmRetTy %PLATY_GLOBAL_RES
--          }
--        |]
  -- Add to the definitions
  addDefinition funcDef

  Monad.when (ident == entrypointFuncIdent) $ do
    let mainDef = [Quote.LLVM.lldef|
      define i32 @main(){
        call i1 $gid:langEntrypointFuncName()
        ret i32 0
      }
    |]
    addDefinition (mainDef)


  return ()


-- | Program Ty => AST.Module
programToModule :: Program Ty -> Either CodegenError AST.Module
programToModule Program{gdefs} = do
  let initEnv = GdefCodegenEnv {
                  definitions         = []
                , globalInitFuncs     = []
                }
      f (LetGdef (Bind {ident, ty})) = (ident, GVarIdentInfo {ty=ty, globalPtrName=genGlobalVarName ident})
      f (FuncGdef {ident, retTy, params})    = (ident, FuncIdentInfo {retTy=retTy, paramTys=[ty | Param {ty} <- params], funcName=genFuncName ident})
      globalVarMap = Map.fromList (fmap f gdefs)

      stdVarMap = Map.map (\NativeStdlib.FuncNativeGdef {retTy,paramTys,funcLLVMName} ->
                           FuncIdentInfo
                           { retTy
                           , paramTys
                           , funcName = funcLLVMName
                           })
                       NativeStdlib.stdlibNativeGdefMap

  GdefCodegenEnv{definitions, globalInitFuncs} <- execStateT (runGdefCodegen $ mapM_ (gdefToGdefCodegen (Map.union globalVarMap stdVarMap)) gdefs) initEnv

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
      NativeStdlib.stdlibLLVMDefs ++ [globalCtorsDef, globalInitFuncDef]++ definitions ++ globalInitFuncs
  }