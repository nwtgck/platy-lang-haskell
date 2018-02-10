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
import qualified LLVM.Pretty


import Debug.Trace

-- ====== Global Language Settings ======
nIntBits  = 32
nCharBits = 8
nBoolBits = 1
nUnitBits = 1
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
  deriving (Show, Eq, Ord)

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
  ApplyExpr {calleeIdent :: Ident, params :: [Expr]} | -- NOTE: params can be Expr instead of [Expr] in the future -- TODO: Rename params to args
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
tyToLLVMTy CharTy = AST.IntegerType {typeBits=nCharBits}
tyToLLVMTy BoolTy = AST.IntegerType {typeBits=nBoolBits}
tyToLLVMTy UnitTy = AST.IntegerType {typeBits=nUnitBits}

-- | Lit => LLVM Type
litToLLVMType :: Lit -> AST.Type
litToLLVMType (IntLit _)  = AST.IntegerType {typeBits=nIntBits}
litToLLVMType (CharLit _) = AST.IntegerType {typeBits=nCharBits}
litToLLVMType (BoolLit _) = AST.IntegerType {typeBits=nBoolBits}
litToLLVMType (UnitLit)   = AST.IntegerType {typeBits=nUnitBits}

-- | Expr => LLVM Type
exprToLLVMType :: Expr -> AST.Type
exprToLLVMType (LitExpr lit)       = litToLLVMType lit
exprToLLVMType (IdentExpr _)       = undefined -- TODO impl
exprToLLVMType (IfExpr {thenExpr}) = exprToLLVMType thenExpr
exprToLLVMType (ApplyExpr{})       = undefined -- TODO impl
exprToLLVMType (LetExpr{inExpr})   = exprToLLVMType inExpr


data IdentInfo =
  GVarIdentInfo {ty :: Ty, globalPtrName :: AST.Name} |
  LVarIdentInfo {ty :: Ty, localName :: AST.Name} |
  FuncIdentInfo {retTy :: Ty, paramTys :: [Ty], funcName :: AST.Name}
  deriving (Show)

type VarTable = Map Ident IdentInfo

data ExprCodegenEnv =
 ExprCodegenEnv {
   basicBlocks     :: [AST.Global.BasicBlock]
 , stackedInstrs   :: [AST.Named AST.Instruction]
 , globalVarTable  :: VarTable
 , localVarTables  :: [VarTable]
 , count           :: Int
 }
 deriving (Show)

-- TODO: Change better type
type ErrorType = String

newtype ExprCodegen a = ExprCodegen {runExprCodegen :: StateT ExprCodegenEnv (Either ErrorType) a}
  deriving (Functor, Applicative, Monad, MonadState ExprCodegenEnv)

-- | Get fresh int count (zero-origin)
getFreshCount :: ExprCodegen Int
getFreshCount = do
  c <- gets count
  modify (\env -> env {count = c+1})
  return c


-- | Lit => Operand
litToOperand :: Lit -> AST.Operand
litToOperand (IntLit i)   = AST.ConstantOperand AST.Constant.Int {AST.Constant.integerBits=nIntBits, AST.Constant.integerValue=toInteger i}
litToOperand (CharLit ch) = AST.ConstantOperand AST.Constant.Int {AST.Constant.integerBits=nCharBits, AST.Constant.integerValue=toInteger $ Data.Char.ord ch}
litToOperand (BoolLit b)  = AST.ConstantOperand AST.Constant.Int {AST.Constant.integerBits=nBoolBits, AST.Constant.integerValue=if b then 1 else 0}
litToOperand (UnitLit)    = AST.ConstantOperand AST.Constant.Int {AST.Constant.integerBits=nUnitBits, AST.Constant.integerValue=1}

-- | Add a basic block
addBasicBlock :: AST.Global.BasicBlock -> ExprCodegen ()
addBasicBlock (AST.Global.BasicBlock name instrs terminator) = do
  -- Get stacked instructions
  sInstrs <- gets stackedInstrs
  -- Create a basic block with stacked instructions
  let bb = AST.Global.BasicBlock name (sInstrs ++ instrs) terminator
  -- Clear stacked instructions
  modify (\env -> env{stackedInstrs=[]})
  -- Add a basic block
  modify (\env@ExprCodegenEnv{basicBlocks} -> env {basicBlocks = basicBlocks++[bb]})


-- | Stack an instruction
stackInstruction :: Named AST.Instruction -> ExprCodegen ()
stackInstruction instr = do
  modify (\env@ExprCodegenEnv{stackedInstrs} -> env{stackedInstrs=stackedInstrs++[instr]})

-- | Look up local variable tables
lookupLVarTables :: Ident -> [VarTable] -> Maybe IdentInfo
lookupLVarTables _     []     = Nothing
lookupLVarTables ident (v:vs) = Map.lookup ident v <|> lookupLVarTables ident vs


-- | Push & Pop a local variable map
withLVarTable :: Map Ident IdentInfo -> ExprCodegen a -> ExprCodegen a
withLVarTable lVarMap f = do
  -- Push `lVarMap` to  `localVarTables`
  modify (\env@ExprCodegenEnv{localVarTables} -> env{localVarTables=lVarMap:localVarTables})
  ret <- f
  -- Pop lVarMap` from  `localVarTables`
  modify (\env@ExprCodegenEnv{localVarTables=_:rest} -> env{localVarTables=rest})
  return ret

-- TODO: Impl
exprToExprCodegen :: Expr -> ExprCodegen AST.Operand
exprToExprCodegen (LitExpr lit) = return (litToOperand lit)
exprToExprCodegen (IdentExpr ident@(Ident name)) = do
  -- Get local variable tables
  lVarTables <- gets localVarTables
  -- Get global variable table
  gVarTable  <- gets globalVarTable
  -- Find ident from tables
  let varInfoMaybe = lookupLVarTables ident lVarTables <|> Map.lookup ident gVarTable
      notFoundMsg  = [Here.i| Identifier '${name}' is not found|]
  -- Get identifier information
  indentInfo <- ExprCodegen $ Monad.Trans.lift $ Either.Utils.maybeToEither notFoundMsg varInfoMaybe
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
  let f :: Map Ident IdentInfo -> Bind -> ExprCodegen (Map Ident IdentInfo)
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
exprToExprCodegen (ApplyExpr {calleeIdent=calleeIdent@(Ident calleeName), params}) = do
  -- Get fresh prefix number
  prefixNumber <- getFreshCount
  -- Get global variable table
  gVarTable <- gets globalVarTable
  -- Result name
  let resName = AST.Name (strToShort [Here.i|$$apply_res${prefixNumber}|])
  -- Find ident from tables
  let varInfoMaybe = Map.lookup calleeIdent gVarTable
      notFoundMsg  = [Here.i| Callee identifier '${calleeName}' not found|]
  -- Get identifier information
  identInfo <- ExprCodegen $ Monad.Trans.lift $ Either.Utils.maybeToEither notFoundMsg varInfoMaybe
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

      -- Eval args to operands
      argumentOprs <- mapM exprToExprCodegen params

      -- Get LLVM arguments
      -- NOTE: [] may be hard code
      let llvmArgs = fmap (\argExpr -> (argExpr, [])) argumentOprs

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
exprToOperandEither :: VarTable -> Expr -> Either ErrorType (AST.Operand, ExprCodegenEnv)
exprToOperandEither globalVarTable expr = runStateT (runExprCodegen $ exprToExprCodegen expr) initEnv
  where
    initEnv = ExprCodegenEnv {
                basicBlocks    = []
              , stackedInstrs  = []
              , globalVarTable = globalVarTable
              , localVarTables = []
              , count          = 0
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

-- | Generate global ptr name
genGlobalVarName :: Ident -> AST.Name
genGlobalVarName (Ident name) = AST.Name (strToShort [Here.i|${name}/ptr|])

-- | Generate function name
genFuncName :: Ident -> AST.Name
genFuncName (Ident name) = AST.Name (strToShort name)

-- Gdef => GdefCodegen
gdefToGdefCodegen :: VarTable -> Gdef -> GdefCodegen ()
gdefToGdefCodegen globalVarTable (LetGdef (Bind {ident=ident@(Ident name), ty, bodyExpr})) = do
  let globalName   = genGlobalVarName ident
      initFuncName = AST.Name (strToShort [Here.i|$$PLATY_INIT/${name}|])
      llvmTy       = tyToLLVMTy ty
      llvmPtrTy    = AST.Type.ptr llvmTy
  let globalDef = [Quote.LLVM.lldef| $gid:globalName = global $type:llvmTy undef |]
  -- Add the definition
  addDefinition globalDef
  -- Evaluate bodyExpr
  let operandEither = exprToOperandEither globalVarTable bodyExpr
  -- Get operand and env
  (bodyOperand, ExprCodegenEnv{basicBlocks, stackedInstrs}) <- GdefCodegen (Monad.Trans.lift operandEither)
  -- NOTE: Should avoid to using PLATY_GLOBAL_RES if an user uses this name then fail
  let funcDef = AST.GlobalDefinition AST.Global.functionDefaults
        { AST.Global.name        = initFuncName
        , AST.Global.parameters  =([], False)
        , AST.Global.returnType  = AST.Type.void
        , AST.Global.basicBlocks = basicBlocks ++ [restBasicBlock]
        }
        where
          restBasicBlock = AST.Global.BasicBlock
              (AST.Name "nextblock") -- FIXME: Shouldn't use "nextblock" ("nextblock" is determined by llvm-hs-quote)
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

  -- Definition of $$global_getter
  let getterFuncName = AST.Name (strToShort [Here.i|$$global_getter/${name}|])
      getterFuncDef = [Quote.LLVM.lldef|
        define $type:llvmTy $gid:getterFuncName(){
        entry:
          %res = load $type:llvmPtrTy $gid:globalName
          ret $type:llvmTy %res
        }
      |]

  -- Add a global variable getter
  addDefinition getterFuncDef
  return ()
gdefToGdefCodegen globalVarTable (FuncGdef {ident=ident@(Ident name), params, retTy, bodyExpr}) = do
  -- Evaluate bodyExpr
  let operandEither = exprToOperandEither globalVarTable bodyExpr
  -- Get operand and env
  (bodyOperand, ExprCodegenEnv{basicBlocks, stackedInstrs}) <- GdefCodegen (Monad.Trans.lift operandEither)

  let funcName  = genFuncName ident
      llvmRetTy = tyToLLVMTy retTy

  -- NOTE: Should avoid to using PLATY_GLOBAL_RES if an user uses this name then fail
  let funcDef = AST.GlobalDefinition AST.Global.functionDefaults
        { AST.Global.name        = funcName
        , AST.Global.parameters  =([], False)
        , AST.Global.returnType  = llvmRetTy
        , AST.Global.basicBlocks = basicBlocks ++ [restBasicBlock]
        }
        where
          restBasicBlock = AST.Global.BasicBlock
              (AST.Name "nextblock") -- FIXME: Shouldn't use "nextblock" ("nextblock" is determined by llvm-hs-quote)
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

  return ()

-- | [Gdef] => AST.Module
gdefsToModule :: [Gdef] -> Either ErrorType AST.Module
gdefsToModule gdefs = do
  let initEnv = GdefCodegenEnv {
                  definitions         = []
                , globalInitFuncs     = []
                }
      f (LetGdef (Bind {ident, ty})) = (ident, GVarIdentInfo {ty=ty, globalPtrName=genGlobalVarName ident})
      f (FuncGdef {ident, retTy, params})    = (ident, FuncIdentInfo {retTy=retTy, paramTys=[ty | Param {ty} <- params], funcName=genFuncName ident})
      globalVarMap = Map.fromList (fmap f gdefs)

      -- TODO: Remove `stdVarMap` in the future
      stdVarMap = Map.fromList [(Ident "print-int", FuncIdentInfo{retTy=UnitTy, paramTys=[IntTy], funcName=AST.Name "print-int"})]

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

  -- TODO: `stdlibDefs` should move to stdlib for Platy
  let stdlibDefs = [intFormatDef, printfDef, printIntDef]
        where
          intFormatDef = [Quote.LLVM.lldef|
            @int_format_str = private constant [4 x i8] c"%d\0A\00"
          |]
          intFormatTy = AST.Type.ptr AST.ArrayType {
                          AST.nArrayElements = 4
                        , AST.elementType    = AST.IntegerType {AST.typeBits = 8}
                        }
          printfDef = [Quote.LLVM.lldef|
            declare i32 @printf(i8*, ...)
          |]

          -- NOTE: `i1` is Unit Type
          printIntDef = [Quote.LLVM.lldef|
            define i1 @print-int(i32 %v){
            entry:
                %ch_ptr = $instr:gepInstr
                $instr:callPrintfInstr
                ret i1 1
            }
          |]
            where
              -- FIXME: The following values shouldn't be not necessary, because all printIntDef is like the following IR. However llvm-hs-quote has error.
--              define void @"print-int"(i32 %v) {
--              entry:
--                call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @int_format_str, i64 0, i64 0))
--                ret void
--              }
              gepInstr = Left AST.GetElementPtr {
                             AST.inBounds = False,
                             AST.address  = AST.ConstantOperand $ AST.Constant.GlobalReference intFormatTy (AST.Name "int_format_str"),
                             AST.indices  = let i64Zero = AST.ConstantOperand AST.Constant.Int {AST.Constant.integerBits=64, AST.Constant.integerValue=0} in [i64Zero, i64Zero],
                             AST.metadata = []
                           }

              printfTy = AST.Type.ptr
                AST.FunctionType {
                    AST.resultType    = AST.Type.i32
                  , AST.argumentTypes = [AST.Type.ptr AST.Type.i8]
                  , AST.isVarArg      = True
                  }

              callPrintfInstr = Left AST.Call {
                                  AST.tailCallKind       = Nothing,
                                  AST.callingConvention  = AST.CallingConvention.C,
                                  AST.returnAttributes   = [],
                                  AST.function           = Right $ AST.ConstantOperand $ AST.Constant.GlobalReference printfTy (AST.Name "printf"),
                                  AST.arguments          = [(AST.LocalReference (AST.Type.ptr AST.Type.i8) (AST.Name "ch_ptr"), []), (AST.LocalReference (AST.Type.i32) (AST.Name "v"), [])],
                                  AST.functionAttributes = [],
                                  AST.metadata           = []
                                }


  return AST.defaultModule{
    AST.moduleDefinitions =
      stdlibDefs ++ [globalCtorsDef, globalInitFuncDef]++ definitions ++ globalInitFuncs
  }


toLLVM :: AST.Module -> IO ()
toLLVM mod = Context.withContext $ \ctx -> do
  llvm <- Module.withModuleFromAST ctx mod Module.moduleLLVMAssembly
  BS.putStrLn llvm

main :: IO ()
main = do
  let expr1 = LitExpr (IntLit 1515)
  let Right (operand1, exprCodeEnv1) = exprToOperandEither Map.empty expr1
  print operand1
  print exprCodeEnv1

  let expr2 = IfExpr (LitExpr $ BoolLit True) (LitExpr $ IntLit 81818) (LitExpr $ IntLit 23232)
  let Right (operand2, exprCodeEnv2) = exprToOperandEither Map.empty expr2
  print operand2
  print exprCodeEnv2

  putStrLn("====================================")
  let gdef1 = LetGdef {bind=Bind {ident=Ident "myint", ty=IntTy, bodyExpr=IfExpr (LitExpr $ BoolLit True) (LitExpr $ IntLit 81818) (LitExpr $ IntLit 23232)}}
  let gdef2 = LetGdef {bind=Bind {ident=Ident "myint2", ty=IntTy, bodyExpr=IfExpr (LitExpr $ BoolLit True) (LitExpr $ IntLit 7117) (LitExpr $ IntLit 9889)}}
  let Right mod1 = gdefsToModule [gdef1, gdef2]
--  TIO.putStrLn (LLVM.Pretty.ppllvm mod1)
  putStrLn("----------------------------------")

  toLLVM mod1

  let gdef3 = LetGdef {bind=Bind {ident=Ident "myint", ty=IntTy, bodyExpr=LitExpr $ IntLit 2929}}
  let mod2Either = gdefsToModule [gdef3]
  let Right mod2 = mod2Either
--  print mod2
  putStrLn("----------------------------------")
--  TIO.putStrLn (LLVM.Pretty.ppllvm mod2)
--  putStrLn("----------------------------------")
--  TIO.putStrLn (LLVM.Pretty.ppll mod2)
  toLLVM mod2
  putStrLn("----------------------------------")


  let gdef4 :: Gdef
      gdef4 = FuncGdef {ident=Ident "myfunc", params=[], retTy=IntTy, bodyExpr=LitExpr $ IntLit 32323}
  let Right mod3 = gdefsToModule [gdef4]
  toLLVM mod3
  putStrLn("----------------------------------")


  let gdef5 :: Gdef
      gdef5 = FuncGdef {ident=Ident "myfunc", params=[], retTy=IntTy, bodyExpr=IfExpr (LitExpr $ BoolLit True) (LitExpr $ IntLit 5656) (LitExpr $ IntLit 767)}
  let Right mod4 = gdefsToModule [gdef5]
  toLLVM mod4
  putStrLn("----------------------------------")


  let gdef6 = LetGdef {bind=Bind {ident=Ident "myint", ty=IntTy, bodyExpr=LitExpr $ IntLit 8877}}
  let gdef7 = LetGdef {bind=Bind {ident=Ident "myint2", ty=IntTy, bodyExpr=IdentExpr $ Ident "myint"}}
  let Right mod5 = gdefsToModule [gdef6, gdef7]
  toLLVM mod5
  putStrLn("----------------------------------")

  let gdef8 = LetGdef {bind=Bind {ident=Ident "value1", ty=IntTy, bodyExpr=LetExpr {binds = [Bind{ident=Ident "a", ty=IntTy, bodyExpr=LitExpr $ IntLit 8989}, Bind{ident=Ident "b", ty=IntTy, bodyExpr=LitExpr $ IntLit 3344}], inExpr=IdentExpr (Ident "a")}}}
  let Right mod6 = gdefsToModule [gdef8]
  toLLVM mod6
  putStrLn("----------------------------------")


  let gdef9 = LetGdef {bind=Bind {ident=Ident "value1", ty=IntTy, bodyExpr=LetExpr {binds = [Bind{ident=Ident "a", ty=IntTy, bodyExpr=LitExpr $ IntLit 8989}, Bind{ident=Ident "b", ty=IntTy, bodyExpr=IdentExpr (Ident "a")}], inExpr=IdentExpr (Ident "b")}}}
  let Right mod7 = gdefsToModule [gdef9]
  toLLVM mod7
  putStrLn("----------------------------------")
  TIO.putStrLn (LLVM.Pretty.ppll mod7)
  putStrLn("----------------------------------")

  let gdef11 = FuncGdef {ident=Ident "main", params=[], retTy=UnitTy, bodyExpr=ApplyExpr{calleeIdent=Ident "print-int", params=[LitExpr $ IntLit 171717]}}
  let Right mod8 = gdefsToModule [gdef11]
  toLLVM mod8
  putStrLn("----------------------------------")




