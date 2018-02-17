{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- TODO: Don't expose everything
module Platy.NativeStdlib where

import qualified Data.Map as Map
import Data.Map (Map)

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST.Type
import qualified LLVM.Quote.LLVM as Quote.LLVM
import qualified LLVM.AST.Constant as AST.Constant
import qualified LLVM.AST.CallingConvention as AST.CallingConvention

import Platy.Datatypes
import Platy.CodegenUtils


-- | Native Global definition
data NativeGdef =
  FuncNativeGdef
  { retTy :: Ty
  , paramTys :: [Ty]
  , funcLLVMName :: AST.Name
  }


printIntLLVMName  :: AST.Name
printIntLLVMName = AST.Name "print-int"

eqIntLLVMName :: AST.Name
eqIntLLVMName    = AST.Name "eq-int"

addIntLLVMName :: AST.Name
addIntLLVMName   = AST.Name "add-int"

subIntLLVMName :: AST.Name
subIntLLVMName   = AST.Name "sub-int"

orLLVMName :: AST.Name
orLLVMName       = AST.Name "or"


-- Native global definition table in standard library
stdlibNativeGdefMap = Map.fromList [ ( Ident "print-int"
                            , FuncNativeGdef
                              { retTy = UnitTy
                              , paramTys = [IntTy]
                              , funcLLVMName = printIntLLVMName
                              })
                          , ( Ident "eq-int"
                            , FuncNativeGdef
                              { retTy = BoolTy
                              , paramTys = [IntTy, IntTy]
                              , funcLLVMName = eqIntLLVMName
                              })
                          , ( Ident "add-int"
                            , FuncNativeGdef
                              { retTy = IntTy
                              , paramTys = [IntTy, IntTy]
                              , funcLLVMName = addIntLLVMName
                              })
                          , ( Ident "sub-int"
                            , FuncNativeGdef
                              { retTy = IntTy
                              , paramTys = [IntTy, IntTy]
                              , funcLLVMName = subIntLLVMName
                              })
                          , ( Ident "or"
                            , FuncNativeGdef
                              { retTy = BoolTy
                              , paramTys = [BoolTy, BoolTy]
                              , funcLLVMName = orLLVMName
                              })
                          ]

-- | Standard library LLVM definition
stdlibLLVMDefs :: [AST.Definition]
stdlibLLVMDefs = [intFormatDef, printfDef, printIntDef, eqIntDef, addIntDef, subIntDef, orDef]
  where
    intFormatName = AST.Name "$$PLATY/int_format_str"
    intFormatDef = [Quote.LLVM.lldef|
      $gid:intFormatName = private constant [4 x i8] c"%d\0A\00"
    |]
    intFormatTy = AST.Type.ptr AST.ArrayType {
                    AST.nArrayElements = 4
                  , AST.elementType    = AST.IntegerType {AST.typeBits = 8}
                  }
    printfDef = [Quote.LLVM.lldef|
      declare i32 @printf(i8*, ...)
    |]


    printIntParamName = AST.Name "value"
    printIntParamTy   = tyToLLVMTy IntTy
    chPtrName         = AST.Name "ch_ptr"
    llvmUnitTy        = tyToLLVMTy UnitTy
    llvmUnitValue     = litToOperand UnitLit

    -- NOTE: `i1` is Unit Type
    printIntDef = [Quote.LLVM.lldef|
      define $type:llvmUnitTy $gid:printIntLLVMName($type:printIntParamTy $id:printIntParamName){
      entry:
          $id:chPtrName = $instr:gepInstr
          $instr:callPrintfInstr
          %unit_value = $opr:llvmUnitValue
          ret $type:llvmUnitTy %unit_value
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
                       AST.address  = AST.ConstantOperand $ AST.Constant.GlobalReference intFormatTy (intFormatName),
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
                            AST.arguments          = [(AST.LocalReference (AST.Type.ptr AST.Type.i8) (chPtrName), []), (AST.LocalReference (printIntParamTy) (printIntParamName), [])],
                            AST.functionAttributes = [],
                            AST.metadata           = []
                          }

    llvmBoolTy = tyToLLVMTy BoolTy
    llvmIntTy  = tyToLLVMTy IntTy

    -- eq-int
    eqIntDef = [Quote.LLVM.lldef|
      define $type:llvmBoolTy $gid:eqIntLLVMName($type:llvmIntTy %a, $type:llvmIntTy %b){
        %res = icmp eq $type:llvmIntTy %a, %b
        ret $type:llvmBoolTy %res
      }
    |]

    -- add-int
    addIntDef = [Quote.LLVM.lldef|
      define $type:llvmIntTy $gid:addIntLLVMName($type:llvmIntTy %a, $type:llvmIntTy %b){
        %res = add $type:llvmIntTy %a, %b
        ret $type:llvmIntTy %res
      }
    |]

    -- sub-int
    subIntDef = [Quote.LLVM.lldef|
      define $type:llvmIntTy $gid:subIntLLVMName($type:llvmIntTy %a, $type:llvmIntTy %b){
        %res = sub $type:llvmIntTy %a, %b
        ret $type:llvmIntTy %res
      }
    |]

    -- or
    orDef = [Quote.LLVM.lldef|
      define $type:llvmBoolTy $gid:orLLVMName($type:llvmBoolTy %a, $type:llvmBoolTy %b){
        %res = or $type:llvmBoolTy %a, %b
        ret $type:llvmBoolTy %res
      }
    |]