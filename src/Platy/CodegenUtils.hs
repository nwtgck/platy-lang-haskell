module Platy.CodegenUtils where

import qualified Data.Char

import qualified LLVM.AST as AST
import qualified LLVM.Module as Module
import qualified LLVM.Context as Context
import qualified LLVM.Target as Target
import qualified LLVM.AST.Constant as AST.Constant

import Platy.Datatypes
import Platy.Constants

-- | Lit => Operand
litToOperand :: Lit -> AST.Operand
litToOperand (IntLit i)   = AST.ConstantOperand AST.Constant.Int {AST.Constant.integerBits=nIntBits, AST.Constant.integerValue=toInteger i}
litToOperand (CharLit ch) = AST.ConstantOperand AST.Constant.Int {AST.Constant.integerBits=nCharBits, AST.Constant.integerValue=toInteger $ Data.Char.ord ch}
litToOperand (BoolLit b)  = AST.ConstantOperand AST.Constant.Int {AST.Constant.integerBits=nBoolBits, AST.Constant.integerValue=if b then 1 else 0}
litToOperand (UnitLit)    = AST.ConstantOperand AST.Constant.Int {AST.Constant.integerBits=nUnitBits, AST.Constant.integerValue=1}


-- | Lit => LLVM Type
litToLLVMType :: Lit -> AST.Type
litToLLVMType (IntLit _)  = AST.IntegerType {AST.typeBits=nIntBits}
litToLLVMType (CharLit _) = AST.IntegerType {AST.typeBits=nCharBits}
litToLLVMType (BoolLit _) = AST.IntegerType {AST.typeBits=nBoolBits}
litToLLVMType (UnitLit)   = AST.IntegerType {AST.typeBits=nUnitBits}

-- | Ty => LLVM Type
tyToLLVMTy :: Ty -> AST.Type
tyToLLVMTy IntTy  = AST.IntegerType {AST.typeBits=nIntBits}
tyToLLVMTy CharTy = AST.IntegerType {AST.typeBits=nCharBits}
tyToLLVMTy BoolTy = AST.IntegerType {AST.typeBits=nBoolBits}
tyToLLVMTy UnitTy = AST.IntegerType {AST.typeBits=nUnitBits}