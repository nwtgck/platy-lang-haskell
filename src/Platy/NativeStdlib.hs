{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO: Don't expose everything
module Platy.NativeStdlib where

import qualified LLVM.AST as AST
import qualified Data.Map as Map
import Data.Map (Map)


import Platy.Datatypes


-- | Native Global definition
data NativeGdef =
  FuncNativeGdef
  { retTy :: Ty
  , paramTys :: [Ty]
  , funcLLVMName :: AST.Name
  }


-- Native global definition table in standard library
stdlibNativeGdefMap = Map.fromList [ ( Ident "print-int"
                            , FuncNativeGdef
                              { retTy = UnitTy
                              , paramTys = [IntTy]
                              , funcLLVMName = AST.Name "print-int"
                              })
                          , ( Ident "eq-int"
                            , FuncNativeGdef
                              { retTy = BoolTy
                              , paramTys = [IntTy, IntTy]
                              , funcLLVMName = AST.Name "eq-int"
                              })
                          , ( Ident "add-int"
                            , FuncNativeGdef
                              { retTy = IntTy
                              , paramTys = [IntTy, IntTy]
                              , funcLLVMName = AST.Name "add-int"
                              })
                          , ( Ident "sub-int"
                            , FuncNativeGdef
                              { retTy = IntTy
                              , paramTys = [IntTy, IntTy]
                              , funcLLVMName = AST.Name "sub-int"
                              })
                          , ( Ident "or"
                            , FuncNativeGdef
                              { retTy = BoolTy
                              , paramTys = [BoolTy, BoolTy]
                              , funcLLVMName = AST.Name "or"
                              })
                          ]