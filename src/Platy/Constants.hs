{-# LANGUAGE OverloadedStrings #-}

module Platy.Constants where

import qualified GHC.Word as Word

import qualified LLVM.AST as AST

import Platy.Datatypes

-- ====== Global Language Settings ======
nIntBits :: Word.Word32
nIntBits  = 32

nCharBits :: Word.Word32
nCharBits = 8

nBoolBits :: Word.Word32
nBoolBits = 1

nUnitBits :: Word.Word32
nUnitBits = 1

globalInitFuncName :: AST.Name
globalInitFuncName = AST.Name "PLATY_GLOBALS_INIT"

langEntrypointFuncName :: AST.Name
langEntrypointFuncName = AST.Name "$$PLATY_MAIN"

entrypointFuncIdent :: Ident
entrypointFuncIdent = Ident "main"