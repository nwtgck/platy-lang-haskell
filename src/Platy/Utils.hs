module Platy.Utils where

import Data.ByteString.Short
import qualified Data.ByteString.Char8 as BS

import qualified LLVM.AST as AST
import qualified LLVM.Module as Module
import qualified LLVM.Context as Context

-- (for preventing "\22" in [Here.i])
strToShort :: String -> ShortByteString
strToShort = (toShort . BS.pack)

-- | Print Module with FFI
-- (from: https://github.com/llvm-hs/llvm-hs-examples/blob/27d0c403b7aecae810d6a2cb93fbb6a1deae8890/basic/Main.hs)
toLLVM :: AST.Module -> IO ()
toLLVM mod = Context.withContext $ \ctx -> do
  llvm <- Module.withModuleFromAST ctx mod Module.moduleLLVMAssembly
  BS.putStrLn llvm