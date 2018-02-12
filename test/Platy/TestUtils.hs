{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Platy.TestUtils where

import           Test.Hspec

import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Text.Lazy.IO as TIO
import qualified System.IO.Temp as Temp
import qualified System.Process as Process
import qualified System.IO as IO
import qualified Data.String.Here as Here
import qualified Data.Either as Either

import qualified LLVM.AST as AST
import qualified LLVM.Pretty

import Platy.Utils
import Platy.Datatypes
import Platy.Codegen

-- | Convert a module to executable and Run it and Get stdout
execModule :: AST.Module -> IO String
execModule llvmMod = do
  -- Generate object byte string
  objBString <- toObjByteString llvmMod
  -- Create temp directory
  Temp.withSystemTempDirectory "tempdir" $ \dirpath -> do
    -- Create empty obj file
    objfilePath <- Temp.emptyTempFile dirpath "objfile"
    -- Save obj to a file
    ByteString.writeFile objfilePath objBString
    -- Create empty executable file
    execfilePath <- Temp.emptyTempFile dirpath "execfile"
     -- Make executable file
    Process.system [Here.i|gcc ${objfilePath} -o ${execfilePath}|]
    -- Execute the program
    -- (from: http://d.hatena.ne.jp/sirocco/20110216/1297839298)
    (stdinHandle,stdoutHandle,stderrHandle,procHandle) <- Process.runInteractiveProcess execfilePath [] Nothing Nothing
    -- Get stdout
    IO.hGetContents stdoutHandle


-- | Convert Program => Module => Execute it => Stdout
execProgram :: Program -> IO String
execProgram program = do
  -- Generate LLVM module
  let llvmModEither = programToModule program
  -- Is right or not
  llvmModEither `shouldSatisfy` Either.isRight
  -- Extract module
  let Right llvmMod = llvmModEither
  Monad.when False $
    -- Print module
    TIO.putStrLn (LLVM.Pretty.ppllvm llvmMod)
  Monad.when False $
    -- Print module
    toLLVM (llvmMod)
  -- Generate object byte string
  objBString <- toObjByteString llvmMod
  -- Execute and Get stdout
  execModule llvmMod