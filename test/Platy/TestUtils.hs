{-# LANGUAGE QuasiQuotes #-}

module Platy.TestUtils where

import qualified Data.ByteString as ByteString
import qualified System.IO.Temp as Temp
import qualified System.Process as Process
import qualified System.IO as IO
import qualified Data.String.Here as Here

import qualified LLVM.AST as AST

import Platy.Utils

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
