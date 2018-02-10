{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}


module Platy.CodegenSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import qualified Data.String.Here as Here
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import Data.Map (Map)
import qualified System.IO.Temp as Temp
import qualified System.Process as Process
import qualified System.IO as IO


import qualified LLVM.Pretty

import Debug.Trace

import Platy.Datatypes
import Platy.Codegen
import Platy.Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Execution Test" $
    it "simple print main" $ do
      let gdef1 = FuncGdef
                   { ident = Ident "main"
                   , params = []
                   , retTy = UnitTy
                   , bodyExpr =
                     ApplyExpr
                     { calleeIdent = Ident "print-int"
                     , argExprs = [LitExpr $ IntLit 171717]
                     }
                   }

      -- Generate LLVM module
      let Right llvmMod1 = gdefsToModule [gdef1]
      -- Generate object byte string
      objBString <- toObjByteString llvmMod1
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
        stdout <- IO.hGetContents stdoutHandle
        -- Compare with expect
        stdout `shouldBe` "171717\n"
  return ()