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

import qualified LLVM.Pretty

import Debug.Trace

import Platy.Datatypes
import Platy.Codegen
import Platy.Utils
import qualified Platy.TestUtils as  TestUtils

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
      -- Execute and Get stdout
      stdout <- TestUtils.execModule llvmMod1
      -- Compare with exepect
      stdout `shouldBe` "171717\n"
  return ()