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
  describe "Execution Test" $ do
    it "simple-print main" $ do
      -- [corresponding platy code] NOTE: Syntax maybe wrong
      -- (@global-let main Unit
      --    (print-int 171717)
      -- )
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

      -- Execute and Get stdout
      stdout <- TestUtils.execGdefs [gdef1]
      -- Compare with expectation
      stdout `shouldBe` "171717\n"

    it "if-true main" $ do
      -- [corresponding platy code] NOTE: Syntax maybe wrong
      -- (@global-let main Unit
      --    (print-int (@if True
      --      8877
      --      5566
      --    ))
      -- )
      let gdef1 = FuncGdef
                    { ident = Ident "main"
                    , params = []
                    , retTy = UnitTy
                    , bodyExpr =
                      ApplyExpr
                      { calleeIdent = Ident "print-int"
                      , argExprs =
                        [ IfExpr
                          { condExpr = LitExpr $ BoolLit True
                          , thenExpr = LitExpr $ IntLit 8877
                          , elseExpr = LitExpr $ IntLit 5566
                          }
                        ]
                      }
                    }

      -- Execute and Get stdout
      stdout <- TestUtils.execGdefs [gdef1]
      -- Compare with expectation
      stdout `shouldBe` "8877\n"
  return ()